library(tidyverse)
library(move2)
library(sf)
library(lubridate)
library(targets)
library(ctmm)
library(future)
library(furrr)
library(progressr)
library(ks)
library(here)

tar_load(cleaned, store = here("~/Desktop/projects/MvmtSoc/_targets/"))
cleaned <- cleaned %>% mutate(timestamp_il = lubridate::with_tz(timestamp, tzone = "Israel"),
                              date_il = lubridate::date(timestamp_il))

tar_load(deaths)
deaths_simple <- deaths %>%
  select(Nili_id, date_death, death_kg, death_kg_detail) %>%
  distinct() %>%
  arrange(date_death)

# ── 1. Create mm_focal ────────────────────────────────────────────────────────
# Keep only deaths that fall within [earliest GPS date + 60 days, latest GPS date]

gps_start <- min(cleaned$dateOnly, na.rm = TRUE)
gps_end   <- max(cleaned$dateOnly, na.rm = TRUE)
mm_sub_recent <- read_rds(here("data/created/mm_sub_recent.RDS"))

mm_focal <- mm_sub_recent |>
  filter(
    as.Date(date) >= gps_start + days(60),
    as.Date(date) <= gps_end
  )


# ── 2. Re-index clusters so singletons get unique cluster IDs ─────────────────
# cluster_eps5min2 == 0 → singleton; any other value → true cluster.
# We keep true-cluster IDs as-is and assign new integer IDs to each singleton.

max_cluster <- max(mm_focal$cluster_eps5min2[mm_focal$cluster_eps5min2 != 0],
                   na.rm = TRUE)

mm_focal <- mm_focal |>
  mutate(
    cluster_idx = case_when(
      cluster_eps5min2 != 0 ~ cluster_eps5min2,          # keep real cluster ID
      TRUE                  ~ max_cluster + row_number()  # unique ID per singleton
    )
  )
# Note: row_number() is evaluated within the full (already-filtered) data frame,
# so each singleton row gets a distinct offset above max_cluster.


# ── 3 & 4. Compute cluster start date, then split into a named list ───────────

mm_focal <- mm_focal |>
  group_by(cluster_idx) |>
  mutate(cluster_start = min(as.Date(date), na.rm = TRUE)) |>
  ungroup()

deaths_list <- mm_focal |>
  group_by(cluster_idx) |>
  group_split()          # one tibble per cluster; preserves cluster_start

# Optionally name the list elements by their cluster_idx for easy reference
names(deaths_list) <- map_chr(deaths_list, \(x) as.character(x$cluster_idx[1]))


# ── 5. Build three GPS-subset lists (2 months / 1 month / 5 days before) ──────
# For each death cluster, subset cleaned to:
#   • 60 days before  → day before cluster_start  (≈ 2 months)
#   • 30 days before  → day before cluster_start  (≈ 1 month)
#   •  5 days before  → day before cluster_start

subset_gps <- function(cluster_tbl, days_back) {
  ref    <- cluster_tbl$cluster_start[1]        # earliest death in this cluster
  window_start <- ref - days(days_back)
  window_end   <- ref - days(1)                 # day *before* first death
  
  cleaned |>
    filter(dateOnly >= window_start,
           dateOnly <= window_end)
}

gps_2mo_list  <- map(deaths_list, subset_gps, days_back = 60)
gps_1mo_list  <- map(deaths_list, subset_gps, days_back = 30)
gps_5d_list   <- map(deaths_list, subset_gps, days_back = 5)


# ── 6. Convert each GPS subset to a move2 object ──────────────────────────────
# • Uses Nili_id as the track identifier.
# • Single-point tracks (only 1 fix for an individual in the window) are made
#   valid by duplicating the point and adding a tiny spatial jitter (± ~1 mm).

JITTER_M <- 0.001   # 1 mm in metres — invisible but valid for UTM

fix_single_point_tracks <- function(gps_df) {
  counts <- gps_df |>
    st_drop_geometry() |>
    count(Nili_id)
  
  singles <- counts |> filter(n == 1) |> pull(Nili_id)
  
  if (length(singles) == 0) return(gps_df)
  
  jittered_rows <- gps_df |>
    filter(Nili_id %in% singles) |>
    mutate(
      timestamp    = timestamp + seconds(1),
      timestamp_il = timestamp_il + seconds(1),
      geometry     = geometry + c(JITTER_M, JITTER_M)
    )
  
  st_crs(jittered_rows) <- st_crs(gps_df)
  
  bind_rows(gps_df, jittered_rows) |>
    arrange(Nili_id, timestamp)
}

to_move2 <- function(gps_df) {
  if (nrow(gps_df) == 0) return(NULL)
  
  gps_df <- gps_df |> st_transform(crs = 32636)   # reproject to UTM zone 36N
  gps_df <- fix_single_point_tracks(gps_df)
  
  mt_as_move2(
    gps_df,
    crs             = 32636,
    time_column     = "timestamp",
    track_id_column = "Nili_id"
  )
}

move2_2mo_list <- map(gps_2mo_list, to_move2)
move2_1mo_list <- map(gps_1mo_list, to_move2)
move2_5d_list  <- map(gps_5d_list,  to_move2)

# ── Helper: max pairwise distance among each day's points, then average ────────
daily_max_displacement <- function(pts_utm) {
  days <- pts_utm |>
    group_by(dateOnly) |>
    group_split()
  
  daily_diam <- map_dbl(days, \(day_pts) {
    # Faster approximation via convex hull diameter
    if (nrow(day_pts) < 2) return(NA_real_)
    hull  <- st_convex_hull(st_union(day_pts))
    verts <- st_cast(hull, "POINT")
    max(st_distance(verts), na.rm = TRUE)
  })
  
  tibble(
    mean_daily_disp_m = mean(daily_diam, na.rm = TRUE),
    sd_daily_disp_m   = sd(daily_diam,   na.rm = TRUE)
  )
}

# ── Helper: sum of step lengths per day, then average across days ──────────────
daily_distance_traveled <- function(pts_utm) {
  pts_sorted <- pts_utm |>
    arrange(Nili_id, timestamp) |>
    group_by(Nili_id, dateOnly) |>
    mutate(
      x    = st_coordinates(geometry)[, 1],
      y    = st_coordinates(geometry)[, 2],
      step = sqrt((x - lag(x))^2 + (y - lag(y))^2)
    ) |>
    summarise(daily_dist = sum(step, na.rm = TRUE), .groups = "drop")
  
  tibble(
    mean_daily_dist_m = mean(pts_sorted$daily_dist, na.rm = TRUE),
    sd_daily_dist_m   = sd(pts_sorted$daily_dist,   na.rm = TRUE)
  )
}

# ── Helper: proportion of daytime points classified as in-flight ───────────────
flight_proportion <- function(pts_df) {
  day_pts <- pts_df |> filter(daylight == "day")
  if (nrow(day_pts) == 0) return(tibble(prop_daylight_flight = NA_real_))
  tibble(prop_daylight_flight = mean(day_pts$ground_speed > 4, na.rm = TRUE))
}

# ── Helper: KDE via ks ──────────────────────────────────────────────────────
AKDE_MIN_PTS  <- 20   # fewer than this → skip entirely
AKDE_WARN_PTS <- 30   # between this and MIN → warn but attempt

fit_kde <- function(pts_utm, nili_id) {
  n <- nrow(pts_utm)
  
  if (n < AKDE_MIN_PTS) {
    return(tibble(kde_95_km2 = NA_real_, kde_50_km2 = NA_real_))
  }
  
  if (n < AKDE_WARN_PTS) {
    warning(glue::glue(
      "Individual {nili_id}: only {n} points — KDE estimate may be unreliable."
    ))
  }
  
  tryCatch({
    coords <- st_coordinates(pts_utm)   # matrix of X, Y in metres
    
    # Plug-in bandwidth matrix — accounts for point density and spread
    H    <- Hpi(coords)
    fit  <- kde(coords, H = H)
    
    # Extract contour areas at 95% and 50% usage
    contour_area <- function(fit, level) {
      # Find the density value corresponding to this UD level
      ct      <- contourLevels(fit, prob = 1 - level)
      # Convert contour to polygons via sf and compute area
      cl      <- contourLines(fit$eval.points[[1]],
                              fit$eval.points[[2]],
                              fit$estimate,
                              levels = ct)
      if (length(cl) == 0) return(NA_real_)
      
      polys <- map(cl, \(c) st_polygon(list(cbind(c$x, c$y)))) |>
        st_sfc(crs = 32636) |>
        st_union()
      
      as.numeric(st_area(polys)) / 1e6   # m² → km²
    }
    
    tibble(
      kde_95_km2 = contour_area(fit, 0.95),
      kde_50_km2 = contour_area(fit, 0.50)
    )
  }, error = function(e) {
    warning(glue::glue("KDE failed for {nili_id}: {conditionMessage(e)}"))
    tibble(kde_95_km2 = NA_real_, kde_50_km2 = NA_real_)
  })
}

# ── Core: compute all metrics for one individual in one window ─────────────────
compute_metrics_one <- function(pts, nili_id) {
  if (is.null(pts) || nrow(pts) == 0) {
    return(tibble(
      Nili_id              = nili_id,
      n_fixes              = 0L,
      mean_daily_disp_m    = NA_real_,
      sd_daily_disp_m      = NA_real_,
      mean_daily_dist_m    = NA_real_,
      sd_daily_dist_m      = NA_real_,
      prop_daylight_flight = NA_real_,
      kde_95_km2          = NA_real_,
      kde_50_km2          = NA_real_
    ))
  }
  
  bind_cols(
    tibble(Nili_id = nili_id, n_fixes = nrow(pts)),
    daily_max_displacement(pts),
    daily_distance_traveled(pts),
    flight_proportion(pts),
    fit_kde(pts, nili_id), glue::glue("{nili_id} | kde")
  )
}

# ── Core: compute metrics for all individuals in one move2 object ──────────────
compute_metrics_move2 <- function(mv2_obj) {
  if (is.null(mv2_obj)) return(NULL)
  
  # Split move2 object into per-individual sf tibbles
  track_ids <- mt_track_id(mv2_obj)
  
  mv2_obj |>
    mutate(.track = as.character(track_ids)) |>
    group_by(.track) |>
    group_split() |>
    map_dfr(\(ind_df) compute_metrics_one(ind_df, ind_df$.track[1]))
}

# ── Apply across all three window lists ───────────────────────────────────────
# Each list has one element per death cluster; each element is a move2 object.
# We map over clusters and windows, tagging each row with its identifiers.
plan(multisession, workers = 10)

compute_metrics_for_list <- function(mv2_list, window_label, prog) {
  future_imap_dfr(mv2_list, \(mv2_obj, cluster_name) {
    prog()    # tick once per cluster
    compute_metrics_move2(mv2_obj) |>
      mutate(cluster_idx = cluster_name,
             window      = window_label)
  }, .options = furrr_options(seed = TRUE))
}

# Total ticks = number of clusters × 3 windows
n_ticks <- length(move2_2mo_list)

with_progress({
  prog <- progressor(steps = n_ticks)
  
  metrics_all <- bind_rows(
    # compute_metrics_for_list(move2_2mo_list, "2mo", prog),
    # compute_metrics_for_list(move2_1mo_list, "1mo", prog),
    compute_metrics_for_list(move2_5d_list,  "5d",  prog)
  ) |>
    relocate(cluster_idx, window, Nili_id) |>
    arrange(cluster_idx, window, Nili_id)
})


# Return to sequential execution when done
plan(sequential)


# ── Test harness: pull one individual from one cluster ─────────────────────────
# Run these lines first to get a single sf tibble to test with

test_mv2  <- move2_2mo_list[[1]]   # first cluster, 2-month window
test_ids  <- as.character(mt_track_id(test_mv2))
test_id   <- unique(test_ids)[1]   # first individual

# Extract as plain sf tibble — this is what each helper receives
test_pts  <- test_mv2[test_ids == test_id, ]

cat("n points:  ", nrow(test_pts), "\n")
cat("date range:", as.character(range(test_pts$dateOnly)), "\n")
cat("n days:    ", n_distinct(test_pts$dateOnly), "\n")
# ── Test 1: daily max displacement ────────────────────────────────────────────
system.time(
  res_disp <- daily_max_displacement(test_pts)
)
res_disp

# ── Test 2: daily distance traveled ───────────────────────────────────────────
system.time(
  res_dist <- daily_distance_traveled(test_pts)
)
res_dist

# ── Test 3: flight proportion ──────────────────────────────────────────────────
system.time(
  res_flight <- flight_proportion(test_pts)
)
res_flight

# ── Test 4: KDE ───────────────────────────────────────────────────────────────
system.time(
  res_kde <- fit_kde(test_pts, test_id)
)
res_kde

# ── Test 5: full single-individual pipeline ───────────────────────────────────
system.time(
  res_one <- compute_metrics_one(test_pts, test_id)
)
res_one

# ── Test 6: full single-cluster pipeline (all individuals in one cluster) ──────
system.time(
  res_cluster <- compute_metrics_move2(test_mv2)
)
res_cluster
