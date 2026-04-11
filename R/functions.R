get_loginObject <- function(pw){
  load(pw)
  loginObject <- move::movebankLogin(username = "kaijagahm", password = pw)
  rm(pw)
  return(loginObject)
}

get_ww <- function(ww_file, date_cols){
  file <- read_excel(ww_file, sheet = "all gps tags")
  fixed_dates <- file %>%
    mutate(across(all_of(date_cols), ~as.Date(as.numeric(.x), origin = "1899-12-30")))
  return(fixed_dates)
}

get_deaths <- function(ww, min_date, max_date){
  ww %>%
    dplyr::filter(!is.na(date_death)) %>%
    dplyr::filter(date_death >= min_date, date_death <= max_date) %>%
    dplyr::select(Nili_id, date_death, `death/hospital`, comments, death_kg,	death_kg_detail)
}

fix_names_ages <- function(gps, ww_file){
  ww <- readxl::read_excel(ww_file, sheet = "all gps tags")
  # pull out just the names columns, nothing else, and remove any duplicates
  ww_tojoin <- ww %>% dplyr::select(Nili_id, Movebank_id, birth_year) %>% distinct() 
  
  # Prepare for join: are there any individuals in the `local_identifier` column of `joined0` that don't appear in the `Movebank_id` column of `ww_tojoin`?
  problems <- gps %>% filter(!(individual_local_identifier %in% ww_tojoin$Movebank_id)) %>% pull(individual_local_identifier) %>% unique()
  
  # problems # check this against the who's who and try to make changes. The only problem is E60w.
  out <- gps %>%
    left_join(ww_tojoin, by = c("individual_local_identifier" = "Movebank_id"))
  
  out <- out %>%
    mutate(Nili_id = case_when(is.na(Nili_id) & individual_local_identifier == "E60w" ~ "gili", .default = Nili_id),
           birth_year = case_when(individual_local_identifier == "E60w" ~ ww$birth_year[ww$Nili_id == "gili"][1], .default = birth_year))
  
  out <- out %>%
    mutate(age_2021 = 2021-birth_year,
           age_2022 = 2022-birth_year)
  
  return(out)
}

process_deployments <- function(ww_file,
                                movebank_dataset,
                                default_end_date = as.Date("2023-09-15"),
                                verbose = TRUE) {
  #Packages
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("purrr",     quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  
  # Read in the who's who
  ww <- read_excel(ww_file, sheet = "all gps tags")
  ww <- ww %>%
    mutate(across(c("deploy_on_date", "deploy_off_date"), as.numeric))
  
  #Clean deployment dates
  ww <- ww %>%
    dplyr::mutate(
      deploy_on_date  = as.Date(deploy_on_date, origin = "1899-12-30"),
      deploy_off_date = as.Date(deploy_off_date, origin = "1899-12-30")
    )
  
  ww$deploy_off_date[is.na(ww$deploy_off_date)] <- default_end_date
  
  #Remove rows with identical on/off dates
  equal_dates <- ww %>%
    dplyr::filter(!is.na(deploy_on_date),
                  !is.na(deploy_off_date),
                  deploy_on_date == deploy_off_date)
  
  if (verbose && nrow(equal_dates) > 0) {
    message("Check the data: ",
            nrow(equal_dates),
            " record(s) where deploy_on_date equals deploy_off_date. Removing them.")
  }
  
  ww <- dplyr::anti_join(ww, equal_dates,
                         by = c("Nili_id", "deploy_on_date", "deploy_off_date"))
  
  #Flag deploy_off < deploy_on
  invalid_periods <- ww %>%
    dplyr::filter(!is.na(deploy_on_date),
                  !is.na(deploy_off_date),
                  deploy_off_date < deploy_on_date)
  
  if (verbose && nrow(invalid_periods) > 0) {
    message("Warning: Found ",
            nrow(invalid_periods),
            " deployment period(s) where deploy_off_date < deploy_on_date:")
    print(invalid_periods$Nili_id)
  }
  
  #Expand each deployment period to a daily sequence
  periods_to_keep <- ww %>%
    dplyr::select(trnsmt_id, deploy_on_date, deploy_off_date) %>%
    dplyr::mutate(
      deploy_on_date  = lubridate::ymd(deploy_on_date),
      deploy_off_date = lubridate::ymd(deploy_off_date),
      deploy_off_date = dplyr::if_else(is.na(deploy_off_date),
                                       default_end_date,
                                       deploy_off_date)
    ) %>%
    dplyr::filter(!is.na(deploy_on_date),
                  deploy_off_date >= deploy_on_date) %>%
    dplyr::group_by(trnsmt_id) %>%
    dplyr::mutate(date = purrr::map2(
      deploy_on_date, deploy_off_date,
      ~ seq(.x, .y, by = "1 day"))
    ) %>%
    tidyr::unnest(date) %>%
    dplyr::ungroup() %>%
    dplyr::select(trnsmt_id, date) %>%
    dplyr::distinct() %>%
    dplyr::mutate(keep = TRUE)
  
  #Join back to Movebank data
  valid_periods_to_keep <- movebank_dataset %>%
    mutate(date = lubridate::date(timestamp)) %>%
    dplyr::left_join(periods_to_keep, by = c("tag_local_identifier" = "trnsmt_id", "date")) %>%
    mutate(keep = case_when(!(tag_local_identifier %in% periods_to_keep$trnsmt_id) ~ T,
                            .default = keep)) %>%
    filter(keep == TRUE)
  
  return(valid_periods_to_keep)
}

clean_data <- function(dataset){
  dataset <- dataset %>%
    mutate(across(c("barometric_height", "external_temperature", "ground_speed", "heading", "gps_satellite_count", "height_above_msl"), as.numeric), dateOnly = lubridate::date(timestamp))
  cleaned <- vultureUtils::cleanData(dataset = dataset,
                                     precise = F,
                                     removeVars = F,
                                     longCol = "location_long",
                                     latCol = "location_lat",
                                     idCol = "tag_local_identifier",
                                     report = F)
  return(cleaned)
}

downsample_10min <- function(data){
  # Define function for downsampling 
  down <- function(df, mins){
    df <- dplyr::arrange(df, timestamp)
    howmuch <- paste(mins, "minute", sep = " ")
    rounded <- lubridate::round_date(df$timestamp, howmuch)
    keep <- !duplicated(rounded)
    return(df[keep,])
  }
  
  # Split by individual so we can downsample more efficiently
  lst <- group_split(data, tag_local_identifier)
  
  # Perform downsampling
  downsampled_10min <- purrr::list_rbind(map(lst, ~down(df = .x, mins = 10), .progress = T))
  
  return(downsampled_10min)
}

# Split gps data into overlapping increments
split_overlapping <- function(gps, days){
  start_date <- as.Date(min(gps$timestamp, na.rm = T))
  end_date   <- as.Date(max(gps$timestamp, na.rm = T))
  
  window_starts <- seq(from = start_date, to   = end_date - days(days-1),  # last full window
                       by = "1 day")
  
  # make list of windows
  gps_windows <- purrr::map(
    window_starts, function(win_start){
      win_end <- win_start + days(days-1)
      gps %>% filter(timestamp >= as.POSIXct(win_start) &
                       timestamp <  as.POSIXct(win_end + days(1)))}
  )
  
  # name list elements with their cutoff dates
  names(gps_windows) <- map_chr(
    window_starts,
    function(win_start) {
      win_end <- win_start + days(days-1)
      paste0(
        format(win_start, "%Y.%m.%d"),
        "_",
        format(win_end, "%Y.%m.%d")
      )
    }
  )
  return(gps_windows)
}

split_overlapping_roosts <- function(roosts, days){
  start_date <- as.Date(min(roosts$roost_date, na.rm = T))
  end_date   <- as.Date(max(roosts$roost_date, na.rm = T))
  
  window_starts <- seq(from = start_date, to   = end_date - days(days-1),  # last full window
                       by = "1 day")
  
  # make list of windows
  gps_windows <- purrr::map(
    window_starts, function(win_start){
      win_end <- win_start + days(days-1)
      roosts %>% filter(roost_date >= as.POSIXct(win_start) &
                       roost_date <  as.POSIXct(win_end + days(1)))}
  )
  
  # name list elements with their cutoff dates
  names(gps_windows) <- map_chr(
    window_starts,
    function(win_start) {
      win_end <- win_start + days(days-1)
      paste0(
        format(win_start, "%Y.%m.%d"),
        "_",
        format(win_end, "%Y.%m.%d")
      )
    }
  )
  return(gps_windows)
}

make_sri_df <- function(mylist){
  out <- purrr::list_rbind(mylist, names_to = "period") %>% mutate(year = 2021) %>% separate_wider_delim(cols = "period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd))
  return(out)
}

get_layout <- function(dfs){
  all_nodes <- purrr::list_rbind(dfs) %>%
    select(ID1, ID2) %>%
    pivot_longer(everything()) %>%
    distinct(value) %>%
    rename(name = value)
  
  edges_union <- purrr::list_rbind(dfs) %>%
    filter(sri > 0 & !is.na(sri)) %>%
    group_by(ID1, ID2) %>%
    summarise(sri = mean(sri), .groups = "drop")
  
  g_union <- tbl_graph(nodes = all_nodes, edges = edges_union, directed = FALSE)
  
  g_layout <- g_union %>% activate(edges) %>%
    mutate(layout_weight = sri)
  
  layout_tbl <- create_layout(g_layout, layout  = "fr",
                              weights = layout_weight)
  return(layout_tbl)
}

plot_network_fixed <- function(edges, layout_tbl, title = NULL) {
  
  # which nodes are present in THIS network?
  present_nodes <- unique(c(edges$ID1, edges$ID2))
  
  g <- tbl_graph(
    nodes = layout_tbl %>% select(name),
    edges = edges %>% filter(sri > 0 & !is.na(sri)),
    directed = FALSE
  ) %>%
    activate(nodes) %>%
    left_join(layout_tbl, by = "name") %>%
    mutate(present = name %in% present_nodes)
  
  plot <- ggraph(g, layout = "manual", x = x, y = y) +
    geom_edge_link(aes(alpha = sri)) +
    
    ## hide absent nodes
    geom_node_point(aes(alpha = present), size = 2, show.legend = F) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
    
    ## hide labels for absent nodes
    geom_node_text(
      family = "Arial",
      aes(label = if_else(present, name, "")),
      repel = TRUE,
      size = 3,
      color = "blue"
    ) +
    
    ggtitle(title)
  out <- list("graph" = g, "plot" = plot)
  return(out)
}

network_metrics <- function(g, weight = "sri", loops = FALSE) {
  
  nodes <- c((g %>% activate(edges) %>% filter(sri > 0) %>% pull(from)),
             (g %>% activate(edges) %>% filter(sri > 0) %>% pull(to))) %>% unique()
  n <- length(nodes)
  
  ## ---- Global metrics ----
  density <- g %>%
    activate(nodes) %>%
    igraph::edge_density(loops = loops)
  
  assort_degree <- igraph::assortativity(g, values = igraph::degree(g))
  avg_path_length <- igraph::mean_distance(g)
  
  ## ---- Node-level metrics ----
  node_tbl <- g %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree(),
           degree_rel = centrality_degree()/n,
           strength = centrality_degree(weights = sri),
           strength_rel = centrality_degree(weights = sri)/n,
           n = n) %>%
    as_tibble()
  
  ## ---- Network-level metrics ----
  network_tbl <- tibble(density = density, 
                        assort_degree = assort_degree,
                        avg_path_length = avg_path_length) 
  
  ## ---- Output ----
  list(
    network_metrics = network_tbl,
    node_metrics  = node_tbl
  )
}

get_network_metrics_df <- function(metrics, dys){
  out <- purrr::list_rbind(map(metrics, "network_metrics"), names_to = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), days = dys)
  return(out)
}

get_node_metrics_df <- function(metrics, dys){
  out <- map_dfr(metrics, "node_metrics", .id = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), days = dys)
  return(out)
}

get_first_sightings <- function(gps, stmh, gps_spd, ddf, dds, seeds){
  first_sightings <- gps %>%
    filter(time_since_carcass >= 0 & time_since_carcass <= stmh) %>%
    filter((ground_speed > gps_spd & dist_to_carcass <= ddf) |
             (ground_speed <= gps_spd & dist_to_carcass <= dds)) %>%
    group_by(individual_local_identifier) %>%
    arrange(time_since_carcass, timestamp) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(time_since_carcass) %>%
    filter(!(individual_local_identifier %in% seeds))
  return(first_sightings)
}

get_seeds <- function(gps, ddf, dds, gps_spd, time_col, stb_mins){
  time_window_hrs <- stb_mins/60
  gps %>%
    filter(.data[[time_col]] >= -time_window_hrs &
             .data[[time_col]] <= 0 & 
             ((ground_speed > gps_spd & dist_to_carcass <= ddf) |
                (ground_speed <= gps_spd & dist_to_carcass <= dds))) %>%
    pull(individual_local_identifier) %>%
    as.character() %>%
    unique()
}

get_gps_all <- function(carcs, gps_combined, days_after, days_before){
  gps_all <- vector(mode = "list", length = length(carcs))
  for(i in 1:length(carcs)){
    ic <- carcs[[i]]
    cid <- ic$carcID[1]
    carcass_datetime <- ic$datetime[1]
    out <- gps_combined %>%
      select(-c("location_long", "location_lat")) %>%
      filter(timestamp >= (carcass_datetime-days(days_before)) & timestamp <= (carcass_datetime + days(days_after+1))) %>%
      mutate(dist_to_carcass = as.numeric(st_distance(., ic)),
             time_since_carcass = difftime(timestamp, carcass_datetime, units = "hours"),
             carcID = cid)
    out <- sf::st_as_sf(out, crs = 32636) %>% st_transform("WGS84") %>% bind_cols(st_coordinates(.)) %>%
      rename("location_long" = X, "location_lat" = Y)
    gps_all[[i]] <- out
    cat("done with", i, "\n")
  }
  return(gps_all)
}

get_first_sightings <- function(gps, stmh, gps_spd, ddf, dds, seeds){
  first_sightings <- gps %>%
    filter(time_since_carcass >= 0 & time_since_carcass <= stmh) %>%
    filter((ground_speed > gps_spd & dist_to_carcass <= ddf) |
             (ground_speed <= gps_spd & dist_to_carcass <= dds)) %>%
    group_by(individual_local_identifier) %>%
    arrange(time_since_carcass, timestamp) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(time_since_carcass) %>%
    filter(!(individual_local_identifier %in% seeds))
  return(first_sightings)
}

get_daylight_hours <- function(gps, carc){
  suntimes <- suncalc::getSunlightTimes(date = sort(unique(gps$date_il)), lat = 31.434306, lon = 34.991889, keep = c("sunrise", "sunset"), tz = "Israel") %>% 
    dplyr::select("date_il" = date, sunrise, sunset)
  
  gps <- dplyr::left_join(gps, suntimes, by = "date_il")
  gps <- gps %>% dplyr::filter(timestamp_il >= sunrise & timestamp_il <= sunset)
  
  night_df <- suntimes %>%
    dplyr::filter(date_il >= lubridate::date(carc$date)) %>% # only stuff since the carcass
    dplyr::mutate(prev_sunset = dplyr::lag(sunset),
                  night_hrs = as.numeric(difftime(sunrise, prev_sunset, units = "hours")),
                  cumul_night_hrs = cumsum(ifelse(is.na(night_hrs), 0, night_hrs)) + night_hrs*0,
                  cumul_night_hrs = tidyr::replace_na(cumul_night_hrs, 0))
  
  gps <- gps %>%
    dplyr::left_join(dplyr::select(night_df, date_il, cumul_night_hrs), by = "date_il") %>%
    dplyr::mutate(daytime_since_carcass = dplyr::case_when(time_since_carcass >= 0 ~ as.numeric(time_since_carcass)-cumul_night_hrs,
                                                           .default = NA)) %>%
    dplyr::arrange(timestamp_il)
  return(gps)
}

make_shared_layout <- function(graphlist, node_col = "name", layout_fun = igraph::layout_with_fr) {
  
  if(!is.null(names(graphlist))){
    graphnames <- names(graphlist)
  }else{
    graphnames <- paste0("graph_", 1:length(graphlist))
  }
  
  # 1. build unified node set
  all_nodes <- graphlist %>%
    map(~ .x %>%
          activate(nodes) %>%
          as_tibble() %>%
          select(!!sym(node_col))) %>%
    bind_rows() %>%
    distinct()
  
  # 2. create empty union graph
  g_union <- tbl_graph(
    nodes = all_nodes,
    edges = tibble(from = integer(), to = integer())
  )
  
  # 3. compute layout once
  coords <- layout_fun(as.igraph(g_union))
  
  layout_tbl <- all_nodes %>%
    mutate(
      x = coords[, 1],
      y = coords[, 2]
    )
  
  # 4. join layout back to each graph
  graphlist_modified <- graphlist %>%
    map(~ .x %>%
          activate(nodes) %>%
          left_join(layout_tbl, by = node_col))
  names(graphlist_modified) <- graphnames
  
  # 5. return both
  list(
    graphlist_modified = graphlist_modified,
    layout = layout_tbl
  )
}
