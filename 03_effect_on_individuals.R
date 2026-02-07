# Effect on individuals
library(tidyverse)
library(vultureUtils)
library(sf)
library(mapview)
library(tidygraph)
library(ggraph)
library(targets)
library(patchwork)
source("R/getEdges_new_2026-02-01.R")

mm_sub_recent <- readRDS("data/created/mm_sub_recent.RDS")
cluster21dates <- readRDS("data/created/cluster21dates.RDS")
cluster22dates <- readRDS("data/created/cluster22dates.RDS")
rp <- st_read("data/raw/roosts50_kde95_cutOffRegion.kml")

# Read in gps data before/after one or two of the mortalities (can use the same dataset from the previous script)
tar_load(downsampled_masked)

# Define period of time for friendships (say: previous 3 months?)
# yeah sure, let's use 3 months before the mortality event, with a 1-week buffer
death_date <- mm_sub_recent %>% filter(year_cluster_eps5min2 == 4 & year == 2021) %>% pull(date) %>% min()
end <- death_date - lubridate::days(7)
begin <- end - lubridate::days(30)

# Pull data for that period
data_subset <- downsampled_masked %>%
  filter(date_il >= begin & date_il <= end)
dim(data_subset)

# Calculate networks for that period
tar_load(stl)
tar_load(rp)
tar_load(ct)
tar_load(dt)
tar_load(idc)
tar_load(tc)

test_flight <- getEdges_new(data_subset, roostPolygons = rp, roostBuffer = 1000, consecThreshold = ct, distThreshold = dt, speedThreshUpper = NULL, speedThreshLower = stl, timeThreshold = "10 minutes", idCol = idc, return = "edges", timestampCol = tc)

# Now I'm understanding why we need the ability to put in another, larger dataset to define the dyads. The function as it's currently written calculates one SRI value, or an edgelist, for the single dataset it's given. But if what we want instead is to calculate yes/no, did they interact on this day, we're going to need to calculate this over a list of dyads. And then we need to take into account all the possible dyads.
# We could just calculate it for each element of the list (e.g. each day) and then join together all of the results and pivot, and then any dyad that didn't appear on a given day will show up as NA. I think? Let's try it.

data_subset_daylist <- data_subset %>% 
  group_by(date_il) %>% group_split()
dates <- map_chr(data_subset_daylist, ~as.character(.x$date_il[1]))

all_indivs <- sort(unique(data_subset$individual_local_identifier))
all_dyads <- expand.grid(ID1 = all_indivs, ID2 = all_indivs) %>% mutate(pair = paste(ID1, ID2, sep = "_")) %>%
  filter(as.character(ID1) < as.character(ID2)) %>%
  mutate(sri = NA)

flight_sri_eachday <- map(data_subset_daylist, ~getEdges_new(.x, roostPolygons = rp, roostBuffer = 1000, consecThreshold = ct, distThreshold = dt, speedThreshUpper = NULL, speedThreshLower = stl, timeThreshold = "10 minutes", idCol = idc, return = "sri", timestampCol = tc))
length(flight_sri_eachday)
flight_sri_eachday_binary <- map(flight_sri_eachday, ~mutate(.x, sri = case_when(sri > 0 ~ 1, sri == 0 ~ 0, .default = NA)))
flight_sri_eachday_binary <- map2(flight_sri_eachday_binary, dates, ~mutate(.x, date = lubridate::ymd(.y)))

all <- purrr::list_rbind(flight_sri_eachday_binary)

# Reshape by dyads
wide <- all %>%
  pivot_wider(id_cols = c("ID1", "ID2", "pair"), names_from = "date", values_from = "sri", names_prefix = "date_") %>%
  filter(!is.na(ID1) & !is.na(ID2) & !is.na(pair))

if(any(!(all_dyads$pair %in% wide$pair))){
  toadd <- all_dyads %>%
    filter(!(pair %in% wide$pair))
  wide <- bind_rows(wide, toadd) # bind_rows will automatically populate NAs for all the days. If there's a pair that interacts for even one of the days, even if they never are detected together again, they will have NAs for the other dates just because of the pivot_wider from before. Convenient!
}

get_counts <- function(wide_df) {
  mat <- wide_df %>%
    select(starts_with("date_")) %>%
    as.matrix()
  
  out <- tibble(
    ID1    = wide_df$ID1,
    ID2    = wide_df$ID2,
    pair   = wide_df$pair,
    ones   = rowSums(mat == 1, na.rm = TRUE),
    zeroes = rowSums(mat == 0, na.rm = TRUE),
    nas    = rowSums(is.na(mat))
  )
  return(out)
}

actuals <- get_counts(wide)

# Perform randomization
## We need to shuffle within each day column, excluding the NAs
# testdf <- data.frame(pair = c("A", "B", "C"),
#                      date1 = c(NA, 0, 1),
#                      date2 = c(1, 0, NA),
#                      date3 = c(1, 1, 0))

shuffle <- function(df){
  shuffled <- df %>%
    mutate(across(-c("pair", "ID1", "ID2"), ~{
      x <- .
      x[!is.na(x)] <- sample(x[!is.na(x)], replace = FALSE)
      x}))
  return(shuffled)
}

shuffled_100 <- map(1:100, ~shuffle(wide))
length(shuffled_100)

counts_100 <- map(shuffled_100, get_counts)
counts_100_df <- purrr::list_rbind(counts_100, names_to = "iter")

counts_100_bypair_list <- counts_100_df %>%
  ungroup() %>%
  group_by(ID1, ID2, pair) %>%
  group_split(.keep = T)

actuals_bypair_list <- actuals %>%
  group_by(ID1, ID2, pair) %>%
  group_split(.keep = T)

all(map_chr(actuals_bypair_list, ~.x$pair[1]) == map_chr(counts_100_bypair_list, ~.x$pair[1])) # check that the pairs are in the same order

head(counts_100_bypair_list[[1]])

# Plot some examples
six_random <- sample(actuals$pair, 6)

counts_100_df %>%
  filter(pair %in% six_random) %>%
  ggplot(aes(x = ones))+
  geom_density()+
  theme_minimal()+
  facet_wrap(~pair) +
  geom_vline(data = actuals[actuals$pair %in% six_random,], aes(xintercept = ones), col = "red")

test_results <- map2(counts_100_bypair_list, actuals_bypair_list, ~{
  if(.x$pair[1] != .y$pair[1]){
    stop("pairs not equal")
  }else{
    test_result <- t.test(.x$ones, mu = .y$ones[1], alternative = "greater", conf.level = (0.95/length(counts_100_bypair_list)))
  }
  return(test_result)
}) # XXX I don't understand what's going on with these intervals. Need to revisit with fresh eyes, I think.

test_results_uncorrected <- map2(counts_100_bypair_list, actuals_bypair_list, ~{
  if(.x$pair[1] != .y$pair[1]){
    stop("pairs not equal")
  }else{
    test_result <- t.test(.x$ones, mu = .y$ones[1], alternative = "greater", conf.level = 0.95)
  }
  return(test_result)
})

pvals <- map_dbl(test_results, "p.value")
pvals_uncorrected <- map_dbl(test_results_uncorrected, "p.value")

out <- map2(counts_100_bypair_list, pvals, ~{
  bind_cols(.x[1,], "p" = .y)
}) %>% purrr::list_rbind() %>% select(ID1, ID2, pair, p)

out_uncorrected <- map2(counts_100_bypair_list, pvals_uncorrected, ~{
  bind_cols(.x[1,], "p" = .y)
}) %>% purrr::list_rbind() %>% select(ID1, ID2, pair, p)

out %>%
  ggplot(aes(x = p))+
  geom_density()+
  theme_minimal()+
  geom_vline(aes(xintercept = 0.01), color = "red")+
  labs(y = "Density", x = "p-value (after Bonferroni correction)",
       title = "Flight nets, 30 days")

out_uncorrected %>%
  ggplot(aes(x = p))+
  geom_density()+
  theme_minimal()+
  geom_vline(aes(xintercept = 0.01), color = "red")+
  labs(y = "Density", x = "p-value (uncorrected)",
       title = "Flight nets, 30 days")
# after applying a bonferroni correction, almost nobody shows up as "friends" from the flight network.

