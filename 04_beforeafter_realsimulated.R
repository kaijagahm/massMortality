# Flack-style before and after
# For each mortality, get:
# A: real network before
# A': real network before, but with individuals that die "removed" in silico
# B: real network after

library(targets)
library(tidyverse)
library(sf)
library(vultureUtils)
library(tidygraph)
library(ggraph)
source("R/functions.R")
source("R/getEdges_new_2026-02-01.R")

# Let's give a 1-day buffer before and after each death cluster, and then take a 5-day window backwards and forwards
deaths_clustered <- readRDS("data/created/mm_sub_recent.RDS")

# 1. Read in death cluster data
# 2. Read in GPS data before/after deaths--make sure it's cleaned and downsampled.
tar_load(downsampled_masked)
range(downsampled_masked$date_il) # this is quite a big, long-term dataset.

# 3. Write a function that pulls gps data for before/after
cluster_dates <- deaths_clustered %>% 
  filter(cluster_eps5min2 != 0) %>%
  group_by(cluster_eps5min2) %>%
  summarize(mindate = min(date), maxdate = max(date)) %>%
  mutate(before_start_date = mindate-lubridate::days(6), before_end_date = mindate-lubridate::days(1),
         after_start_date = maxdate+lubridate::days(1),
         after_end_date = maxdate+lubridate::days(6))
nms <- cluster_dates$cluster_eps5min2

# 4. Obtain real before/after GPS data for each death cluster event
cluster_before_data <- map2(cluster_dates$before_start_date, cluster_dates$before_end_date, ~{
  downsampled_masked %>% filter(date_il >= .x & date_il <= .y)
})
names(cluster_before_data) <- nms

cluster_after_data <- map2(cluster_dates$after_start_date, cluster_dates$after_end_date, ~{
  downsampled_masked %>% filter(date_il >= .x & date_il <= .y)
})
names(cluster_after_data) <- nms
map_dbl(cluster_after_data, nrow) # we're going to need to download data from a greater length of time for this to work properly. For now, we just have a somewhat limited amount.

# 5. Artificially remove individuals from the before data
whodied <- deaths_clustered %>% 
  filter(cluster_eps5min2 != 0) %>%
  group_by(cluster_eps5min2) %>%
  group_split() %>%
  map(., "id")

cluster_before_removed <- map2(cluster_before_data, whodied, ~{
  removed <- .x %>% 
    filter(!(individual_local_identifier %in% .y)) %>%
    mutate(n_removed = sum(.y %in% .x$individual_local_identifier),
           n_died = length(.y),
           n_in_network_before = length(unique(.x$individual_local_identifier)),
           prop_removed = n_removed/n_in_network_before)
  return(removed)
})
names(cluster_before_removed) <- nms

# 6. Calculate social networks for before, before-removed, and after data (flight, feeding, roost)
## get roosts
roosts_all <- vultureUtils::get_roosts_df(downsampled_masked, id = "individual_local_identifier", timestamp = "timestamp_il")

## split roost data into the same subsets (but with the date range expanded backwards one day)
cluster_before_roosts <- map2(cluster_dates$before_start_date, cluster_dates$before_end_date, ~{
  roosts_all %>% filter(roost_date >= .x-lubridate::days(1) & roost_date <= .y)
})
names(cluster_before_roosts) <- nms

cluster_after_roosts <- map2(cluster_dates$after_start_date, cluster_dates$after_end_date, ~{
  roosts_all %>% filter(roost_date >= .x-lubridate::days(1) & roost_date <= .y)
})
names(cluster_after_data) <- nms

cluster_before_removed_roosts <- map2(cluster_before_roosts, whodied, ~{
  removed <- .x %>% 
    filter(!(individual_local_identifier %in% .y)) %>%
    mutate(n_removed = sum(.y %in% .x$individual_local_identifier),
           n_died = length(.y),
           n_in_network_before = length(unique(.x$individual_local_identifier)),
           prop_removed = n_removed/n_in_network_before)
  return(removed)
})
names(cluster_before_removed_roosts) <- nms

## flight
tar_load(ct)
tar_load(dt)
tar_load(stl)
tar_load(idc)
tar_load(r)
tar_load(tc)
tar_load(rp)
flight_sris_before <- purrr::map(cluster_before_data, ~{
  if(nrow(.x) > 0){
    getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp)
  }else{NULL}
}) 
flight_graph_before <- map(flight_sris_before, ~{
  if(!is.null(.x)){as_tbl_graph(filter(select(.x, ID1, ID2, sri), sri > 0), directed = F)}})
metrics_before <- map(flight_graph_before, ~{
  if(!is.null(.x)){
    network_metrics(.x, weight = "sri")
  }else{NULL}
})
names(metrics_before) <- nms

flight_sris_after <- purrr::map(cluster_after_data, ~{
  if(nrow(.x) > 0){
    getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp)
  }else{NULL}
}) 
flight_graph_after <- map(flight_sris_after, ~{
  if(!is.null(.x)){as_tbl_graph(filter(select(.x, ID1, ID2, sri), sri > 0), directed = F)}})
metrics_after <- map(flight_graph_after, ~{
  if(!is.null(.x)){
    network_metrics(.x, weight = "sri")
  }else{NULL}
})
names(metrics_after) <- nms

flight_sris_before_removed <- purrr::map(cluster_before_removed, ~{
  if(nrow(.x) > 0){
    getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp)
  }else{NULL}
}) 
flight_graph_before_removed <- map(flight_sris_before_removed, ~{
  if(!is.null(.x)){as_tbl_graph(filter(select(.x, ID1, ID2, sri), sri > 0), directed = F)}})
metrics_before_removed <- map(flight_graph_before_removed, ~{
  if(!is.null(.x)){
    network_metrics(.x, weight = "sri")
  }else{NULL}
})
names(metrics_before_removed) <- nms

# 7. Compile data frame, which can be joined back to the death cluster data later: clusterID, cluster_early_date, cluster_late_date, cluster_drtn_days, n_died, before_start_date, before_end_date, after_start_date, after_end_date, n_in_network_before, prop_died, n_in_network_after, social_situation, measure_density, measure_degree_assortativity, measure_average_path_length

df_before <- purrr::list_rbind(map(metrics_before, "network_metrics"), names_to = "cluster") %>% mutate(period = "before")
df_after <- purrr::list_rbind(map(metrics_after, "network_metrics"), names_to = "cluster") %>% mutate(period = "after")
df_before_removed <- purrr::list_rbind(map(metrics_before_removed, "network_metrics"), names_to = "cluster") %>% mutate(period = "before_removed")
df <- bind_rows(df_before, df_after, df_before_removed) %>%
  mutate(cluster = as.numeric(cluster)) %>%
  left_join(cluster_dates, by = c("cluster" = "cluster_eps5min2")) %>%
  mutate(cluster_drtn_days = as.numeric(difftime(maxdate, mindate, units = "days"))+1)

rem <- map(cluster_before_removed, ~{
  .x %>% sf::st_drop_geometry() %>% select(n_removed, n_died, n_in_network_before, prop_removed) %>%
    distinct()
})
names(rem) <- nms
rem <- purrr::list_rbind(rem, names_to = "cluster") %>% mutate(cluster = as.numeric(cluster))

df <- left_join(df, rem, by = "cluster")

# Some exploratory data analysis ------------------------------------------
## how will we deal with the number of individuals in the network?
df %>%
  ggplot(aes(x = n_died, y = n_removed))+
  geom_point()+
  geom_smooth(method = "lm") # we have a very small sample size because in a lot of these cases, the ones that died weren't tracked, so they didn't get removed from the data.

df %>%
  filter(n_removed > 0) %>%
  pivot_longer(cols = c("density", "avg_path_length", "assort_degree"), names_to = "measure", values_to = "value") %>%
  mutate(period = factor(period, levels = c("before", "before_removed", "after"))) %>%
  mutate(cluster = droplevels(factor(cluster, levels = 1:21)))%>%
  ggplot(aes(x = cluster, y = value, color = period))+
  geom_point(size = 5, pch  =1)+
  scale_color_manual(values = c("skyblue1", "skyblue4", "firebrick"))+
  theme_minimal()+
  facet_wrap(~measure, scales = "free_y")

# Maybe the next move is to take a random sampling of how different 5-day periods separated by a small amount of time are from each other, and then see if these differences are statistically significant?

