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
  mutate(before_start_date = mindate-lubridate::days(5), before_end_date = mindate-lubridate::days(1),
         after_start_date = maxdate+lubridate::days(1),
         after_end_date = maxdate+lubridate::days(5))
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
# Named list of who died, keyed by cluster ID
whodied <- deaths_clustered %>%
  filter(cluster_eps5min2 != 0) %>%
  split(.$cluster_eps5min2) %>%
  map(~ .x$id)

# Reorder explicitly to match cluster_dates, then verify
whodied <- whodied[as.character(nms)]
stopifnot(
  !anyNA(names(whodied)),
  identical(names(whodied), as.character(nms)),
  identical(names(cluster_before_data), as.character(nms))
)

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
names(cluster_after_roosts) <- nms

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
# tar_load(ct)
# tar_load(dt)
# tar_load(stl)
# tar_load(idc)
# tar_load(r)
# tar_load(tc)
# tar_load(rp)
# flight_sris_before <- purrr::map(cluster_before_data, ~{
#   if(nrow(.x) > 0){
#     getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp)
#   }else{NULL}
# }) 
# flight_graph_before <- map(flight_sris_before, ~{
#   if(!is.null(.x)){as_tbl_graph(filter(select(.x, ID1, ID2, sri), sri > 0), directed = F)}})
# metrics_before <- map(flight_graph_before, ~{
#   if(!is.null(.x)){
#     network_metrics(.x, weight = "sri")
#   }else{NULL}
# })
# names(metrics_before) <- nms
# 
# flight_sris_after <- purrr::map(cluster_after_data, ~{
#   if(nrow(.x) > 0){
#     getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp)
#   }else{NULL}
# }) 
# flight_graph_after <- map(flight_sris_after, ~{
#   if(!is.null(.x)){as_tbl_graph(filter(select(.x, ID1, ID2, sri), sri > 0), directed = F)}})
# metrics_after <- map(flight_graph_after, ~{
#   if(!is.null(.x)){
#     network_metrics(.x, weight = "sri")
#   }else{NULL}
# })
# names(metrics_after) <- nms
# 
# flight_sris_before_removed <- purrr::map(cluster_before_removed, ~{
#   if(nrow(.x) > 0){
#     getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp)
#   }else{NULL}
# }) 
# flight_graph_before_removed <- map(flight_sris_before_removed, ~{
#   if(!is.null(.x)){as_tbl_graph(filter(select(.x, ID1, ID2, sri), sri > 0), directed = F)}})
# metrics_before_removed <- map(flight_graph_before_removed, ~{
#   if(!is.null(.x)){
#     network_metrics(.x, weight = "sri")
#   }else{NULL}
# })
# names(metrics_before_removed) <- nms
# 
# # 7. Compile data frame, which can be joined back to the death cluster data later: clusterID, cluster_early_date, cluster_late_date, cluster_drtn_days, n_died, before_start_date, before_end_date, after_start_date, after_end_date, n_in_network_before, prop_died, n_in_network_after, social_situation, measure_density, measure_degree_assortativity, measure_average_path_length
# 
# df_before <- purrr::list_rbind(map(metrics_before, "network_metrics"), names_to = "cluster") %>% mutate(period = "before")
# df_after <- purrr::list_rbind(map(metrics_after, "network_metrics"), names_to = "cluster") %>% mutate(period = "after")
# df_before_removed <- purrr::list_rbind(map(metrics_before_removed, "network_metrics"), names_to = "cluster") %>% mutate(period = "before_removed")
# df <- bind_rows(df_before, df_after, df_before_removed) %>%
#   mutate(cluster = as.numeric(cluster)) %>%
#   left_join(cluster_dates, by = c("cluster" = "cluster_eps5min2")) %>%
#   mutate(cluster_drtn_days = as.numeric(difftime(maxdate, mindate, units = "days"))+1)
# write_rds(df, file = "data/created/df.RDS")
df <- readRDS("data/created/df.RDS")

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
  mutate(cluster = factor(cluster, levels = sort(unique(cluster)))) %>%    ggplot(aes(x = cluster, y = value, color = period))+
  geom_point(size = 5, pch  =1)+
  scale_color_manual(values = c("olivedrab2", "olivedrab4", "darkorange"))+
  theme_minimal()+
  facet_wrap(~measure, scales = "free_y")+
  labs(y = "Value", x = "Deaths cluster", )+
  theme(legend.position = "bottom")
clusters_kept <- df %>% filter(n_removed > 0) %>% pull(cluster) %>% unique() %>% sort()

#Next move is to take a random sampling of how different 5-day periods separated by a small amount of time are from each other, and then see if these differences are statistically significant?
# XXX also should come back to this later and look at roosts

## Get roosts for the entire period so we can just select from them later!

# pairs of 5-day periods separated by whatever the average death period length is
inter_period_length <- mean(df$cluster_drtn_days, na.rm = T) #2.82, so let's use three days
# draw pairs from the past 6 months before each of the deaths clusters.
## for each death, get 6mos before and 6mos after
## draw random dates from this period
## before period: date-5 through date-1 
## after period: date+3 through date+7
# after drawing each pair, check that no deaths occur: for entire period--before start through after end--cross-reference with complete known deaths table and see if any are present. If so, remove. [maybe skip this for now?]
# keep track of the individuals present in before/after, but don't remove any individuals for now
## grab list of unique individuals for before/after period and save them
# calculate networks for each and get the network measures
## Get SRIs for each period before/after
## Make networks for each period before/after
## Calculate network measures for each period before/after
## Save info and write out.

## Do all of this 1000 times or whatever.

earlies <- cluster_dates$mindate - lubridate::days(180)
lates <- cluster_dates$maxdate + lubridate::days(180)
intervals <- map2(earlies, lates, ~seq(from = .x, to = .y, by = "day"))
n <- 100
after_ends <- map(intervals, ~{sample(.x, size = n, replace = F)})
after_beginnings <- map(after_ends, ~{.x-days(4)})
before_ends <- map(after_ends, ~{.x-days(7)})
before_beginnings <- map(after_ends, ~{.x-days(11)})

# split the data by date so we can easily reference it
dates <- downsampled_masked %>% group_by(date_il) %>% group_split(.keep = T)
names(dates) <- map_chr(dates, ~as.character(.x$date_il[1]))
rm(downsampled_masked)

## define some helpers
### get dataset
get_dataset <- function(begin_date, end_date){
  sequence <- as.character(seq(from = begin_date, to = end_date, by = "day"))
  dat_subsets <- dates[names(dates) %in% sequence]
  dat <- purrr::list_rbind(dat_subsets)
  return(dat)
}

### get network and metrics from that dataset
get_network_and_metrics <- function(dataset){
  if(nrow(dataset) > 0){
    sris <- getEdges_new(dataset = dataset, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp)
    grph <- as_tbl_graph(filter(select(sris, ID1, ID2, sri), sri > 0), directed = F)
    metr <- network_metrics(grph, weight = "sri")
    return(metr)
  }else{
    NULL
  }
}

# outs <- vector(mode = "list", length = length(before_beginnings))
# for(i in 1:length(before_beginnings)){
#   metrs <- vector(mode = "list", length = length(before_beginnings[[i]]))
#   for(j in 1:length(before_beginnings[[i]])){
#     before_beginning <- before_beginnings[[i]][[j]]
#     before_end <- before_ends[[i]][[j]]
#     after_beginning <- after_beginnings[[i]][[j]]
#     after_end <- after_ends[[i]][[j]]
#     before_dat <- get_dataset(before_beginning, before_end)
#     after_dat <- get_dataset(after_beginning, after_end)
#     
#     before_metr <- suppressMessages(get_network_and_metrics(before_dat))
#     after_metr <- suppressMessages(get_network_and_metrics(after_dat))
#     metrs[[j]] <- list(before_metr, after_metr)
#     cat("for death cluster", i, ", completed iteration", j, "\n")
#   }
#   outs[[i]] <- metrs
#   cat("completed death cluster", i, "\n")
# }
#write_rds(outs, file = "data/created/outs.RDS")
outs <- readRDS("data/created/outs.RDS")

# Compile the data into a more usable format:
# One long data frame per cluster, network-level metrics only
metrs_by_cluster <- imap(outs, function(reps, cluster_i) {
  reps |>
    imap(function(rep_list, rep_i) {
      bind_rows(
        before = rep_list[[1]]$network_metrics,
        after  = rep_list[[2]]$network_metrics,
        .id = "period"
      ) |>
        mutate(rep = rep_i, .before = 1)
    }) |>
    list_rbind() |>
    mutate(cluster = cluster_i, .before = 1)
}) %>%
  purrr::list_rbind() %>%
  group_by(cluster, rep) %>%
  arrange(period, .by_group = T) %>%
  summarize(density_diff = density[1]-density[2],
            assort_degree_diff = assort_degree[1]-assort_degree[2],
            avg_path_length_diff = avg_path_length[1]-avg_path_length[2]) %>%
  ungroup() %>%
  filter(cluster %in% clusters_kept)

df_diffs <- df %>%
  filter(cluster %in% clusters_kept) %>%
  filter(period %in% c("after", "before")) %>%
  group_by(cluster) %>%
  arrange(period, .by_group = T) %>%
  summarize(density_diff = density[1]-density[2],
            assort_degree_diff = assort_degree[1]-assort_degree[2],
            avg_path_length_diff = avg_path_length[1]-avg_path_length[2]) %>%
  ungroup()

ggplot(data = metrs_by_cluster)+
  aes(x = factor(cluster), y = density_diff)+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  geom_point(data = df_diffs, color = "red", size = 2)+
  labs(title = "Density", x = "Death cluster",
       y = "Δ Density (after-before)",
       caption = "Boxplots derived from 100 randomly selected pairs of 5-day periods,\n separated by 3 days, within the 360 days surrounding the mortality event.")

ggplot(data = metrs_by_cluster)+
  aes(x = factor(cluster), y = avg_path_length_diff)+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  geom_point(data = df_diffs, color = "dodgerblue", size = 2)+
  labs(title = "Average path length", x = "Death cluster",
       y = "Δ Average path length (after-before)",
       caption = "Boxplots derived from 100 randomly selected pairs of 5-day periods,\n separated by 3 days, within the 360 days surrounding the mortality event.")

ggplot(data = metrs_by_cluster)+
  aes(x = factor(cluster), y = assort_degree_diff)+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  geom_point(data = df_diffs, color = "purple", size = 2)+
  labs(title = "Degree assortativity", x = "Death cluster",
       y = "Δ Degree assortativity (after-before)",
       caption = "Boxplots derived from 100 randomly selected pairs of 5-day periods,\n separated by 3 days, within the 360 days surrounding the mortality event.")

dfs <- vector(mode = "list", length = length(clusters_kept))
names(dfs) <- clusters_kept
for(i in clusters_kept){
  obs_dens <- df_diffs$density_diff[df_diffs$cluster == i]
  obs_ad <- df_diffs$assort_degree_diff[df_diffs$cluster == i]
  obs_apl <- df_diffs$avg_path_length_diff[df_diffs$cluster == i]
  
  rand_dens <- metrs_by_cluster$density_diff[metrs_by_cluster$cluster == i]
  rand_ad <- metrs_by_cluster$assort_degree_diff[metrs_by_cluster$cluster == i]
  rand_apl <- metrs_by_cluster$avg_path_length_diff[metrs_by_cluster$cluster == i]
  
  obs_gr_rand_dens <- mean(obs_dens > rand_dens)
  obs_gr_rand_ad <- mean(obs_ad > rand_ad)
  obs_gr_rand_apl <- mean(obs_apl > rand_apl)
  dfs[[as.character(i)]] <- data.frame("obs_gr_rand_dens" = obs_gr_rand_dens, 
                    "obs_gr_rand_ad" = obs_gr_rand_ad, 
                    "obs_gr_rand_apl" = obs_gr_rand_apl)
}
ps <- purrr::map(clusters_kept, function(i) {
  data.frame(
    cluster = i,
    obs_gr_rand_dens = mean(df_diffs$density_diff[df_diffs$cluster == i] >
                              metrs_by_cluster$density_diff[metrs_by_cluster$cluster == i]),
    obs_gr_rand_ad   = mean(df_diffs$assort_degree_diff[df_diffs$cluster == i] >
                              metrs_by_cluster$assort_degree_diff[metrs_by_cluster$cluster == i]),
    obs_gr_rand_apl  = mean(df_diffs$avg_path_length_diff[df_diffs$cluster == i] >
                              metrs_by_cluster$avg_path_length_diff[metrs_by_cluster$cluster == i])
  )
}) %>%
  purrr::list_rbind()

ps_long <- ps %>%
  pivot_longer(cols = starts_with("obs"), names_to = "param", values_to = "value") %>%
  mutate(param = str_remove(param, "obs_gr_rand_")) %>%
  mutate(param = case_when(param == "ad" ~ "assort_degree",
                           param == "apl" ~ "avg_path_length",
                           param == "dens" ~ "density")) #%>%
  # ungroup() %>%
  # group_by(param) %>%
  # mutate(value_adj = p.adjust(value, method = "BH")) %>%
  # ungroup()


ps_long %>%
  ggplot(aes(x = factor(cluster), y = value, color = param))+
  geom_point(aes(alpha = abs(value - 0.5) * 2), size = 2)+ # symmetrical alpha scaling
  scale_alpha_continuous(range = c(0.01, 1), limits = c(0, 1),
                     name = "Distance from 0.5")+
  theme_minimal()+
  labs(y = "Prop obs > rand", color = "Network measure", x = "Deaths cluster", title = "Obs vs. random (raw proportions)")+
  guides(alpha = "none")+
  scale_color_manual(values = c("purple", "dodgerblue", "red"))+
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
  geom_hline(aes(yintercept = 0.95))+
  geom_hline(aes(yintercept = 0.05))
# 
# ps_long %>%
#   ggplot(aes(x = factor(cluster), y =  value_adj, color = param))+
#   geom_point(aes(alpha = abs(value_adj - 0.5) * 2), size = 2)+ # symmetrical alpha scaling
#   scale_alpha_continuous(range = c(0.01, 1), limits = c(0, 1),
#                          name = "Distance from 0.5")+
#   theme_minimal()+
#   labs(y = "Prop obs > rand, FDR-corrected", color = "Network measure", x = "Deaths cluster", title = "Obs vs. random--corrected proportions")+
#   guides(alpha = "none")+
#   scale_color_manual(values = c("red", "dodgerblue", "purple"))+
#   theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
#   geom_hline(aes(yintercept = 0.95))+
#   geom_hline(aes(yintercept = 0.05))
  