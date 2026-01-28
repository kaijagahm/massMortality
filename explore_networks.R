# Exploring networks before/after mortalities
library(tidyverse)
library(vultureUtils)
library(sf)
library(mapview)
library(tidygraph)
library(ggraph)
library(targets)
library(patchwork)
tar_load(downsampled_masked_2021)
tar_load(downsampled_masked_2022)

# Some font stuff for the plots (attempt to make the warnings stop appearing, not really successful, oh well.)
windowsFonts(Arial = windowsFont("Arial"))
theme_set(
  theme_graph(base_family = "Arial") +
    theme(text = element_text(family = "Arial"))
)

mm_sub_recent <- readRDS("data/created/mm_sub_recent.RDS")
cluster21dates <- readRDS("data/created/cluster21dates.RDS")
cluster22dates <- readRDS("data/created/cluster22dates.RDS")
rp <- st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
source("getEdges_new.R")

# Dedicated before/after time windows
# gps_before_21 <- downsampled_masked %>% filter(date_il >= cluster21dates[[1]][1],
#                                         date_il <= cluster21dates[[1]][2]) %>% st_as_sf(remove = F)
# gps_after_21 <- downsampled_masked %>% filter(date_il >= cluster21dates[[2]][1],
#                                       date_il <= cluster21dates[[2]][2])%>% st_as_sf(remove = F)
# gps_before_22 <- downsampled_masked %>% filter(date_il >= cluster22dates[[1]][1],
#                                         date_il <= cluster22dates[[1]][2])%>% st_as_sf(remove = F)
# gps_after_22 <- downsampled_masked %>% filter(date_il >= cluster22dates[[2]][1],
#                                        date_il <= cluster22dates[[2]][2])%>% st_as_sf(remove = F)

# Split gps data into overlapping increments
split_overlapping <- function(gps, days){
  start_date <- as.Date(min(gps$timestamp))
  end_date   <- as.Date(max(gps$timestamp))
  
  window_starts <- seq(from = start_date, to   = end_date - days(days-1),  # last full window
    by = "1 day")
  
  # make list of 5-day windows
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

windows_21 <- split_overlapping(downsampled_masked_2021, days = 5)
windows_22 <- split_overlapping(downsampled_masked_2022, days = 5)

windows_21_3 <- split_overlapping(downsampled_masked_2021, days = 3)
windows_22_3 <- split_overlapping(downsampled_masked_2022, days = 3)

windows_21_10 <- split_overlapping(downsampled_masked_2021, days = 10)
windows_22_10 <- split_overlapping(downsampled_masked_2022, days = 10)

# # Let's get which individuals died
# mm_sub_recent
# cluster21 <- mm_sub_recent %>% filter(year_cluster_eps5min2 == 4 & year == 2021)
# cluster22 <- mm_sub_recent %>% filter(year_cluster_eps5min2 == 2 & year == 2022)
# died21 <- cluster21$id
# died21 %in% gps_before_21$individual_local_identifier
# died21 %in% gps_after_21$individual_local_identifier # well it's a relief that none of these guys show up after they're dead!
# died22 <- cluster22$id
# died22 %in% gps_before_22$individual_local_identifier
# died22 %in% gps_after_22$individual_local_identifier # also here, nobody shows up after. good!
# 
# # what's up with the ones that don't show up?
# # example: died21[3]
# died21[3]
# tar_load(ww)
# # hmm, can't find it, but I can't tell if I just didn't match the names up correctly or not.

# Let's skip this for now, in the interest of time. Will need to do more careful names matching, but for now, I'm going to make the networks before and after and just see what they look like.

ct <- 1
dt <- 1000
stl <- 4
idc <- "individual_local_identifier"
r <- "sri"
tc <- "timestamp_il"

sris_2021 <- map(windows_21, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))
sris_2022 <- map(windows_22, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))

sris_2021_3 <- map(windows_21_3, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))
sris_2022_3 <- map(windows_22_3, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))

sris_2021_10 <- map(windows_21_10, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))
sris_2022_10 <- map(windows_22_10, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))
# sri_before_21 <- getEdges_new(dataset = gps_before_21, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 
# sri_after_21 <- getEdges_new(dataset = gps_after_21, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 
# sri_before_22 <- getEdges_new(dataset = gps_before_22, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 
# sri_after_22 <- getEdges_new(dataset = gps_after_22, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 

# sri_21 <- left_join(sri_before_21, sri_after_21, by = c("ID1", "ID2", "pair"), suffix = c("_before", "_after")) %>% pivot_longer(cols = starts_with("sri_"), values_to = "sri", names_to = "beforeafter")%>% mutate(beforeafter = factor(str_remove(beforeafter, "sri_"), levels = c("before", "after")))
# sri_22 <- left_join(sri_before_22, sri_after_22, by = c("ID1", "ID2", "pair"), suffix = c("_before", "_after")) %>% pivot_longer(cols = starts_with("sri_"), values_to = "sri", names_to = "beforeafter") %>% mutate(beforeafter = factor(str_remove(beforeafter, "sri_"), levels = c("before", "after")))

# sris <- sri_21 %>% mutate(year = 2021) %>% bind_rows(sri_22 %>% mutate(year = 2022))

make_sri_df <- function(mylist){
  out <- purrr::list_rbind(mylist, names_to = "period") %>% mutate(year = 2021) %>% separate_wider_delim(cols = "period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd))
  return(out)
}

sris_2021_df <- make_sri_df(sris_2021)
sris_2022_df <- make_sri_df(sris_2022)

sris_2021_df_3 <- make_sri_df(sris_2021_3)
sris_2022_df_3 <- make_sri_df(sris_2022_3)

sris_2021_df_10 <- make_sri_df(sris_2021_10)
sris_2022_df_10 <- make_sri_df(sris_2022_10)

# SRI over time for all dyads. Very slow. Interesting that it seems to be going slightly upward overall, at least for 2021. I wonder why?
# sris_2021_df %>%
#   ggplot(aes(x = period_start, y = log(sri), group = pair))+
#   geom_line(alpha = 0.1)+
#   theme_minimal()
# 
# sris_2022_df %>%
#   ggplot(aes(x = period_start, y = log(sri), group = pair))+
#   geom_line(alpha = 0.1)+
#   theme_minimal()

gs_2021 <- map(sris_2021, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))
gs_2022 <- map(sris_2022, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))

gs_2021_3 <- map(sris_2021_3, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))
gs_2022_3 <- map(sris_2022_3, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))

gs_2021_10 <- map(sris_2021_10, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))
gs_2022_10 <- map(sris_2022_10, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))

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

layout_2021 <- get_layout(sris_2021)
layout_2022 <- get_layout(sris_2022)

layout_2021_3 <- get_layout(sris_2021_3)
layout_2022_3 <- get_layout(sris_2022_3)

layout_2021_10 <- get_layout(sris_2021_10)
layout_2022_10 <- get_layout(sris_2022_10)

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

plots_graphs_2021 <- map2(sris_2021, names(sris_2021), ~plot_network_fixed(.x, layout_2021, title = .y))
plots_graphs_2022 <- map2(sris_2022, names(sris_2022), ~plot_network_fixed(.x, layout_2022, title = .y))
plots_2021 <- map(plots_graphs_2021, "plot")
graphs_2021 <- map(plots_graphs_2021, "graph")
plots_2022 <- map(plots_graphs_2022, "plot")
graphs_2022 <- map(plots_graphs_2022, "graph")

plots_graphs_2021_3 <- map2(sris_2021_3, names(sris_2021_3), ~plot_network_fixed(.x, layout_2021_3, title = .y))
plots_graphs_2022_3 <- map2(sris_2022_3, names(sris_2022_3), ~plot_network_fixed(.x, layout_2022_3, title = .y))
plots_2021_3 <- map(plots_graphs_2021_3, "plot")
graphs_2021_3 <- map(plots_graphs_2021_3, "graph")
plots_2022_3 <- map(plots_graphs_2022_3, "plot")
graphs_2022_3 <- map(plots_graphs_2022_3, "graph")

plots_graphs_2021_10 <- map2(sris_2021_10, names(sris_2021_10), ~plot_network_fixed(.x, layout_2021_10, title = .y))
plots_graphs_2022_10 <- map2(sris_2022_10, names(sris_2022_10), ~plot_network_fixed(.x, layout_2022, title = .y))
plots_2021_10 <- map(plots_graphs_2021_10, "plot")
graphs_2021_10 <- map(plots_graphs_2021_10, "graph")
plots_2022_10 <- map(plots_graphs_2022_10, "plot")
graphs_2022_10 <- map(plots_graphs_2022_10, "graph")

# walk2(plots_2021, names(plots_2021), ~ggsave(.x, filename = paste0("data/created/rollingwindow_networks/days_5/2021/", .y, ".png"), width = 6, height = 6))
# walk2(plots_2022, names(plots_2022), ~ggsave(.x, filename = paste0("data/created/rollingwindow_networks/days_5/2022/", .y, ".png"), width = 6, height = 6))
# 
# walk2(plots_2021_3, names(plots_2021_3), ~ggsave(.x, filename = paste0("data/created/rollingwindow_networks/days_3/2021/", .y, ".png"), width = 6, height = 6))
# walk2(plots_2022_3, names(plots_2022_3), ~ggsave(.x, filename = paste0("data/created/rollingwindow_networks/days_3/2022/", .y, ".png"), width = 6, height = 6))
# 
# walk2(plots_2021_10, names(plots_2021_10), ~ggsave(.x, filename = paste0("data/created/rollingwindow_networks/days_10/2021/", .y, ".png"), width = 6, height = 6))
# walk2(plots_2022_10, names(plots_2022_10), ~ggsave(.x, filename = paste0("data/created/rollingwindow_networks/days_10/2022/", .y, ".png"), width = 6, height = 6))

# Interesting! Just anecdotally, the networks look less dense. Not sure if they actually are. It would be interesting to track the density over time; I would be really confused if it actually turns out that the poisoning had this huge an impact.
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

# Apply to graphs
metrics_2021 <- map(graphs_2021, network_metrics, weight = "sri")
metrics_2022 <- map(graphs_2022, network_metrics, weight = "sri")

metrics_2021_3 <- map(graphs_2021_3, network_metrics, weight = "sri")
metrics_2022_3 <- map(graphs_2022_3, network_metrics, weight = "sri")

metrics_2021_10 <- map(graphs_2021_10, network_metrics, weight = "sri")
metrics_2022_10 <- map(graphs_2022_10, network_metrics, weight = "sri")

# network-level metrics
get_network_metrics_df <- function(metrics, yr, dys){
  out <- purrr::list_rbind(map(metrics, "network_metrics"), names_to = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), year = yr, days = dys)
  return(out)
}
network_metrics_2021 <- get_network_metrics_df(metrics_2021, yr = 2021, dys = 5)
network_metrics_2022 <- get_network_metrics_df(metrics_2022, yr = 2022, dys = 5)

network_metrics_2021_3 <- get_network_metrics_df(metrics_2021_3, yr = 2021, dys = 3)
network_metrics_2022_3 <- get_network_metrics_df(metrics_2022_3, yr = 2022, dys = 3)

network_metrics_2021_10 <- get_network_metrics_df(metrics_2021_10, yr = 2021, dys = 10)
network_metrics_2022_10 <- get_network_metrics_df(metrics_2022_10, yr = 2022, dys = 10)

get_node_metrics_df <- function(metrics, yr, dys){
  out <- map_dfr(metrics, "node_metrics", .id = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), year = yr, days = dys)
  return(out)
}

# node-level metrics
node_metrics_2021 <- get_node_metrics_df(metrics_2021, yr = 2021, dys = 5)
node_metrics_2022 <- get_node_metrics_df(metrics_2021, yr = 2022, dys = 5)

node_metrics_2021_3 <- get_node_metrics_df(metrics_2021_3, yr = 2021, dys = 3)
node_metrics_2022_3 <- get_node_metrics_df(metrics_2021_3, yr = 2022, dys = 3)

node_metrics_2021_10 <- get_node_metrics_df(metrics_2021_10, yr = 2021, dys = 10)
node_metrics_2022_10 <- get_node_metrics_df(metrics_2021_10, yr = 2022, dys = 10)


death_date_2021_min <- lubridate::ymd("2021-10-24")
death_date_2021_max <- lubridate::ymd("2021-10-24")
death_date_2022_min <- lubridate::ymd("2022-10-12")
death_date_2022_max <- lubridate::ymd("2022-10-15")
death_df <- data.frame(death_min = c(death_date_2021_min, death_date_2022_min),
                       death_max = c(death_date_2021_max, death_date_2022_max),
                       year = c(2021, 2022))

network_metrics_all <- purrr::list_rbind(list(network_metrics_2021, network_metrics_2022, network_metrics_2021_3, network_metrics_2022_3, network_metrics_2021_10, network_metrics_2022_10)) %>%
  left_join(death_df, by = "year")

node_metrics_all <- purrr::list_rbind(list(node_metrics_2021, node_metrics_2022, node_metrics_2021_3, node_metrics_2022_3, node_metrics_2021_10, node_metrics_2022_10)) %>%
  left_join(death_df, by = "year")

write_rds(network_metrics_all, file = "data/created/network_metrics_all_2026-01-27.RDS")
write_rds(node_metrics_all, file = "data/created/node_metrics_all_2026-01-27.RDS")
# network_metrics_all <- readRDS("data/created/network_metrics_all_2026-01-27.RDS")
# node_metrics_all <- readRDS("data/created/node_metrics_all_2026-01-27.RDS")

# Three zoom scales
# 240 days (8 months)
# 120 days (4 months)
# 60 days (2 months)
# 30 days (1 month)
# Basing these all on a single death date, death_min, not accounting for death_max yet.

plot_fun <- function(dataset, ndays, var, axis_label, death){
  p <- dataset %>%
    filter(period_start >= (death_min - lubridate::days(ndays)) & period_end <= (death_min + lubridate::days(ndays))) %>%
    ggplot(aes(x = period_start, y = .data[[var]], color = factor(days)))+
    geom_line()+
    theme_minimal()+
    facet_wrap(~year, scales = "free_x", ncol = 1)+
    geom_vline(data = death, aes(xintercept = death_min), color = "black", alpha = 0.7)+
    labs(y = axis_label,
         color = "Time window (days)",
         caption = paste0(ndays, " days"))+
    theme(legend.position = "bottom",
          axis.title.x = element_blank())
  return(p)
}

# 240 days
den_240 <- plot_fun(network_metrics_all, ndays = 240, var = "density", axis_label = "Network density", death = death_df)
asd_240 <- plot_fun(network_metrics_all, ndays = 240, var = "assort_degree", axis_label = "Degree assortativity", death = death_df)
apl_240 <- plot_fun(network_metrics_all, ndays = 240, var = "avg_path_length", axis_label = "Avg. path length", death = death_df)

# 120 days
den_120 <- plot_fun(network_metrics_all, ndays = 120, var = "density", axis_label = "Network density", death = death_df)
asd_120 <- plot_fun(network_metrics_all, ndays = 120, var = "assort_degree", axis_label = "Degree assortativity", death = death_df)
apl_120 <- plot_fun(network_metrics_all, ndays = 120, var = "avg_path_length", axis_label = "Avg. path length", death = death_df)

# 60 days
den_60 <- plot_fun(network_metrics_all, ndays = 60, var = "density", axis_label = "Network density", death = death_df)
asd_60 <- plot_fun(network_metrics_all, ndays = 60, var = "assort_degree", axis_label = "Degree assortativity", death = death_df)
apl_60 <- plot_fun(network_metrics_all, ndays = 60, var = "avg_path_length", axis_label = "Avg. path length", death = death_df)

# 30 days
den_30 <- plot_fun(network_metrics_all, ndays = 30, var = "density", axis_label = "Network density", death = death_df)
asd_30 <- plot_fun(network_metrics_all, ndays = 30, var = "assort_degree", axis_label = "Degree assortativity", death = death_df)
apl_30 <- plot_fun(network_metrics_all, ndays = 30, var = "avg_path_length", axis_label = "Avg. path length", death = death_df)

no_y_axis <- theme(
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank()
)

(den_30 | (den_60 + no_y_axis))/(den_120 | (den_240 + no_y_axis)) + plot_layout(guides = "collect", axes = "collect") & theme(legend.position = "bottom")

(asd_30 | (asd_60 + no_y_axis))/(asd_120 | (asd_240 + no_y_axis)) + plot_layout(guides = "collect", axes = "collect") & theme(legend.position = "bottom")

(apl_30 | (apl_60 + no_y_axis))/(apl_120 | (apl_240 + no_y_axis)) + plot_layout(guides = "collect", axes = "collect") & theme(legend.position = "bottom")

# XXX NEXT STEPS
# Figure out the names matching
# Roost networks
# Dig into literature: expected consequences of losing individuals, so we can test
# Discuss what to do next

