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

# Split gps data into overlapping 5-day increments
split_overlapping <- function(gps){
  start_date <- as.Date(min(gps$timestamp))
  end_date   <- as.Date(max(gps$timestamp))
  
  window_starts <- seq(from = start_date, to   = end_date - days(4),  # last full 5-day window
    by = "1 day")
  
  # make list of 5-day windows
  gps_5day_windows <- purrr::map(
    window_starts, function(win_start){
      win_end <- win_start + days(4)
      gps %>% filter(timestamp >= as.POSIXct(win_start) &
            timestamp <  as.POSIXct(win_end + days(1)))}
  )
  
  # name list elements with their cutoff dates
  names(gps_5day_windows) <- map_chr(
    window_starts,
    function(win_start) {
      win_end <- win_start + days(4)
      paste0(
        format(win_start, "%Y.%m.%d"),
        "_",
        format(win_end, "%Y.%m.%d")
      )
    }
  )
  return(gps_5day_windows)
}

windows_21 <- split_overlapping(downsampled_masked_2021)
windows_22 <- split_overlapping(downsampled_masked_2022)

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

# sri_before_21 <- getEdges_new(dataset = gps_before_21, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 
# sri_after_21 <- getEdges_new(dataset = gps_after_21, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 
# sri_before_22 <- getEdges_new(dataset = gps_before_22, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 
# sri_after_22 <- getEdges_new(dataset = gps_after_22, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 

# sri_21 <- left_join(sri_before_21, sri_after_21, by = c("ID1", "ID2", "pair"), suffix = c("_before", "_after")) %>% pivot_longer(cols = starts_with("sri_"), values_to = "sri", names_to = "beforeafter")%>% mutate(beforeafter = factor(str_remove(beforeafter, "sri_"), levels = c("before", "after")))
# sri_22 <- left_join(sri_before_22, sri_after_22, by = c("ID1", "ID2", "pair"), suffix = c("_before", "_after")) %>% pivot_longer(cols = starts_with("sri_"), values_to = "sri", names_to = "beforeafter") %>% mutate(beforeafter = factor(str_remove(beforeafter, "sri_"), levels = c("before", "after")))

# sris <- sri_21 %>% mutate(year = 2021) %>% bind_rows(sri_22 %>% mutate(year = 2022))

sris_2021_df <- purrr::list_rbind(sris_2021, names_to = "period") %>% mutate(year = 2021) %>% separate_wider_delim(cols = "period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd))

sris_2022_df <- purrr::list_rbind(sris_2022, names_to = "period") %>% mutate(year = 2022) %>% separate_wider_delim(cols = "period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd))

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

# walk2(plots_2021, names(plots_2021), ~ggsave(.x, filename = paste0("data/created/5day_rollingwindow_networks/2021/", .y, ".png"), width = 6, height = 6))
# walk2(plots_2022, names(plots_2022), ~ggsave(.x, filename = paste0("data/created/5day_rollingwindow_networks/2022/", .y, ".png"), width = 6, height = 6))

# Interesting! Just anecdotally, the networks look less dense. Not sure if they actually are. It would be interesting to track the density over time; I would be really confused if it actually turns out that the poisoning had this huge an impact.

network_metrics <- function(g, weight = "sri", loops = FALSE) {
  
  nodes <- c((g %>% activate(edges) %>% filter(sri > 0) %>% pull(from)),
             (g %>% activate(edges) %>% filter(sri > 0) %>% pull(to))) %>% unique()
  n <- length(nodes)
  
  ## ---- Global metrics ----
  density <- g %>%
    activate(nodes) %>%
    igraph::edge_density(loops = loops)
  
  ## ---- Node-level metrics ----
  node_tbl <- g %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree(),
           degree_rel = centrality_degree()/n,
           strength = centrality_degree(weights = sri),
           strength_rel = centrality_degree(weights = sri)/n,
           n = n) %>%
    as_tibble()
  
  ## ---- Output ----
  list(
    density       = density,
    node_metrics  = node_tbl
  )
} # XXX start here--add n

# Apply to graphs
metrics_2021 <- map(graphs_2021, network_metrics, weight = "sri")
metrics_2022 <- map(graphs_2022, network_metrics, weight = "sri")

# network-level metrics
network_metrics_2021 <- map_dfr(metrics_2021, ~ tibble(density  = .x$density,), .id = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), year = 2021)

network_metrics_2022 <- map_dfr(metrics_2022, ~ tibble(density  = .x$density,), .id = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), year = 2022)

# node-level metrics
node_metrics_2021 <- map_dfr(metrics_2021, "node_metrics", .id = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), year = 2021)

node_metrics_2022 <- map_dfr(metrics_2022, "node_metrics", .id = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), year = 2022)

death_date_2021_min <- lubridate::ymd("2021-10-24")
death_date_2021_max <- lubridate::ymd("2021-10-24")
death_date_2022_min <- lubridate::ymd("2022-10-12")
death_date_2022_max <- lubridate::ymd("2022-10-15")
death_df <- data.frame(min = c(death_date_2021_min, death_date_2022_min),
                       max = c(death_date_2021_max, death_date_2022_max),
                       year = c(2021, 2022))

network_metrics_2021 %>%
  bind_rows(network_metrics_2022) %>%
  ggplot(aes(x = period_start, y = density))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~year, scales = "free_x", ncol = 1)+
  geom_vline(data = death_df, aes(xintercept = min), color = "red", alpha = 0.5)+
  labs(x = "Start of 5-day window (overlapping)",
       y = "Network density")

# yeah, so it's pretty clear that these poisonings do not cause fluctuations that are at all out of the ordinary in these networks. That's good, but also kind of a bummer for the sake of this analysis.

node_metrics_2021 %>%
  bind_rows(node_metrics_2022) %>%
  ggplot(aes(x = factor(period_start), y = degree_rel))+
  geom_boxplot(fill = "gray", size = 0.5)+
  theme_minimal()+
  facet_wrap(~year, scales = "free_x", ncol = 1)+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank())



# XXX NEXT STEPS
# Figure out the names matching
# Look at Conner's papers about network-level metrics.
# Discuss what to do next
