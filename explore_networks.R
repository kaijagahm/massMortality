# Exploring networks before/after mortalities
library(tidyverse)
library(vultureUtils)
library(sf)
library(mapview)
library(tidygraph)
library(ggraph)
library(targets)
library(patchwork)
tar_load(downsampled_masked)
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

gps_before_21 <- downsampled_masked %>% filter(date_il >= cluster21dates[[1]][1],
                                        date_il <= cluster21dates[[1]][2]) %>% st_as_sf(remove = F)
gps_after_21 <- downsampled_masked %>% filter(date_il >= cluster21dates[[2]][1],
                                      date_il <= cluster21dates[[2]][2])%>% st_as_sf(remove = F)
gps_before_22 <- downsampled_masked %>% filter(date_il >= cluster22dates[[1]][1],
                                        date_il <= cluster22dates[[1]][2])%>% st_as_sf(remove = F)
gps_after_22 <- downsampled_masked %>% filter(date_il >= cluster22dates[[2]][1],
                                       date_il <= cluster22dates[[2]][2])%>% st_as_sf(remove = F)

# Let's get which individuals died
mm_sub_recent
cluster21 <- mm_sub_recent %>% filter(year_cluster_eps5min2 == 4 & year == 2021)
cluster22 <- mm_sub_recent %>% filter(year_cluster_eps5min2 == 2 & year == 2022)
died21 <- cluster21$id
died21 %in% gps_before_21$individual_local_identifier
died21 %in% gps_after_21$individual_local_identifier # well it's a relief that none of these guys show up after they're dead!
died22 <- cluster22$id
died22 %in% gps_before_22$individual_local_identifier
died22 %in% gps_after_22$individual_local_identifier # also here, nobody shows up after. good!

# what's up with the ones that don't show up?
# example: died21[3]
died21[3]
tar_load(ww)
# hmm, can't find it, but I can't tell if I just didn't match the names up correctly or not.

# Let's skip this for now, in the interest of time. Will need to do more careful names matching, but for now, I'm going to make the networks before and after and just see what they look like.

ct <- 1
dt <- 1000
stl <- 4
idc <- "individual_local_identifier"
r <- "sri"
tc <- "timestamp_il"

sri_before_21 <- getEdges_new(dataset = gps_before_21, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 
sri_after_21 <- getEdges_new(dataset = gps_after_21, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 
sri_before_22 <- getEdges_new(dataset = gps_before_22, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 
sri_after_22 <- getEdges_new(dataset = gps_after_22, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp) 

sri_21 <- left_join(sri_before_21, sri_after_21, by = c("ID1", "ID2", "pair"), suffix = c("_before", "_after")) %>% pivot_longer(cols = starts_with("sri_"), values_to = "sri", names_to = "beforeafter")%>% mutate(beforeafter = factor(str_remove(beforeafter, "sri_"), levels = c("before", "after")))
sri_22 <- left_join(sri_before_22, sri_after_22, by = c("ID1", "ID2", "pair"), suffix = c("_before", "_after")) %>% pivot_longer(cols = starts_with("sri_"), values_to = "sri", names_to = "beforeafter") %>% mutate(beforeafter = factor(str_remove(beforeafter, "sri_"), levels = c("before", "after")))

sris <- sri_21 %>% mutate(year = 2021) %>% bind_rows(sri_22 %>% mutate(year = 2022))

sris %>%
  ggplot(aes(x = beforeafter, y = log(sri), group = pair))+
  geom_point()+
  geom_line(alpha = 0.1)+
  facet_wrap(~year)+
  theme_minimal()

sri_dfs <- list(sri_before_21, sri_after_21, sri_before_22, sri_after_22)
gs <- map(sri_dfs, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))

# Going to build a bunch of networks, all with the same layout
all_nodes <- purrr::list_rbind(sri_dfs) %>%
  select(ID1, ID2) %>%
  pivot_longer(everything()) %>%
  distinct(value) %>%
  rename(name = value)

edges_union <- purrr::list_rbind(sri_dfs) %>%
  filter(sri > 0) %>%
  group_by(ID1, ID2) %>%
  summarise(sri = mean(sri), .groups = "drop")

g_union <- tbl_graph(
  nodes = all_nodes,
  edges = edges_union,
  directed = FALSE
)

g_layout <- g_union %>%
  activate(edges) %>%
  mutate(layout_weight = sri)

layout_tbl <- create_layout(
  g_layout,
  layout  = "fr",
  weights = layout_weight
)

plot_network_fixed <- function(edges, layout_tbl, title = NULL) {
  
  # which nodes are present in THIS network?
  present_nodes <- unique(c(edges$ID1, edges$ID2))
  
  g <- tbl_graph(
    nodes = layout_tbl %>% select(name),
    edges = edges %>% filter(sri > 0),
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

titles <- c("2021_before", "2021_after", "2022_before", "2022_after")
lists <- map2(sri_dfs, titles, ~plot_network_fixed(.x, layout_tbl, title = .y))
ps <- map(lists, "plot")
gs <- map(lists, "graph")

ggsave(ps[[1]], file = "data/created/2021_before_net.png", width = 8, height = 8)
ggsave(ps[[2]], file = "data/created/2021_after_net.png", width = 8, height = 8)
ggsave(ps[[3]], file = "data/created/2022_before_net.png", width = 8, height = 8)
ggsave(ps[[4]], file = "data/created/2022_after_net.png", width = 8, height = 8)

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
           strength_rel = centrality_degree(weights = sri)/n) %>%
    as_tibble() %>%
    select(name, degree_rel, degree, strength, strength_rel)
  
  ## ---- Output ----
  list(
    density       = density,
    node_metrics  = node_tbl
  )
}

# Apply to graphs
names(gs) <- titles
metrics <- map(gs, network_metrics, weight = "sri")

# density summary
summary_tbl <- map_dfr(
  metrics,
  ~ tibble(
    density    = .x$density,
  ),
  .id = "time_period"
) %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("year", "period"))

# node-level metrics
node_metrics_all <- map_dfr(
  metrics,
  "node_metrics",
  .id = "time_period"
) %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("year", "period"))

node_metrics_all %>%
  mutate(period = factor(period, levels = c("before", "after"))) %>%
  ggplot(aes(x = degree_rel, color = period, fill = period))+
  geom_density(alpha = 0.2)+
  theme_minimal()+
  facet_wrap(~year)

node_metrics_all %>%
  mutate(period = factor(period, levels = c("before", "after"))) %>%
  ggplot(aes(x = strength_rel, color = period, fill = period))+
  geom_density(alpha = 0.2)+
  theme_minimal()+
  facet_wrap(~year)

# Need to compare to the average distribution of 1-week segments.

# XXX NEXT STEPS
# Figure out the names matching
# Moving window time series
# Look at Conner's papers about newtork-level metrics.
