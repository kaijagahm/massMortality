library(tidyverse)
library(targets)
library(ggraph)
library(tidygraph)
source("R/functions.R")
source("R/getEdges_new_2026-02-01.R")

# Get data for nine mortalities
tar_load(mm_sub_recent)
tar_load(downsampled_masked)
mm_sub_recent <- mm_sub_recent %>%
  filter(date > min(downsampled_masked$date_il)+days(15),
         date < max(downsampled_masked$date_il)-days(15))
table(mm_sub_recent$cluster_eps5min2)
mm_chosen <- mm_sub_recent %>% filter(cluster_eps5min2 != 0)
mm_dates_chosen_first <- group_split(group_by(mm_chosen, cluster_eps5min2)) %>%
  map_chr(., ~as.character(min(.x$date))) %>% lubridate::ymd()
mm_dates_chosen_last <- group_split(group_by(mm_chosen, cluster_eps5min2)) %>%
  map_chr(., ~as.character(max(.x$date))) %>% lubridate::ymd()

# Pull GPS data for 2 weeks before and 2 weeks after
befores_obs <- map(mm_dates_chosen_first, ~{
  downsampled_masked %>%
    filter(date_il >= .x - days(15),
           date_il <= .x - days(1))
})

afters_obs <- map(mm_dates_chosen_last, ~{
  downsampled_masked %>%
    filter(date_il >= .x + days(1),
           date_il <= .x + days(15))
})

# Create before/after networks for each
tar_load(ct)
tar_load(dt)
tar_load(stl)
tar_load(idc)
tar_load(r)
tar_load(tc)
tar_load(rp)

befores_nets_obs <- map(befores_obs, ~{
  getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp)
})

afters_nets_obs <- map(afters_obs, ~{
  getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp)
})

befores_nets_obs_grph <- map(befores_nets_obs, ~{
  .x %>%
    rename("from" = ID1, "to" = ID2) %>%
    as_tbl_graph(directed = F) %>%
    activate(edges) %>%
    filter(sri > 0)
})

#ggraph(befores_nets_obs_grph[[1]])+geom_edge_link(aes(edge_width = sri))+geom_node_point()+  scale_edge_width_continuous(range = c(0.1, 1))
afters_nets_obs_grph <- map(afters_nets_obs, ~{
  .x %>%
    rename("from" = ID1, "to" = ID2) %>%
    as_tbl_graph(directed = F) %>%
    activate(edges) %>%
    filter(sri > 0)
})


# Pull network size for each
nodes_before_obs <- map_dbl(befores_nets_obs_grph, igraph::gorder)
nodes_after_obs <- map_dbl(afters_nets_obs_grph, igraph::gorder)

# Measure density, avg path length, and clustering coef of all networks and plot vs. network size
dens_before_obs <- map_dbl(befores_nets_obs_grph, igraph::edge_density)
dens_after_obs <- map_dbl(afters_nets_obs_grph, igraph::edge_density)
apl_before_obs <- map_dbl(befores_nets_obs_grph, igraph::mean_distance)
apl_after_obs <- map_dbl(afters_nets_obs_grph, igraph::mean_distance)
cc_before_obs <- map_dbl(befores_nets_obs_grph, igraph::transitivity)
cc_after_obs <- map_dbl(afters_nets_obs_grph, igraph::transitivity)

graph_stats <- data.frame(type = "obs",
                          beforeafter = rep(c("before", "after"), each = 9),
                          n = c(nodes_before_obs, nodes_after_obs),
                          dens = c(dens_before_obs, dens_after_obs),
                          path_length = c(apl_before_obs, apl_after_obs),
                          cc = c(cc_before_obs, cc_after_obs))

## Density ~ Network size
graph_stats %>%
  ggplot(aes(x = n, y = dens, color = beforeafter))+
  geom_point(pch = 1, size = 2)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(x = "Graph size", y = "Density")
summary(lm(dens ~ n*beforeafter, data = graph_stats)) # n.s.

## Path length ~ Network size
graph_stats %>%
  ggplot(aes(x = n, y = path_length, color = beforeafter))+
  geom_point(pch = 1, size = 2)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(x = "Graph size", y = "Average path length")
summary(lm(path_length ~ n*beforeafter, data = graph_stats)) # n.s.

## CC ~ Network size
graph_stats %>%
  ggplot(aes(x = n, y = cc, color = beforeafter))+
  geom_point(pch = 1, size = 2)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(x = "Graph size", y = "Clustering coefficient")
summary(lm(cc ~ n*beforeafter, data = graph_stats)) # n.s.

## no relationship between graph size and any of the structural measures in these nine mortality events.

# Generate networks with same density
# need to back-compute number of edges that would be required to give the density
#n(n-1)/2 = total number of edges
# m/(n(n-1)/2) = density
# m = density*(n(n-1)/2)
## "Using the number of nodes and edges specific to each colony, I then constructed random networks with the same densities"
m_before_obs <- round(dens_before_obs*(nodes_before_obs*(nodes_before_obs-1)/2))
befores_nets_rand_grph <- map2(nodes_before_obs, m_before_obs, ~play_gnm(n = .x, m = .y, directed = F))
# ggraph(befores_nets_rand_grph[[2]])+geom_edge_link()+geom_node_point()
# ggraph(befores_nets_obs_grph[[2]])+geom_edge_link()+geom_node_point()

# path length and cc of the random networks
dens_before_rand <- map_dbl(befores_nets_rand_grph, igraph::edge_density)
apl_before_rand <- map_dbl(befores_nets_rand_grph, igraph::mean_distance)
cc_before_rand <- map_dbl(befores_nets_rand_grph, igraph::transitivity)

# wilcoxon signed rank test path length rand vs. obs before
wilcox.test(x = apl_before_obs[c(1, 3:9)], y = apl_before_rand[c(1, 3:9)], paired = T, alternative = "two.sided") # n.s.--no difference between path lengths between observed and random pre-removal

# wilcoxon signed rank test clustering coefficient rand vs. obs before
wilcox.test(x = cc_before_obs, y = cc_before_rand, paired = T, alternative = "two.sided") # clustering coefficient is significantly different! So far this is the same as what naug found except that we don't have significant relationships between network size and any of the measures, whereas they did.

# before moving on to the before vs. after, need to do the following
## 1. go back and don't start the "after" networks until after all removals are completed
## 2. make sure all other individuals are the same before and after--don't include them if not.

# wilcoxon density obs before vs. after
# wilcoxon density rand before vs. after
# wilcoxon path length obs before vs. after
# wilcoxon path length rand before vs. after