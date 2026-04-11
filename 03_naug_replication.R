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
  filter(date > min(downsampled_masked$date_il)+days(30),
         date < max(downsampled_masked$date_il)-days(30))
table(mm_sub_recent$cluster_eps5min2)
mm_chosen <- mm_sub_recent %>% filter(cluster_eps5min2 != 0)
mm_dates_chosen_first <- group_split(group_by(mm_chosen, cluster_eps5min2)) %>%
  map_chr(., ~as.character(min(.x$date))) %>% lubridate::ymd()
mm_dates_chosen_last <- group_split(group_by(mm_chosen, cluster_eps5min2)) %>%
  map_chr(., ~as.character(max(.x$date))) %>% lubridate::ymd()

# Pull GPS data for 2 weeks (before first mortality of each cluster) and 2 weeks (after last mortality of each cluster)
befores_obs <- map(mm_dates_chosen_first, ~{
  downsampled_masked %>%
    filter(date_il >= .x - days(35),
           date_il <= .x - days(5))
})

afters_obs <- map(mm_dates_chosen_last, ~{
  downsampled_masked %>%
    filter(date_il >= .x + days(5),
           date_il <= .x + days(35))
})

whodied <- mm_chosen %>% group_by(cluster_eps5min2) %>% group_split() %>% map(., ~sort(unique(.x$id)))
whoislost <- map2(befores_obs, afters_obs, ~{
  sort(unique(.x$individual_local_identifier))[!(sort(unique(.x$individual_local_identifier)) %in% sort(unique(.y$individual_local_identifier)))]
}) # did manual comparison of who is lost to who died
removefrombefore <- list(c("A09w", "A90 White", "J11w", "J18w", "J20 White"),
                         NULL, # exclude entirely
                         NULL, # keep; don't need to remove anyone
                         NULL, # exclude entirely
                         c("A01w", "A02w", "A08w", "A19w", "A33w", "A36w", "A38w", "A50W (Y40>S70)", "A58w", "A68 White", "A73w", "C53 Yellow", "E00w", "E01w", "E02w", "E03w", "E04w", "E14w", "J11w", "J15w", "J16w", "J18w", "J24 White", "J52w", "T76w", "T77w", "T91b", "Y26b"),
                         c("A36w", "A38w", "A53w", "A68 White", "C53 Yellow", "E00w", "E01w", "E07w", "E10w", "E11w", "E14w", "J18w", "J24 White", "J31w", "J39w", "J52w", "T17w", "T54 White", "T56b", "T66w", "T69b", "T77w", "T83 B"),
                         NULL,
                         NULL,
                         c("A01w", "A36w", "A55w", "A58w", "E00w", "E01w", "E05w", "E11w", "E39w", "J18w", "J94 White", "T54 White", "T91b", "Y69>T13W"))

befores_obs <- map2(befores_obs, removefrombefore, ~{
  .x %>%
    filter(!(individual_local_identifier %in% .y))
})

whoisgained <- map2(befores_obs, afters_obs, ~{
  sort(unique(.y$individual_local_identifier))[!(sort(unique(.y$individual_local_identifier)) %in% sort(unique(.x$individual_local_identifier)))]
})

removefromafter <- whoisgained
afters_obs <- map2(afters_obs, removefromafter, ~{
  .x %>%
    filter(!(individual_local_identifier %in% .y))
})
keepevents <- c(1, 3, 5, 6, 9)

befores_obs <- befores_obs[keepevents]
afters_obs <- afters_obs[keepevents]
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

# Because we have an incredibly skewed distribution of SRI values, I'm going to implement a cutoff a little bit higher than 0. Let's say something around the 75th percentile, just roughly?
map(befores_nets_obs, ~quantile(.x$sri[.x$sri > 0 & !is.na(.x$sri)]))
# looks like we could use a cutoff SRI value of 0.003.
cutoff_sri <- 0.003

befores_nets_obs_grph <- map(befores_nets_obs, ~{
  .x %>%
    rename("from" = ID1, "to" = ID2) %>%
    as_tbl_graph(directed = F) %>%
    activate(edges) %>%
    filter(sri > cutoff_sri)
})

#ggraph(befores_nets_obs_grph[[1]])+geom_edge_link(aes(edge_width = sri))+geom_node_point()+  scale_edge_width_continuous(range = c(0.1, 1))
afters_nets_obs_grph <- map(afters_nets_obs, ~{
  .x %>%
    rename("from" = ID1, "to" = ID2) %>%
    as_tbl_graph(directed = F) %>%
    activate(edges) %>%
    filter(sri > cutoff_sri)
})

# Pull network size for each
nodes_before_obs <- map_dbl(befores_nets_obs_grph, igraph::gorder)
nodes_after_obs <- map_dbl(afters_nets_obs_grph, igraph::gorder)
nodes_before_obs-nodes_after_obs # check that this is less than or equal to the number of indivs who died
map_dbl(whodied[keepevents], length) # this is the number who are supposed to have died
# they don't match, but whodied is always higher if anything, which is because some untagged individuals died. That's okay.

# Measure density, avg path length, and clustering coef of all networks and plot vs. network size
dens_before_obs <- map_dbl(befores_nets_obs_grph, igraph::edge_density)
dens_after_obs <- map_dbl(afters_nets_obs_grph, igraph::edge_density)
apl_before_obs <- map_dbl(befores_nets_obs_grph, igraph::mean_distance)
apl_after_obs <- map_dbl(afters_nets_obs_grph, igraph::mean_distance)
cc_before_obs <- map_dbl(befores_nets_obs_grph, igraph::transitivity)
cc_after_obs <- map_dbl(afters_nets_obs_grph, igraph::transitivity)

graph_stats <- data.frame(type = "obs",
                          beforeafter = rep(c("before", "after"), each = 5),
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
summary(lm(dens ~ n*beforeafter, data = graph_stats)) # marginally significant

## Path length ~ Network size
graph_stats %>%
  ggplot(aes(x = n, y = path_length, color = beforeafter))+
  geom_point(pch = 1, size = 2)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(x = "Graph size", y = "Average path length")
summary(lm(path_length ~ n*beforeafter, data = graph_stats)) # marginal

## CC ~ Network size
graph_stats %>%
  ggplot(aes(x = n, y = cc, color = beforeafter))+
  geom_point(pch = 1, size = 2)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(x = "Graph size", y = "Clustering coefficient")
summary(lm(cc ~ n*beforeafter, data = graph_stats)) # n.s.

## no strong relationship between graph size and any of the structural measures in these nine mortality events.

# Generate networks with same density
# need to back-compute number of edges that would be required to give the density
#n(n-1)/2 = total number of edges
# m/(n(n-1)/2) = density
# m = density*(n(n-1)/2)
## "Using the number of nodes and edges specific to each colony, I then constructed random networks with the same densities"
m_before_obs <- round(dens_before_obs*(nodes_before_obs*(nodes_before_obs-1)/2))
befores_nets_rand_grph <- map2(nodes_before_obs, m_before_obs, ~play_gnm(n = .x, m = .y, directed = F))
befores_nets_rand_grph <- map(befores_nets_rand_grph, ~{
  .x %>% activate(nodes) %>%
    mutate(node_id = 1:n())
})
# ggraph(befores_nets_rand_grph[[2]])+geom_edge_link()+geom_node_point()
# ggraph(befores_nets_obs_grph[[2]])+geom_edge_link()+geom_node_point()

# path length and cc of the random networks
dens_before_rand <- map_dbl(befores_nets_rand_grph, igraph::edge_density)
apl_before_rand <- map_dbl(befores_nets_rand_grph, igraph::mean_distance)
cc_before_rand <- map_dbl(befores_nets_rand_grph, igraph::transitivity)

# wilcoxon signed rank test path length rand vs. obs before
wilcox.test(x = apl_before_obs, y = apl_before_rand, paired = T, alternative = "two.sided") # n.s.--no difference between path lengths between observed and random pre-removal
# XXX WHY ARE SO MANY OF THESE THE SAME?? Answer: networks were too dense.

# wilcoxon signed rank test clustering coefficient rand vs. obs before
wilcox.test(x = cc_before_obs, y = cc_before_rand, paired = T, alternative = "two.sided") # marginally different clustering coefficient--not significant, but close.

# Create "after" networks for the random networks by removing the same number of nodes
n_toremove <- nodes_before_obs-nodes_after_obs # check that this is equal to the number of indivs who died

set.seed(3)
to_remove <- map2(n_toremove, befores_nets_rand_grph, ~sample(1:gorder(.y), size = .x, replace = F))

afters_nets_rand_grph <- map2(befores_nets_rand_grph, to_remove, ~{
  .x %>% activate(nodes) %>%
    filter(!(node_id %in% .y)) %>%
    activate(edges) %>%
    filter(!(from %in% .y) & !(to %in% .y))
})

dens_after_rand <- map_dbl(afters_nets_rand_grph, igraph::edge_density)
apl_after_rand <- map_dbl(afters_nets_rand_grph, igraph::mean_distance)
cc_after_rand <- map_dbl(afters_nets_rand_grph, igraph::transitivity)

nodes_before_rand <- map_dbl(befores_nets_rand_grph, igraph::gorder)
nodes_after_rand <- map_dbl(afters_nets_rand_grph, igraph::gorder)

graph_stats_rand <- data.frame(type = "rand",
                          beforeafter = rep(c("before", "after"), each = 5),
                          n = c(nodes_before_rand, nodes_after_rand),
                          dens = c(dens_before_rand, dens_after_rand),
                          path_length = c(apl_before_rand, apl_after_rand),
                          cc = c(cc_before_rand, cc_after_rand))

graph_stats <- bind_rows(graph_stats, graph_stats_rand)

## Density before/after
# wilcoxon density obs before vs. after (OBS)
wilcox.test(x = graph_stats$dens[graph_stats$type == "obs" & graph_stats$beforeafter == "before"], 
            y = graph_stats$dens[graph_stats$type == "obs" & graph_stats$beforeafter == "after"], 
            paired = T, alternative = "two.sided") # no change in density after loss (observed networks)
graph_stats$dens[graph_stats$type == "obs" & graph_stats$beforeafter == "after"] - 
  graph_stats$dens[graph_stats$type == "obs" & graph_stats$beforeafter == "before"]
# some increases, some decreases

# wilcoxon density rand before vs. after (RAND)
xx <- graph_stats$dens[graph_stats$type == "rand" & graph_stats$beforeafter == "before"]
yy <- graph_stats$dens[graph_stats$type == "rand" & graph_stats$beforeafter == "after"]
wilcox.test(x = xx, 
            y = yy, 
            paired = T, alternative = "two.sided") # change in density after loss, in random networks
yy-xx # all negative--overall, we have a decrease in density after loss

## Path length before/after
# wilcoxon path length obs before vs. after (OBS)
wilcox.test(x = graph_stats$path_length[graph_stats$type == "obs" & graph_stats$beforeafter == "before"], 
            y = graph_stats$path_length[graph_stats$type == "obs" & graph_stats$beforeafter == "after"], 
            paired = T, alternative = "two.sided") # n.s.

# wilcoxon path length rand before vs. after (RAND)
wilcox.test(x = graph_stats$path_length[graph_stats$type == "rand" & graph_stats$beforeafter == "before"], 
            y = graph_stats$path_length[graph_stats$type == "rand" & graph_stats$beforeafter == "after"], 
            paired = T, alternative = "two.sided") # aggressively n.s.
# XXX why is this exactly the same p value as OBS???

## Clustering coefficient before/after
# wilcoxon path length obs before vs. after (OBS)
wilcox.test(x = graph_stats$cc[graph_stats$type == "obs" & graph_stats$beforeafter == "before"], 
            y = graph_stats$cc[graph_stats$type == "obs" & graph_stats$beforeafter == "after"], 
            paired = T, alternative = "two.sided") # n.s.

# wilcoxon path length rand before vs. after (RAND)
wilcox.test(x = graph_stats$cc[graph_stats$type == "rand" & graph_stats$beforeafter == "before"], 
            y = graph_stats$cc[graph_stats$type == "rand" & graph_stats$beforeafter == "after"], 
            paired = T, alternative = "two.sided") # n.s.

# Can't draw any conclusions without more data here. But this provides a potential path forward in comparing before/after with removals
# XXX Why are so many of these the same??

# Plots -------------------------------------------------------------------
# manually edit (because of potentially non-correspondence in IDs, *sigh*) to show which individuals died
died_status <- map(befores_nets_obs_grph, ~{
  nms <- .x %>% activate(nodes) %>% pull(name) %>% sort()
  dat <- data.frame(name = nms, died = NA)
})

whodied[keepevents][[2]] # have to subset first by keepevents

died_status[[1]] <- died_status[[1]] %>% mutate(died = case_when(name %in% c("J25 White", "J54 White", "T85w") ~ T, .default = F))
died_status[[2]] <- died_status[[2]] %>% mutate(died = case_when(name %in% c("J36w") ~ T, .default = F))
died_status[[3]] <- died_status[[3]] %>% mutate(died = case_when(name %in% c("A17w", "A21w", "A27w", "A77w", "E07w", "E08w", "E14w", "E43w", "T17w", "T56b", "T66w", "T69b", "T83 B") ~ T, .default = F))
died_status[[4]] <- died_status[[4]] %>% mutate(died = case_when(name %in% c("E19w", "J15w", "Y82v") ~ T, .default = F))
died_status[[5]] <- died_status[[5]] %>% mutate(died = case_when(name %in% c("E03", "E09w", "E10w", "E33w", "T77w", "T86b") ~ T, .default = F))

befores_nets_obs_grph_modified <- map2(befores_nets_obs_grph, died_status, ~{
  .x %>% activate(nodes) %>%
    left_join(.y, by = "name")
})

shared_layouts_obs <- map2(befores_nets_obs_grph_modified, afters_nets_obs_grph, ~{
  make_shared_layout(graphlist = list("before" = .x, "after" = .y), node_col = "name")
})

graphs_shared_layouts_obs <- map(shared_layouts_obs, "graphlist_modified")
layouts_shared_layouts_obs <- map(shared_layouts_obs, "layout")

beforeafter_pairs <- function(before, after){
  a <- ggraph(before, layout = "manual", x = x, y = y)+
    geom_edge_link(alpha = 0.3)+
    geom_node_point(size = 3, aes(color = died))+
    scale_color_manual(values = c("lightblue", "red"))+
    theme(legend.position = "none")+
    ggtitle("Before")
  b <- ggraph(after, layout = "manual", x = x, y = y)+
    geom_edge_link(alpha = 0.3)+
    geom_node_point(size = 3, color = "skyblue3")+
    theme(legend.position = "none")+
    ggtitle("After")
  return(a+b)
}

plots <- map2(map(graphs_shared_layouts_obs, 1), map(graphs_shared_layouts_obs, 2), beforeafter_pairs)

# Higher SRI cutoff shows a little bit more variation. Still want to try roosting networks, though.
