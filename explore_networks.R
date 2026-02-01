# Exploring networks before/after mortalities
library(tidyverse)
library(vultureUtils)
library(sf)
library(mapview)
library(tidygraph)
library(ggraph)
library(targets)
library(patchwork)

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
source("R/getEdges_new.R")

tar_load(network_metrics_all)
tar_load(node_metrics_all)
tar_load(death_df)

# Four zoom scales
# Basing these all on a single death date, death_min, not accounting for death_max yet.
death_clusters <-  mm_sub_recent %>%
  dplyr::mutate(across(starts_with("com_words"), as.factor)) %>%
  dplyr::select(id, year, date, date_in_year, year_cluster_eps5min2, com_words_1) %>%
  mutate(clust = factor(year_cluster_eps5min2)) %>%
  filter(clust != 0) %>%
  group_by(year, clust) %>%
  summarize(mindate = min(date), n = n())

rng <- c(min(network_metrics_all$period_start), max(network_metrics_all$period_end))
death_clusters_touse <- death_clusters %>% filter(mindate >= rng[1] & mindate <= rng[2]) %>% mutate(date_in_year = lubridate::yday(mindate), label = paste0("n = ", n))

network_metrics_all %>%
  mutate(year = lubridate::year(period_start),
         date_in_year = lubridate::yday(period_start)) %>%
  ggplot(aes(x = date_in_year, y = .data[["density"]], color = factor(days)))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~year, ncol = 1)+
  geom_vline(data = death_clusters_touse, 
             aes(xintercept = date_in_year, linewidth = n))+
  labs(y = axis_label,
       color = "Time window (days)",
       caption = paste0(ndays, " days"))+
  theme(legend.position = "bottom",
        axis.title.x = element_blank())+
  scale_x_continuous(
    limits = c(1, 366),
    breaks = c(1, 91, 182, 274, 366),
    labels = c("Jan", "Apr", "Jul", "Oct", "Dec")
  ) +
  scale_linewidth(range = c(0.1, 1), breaks = seq(from = 1, to = 16, by = 4))+
  geom_text(
    data = death_clusters_touse,
    aes(x = date_in_year,
      y = 0.2,
      label = label),
    inherit.aes = FALSE,
    angle = 90,
    hjust = 0,
    size = 3,
    vjust = -0.5
  )

# 365 days
den_240 <- plot_fun(network_metrics_all, ndays = 365, var = "density", axis_label = "Network density", death = death_clusters)
asd_240 <- plot_fun(network_metrics_all, ndays = 365, var = "assort_degree", axis_label = "Degree assortativity", death = death_clusters)
apl_240 <- plot_fun(network_metrics_all, ndays = 365, var = "avg_path_length", axis_label = "Avg. path length", death = death_clusters)

# XXX NEXT STEPS
# Figure out the names matching
# Roost networks
# Dig into literature: expected consequences of losing individuals, so we can test
# Discuss what to do next

