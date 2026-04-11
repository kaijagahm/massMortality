# Effect of mortalities on measures of network *function*: latency to feed at carcasses
# Exploring networks before/after mortalities
library(tidyverse)
library(vultureUtils)
library(sf)
library(mapview)
library(tidygraph)
library(ggraph)
library(targets)
library(patchwork)

tar_load(cluster21)
tar_load(cluster22)
date_21 <- min(cluster21$date)
date_22 <- min(cluster22$date)

tar_load(stn_carcs_21)
tar_load(stn_carcs_22)
tar_load(stn_carcs_21_list)
tar_load(stn_carcs_22_list)
length(stn_carcs_21_list)
length(stn_carcs_22_list)
# cool! very similar number of station carcasses

tar_load(first_sightings_21)
tar_load(first_sightings_22)

fs21 <- purrr::list_rbind(first_sightings_21) %>%
  left_join(sf::st_drop_geometry(select(stn_carcs_21, carcID, "carcDate" = date)), by = "carcID") %>%
  mutate(beforeafter = case_when(carcDate < date_21 ~ "before", carcDate > date_21 ~ "after", carcDate == date_21 ~ "same day")) %>%
  mutate(beforeafter = factor(beforeafter, levels = c("before", "same day", "after"))) %>%
  group_by(carcID) %>%
  mutate(idx = 1:n())

fs22 <- purrr::list_rbind(first_sightings_22) %>%
  left_join(sf::st_drop_geometry(select(stn_carcs_22, carcID, "carcDate" = date)), by = "carcID") %>%
  mutate(beforeafter = case_when(carcDate < date_22 ~ "before", carcDate > date_22 ~ "after", carcDate == date_22 ~ "same day")) %>%
  mutate(beforeafter = factor(beforeafter, levels = c("before", "same day", "after"))) %>%
  group_by(carcID) %>%
  mutate(idx = 1:n())

# Look at the acquisition curves
fs <- bind_rows(mutate(fs21, year = 2021), mutate(fs22, year = 2022))
fs %>%
  ggplot(aes(x = daytime_since_carcass, y = idx, group = carcID, color = beforeafter))+
  geom_line()+
  scale_color_manual(values = c("dodgerblue", "gray", "firebrick3"))+
  facet_wrap(~year)+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())+ 
  labs(x = "Daytime hours since carcass",
       y = "# vultures sighted", color = NULL)

# Ways to measure curves
# Time to first arrival
# Time to 50% of arrivals
# Vultures per hour of daylight

summ <- fs %>%
  group_by(carcID, year, beforeafter) %>%
  summarize(hrs_to_first = min(daytime_since_carcass),
            hrs_to_median = daytime_since_carcass[idx = median(idx)],
            vultures_per_hr = max(idx)/max(as.numeric(daytime_since_carcass))) %>%
  mutate(carcass = case_when(year == 2021 ~ "Carcass 1",
                             year == 2022 ~ "Carcass 2"))

a <- summ %>%
  filter(beforeafter %in% c("before", "after")) %>%
  ggplot(aes(x = carcass, y = hrs_to_first, fill = beforeafter, group = interaction(carcass, beforeafter)))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(group = interaction(carcass, beforeafter)),
             position=position_jitterdodge(jitter.width = 0.3),
             pch = 1,alpha = 0.5, size = 2)+
  scale_fill_manual(values = c("lightblue", "skyblue3"))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Carcass",
       y = "Hours to first detection",
       fill = NULL)
b <- summ %>%
  filter(beforeafter %in% c("before", "after")) %>%
  ggplot(aes(x = carcass, y = hrs_to_median, fill = beforeafter, group = interaction(carcass, beforeafter)))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(group = interaction(carcass, beforeafter)),
             position=position_jitterdodge(jitter.width = 0.3),
             pch = 1,alpha = 0.5, size = 2)+
  scale_fill_manual(values = c("lightblue", "skyblue3"))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Carcass",
       y = "Hours to median detection",
       fill = NULL)
c <- summ %>%
  filter(beforeafter %in% c("before", "after")) %>%
  ggplot(aes(x = carcass, y = vultures_per_hr, fill = beforeafter, group = interaction(carcass, beforeafter)))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(group = interaction(carcass, beforeafter)),
             position=position_jitterdodge(jitter.width = 0.4),
             pch = 1,alpha = 0.5)+
  scale_fill_manual(values = c("lightblue", "skyblue3"))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Carcass",
       y = "Vultures per hour",
       fill = NULL)

(a+b+c+plot_layout(axis_titles = "collect"))+plot_annotation(title = "Before/after mortality events", subtitle = "Two example mass mortality events", caption = "Light blue = 1 month before\nDark blue = 1 month after")


# Covariates to incorporate:
## Number of individuals that died
## Size of the network
## Number of other carcasses currently on the landscape

