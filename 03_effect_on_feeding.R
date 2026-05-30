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
tar_load(gps_21)
tar_load(gps_22)
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

# Number of tracked vultures in each
ntracked_21 <- map(gps_21, ~data.frame(carcID = .x$carcID[1], n_total_tracked = length(unique(.x$individual_local_identifier)))) %>% purrr::list_rbind()
ntracked_22 <- map(gps_22, ~data.frame(carcID = .x$carcID[1], n_total_tracked = length(unique(.x$individual_local_identifier)))) %>% purrr::list_rbind()

# Look at the acquisition curves
fs <- bind_rows(mutate(fs21, year = 2021), mutate(fs22, year = 2022)) %>%
  left_join(bind_rows(ntracked_21, ntracked_22))
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
# # vultures at 50% time (approx. 20 daylight hours, though it varies... how do we make this the same?)
# Time to 25% of tagged vultures having seen the carcass
# Vultures per hour of daylight

summ <- fs %>%
  group_by(carcID, year, beforeafter) %>%
  summarize(hrs_to_first = min(daytime_since_carcass),
            vultures_at_20hrs = min(idx[daytime_since_carcass >= 20], na.rm = T),
            vultures_per_hr = max(idx)/max(as.numeric(daytime_since_carcass)),
            total_tracked = n_total_tracked[1],
            final_vultures = max(idx),
            time_to_25pct = min(daytime_since_carcass[idx >= n_total_tracked/4])) %>%
  mutate(mortality_event = case_when(year == 2021 ~ "Mortality event 1",
                             year == 2022 ~ "Mortality event 2")) %>%
  mutate(across(c("vultures_at_20hrs", "time_to_25pct"), ~replace(.x, is.infinite(.x), NA)))

a <- summ %>%
  filter(beforeafter %in% c("before", "after")) %>%
  ggplot(aes(x = mortality_event, y = hrs_to_first, fill = beforeafter, group = interaction(mortality_event, beforeafter)))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(group = interaction(mortality_event, beforeafter)),
             position=position_jitterdodge(jitter.width = 0.3),
             pch = 1,alpha = 0.5, size = 2)+
  scale_fill_manual(values = c("lightblue", "skyblue3"))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Mortality event",
       y = "Hours to first detection",
       fill = NULL)

b <- summ %>%
  filter(beforeafter %in% c("before", "after")) %>%
  ggplot(aes(x = mortality_event, y = vultures_at_20hrs, fill = beforeafter, group = interaction(mortality_event, beforeafter)))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(group = interaction(mortality_event, beforeafter)),
             position=position_jitterdodge(jitter.width = 0.3),
             pch = 1,alpha = 0.5, size = 2)+
  scale_fill_manual(values = c("lightblue", "skyblue3"))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Mortality event",
       y = "Vultures after 20hrs",
       fill = NULL)
c <- summ %>%
  filter(beforeafter %in% c("before", "after")) %>%
  ggplot(aes(x = mortality_event, y = vultures_per_hr, fill = beforeafter, group = interaction(mortality_event, beforeafter)))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(group = interaction(mortality_event, beforeafter)),
             position=position_jitterdodge(jitter.width = 0.4),
             pch = 1,alpha = 0.5)+
  scale_fill_manual(values = c("lightblue", "skyblue3"))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Mortality event",
       y = "Vultures per hour",
       fill = NULL)

d <- summ %>%
  filter(beforeafter %in% c("before", "after")) %>%
  ggplot(aes(x = mortality_event, y = time_to_25pct, fill = beforeafter, group = interaction(mortality_event, beforeafter)))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(group = interaction(mortality_event, beforeafter)),
             position=position_jitterdodge(jitter.width = 0.4),
             pch = 1,alpha = 0.5)+
  scale_fill_manual(values = c("lightblue", "skyblue3"))+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(x = "Mortality event",
       y = "Time to 25% tracked vultures",
       caption = "Includes only carcasses that reached 25% of tracked vultures",
       fill = NULL)

(a+b+c+d+plot_layout(axis_titles = "collect"))+plot_annotation(title = "Before/after mortality events", subtitle = "Two example mass mortality events", caption = "Light blue = 1 month before\nDark blue = 1 month after")


# Covariates to incorporate:
## Number of individuals that died
## Size of the network
## Number of other carcasses currently on the landscape

