# Exploring networks before/after mortalities
library(tidyverse)
library(vultureUtils)
library(sf)
library(mapview)
tar_load(downsampled)
mm_sub_recent <- readRDS("data/created/mm_sub_recent.RDS")
cluster21dates <- readRDS("data/created/cluster21dates.RDS")
cluster22dates <- readRDS("data/created/cluster22dates.RDS")
rp <- st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
source("getEdges_new.R")

gps_before_21 <- downsampled %>% filter(date_il >= cluster21dates[[1]][1],
                                        date_il <= cluster21dates[[1]][2])
gps_after_21 <- downsampled %>% filter(date_il >= cluster21dates[[2]][1],
                                      date_il <= cluster21dates[[2]][2])
gps_before_22 <- downsampled %>% filter(date_il >= cluster22dates[[1]][1],
                                        date_il <= cluster22dates[[1]][2])
gps_after_22 <- downsampled %>% filter(date_il >= cluster22dates[[2]][1],
                                       date_il <= cluster22dates[[2]][2])

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

# XXX NEXT STEPS
# Make networks
# Density of networks
# Number of individuals in networks
# Degree distributions
