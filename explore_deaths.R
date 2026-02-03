library(here)
library(tidyverse)
library(sf)
library(targets)
library(dbscan)

# Examining the deaths data
tar_load(deaths)
deaths_simple <- deaths %>%
  select(Nili_id, date_death, death_kg, death_kg_detail) %>%
  distinct() %>%
  arrange(date_death)
if(!nrow(deaths_simple) == length(unique(deaths_simple$Nili_id))){stop("Number of individuals does not equal number of rows")} # double check that there is only one cause of death for each individual

deaths_simple %>%
  mutate(cause = case_when(death_kg %in% c("acclimation", "heatstroke") ~ "acclimation or heatstroke", 
                           death_kg == "collision" & death_kg_detail == "powerline" ~ "electrocution",
                           death_kg == "poisoning" & death_kg_detail == "carbamates" ~ "poisoning (carbamates)",
                           death_kg == "poisoning" & death_kg_detail == "NSAID" ~ "poisoning (NSAID)",
                           .default = death_kg)) %>%
  group_by(year = lubridate::year(date_death), cause) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n, fill = cause))+
  geom_col()+
  theme_minimal()+
  labs(y = "Number of vultures", x = "Year")+
  scale_fill_manual(name = "Cause of death",
                    values = c("orange", "red", "yellow", "darkblue", "dodgerblue", "dodgerblue2", "dodgerblue3", "firebrick3", "green3", "gray90"))

# Proof of concept: movement in the two weeks leading up to death ---------
# 1. Load in data from mvmt soc
tar_load(cleaned, store = here("~/Desktop/projects/MvmtSoc/_targets/"))
cleaned <- cleaned %>% mutate(timestamp_il = lubridate::with_tz(timestamp, tzone = "Israel"),
                              date_il = lubridate::date(timestamp_il))

# 2. Restrict deaths to vultures that died within the bounds of that dataset
mindate <- min(cleaned$date_il)
maxdate <- max(cleaned$date_il)
focal_deaths <- deaths_simple %>%
  mutate(date_death = lubridate::ymd(date_death)) %>%
  filter(date_death > mindate + days(30),
         date_death < maxdate)
dim(focal_deaths) # we have 49 deaths during this period

# 3. Grab data for those vultures for the 2 weeks leading up to their date of death
data_list_2weeksbefore <- vector(mode = "list", length = nrow(focal_deaths))
for(i in 1:length(data_list_2weeksbefore)){
  if(focal_deaths$Nili_id[i] %in% cleaned$Nili_id){
    out <- cleaned %>%
      filter(Nili_id == focal_deaths$Nili_id[i],
             date_il >= focal_deaths$date_death[i]-days(14),
             date_il <= focal_deaths$date_death[i]) %>%
      mutate(days_before_death = as.numeric(difftime(focal_deaths$date_death[i], date_il, units = "days")))
  }else{
    out <- cleaned[0,]
  }
  data_list_2weeksbefore[[i]] <- out
}

data_list_1month_2weeksbefore <- vector(mode = "list", length = nrow(focal_deaths))
for(i in 1:length(data_list_1month_2weeksbefore)){
  if(focal_deaths$Nili_id[i] %in% cleaned$Nili_id){
    out <- cleaned %>%
      filter(Nili_id == focal_deaths$Nili_id[i],
             dateOnly >= focal_deaths$date_death[i]-days(30),
             dateOnly <= focal_deaths$date_death[i]-days(14)) %>%
      mutate(days_before_death = as.numeric(difftime(focal_deaths$date_death[i], date_il, units = "days")))
  }else{
    out <- cleaned[0,]
  }
  data_list_1month_2weeksbefore[[i]] <- out
}

mean(map_dbl(data_list_2weeksbefore, nrow)>0) # We have at least some GPS data on 90% of the individuals that died--decent coverage
mean(map_dbl(data_list_1month_2weeksbefore, nrow)>0) # We have a similar number of individuals that have GPS coverage for the period 1 month-2 weeks before death. I wonder if there's an individual in there that was released only a few days before death, or something.

month <- purrr::list_rbind(data_list_1month_2weeksbefore)
twoweeks <- purrr::list_rbind(data_list_2weeksbefore)

# Convert to sf
month <- month %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  st_transform(32636)
twoweeks <- twoweeks %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  st_transform(32636)

# 4. Calculate some very basic movement metrics for each day (% points in flight, distance traveled, average speed)
month_stats <- month %>%
  arrange(timestamp_il) %>%
  group_by(Nili_id) %>%
  mutate(dist_to_next = st_distance(geometry, 
                                    lead(geometry),
                                    by_element = TRUE)) %>%
  group_by(Nili_id, days_before_death, date_il) %>%
  summarize(pts_in_flight = sum(ground_speed >= 5)/n(),
            dist_traveled = sum(dist_to_next),
            avg_speed = mean(ground_speed))
twoweek_stats <- twoweeks %>%
  arrange(timestamp_il) %>%
  group_by(Nili_id) %>%
  mutate(dist_to_next = st_distance(geometry, 
                                    lead(geometry),
                                    by_element = TRUE)) %>%
  group_by(Nili_id, days_before_death, date_il) %>%
  summarize(pts_in_flight = sum(ground_speed >= 5)/n(),
            dist_traveled = sum(dist_to_next),
            avg_speed = mean(ground_speed))

# Visualize
twoweek_stats %>%
  ggplot(aes(x = days_before_death, y = pts_in_flight, col = Nili_id))+
  geom_line()+
  theme(legend.position = "none") # super duper messy

twoweek_stats %>%
  ggplot(aes(x = days_before_death, y = as.numeric(dist_traveled), col = Nili_id))+
  geom_line()+
  theme(legend.position = "none")

twoweek_stats %>%
  ggplot(aes(x = days_before_death, y = as.numeric(avg_speed), col = Nili_id))+
  geom_line()+
  theme(legend.position = "none")

# We see hints at decreases in some of these cases, but not anything conclusive. It would be interesting to look at whether there are differences between different types of deaths.

tar_load(mm)
# Need to recode the causes of mortality according to the legend
names(mm) <- tolower(names(mm))
mm <- mm %>%
  mutate(com = case_when(is.na(com) ~ "uk",
                         .default = com)) %>%
  mutate(com_code = case_when(com %in% c("acclim", "acclimation issue") ~ "4",
                              com == "anemia" ~ "1.4",
                              com == "bite wounds" ~ "1.2",
                              com == "chever syndrome" ~ "2.5",
                              com == "collision" ~ "2.3",
                              com %in% c("collision power line", "collision with powerline") ~ "2.3.1",
                              com == "collision with helicopter" ~ "2.3.4",
                              com == "collision with plane" ~ "2.3.4",
                              com == "collision/acclim" ~ "2.3",
                              com == "congenital" ~ "1.5",
                              com == "disease" ~ "1.4",
                              com == "drowned" ~ "1.1",
                              com == "egg laying" ~ "1.5",
                              com == "electrocution" ~ "2.2",
                              com == "gout" ~ "2.4.3",
                              com %in% c("gun_shot", "gun shot") ~ "2.1",
                              com == "heat stroke/acclim" ~ "4.1",
                              com == "heat_stroke" ~ "4.1",
                              com == "hydrocephalus" ~ "1.5",
                              com == "infection" ~ "1.5",
                              com %in% c("lead poisoning", "lead_poisoning") ~ "2.4.2",
                              com == "n" ~ "3",
                              com == "neuro signs" ~ "2.5",
                              com == "pancreatitis" ~ "1.4",
                              com == "pentobarbital" ~ "2.4",
                              com == "persecution" ~ "2.1",
                              com %in% c("pesticide", "pesticides", "poison") ~ "2.4",
                              com == "rickets" ~ "1.4",
                              com == "rikoshet" ~ "2.3",
                              com == "rotten chick" ~ "1.5",
                              com == "sus. Collision" ~ "2.3",
                              com == "uk" ~ "3",
                              com == "viper" ~ "1.5",
                              .default = NA
  )) %>%
  # extract the different portions of the code
  mutate(com_code_1 = as.numeric(str_extract(com_code, pattern = "^[0-9]")),
         com_code_2 = as.numeric(str_extract(com_code, pattern = "(?<=^[0-9]\\.)[0-9]")),
         com_code_3 = as.numeric(str_extract(com_code, pattern = "(?<=^[0-9]\\.[0-9]\\.)[0-9]"))) %>%
  mutate(com_words = case_when(com_code == "1" ~ "nat",
                               com_code == "1.1" ~ "nat_drow",
                               com_code == "1.2" ~ "nat_pred",
                               com_code == "1.3" ~ "nat_star",
                               com_code == "1.4" ~ "nat_dise",
                               com_code == "1.5" ~ "nat_othe",
                               com_code == "2" ~ "hum",
                               com_code == "2.1" ~ "hum_hunt",
                               com_code == "2.2" ~ "hum_elec",
                               com_code == "2.3" ~ "hum_coll",
                               com_code == "2.3.1" ~ "hum_coll_powe",
                               com_code == "2.3.2" ~ "hum_coll_wind",
                               com_code == "2.3.3" ~ "hum_coll_road",
                               com_code == "2.3.4" ~ "hum_coll_othe",
                               com_code == "2.3.5" ~ "hum_coll_zepp",
                               com_code == "2.4" ~ "hum_pois",
                               com_code == "2.4.1" ~ "hum_pois_chei",
                               com_code == "2.4.2" ~ "hum_pois_lead",
                               com_code == "2.4.3" ~ "hum_pois_nsai",
                               com_code == "2.4.4" ~ "hum_pois_othe",
                               com_code == "2.5" ~ "hum_othe",
                               com_code == "3" ~ "unk",
                               com_code == "4" ~ "acc",
                               com_code == "4.1" ~ "acc_heat",
                               .default = NA)) %>%
  mutate(com_words_1 = str_extract(com_words, "^[a-z]{3}"),
         com_words_2 = str_extract(com_words, "(?<=^[a-z]{3}\\_)[a-z]{4}"),
         com_words_3 = str_extract(com_words, "(?<=^[a-z]{3}\\_[a-z]{4}\\_)[a-z]{4}")) %>%
  mutate(across(starts_with("com_code"), ~replace_na(.x, 0))) %>%
  mutate(across(starts_with("com_words"), ~replace_na(.x, ""))) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date))
  

mm_sub <- mm %>% select(id, transm, origin, status, date, year, month, place, `year born`, sex, starts_with("com"))

mm_sub_recent <- mm_sub %>%
  filter(year %in% 2020:2025)

mm_sub_recent %>%
  mutate(across(starts_with("com_words"), as.factor)) %>%
  ggplot(aes(x = as.factor(month), y = com_words_1, col = com_words_2, shape = com_words_3))+
  geom_point(size = 2)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  facet_wrap(~year, ncol = 1)+
  labs(y = "COM_broad",
    col = "COM_medium",
    shape = "COM_fine",
    x = "Month")+
  scale_color_viridis_d()+
  scale_shape_manual(values = c(1, 16, 17, 8))

mm_sub_recent %>%
  mutate(date_in_year = as.Date(sprintf(
      "2000-%02d-%02d",
      month(date),
      day(date)))) %>%
  mutate(across(starts_with("com_words"), as.factor)) %>%
  ggplot(aes(x = date_in_year, y = 1, col = com_words_1, shape = com_words_2))+
  geom_point(size = 3, alpha = 0.7)+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_x_date(
    limits = as.Date(c("2000-01-01", "2000-12-31")),
    breaks = as.Date(paste0("2000-", sprintf("%02d", 1:12), "-01")),
    labels = 1:12)+
  facet_wrap(~year, ncol = 1)+
  labs(y = "",
       col = "COM_broad",
       shape = "COM_medium",
       x = "Month",
       title = "Causes of Mortality, 2020-2025")+
  scale_shape_manual(values = c(1, 4, 14, 8, 15, 18, 0, 17))+
  NULL

# Now let's try clustering the event dates into "mortality groups" using 1D DBSCAN
#eps = maximum gap
#minPts = minimum events per cluster
# going to set eps as 3 days and minPts as 2, just offhand.
# dates converted to numeric will be in seconds, not days
eps_days <- 5
minpts <- 2
eps_seconds <- 3*24*60*60
death_dates <- as.matrix(as.numeric(mm_sub_recent$date))
db <- dbscan(death_dates, eps = eps_seconds, minPts = minpts)
mm_sub_recent$cluster_eps5min2 <- db$cluster

# Make new cluster IDs on a per-year basis
mm_sub_recent <- mm_sub_recent %>%
  arrange(year, date) %>%   # ensure time order
  group_by(year) %>%
  mutate(
    year_cluster_eps5min2 = case_when(
      cluster_eps5min2 == 0 ~ 0L,
      TRUE ~ as.integer(factor(cluster_eps5min2, levels = unique(cluster_eps5min2[cluster_eps5min2 != 0])))
    )
  ) %>%
  ungroup()
table(mm_sub_recent$year_cluster_eps5min2)

# Now that these are clustered, let's re-plot them, now coloring by cluster and faceting by year.
mm_sub_recent <- mm_sub_recent %>%
  mutate(
    date_in_year = as.Date(sprintf(
      "2000-%02d-%02d",
      month(date),
      day(date)
    ))
  )

cluster_labels <- mm_sub_recent  %>%
  filter(year_cluster_eps5min2 != 0) %>%
  group_by(year, year_cluster_eps5min2) %>%
  summarise(
    n = n(),
    label_x = mean(date_in_year),
    .groups = "drop"
  )

mm_sub_recent %>%
  mutate(across(starts_with("com_words"), as.factor)) %>%
  ggplot(aes(x = date_in_year, y = 1, col = factor(year_cluster_eps5min2), shape = com_words_1))+
  geom_point(size = 3, alpha = 0.6)+
  theme_bw()+
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_x_date(
    limits = as.Date(c("2000-01-01", "2000-12-31")),
    breaks = as.Date(paste0("2000-", sprintf("%02d", 1:12), "-01")),
    labels = 1:12)+
  facet_wrap(~year, ncol = 1)+
  labs(y = "",
       col = "Cluster",
       shape = "COM_broad",
       x = "Month",
       title = "Mortality clusters, 2020-2025")+
  scale_color_manual(values = c("gray", "red", "darkorange", "gold", "olivedrab", "dodgerblue", "purple"))+
  guides(color = "none")+
  geom_text(
    data = cluster_labels,
    aes(x = label_x, y = 1, label = n,
      col = factor(year_cluster_eps5min2)),
    inherit.aes = FALSE,
    vjust = -1, size = 3)+
  NULL

write_rds(mm_sub_recent, file = here("data/created/mm_sub_recent.RDS"))

# Okay, now it's time to investigate a single poisoning event/clustered mortality event.

# A couple candidates: there's one with 5 individuals in 2022, and there's also one with 13 individuals in 2021.
# Let's look a bit closer at these.
# 2021 cluster 4 and 2022 cluster 2.

cluster21 <- mm_sub_recent %>% filter(year_cluster_eps5min2 == 4 & year == 2021)
cluster22 <- mm_sub_recent %>% filter(year_cluster_eps5min2 == 2 & year == 2022)

cluster21 # this is the kina valley mass poisoning
View(cluster21) # sure enough, all of the individuals involved in this event died by poisoning
range(cluster21$date) # all the same day, 10/24

days_before_max <- 365
days_before_min <- 0
days_after_min <- 0
days_after_max <- 365

cluster21_before_dates <- c(min(cluster21$date)-days(days_before_max), min(cluster21$date)-days(days_before_min))
cluster21_after_dates <- c(max(cluster21$date)+days(days_after_min), max(cluster21$date)+days(days_after_max))
cluster21dates <- list(cluster21_before_dates, cluster21_after_dates)

cluster22 # this is also probably a mass poisoning? COM is gout and it's all in the same area, large crater
View(cluster22) # and yes, sure enough, cause of death for all of them is poisoning.
cluster22_before_dates <- c(min(cluster22$date)-days(days_before_max), min(cluster22$date)-days(days_before_min))
cluster22_after_dates <- c(max(cluster22$date)+days(days_after_min), max(cluster22$date)+days(days_after_max))
cluster22dates <- list(cluster22_before_dates, cluster22_after_dates)

write_rds(cluster21dates, file = "data/created/cluster21dates.RDS")
write_rds(cluster22dates, file = "data/created/cluster22dates.RDS")

# Next step is to grab the cleaned data. This might get annoying; we'll see. I'm going to copy the data infrastructure straight from arrivalatcarcasses.
