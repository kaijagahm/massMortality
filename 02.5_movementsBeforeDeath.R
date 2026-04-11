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
