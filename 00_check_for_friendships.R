# Proof of concept: checking for friendships
library(tidyverse)
library(targets)
library(sf)
library(vultureUtils)
library(future)
library(furrr)
library(changepoint)
source("R/getEdges_new_2026-02-01.R")

# tar_load(windows_10)
# touse <- windows_10[200:300]
# write_rds(touse, file = "data/created/touse.RDS")
touse <- readRDS("data/created/touse.RDS")
rm(windows_10)
test <- touse[[10]]

# Functions
prepare_times <- function(df){
  out <- df %>% mutate(time = substr(as.character(timestamp_il), 12, 19))
  return(out)
}

# rotate_data_table ------------------------------------------------------------
# Note: this no longer actually takes a data_table, just keeping the name for consistency with previous.
# Unlike the previous function, this one requires that you separate the dates and times into separate columns beforehand.
rotate_data_table <- function(dataset, shiftMax, idCol = "indiv", dateCol = "date", timeCol = "time"){
  indivList <- dataset %>%
    group_by(.data[[idCol]]) %>%
    group_split(.keep = T)
  joined <- vector(mode = "list", length = length(indivList))
  for(indiv in 1:length(indivList)){
    x <- indivList[[indiv]]
    shift <- sample(-(shiftMax):shiftMax, size = 1)
    #cat(shift, "\n")
    # get all unique days that show up
    days <- sort(unique(x[[dateCol]]))
    
    # get min and max dates to shift around (the "poles" of the conveyor)
    selfMinDate <- min(days, na.rm = T)
    selfMaxDate <- max(days, na.rm = T)
    
    # create a total sequence of dates to select from
    daysFilled <- seq(lubridate::ymd(selfMinDate), lubridate::ymd(selfMaxDate), by = "day")
    # converting to numbers so we can use %%--which dates are the ones we started with?
    vec <- which(daysFilled %in% days)
    shiftedvec <- vec + shift # shift
    new <- (shiftedvec - min(vec)) %% (max(vec)-min(vec)+1)+1 # new dates as numbers
    shiftedDates <- daysFilled[new] # select those dates from the possibilities
    
    # Make a data frame to hold the old and new dates
    daysDF <- bind_cols({{dateCol}} := days, 
                        "newDate" = shiftedDates,
                        shift = shift)
    nw <- left_join(x, daysDF, by = dateCol)
    
    if(!is.null(timeCol)){
      nw$newdatetime <- lubridate::ymd_hms(paste(nw$newDate, nw[[timeCol]]))
    }
    joined[[indiv]] <- nw
  }
  out <- purrr::list_rbind(joined)
  return(out)
}

# Get flight networks with SRI (new)
# tar_load(sris_10)
# sris_10_touse <- sris_10[200:300]
# write_rds(sris_10_touse, file = "data/created/sris_10_touse.RDS")
sris_10_touse <- readRDS("data/created/sris_10_touse.RDS")
rm(sris_10)

sris_df <- purrr::list_rbind(sris_10_touse, names_to = "period") %>%
  mutate(period_idx = as.numeric(factor(period)))

# Visualize observed dyad strengths (e.g. overall, over time)

## How consistent are observed dyad strengths over time?
sris_df %>%
  filter(pair %in% sample(unique(sris_df$pair), 15)) %>%
  ggplot(aes(x = period_idx, y = sri, color = pair))+
  geom_line(alpha = 0.3)+
  theme_classic() # SRI does not appear to be particularly consistent over time, although there might possibly be some trends? To really figure this out, we'd have to see about the average SRI at the time across all the pairs.

## What about individuals?
all(sris_df$ID1 < sris_df$ID2) # TRUE, so we don't have the dyads listed both ways. In order to look at individuals, we'll need to reverse that and add it

backwards <- sris_df %>% rename("ID1" = ID2, "ID2" = ID1)
sris_df_doubled <- bind_rows(sris_df, backwards) # now we have all dyads backwards and forwards.

sris_df_doubled_test <- sris_df_doubled %>% filter(period_idx == 10)

friends_ranked <- sris_df_doubled %>%
  group_by(period, ID1) %>%
  arrange(desc(sri)) %>%
  mutate(rank = 1:n()) %>%
  ungroup()

friends_ranked_test <- sris_df_doubled_test %>%
  group_by(period, ID1) %>%
  arrange(desc(sri)) %>%
  mutate(rank = 1:n()) %>%
  ungroup()

friends_ranked %>%
  filter(period_idx %in% 1:9,
         rank < 20) %>% # have to cut it off somewhere to be able to see the curves
  ggplot(aes(x = rank, y = sri, group = ID1))+
  geom_line(alpha = 0.2)+
  facet_wrap(~period_idx)+
  theme_classic() # some individuals have a few relationships that are stronger than all the others (in *observed* values), but many of them don't.

# What if we exclude the ones over 0.03?
friends_ranked %>%
  filter(period_idx %in% 1:9,
         sri < 0.03,
         rank < 20) %>% # have to cut it off somewhere to be able to see the curves
  ggplot(aes(x = rank, y = sri, group = ID1))+
  geom_line(alpha = 0.2)+
  facet_wrap(~period_idx)+
  theme_classic() # yeah, so some differentiation, but we don't see clear cutpoints for most individuals. There are some that have asymmetrical curves. Now of course the question will be comparing this to the shuffled data.

# Create 100x wraparound shuffled datasets
n <- 50
# Prepare datasets for shuffling
with_times <- purrr::map(touse, ~prepare_times(.x))
test <- with_times[[10]]
#sris_10_touse

shuffle_function <- function(dataset_with_times, sris_obs, n_iter, sm){
  wrapped <- purrr::map(1:n_iter, ~rotate_data_table(dataset = dataset_with_times, shiftMax = sm, idCol = "individual_local_identifier", dateCol = "date_il", timeCol = "time"), .progress = T)
  
  sris <- purrr::map(wrapped, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = "newdatetime", roostPolygons = rp))
  
  sris_df <- purrr::list_rbind(sris, names_to = "iter")
  
  # Visualize
  obs <- sris_obs %>%
    rename("sri_obs" = sri)
  
  joined <- sris_df %>%
    left_join(obs, by = c("ID1", "ID2", "pair"))
  
  zscores <- joined %>%
    group_by(ID1, ID2, pair) %>%
    summarize(z = (sri_obs[1]-mean(sri, na.rm = T))/sd(sri, na.rm = T),
              p = mean(sri > sri_obs))
  
  backwards <- zscores %>% rename("ID1" = ID2, "ID2" = ID1)
  
  doubled <- bind_rows(zscores, backwards)
  
  friends_ranked <- doubled %>%
    group_by(ID1) %>%
    arrange(desc(z)) %>%
    mutate(rank = 1:n()) %>%
    ungroup()
  
  return(list("zscores_doubled" = doubled,
              "friends_ranked" = friends_ranked))
}

future::plan(multisession, workers = 10)
output_shuffled <- furrr::future_map2(with_times[10:19], sris_10_touse[10:19], ~shuffle_function(.x, .y, n_iter = 100, sm = 2), .progress = T)

names(output_shuffled) <- names(with_times[10:19])

zscores_over_time <- purrr::list_rbind(map(output_shuffled, 1), names_to = "period")
friends_ranked_over_time <- purrr::list_rbind(map(output_shuffled, 2), names_to = "period")

zscores_over_time %>%
  mutate(pair = factor(pair)) %>%
  filter(pair %in% sample(unique(zscores_over_time$pair), 15),
         !is.na(z) & !is.nan(z) & !is.infinite(z)) %>%
  ggplot(aes(x = period, y = z, group = pair, color = pair))+
  geom_line()+
  theme_classic()

# 1. Do dyads interact more often than expected by chance?
zscores_over_time %>%
  mutate(period = factor(as.numeric(as.factor(period)))) %>%
  ggplot(aes(x = period, y = z))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha = 0.1, pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "Z-score",
       x = "2-week period")+
  theme(text = element_text(size = 18))
  # Almost all dyads interact significantly more often than expected by chance.

# 2. Raw (observed) SRI values don't appear to show much differentiation between different dyad relationships
raw <- purrr::list_rbind(sris_10_touse[10:19], names_to = "period") %>%
  mutate(period_idx = factor(as.numeric(as.factor(period))))

friends_ranked_obs <- raw %>%
  group_by(period_idx, ID1) %>%
  arrange(desc(sri)) %>%
  mutate(rank = 1:n()) %>%
  ungroup()

friends_ranked_obs %>%
  filter(period_idx == 1) %>%
  ggplot(aes(x = rank, y = sri, group = ID1))+
  geom_line(alpha = 0.5)+
  theme_minimal()+
  theme(text = element_text(size = 18))+
  labs(y = "Raw SRI (flight)",
       x = "Rank")

friends_ranked_obs %>%
  arrange(pair, period_idx) %>%
  group_by(pair) %>%
  mutate(col = sri[1]) %>%
  ungroup() %>%
  mutate(init_sri_rank = as.numeric(as.factor(col))) %>%
  ggplot(aes(x = period_idx, y = sri, group = pair))+
  geom_line(alpha = 0.2, aes(color = init_sri_rank), linewidth = 1)+
  theme_minimal()+
  scale_color_viridis_c(option = 2)+
  labs(y = "Raw SRI (flight)",
       x = "2-week period",
       color = "Initial SRI rank")+
  theme(text = element_text(size = 18))

friends_ranked_obs %>%
  arrange(pair, period_idx) %>%
  group_by(pair) %>%
  mutate(col = rank[1]) %>%
  ungroup() %>%
  mutate(init_rank_rank = as.numeric(as.factor(col))) %>%
  ggplot(aes(x = period_idx, y = rank, group = pair))+
  geom_line(alpha = 0.2, aes(color = init_rank_rank), linewidth = 1)+
  theme_minimal()+
  scale_color_viridis_c(option = 2)+
  labs(y = "Dyad rank (raw flight SRI)",
       x = "2-week period",
       color = "Initial dyad rank")+
  theme(text = element_text(size = 18))

# 2. After accounting for space, non-incidental z-scores show more differentiation between relationships
friends_ranked_over_time %>%
  mutate(period_idx = as.factor(as.numeric(as.factor(period)))) %>%
  filter(!is.infinite(z) & !is.na(z) & !is.nan(z)) %>%
  filter(period_idx == 1) %>%
  ggplot(aes(x = rank, y = z, group = pair))+
  geom_line() # XXX SOMETHING IS WRONG HERE



# trying two different shift parameters
# wrapped_sm2 <- purrr::map(1:n, ~rotate_data_table(dataset = test, shiftMax = 2, idCol = "individual_local_identifier", dateCol = "date_il", timeCol = "time"), .progress = T)
# wrapped_sm1 <- purrr::map(1:n, ~rotate_data_table(dataset = test, shiftMax = 1, idCol = "individual_local_identifier", dateCol = "date_il", timeCol = "time"), .progress = T)

# write_rds(wrapped_sm2, file = "data/created/wrapped_sm2.RDS")
wrapped_sm2 <- readRDS("data/created/wrapped_sm2.RDS")

# write_rds(wrapped_sm1, file = "data/created/wrapped_sm1.RDS")
wrapped_sm1 <- readRDS("data/created/wrapped_sm1.RDS")

# Create networks for each of those with SRI
tar_load(ct)
tar_load(dt)
tar_load(stl)
tar_load(idc)
tar_load(r)
tar_load(tc)
tar_load(rp)

#future::plan(multisession, workers = 10)

# sris_wrapped_sm2 <- furrr::future_map(wrapped_sm2, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = "newdatetime", roostPolygons = rp))
# write_rds(sris_wrapped_sm2, file = "data/created/sris_wrapped_sm2.RDS")
sris_wrapped_sm2 <- readRDS("data/created/sris_wrapped_sm2.RDS")

# sris_wrapped_sm1 <- furrr::future_map(wrapped_sm1, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = "newdatetime", roostPolygons = rp))
# write_rds(sris_wrapped_sm1, file = "data/created/sris_wrapped_sm1.RDS")
sris_wrapped_sm1 <- readRDS("data/created/sris_wrapped_sm1.RDS")

# Join shuffled to observed SRI values
sris_df_wrapped_sm2 <- purrr::list_rbind(sris_wrapped_sm2, names_to = "iter")
sris_df_wrapped_sm1 <- purrr::list_rbind(sris_wrapped_sm1, names_to = "iter")

# Visualize
obs <- sris_10_touse[[10]] %>%
  rename("sri_obs" = sri)

joined_sm2 <- sris_df_wrapped_sm2 %>%
  left_join(obs, by = c("ID1", "ID2", "pair"))
joined_sm1 <- sris_df_wrapped_sm1 %>%
  left_join(obs, by = c("ID1", "ID2", "pair"))

zscores_sm2 <- joined_sm2 %>%
  group_by(ID1, ID2, pair) %>%
  summarize(z = (sri_obs[1]-mean(sri, na.rm = T))/sd(sri, na.rm = T),
            p = mean(sri > sri_obs)) %>%
  mutate(z = case_when(is.infinite(z)|is.nan(z)|is.na(z) ~ 0, .default = z)) # hmm. here they all seem to be exceeding expected, which is surprising.

zscores_sm1 <- joined_sm1 %>%
  group_by(ID1, ID2, pair) %>%
  summarize(z = (sri_obs[1]-mean(sri, na.rm = T))/sd(sri, na.rm = T),
            p = mean(sri > sri_obs)) %>%
  mutate(z = case_when(is.infinite(z)|is.nan(z)|is.na(z) ~ 0, .default = z))

zscores_backwards_sm2 <- zscores_sm2 %>% rename("ID1" = ID2, "ID2" = ID1)
zscores_backwards_sm1 <- zscores_sm1 %>% rename("ID1" = ID2, "ID2" = ID1)

zscores_doubled_sm2 <- bind_rows(zscores_sm2, zscores_backwards_sm2) # now we have all dyads backwards and forwards.
zscores_doubled_sm1 <- bind_rows(zscores_sm1, zscores_backwards_sm1)

friends_ranked_zscores_sm2 <- zscores_doubled_sm2 %>%
  group_by(ID1) %>%
  arrange(desc(z)) %>%
  mutate(rank = 1:n()) %>%
  ungroup()

friends_ranked_zscores_sm1 <- zscores_doubled_sm1 %>%
  group_by(ID1) %>%
  arrange(desc(z)) %>%
  mutate(rank = 1:n()) %>%
  ungroup()

friends_ranked_zscores_sm2 %>%
  ggplot(aes(x = rank, y = z, group = ID1))+
  geom_line(alpha = 0.2)+
  theme_classic()+
  ggtitle("shiftMax 2")# looks like after accounting for z-score, we do have some relationships that are stronger than others!

friends_ranked_zscores_sm1 %>%
  ggplot(aes(x = rank, y = z, group = ID1))+
  geom_line(alpha = 0.2)+
  theme_classic()+
  ggtitle("shiftMax 1") # looks qualitatively similar.

mylist_sm2 <- friends_ranked_zscores_sm2 %>%
  arrange(ID1) %>%
  group_by(ID1) %>%
  group_split()
ids_sm2 <- map_chr(mylist_sm2, ~.x$ID1[1])

mylist_sm1 <- friends_ranked_zscores_sm1 %>%
  arrange(ID1) %>%
  group_by(ID1) %>%
  group_split()
ids_sm1 <- map_chr(mylist_sm1, ~.x$ID1[1])

# Changepoint analysis: z-scores
## By mean
cpts_mean_sm2 <- map(mylist_sm2, ~cpt.mean(.x$z))
n.cpts_mean_sm2 <- map_dbl(cpts_mean_sm2, ~length(.x@cpts))
idx.cpts_mean_sm2 <- map(cpts_mean_sm2, ~.x@cpts)
names(idx.cpts_mean_sm2) <- ids_sm2
cpts_df_sm2 <- purrr::list_rbind(map(idx.cpts_mean_sm2, ~data.frame("cpt_idx" = .x)), names_to = "ID1")
hist(n.cpts_mean_sm2)
cpts_df_sm2 %>%
  ggplot(aes(x = cpt_idx))+
  geom_histogram() # no particularly clear pattern as to where the splits happen.

cpts_mean_sm1 <- map(mylist_sm1, ~cpt.mean(.x$z))
n.cpts_mean_sm1 <- map_dbl(cpts_mean_sm1, ~length(.x@cpts))
idx.cpts_mean_sm1 <- map(cpts_mean_sm1, ~.x@cpts)
names(idx.cpts_mean_sm1) <- ids_sm1
cpts_df_sm1 <- purrr::list_rbind(map(idx.cpts_mean_sm1, ~data.frame("cpt_idx" = .x)), names_to = "ID1")
hist(n.cpts_mean_sm1)
cpts_df_sm1 %>%
  ggplot(aes(x = cpt_idx))+
  geom_histogram() #very similar to sm2.

# I wonder how this compares to the changepoints in the observed data?
obs_ranked_list <- friends_ranked_test %>%
  mutate(sri = case_when(is.na(sri)|is.nan(sri)|is.infinite(sri) ~ 0, .default = sri)) %>%
  ungroup() %>%
  arrange(ID1) %>%
  group_by(ID1) %>%
  group_split()
ids_obs <- map_chr(obs_ranked_list, ~.x$ID1[1])

scaled <- map(obs_ranked_list, ~replace_na(scale(.x$sri)[,1], 0))
cpts_mean_obs <- map(scaled, ~cpt.mean(.x))
n.cpts_mean_obs <- map_dbl(cpts_mean_obs, ~length(.x@cpts))
idx.cpts_mean_obs <- map(cpts_mean_obs, ~.x@cpts)
names(idx.cpts_mean_obs) <- ids_obs
cpts_df_obs <- purrr::list_rbind(map(idx.cpts_mean_obs, ~data.frame("cpt_idx" = .x)), names_to = "ID1")
hist(n.cpts_mean_obs)
cpts_df_obs %>%
  ggplot(aes(x = cpt_idx))+
  geom_histogram() # this distribution looks pretty different!

# Clearly, the shuffling is having an effect when it comes to the distribution of friendships.