# Proof of concept: checking for friendships
library(tidyverse)
library(targets)
library(sf)
library(vultureUtils)
library(future)
library(furrr)
library(changepoint)
source("R/getEdges_new_2026-02-01.R")

tar_load(ct)
tar_load(dt)
tar_load(stl)
tar_load(idc)
tar_load(r)
tar_load(tc)
tar_load(rp)

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

# 2. For a given time period, there's quite a bit of differentiation between dyads (raw SRI values)
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

# 2. Raw SRI values are not particularly consistent over time.
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

set.seed(3)
friends_ranked_obs %>%
  filter(pair %in% sample(unique(friends_ranked_obs$pair), 15)) %>%
  arrange(pair, period_idx) %>%
  group_by(pair) %>%
  mutate(col = sri[1]) %>%
  ungroup() %>%
  mutate(init_sri_rank = as.numeric(as.factor(col))) %>%
  ggplot(aes(x = period_idx, y = sri, group = pair))+
  geom_line(alpha = 0.7, aes(color = init_sri_rank), linewidth = 1.5)+
  theme_minimal()+
  scale_color_viridis_c(option = 2)+
  labs(y = "Raw SRI (flight)",
       x = "2-week period",
       color = "Initial SRI rank")+
  theme(text = element_text(size = 18))

# 2. Raw SRI ranks are also not particularly consistent over time.
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

set.seed(3)
friends_ranked_obs %>%
  filter(pair %in% sample(unique(friends_ranked_obs$pair), 15)) %>%
  arrange(pair, period_idx) %>%
  group_by(pair) %>%
  mutate(col = rank[1]) %>%
  ungroup() %>%
  mutate(init_rank_rank = as.numeric(as.factor(col))) %>%
  ggplot(aes(x = period_idx, y = rank, group = pair))+
  geom_line(alpha = 0.7, aes(color = init_rank_rank), linewidth = 1.5)+
  theme_minimal()+
  scale_color_viridis_c(option = 2)+
  labs(y = "Dyad rank (raw flight SRI)",
       x = "2-week period",
       color = "Initial dyad rank")+
  theme(text = element_text(size = 18))

# 2. After accounting for space, non-incidental z-scores still show differentiation between relationships
friends_ranked_over_time %>%
  mutate(period_idx = as.factor(as.numeric(as.factor(period)))) %>%
  filter(!is.infinite(z) & !is.na(z) & !is.nan(z)) %>%
  filter(period_idx == 1) %>%
  ggplot(aes(x = rank, y = z, group = ID1))+
  geom_line(alpha = 0.4)+
  theme_minimal()+
  theme(text = element_text(size = 18))+
  labs(y = "Z-score (flight)",
       x = "Rank")

# 3. Dyad z-scores are not particularly consistent over time.
friends_ranked_over_time %>%
  filter(!is.na(z) & !is.infinite(z) & !is.nan(z)) %>%
  mutate(period_idx = as.factor(as.numeric(as.factor(period)))) %>%
  arrange(pair, period_idx) %>%
  group_by(pair) %>%
  mutate(col = z[1]) %>%
  ungroup() %>%
  mutate(init_z_rank = as.numeric(as.factor(col))) %>%
  ggplot(aes(x = period_idx, y = z, group = pair))+
  geom_line(alpha = 0.2, aes(color = init_z_rank), linewidth = 1)+
  theme_minimal()+
  scale_color_viridis_c(option = 2)+
  labs(y = "Dyad z-score",
       x = "2-week period",
       color = "Initial dyad z-score")+
  theme(text = element_text(size = 18))

set.seed(3)
friends_ranked_over_time %>%
  filter(!is.na(z) & !is.infinite(z) & !is.nan(z)) %>%
  mutate(period_idx = as.factor(as.numeric(as.factor(period)))) %>%
  filter(pair %in% sample(unique(.$pair), 15)) %>%
  arrange(pair, period_idx) %>%
  group_by(pair) %>%
  mutate(col = z[1]) %>%
  ungroup() %>%
  mutate(init_z_rank = as.numeric(as.factor(col))) %>%
  ggplot(aes(x = period_idx, y = z, group = pair))+
  geom_line(alpha = 0.7, aes(color = init_z_rank), linewidth = 1.5)+
  theme_minimal()+
  scale_color_viridis_c(option = 2)+
  labs(y = "Dyad z-score",
       x = "2-week period",
       color = "Initial dyad z-score")+
  theme(text = element_text(size = 18))

# 3. Dyad ranks are not particularly consistent over time.
friends_ranked_over_time %>%
  filter(ID1 < ID2) %>%
  filter(!is.na(z) & !is.infinite(z) & !is.nan(z)) %>%
  mutate(period_idx = as.factor(as.numeric(as.factor(period)))) %>%
  arrange(pair, period_idx) %>%
  group_by(pair) %>%
  mutate(col = rank[1]) %>%
  ungroup() %>%
  mutate(init_rank_rank = as.numeric(as.factor(col))) %>%
  ggplot(aes(x = period_idx, y = rank, group = pair))+
  geom_line(alpha = 0.2, aes(color = init_rank_rank), linewidth = 1)+
  theme_minimal()+
  scale_color_viridis_c(option = 2)+
  labs(y = "Dyad rank",
       x = "2-week period",
       color = "Initial dyad rank")+
  theme(text = element_text(size = 18))

set.seed(3)
friends_ranked_over_time %>%
  filter(ID1 < ID2) %>%
  filter(!is.na(z) & !is.infinite(z) & !is.nan(z)) %>%
  mutate(period_idx = as.factor(as.numeric(as.factor(period)))) %>%
  filter(pair %in% sample(unique(.$pair), 15)) %>%
  arrange(pair, period_idx) %>%
  group_by(pair) %>%
  mutate(col = rank[1]) %>%
  ungroup() %>%
  mutate(init_rank_rank = as.numeric(as.factor(col))) %>%
  ggplot(aes(x = period_idx, y = rank, group = pair))+
  geom_line(alpha = 0.7, aes(color = init_rank_rank), linewidth = 1.5)+
  theme_minimal()+
  scale_color_viridis_c(option = 2)+
  labs(y = "Dyad rank",
       x = "2-week period",
       color = "Initial dyad rank")+
  theme(text = element_text(size = 18))

# Can we measure repeatability of these things?
## See the methods document for some ideas on how people have measured the stability of ranks/dyad strengths. It seems to vary a lot and nobody has really figured out a definitive measure.
## Let's try coefficient of variation (standard deviation/mean)

friends_ranked_obs %>%
  group_by(pair) %>%
  mutate(cv_sri = sd(sri)/mean(sri),
            cv_rank = sd(rank)/mean(rank)) %>%
  ggplot(aes(x = period_idx, y = sri, color = cv_sri, group = pair))+
  geom_line()+
  theme_minimal()+
  scale_color_viridis_c(option =2)

library(rptR)

## Observed
totest <- friends_ranked_obs %>% filter(!is.na(sri) & !is.nan(sri) & !is.infinite(sri))
rpt_obs_sri <- rpt(sri ~ (1 | pair), grname = "pair", data = totest, datatype = "Gaussian",
    nboot = 100, npermut = 0)
summary(rpt_obs_sri) # okay wait actually the repeatability for observed sri is quite high! 0.767 (0.747, 0.785)

rpt_obs_rank <- rpt(rank ~ (1 | pair), grname = "pair", data = totest, datatype = "Gaussian",
                    nboot = 100, npermut = 0)
summary(rpt_obs_rank) # repeatability of observed rank is 0.78 (0.763, 0.793)

# We can also look at multiple factors, allegedly
rpt_obs_sri_indiv.dyad <- rpt(sri ~ (1 | pair) + (1 | ID1), grname = c("pair", "ID1"),
            data = totest, datatype = "Gaussian", nboot = 0, npermut = 0)
summary(rpt_obs_sri_indiv.dyad) # this shows the pair and a single individual--which is interesting but probably not statistically sound. Would also need to add the other individual.

### XXX start here with calculating repeatability for the other measures!

## Z-scores
totest_z <- friends_ranked_over_time %>%
  filter(ID1 < ID2) %>%
  filter(!is.na(z), !is.infinite(z), !is.nan(z))