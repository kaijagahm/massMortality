# Proof of concept: checking for friendships
library(tidyverse)
library(targets)
library(sf)
library(vultureUtils)
source("R/getEdges_new_2026-02-01.R")

tar_load(windows_10)
touse <- windows_10[200:300]
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
tar_load(sris_10)
sris_10_touse <- sris_10[200:300]
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

friends_ranked <- backwards %>%
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

# Prepare datasets for shuffling
with_times <- purrr::map(touse, ~prepare_times(.x))
test <- with_times[[10]]

# Create 100x wraparound shuffled datasets
n <- 100
wrapped <- purrr::map(1:n, ~rotate_data_table(dataset = test, shiftMax = 2, idCol = "individual_local_identifier", dateCol = "date_il", timeCol = "time"), .progress = T)

# Create networks for each of those with SRI
tar_load(ct)
tar_load(dt)
tar_load(stl)
tar_load(idc)
tar_load(r)
tar_load(tc)
tar_load(rp)

sris_wrapped <- map(wrapped, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = "newdatetime", roostPolygons = rp), .progress = T)

# Join shuffled to observed SRI values
sris_df_wrapped <- purrr::list_rbind(sris_wrapped, names_to = "iter")

# Visualize
obs <- sris_10_touse[[10]] %>%
  rename("sri_obs" = sri)

joined <- sris_df_wrapped %>%
  left_join(obs, by = c("ID1", "ID2", "pair"))

zscores <- joined %>%
  group_by(ID1, ID2, pair) %>%
  summarize(z = (sri_obs[1]-mean(sri))/sd(sri),
            p = mean(sri > sri_obs)) # hmm. here they all seem to be exceeding expected, which doesn't make sense. Something must have gone wrong with the shuffling...

hist(zscores$p)
