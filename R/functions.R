get_loginObject <- function(pw){
  load(pw)
  loginObject <- move::movebankLogin(username = "kaijagahm", password = pw)
  rm(pw)
  return(loginObject)
}

get_ww <- function(ww_file, date_cols){
  file <- read_excel(ww_file, sheet = "all gps tags")
  fixed_dates <- file %>%
    mutate(across(all_of(date_cols), ~as.Date(as.numeric(.x), origin = "1899-12-30")))
  return(fixed_dates)
}

get_deaths <- function(ww, min_date, max_date){
  ww %>%
    dplyr::filter(!is.na(date_death)) %>%
    dplyr::filter(date_death >= min_date, date_death <= max_date) %>%
    dplyr::select(Nili_id, date_death, `death/hospital`, comments, death_kg,	death_kg_detail)
}

fix_names_ages <- function(gps, ww_file){
  ww <- readxl::read_excel(ww_file, sheet = "all gps tags")
  # pull out just the names columns, nothing else, and remove any duplicates
  ww_tojoin <- ww %>% dplyr::select(Nili_id, Movebank_id, birth_year) %>% distinct() 
  
  # Prepare for join: are there any individuals in the `local_identifier` column of `joined0` that don't appear in the `Movebank_id` column of `ww_tojoin`?
  problems <- gps %>% filter(!(individual_local_identifier %in% ww_tojoin$Movebank_id)) %>% pull(individual_local_identifier) %>% unique()
  
  # problems # check this against the who's who and try to make changes. The only problem is E60w.
  out <- gps %>%
    left_join(ww_tojoin, by = c("individual_local_identifier" = "Movebank_id"))
  
  out <- out %>%
    mutate(Nili_id = case_when(is.na(Nili_id) & individual_local_identifier == "E60w" ~ "gili", .default = Nili_id),
           birth_year = case_when(individual_local_identifier == "E60w" ~ ww$birth_year[ww$Nili_id == "gili"][1], .default = birth_year))
  
  out <- out %>%
    mutate(age_2021 = 2021-birth_year,
           age_2022 = 2022-birth_year)
  
  return(out)
}

process_deployments <- function(ww_file,
                                movebank_dataset,
                                default_end_date = as.Date("2023-09-15"),
                                verbose = TRUE) {
  #Packages
  requireNamespace("dplyr",     quietly = TRUE)
  requireNamespace("purrr",     quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  
  # Read in the who's who
  ww <- read_excel(ww_file, sheet = "all gps tags")
  ww <- ww %>%
    mutate(across(c("deploy_on_date", "deploy_off_date"), as.numeric))
  
  #Clean deployment dates
  ww <- ww %>%
    dplyr::mutate(
      deploy_on_date  = as.Date(deploy_on_date, origin = "1899-12-30"),
      deploy_off_date = as.Date(deploy_off_date, origin = "1899-12-30")
    )
  
  ww$deploy_off_date[is.na(ww$deploy_off_date)] <- default_end_date
  
  #Remove rows with identical on/off dates
  equal_dates <- ww %>%
    dplyr::filter(!is.na(deploy_on_date),
                  !is.na(deploy_off_date),
                  deploy_on_date == deploy_off_date)
  
  if (verbose && nrow(equal_dates) > 0) {
    message("Check the data: ",
            nrow(equal_dates),
            " record(s) where deploy_on_date equals deploy_off_date. Removing them.")
  }
  
  ww <- dplyr::anti_join(ww, equal_dates,
                         by = c("Nili_id", "deploy_on_date", "deploy_off_date"))
  
  #Flag deploy_off < deploy_on
  invalid_periods <- ww %>%
    dplyr::filter(!is.na(deploy_on_date),
                  !is.na(deploy_off_date),
                  deploy_off_date < deploy_on_date)
  
  if (verbose && nrow(invalid_periods) > 0) {
    message("Warning: Found ",
            nrow(invalid_periods),
            " deployment period(s) where deploy_off_date < deploy_on_date:")
    print(invalid_periods$Nili_id)
  }
  
  #Expand each deployment period to a daily sequence
  periods_to_keep <- ww %>%
    dplyr::select(trnsmt_id, deploy_on_date, deploy_off_date) %>%
    dplyr::mutate(
      deploy_on_date  = lubridate::ymd(deploy_on_date),
      deploy_off_date = lubridate::ymd(deploy_off_date),
      deploy_off_date = dplyr::if_else(is.na(deploy_off_date),
                                       default_end_date,
                                       deploy_off_date)
    ) %>%
    dplyr::filter(!is.na(deploy_on_date),
                  deploy_off_date >= deploy_on_date) %>%
    dplyr::group_by(trnsmt_id) %>%
    dplyr::mutate(date = purrr::map2(
      deploy_on_date, deploy_off_date,
      ~ seq(.x, .y, by = "1 day"))
    ) %>%
    tidyr::unnest(date) %>%
    dplyr::ungroup() %>%
    dplyr::select(trnsmt_id, date) %>%
    dplyr::distinct() %>%
    dplyr::mutate(keep = TRUE)
  
  #Join back to Movebank data
  valid_periods_to_keep <- movebank_dataset %>%
    mutate(date = lubridate::date(timestamp)) %>%
    dplyr::left_join(periods_to_keep, by = c("tag_local_identifier" = "trnsmt_id", "date")) %>%
    mutate(keep = case_when(!(tag_local_identifier %in% periods_to_keep$trnsmt_id) ~ T,
                            .default = keep)) %>%
    filter(keep == TRUE)
  
  return(valid_periods_to_keep)
}

clean_data <- function(dataset){
  dataset <- dataset %>%
    mutate(across(c("barometric_height", "external_temperature", "ground_speed", "heading", "gps_satellite_count", "height_above_msl"), as.numeric), dateOnly = lubridate::date(timestamp))
  cleaned <- vultureUtils::cleanData(dataset = dataset,
                                     precise = F,
                                     removeVars = F,
                                     longCol = "location_long",
                                     latCol = "location_lat",
                                     idCol = "tag_local_identifier",
                                     report = F)
  return(cleaned)
}

downsample_10min <- function(data){
  # Define function for downsampling 
  down <- function(df, mins){
    df <- dplyr::arrange(df, timestamp)
    howmuch <- paste(mins, "minute", sep = " ")
    rounded <- lubridate::round_date(df$timestamp, howmuch)
    keep <- !duplicated(rounded)
    return(df[keep,])
  }
  
  # Split by individual so we can downsample more efficiently
  lst <- group_split(data, tag_local_identifier)
  
  # Perform downsampling
  downsampled_10min <- purrr::list_rbind(map(lst, ~down(df = .x, mins = 10), .progress = T))
  
  return(downsampled_10min)
}