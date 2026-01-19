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