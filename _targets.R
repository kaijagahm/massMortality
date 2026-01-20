# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("readxl", "dplyr", "tidyr", "move2", "here", "sf", "purrr") # Packages that your targets need for their tasks
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(min_date, lubridate::ymd("2018-01-01")),
  tar_target(max_date, lubridate::ymd("2026-01-01")),
  tar_target(pw, "movebankCredentials/pw.Rda", format = "file"),
  tar_target(loginObject, get_loginObject(pw)),
  tar_target(ww_file, "data/raw/whoswho_vultures_20250422_new_kg.xlsx", format = "file"),
  tar_target(ww_file_new, "data/raw/whoswho_vultures_20251201_new.xlsx", format = "file"),
  tar_target(ww, get_ww(ww_file, date_cols = c("deploy_on_date", "deploy_off_date", "date_death"))),
  tar_target(deaths, get_deaths(ww, min_date, max_date)),
  tar_target(mm, readxl::read_excel("data/raw/M&m_new_2010_2024 (1).xlsx", sheet = 1)),
  tar_target(inpa_data_2021, readRDS(here("data/created/download_gps_data/inpa_data_2021_version2026-01-19.RDS"))),
  tar_target(inpa_data_2022, readRDS(here("data/created/download_gps_data/inpa_data_2022_version2026-01-19.RDS"))),
  tar_target(ornitela_data_2021, readRDS(here("data/created/download_gps_data/ornitela_data_2021_version2026-01-19.RDS"))),
  tar_target(ornitela_data_2022, readRDS(here("data/created/download_gps_data/ornitela_data_2022_version2026-01-19.RDS"))),
  
  tar_target(gps_2021_1, sf::st_as_sf(bind_rows(as.data.frame(ornitela_data_2021), as.data.frame(inpa_data_2021)), crs = "WGS84")),
  tar_target(gps_2022_1, sf::st_as_sf(bind_rows(as.data.frame(ornitela_data_2022), as.data.frame(inpa_data_2022)), crs = "WGS84")),
  
  tar_target(gps_2021, dplyr::bind_cols(gps_2021_1, setNames(as.data.frame(sf::st_coordinates(gps_2021_1)), c("location_long", "location_lat")))),
  tar_target(gps_2022, dplyr::bind_cols(gps_2022_1, setNames(as.data.frame(sf::st_coordinates(gps_2022_1)), c("location_long", "location_lat")))),

  tar_target(gps_2021_fixed, fix_names_ages(gps_2021, ww_file_new)),
  tar_target(gps_2022_fixed, fix_names_ages(gps_2022, ww_file_new)),

  # Data cleaning -----------------------------------------------------------
  tar_target(ww_new, readxl::read_excel(ww_file_new, sheet = "all gps tags")),
  ## Remove dates before/after the deploy period
  tar_target(removed_deploy_2021, process_deployments(ww_file_new,
                                                             gps_2021_fixed,
                                                             default_end_date = as.Date("2026-01-01"),
                                                             verbose = TRUE)),
  tar_target(removed_deploy_2022, process_deployments(ww_file_new,
                                                      gps_2022_fixed,
                                                      default_end_date = as.Date("2026-01-01"),
                                                      verbose = TRUE)),
  ## Remove hospital/invalid periods (# XXX COME BACK TO THIS)
  # tar_target(removed_periods, remove_periods(ww_file, removed_beforeafter_deploy)),
  ## Clean the data with the various steps in the vultureUtils::cleanData function
  tar_target(cleaned_2021, clean_data(removed_deploy_2021)),
  tar_target(cleaned_2022, clean_data(removed_deploy_2022)),
  tar_target(cleaned, st_as_sf(bind_rows(cleaned_2021, cleaned_2022), remove = F)),
  
  tar_target(downsampled, mutate(sf::st_transform(sf::st_as_sf(downsample_10min(cleaned), coords = c("location_long", "location_lat"), crs = "WGS84", remove = F), 32636), timestamp_il = lubridate::with_tz(timestamp, tzone = "Israel"), date_il = lubridate::date(timestamp_il)))
)
