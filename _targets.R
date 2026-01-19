# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("readxl", "dplyr", "tidyr", "move") # Packages that your targets need for their tasks
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
  tar_target(ww, get_ww(ww_file, date_cols = c("deploy_on_date", "deploy_off_date", "date_death"))),
  tar_target(deaths, get_deaths(ww, min_date, max_date)),
  tar_target(mm, readxl::read_excel("data/raw/M&m_new_2010_2024 (1).xlsx", sheet = 1))
)
