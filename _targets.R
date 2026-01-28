library(targets)
library(tarchetypes)
library(crew)

# Set target options:
tar_option_set(
  packages = c("readxl", "dplyr", "tidyr", "move2", "here", "sf", "purrr"),
  controller = crew_controller_local(workers = 2)
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
  tar_target(inpa_data_2021, mutate(readRDS(here("data/created/download_gps_data/inpa_data_2021_version2026-01-19.RDS")), source = "inpa")),
  tar_target(inpa_data_2022, mutate(readRDS(here("data/created/download_gps_data/inpa_data_2022_version2026-01-19.RDS")), source = "inpa")),
  tar_target(ornitela_data_2021, mutate(readRDS(here("data/created/download_gps_data/ornitela_data_2021_version2026-01-19.RDS")), source = "ornitela")),
  tar_target(ornitela_data_2022, mutate(readRDS(here("data/created/download_gps_data/ornitela_data_2022_version2026-01-19.RDS")), source = "ornitela")),
  
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
  
  tar_target(downsampled_2021, mutate(sf::st_transform(sf::st_as_sf(downsample_10min(cleaned_2021), coords = c("location_long", "location_lat"), crs = "WGS84", remove = F), 32636), timestamp_il = lubridate::with_tz(timestamp, tzone = "Israel"), date_il = lubridate::date(timestamp_il))),
  tar_target(downsampled_2022, mutate(sf::st_transform(sf::st_as_sf(downsample_10min(cleaned_2022), coords = c("location_long", "location_lat"), crs = "WGS84", remove = F), 32636), timestamp_il = lubridate::with_tz(timestamp, tzone = "Israel"), date_il = lubridate::date(timestamp_il))),
  tar_target(bbox_south_big, sf::st_transform(
    st_as_sfc(st_set_crs(st_bbox(c("xmin" = 34.205, "xmax" = 35.787,
                                   "ymin" = 29.478, "ymax" = 31.775)), 
                         "WGS84")), 32636)),
  tar_target(downsampled_masked_2021, st_crop(downsampled_2021, bbox_south_big)),
  tar_target(downsampled_masked_2022, st_crop(downsampled_2022, bbox_south_big)),
  tar_target(mm_sub_recent, readRDS("data/created/mm_sub_recent.RDS")),
  tar_target(cluster21dates, readRDS("data/created/cluster21dates.RDS")),
  tar_target(cluster22dates, readRDS("data/created/cluster22dates.RDS")),
  tar_target(rp, sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")),
  tar_target(windows_21_3, split_overlapping(downsampled_masked_2021, days = 3)),
  tar_target(windows_22_3, split_overlapping(downsampled_masked_2022, days = 3)),
  tar_target(windows_21_5, split_overlapping(downsampled_masked_2021, days = 5)),
  tar_target(windows_22_5, split_overlapping(downsampled_masked_2022, days = 5)),
  tar_target(windows_21_10, split_overlapping(downsampled_masked_2021, days = 10)),
  tar_target(windows_22_10, split_overlapping(downsampled_masked_2022, days = 10)),
  
  tar_target(ct, 1),
  tar_target(dt, 1000),
  tar_target(stl, 4),
  tar_target(idc, "individual_local_identifier"),
  tar_target(return, "sri"),
  tar_target(timestampCol, "timestamp_il"),
  
  tar_target(sris_2021_3, purrr::map(windows_21_3, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))),
  tar_target(sris_2022_3, purrr::map(windows_22_3, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))),
  
  tar_target(sris_2021_5, purrr::map(windows_21_5, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))),
  tar_target(sris_2022_5, purrr::map(windows_22_5, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))),
  
  tar_target(sris_2021_10, purrr::map(windows_21_10, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))),
  tar_target(sris_2022_10, purrr::map(windows_22_10, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))),
  
  tar_target(sris_2021_df_3, make_sri_df(sris_2021_3)),
  tar_target(sris_2022_df_3, make_sri_df(sris_2022_3)),
  
  tar_target(sris_2021_df_5, make_sri_df(sris_2021_5)),
  tar_target(sris_2022_df_5, make_sri_df(sris_2022_5)),
  
  tar_target(sris_2021_df_10, make_sri_df(sris_2021_10)),
  tar_target(sris_2022_df_10, make_sri_df(sris_2022_10)),

  tar_target(gs_2021_3, map(sris_2021_3, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  tar_target(gs_2022_3, map(sris_2022_3, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  
  tar_target(gs_2021_5, map(sris_2021_5, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  tar_target(gs_2022_5, map(sris_2022_5, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  
  tar_target(gs_2021_10, map(sris_2021_10, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  tar_target(gs_2022_10, map(sris_2022_10, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  
  tar_target(layout_2021_3, get_layout(sris_2021_3)),
  tar_target(layout_2022_3, get_layout(sris_2022_3)),
  
  tar_target(layout_2021_5, get_layout(sris_2021_5)),
  tar_target(layout_2022_5, get_layout(sris_2022_5)),
  
  tar_target(layout_2021_10, get_layout(sris_2021_10)),
  tar_target(layout_2022_10, get_layout(sris_2022_10)),
  
  tar_target(plots_graphs_2021_3, map2(sris_2021_3, names(sris_2021_3), ~plot_network_fixed(.x, layout_2021_3, title = .y))),
  tar_target(plots_graphs_2022_3, map2(sris_2022_3, names(sris_2022_3), ~plot_network_fixed(.x, layout_2022_3, title = .y))),
  tar_target(plots_2021_3, map(plots_graphs_2021_3, "plot")),
  tar_target(graphs_2021_3, map(plots_graphs_2021_3, "graph")),
  tar_target(plots_2022_3, map(plots_graphs_2022_3, "plot")),
  tar_target(graphs_2022_3, map(plots_graphs_2022_3, "graph")),
  
  tar_target(plots_graphs_2021_5, map2(sris_2021_5, names(sris_2021_5), ~plot_network_fixed(.x, layout_2021_5, title = .y))),
  tar_target(plots_graphs_2022_5, map2(sris_2022_5, names(sris_2022_5), ~plot_network_fixed(.x, layout_2022_5, title = .y))),
  tar_target(plots_2021_5, map(plots_graphs_2021_5, "plot")),
  tar_target(graphs_2021_5, map(plots_graphs_2021_5, "graph")),
  tar_target(plots_2022_5, map(plots_graphs_2022_5, "plot")),
  tar_target(graphs_2022_5, map(plots_graphs_2022_5, "graph")),
  
  tar_target(plots_graphs_2021_10, map2(sris_2021_10, names(sris_2021_10), ~plot_network_fixed(.x, layout_2021_10, title = .y))),
  tar_target(plots_graphs_2022_10, map2(sris_2022_10, names(sris_2022_10), ~plot_network_fixed(.x, layout_2022_10, title = .y))),
  tar_target(plots_2021_10, map(plots_graphs_2021_10, "plot")),
  tar_target(graphs_2021_10, map(plots_graphs_2021_10, "graph")),
  tar_target(plots_2022_10, map(plots_graphs_2022_10, "plot")),
  tar_target(graphs_2022_10, map(plots_graphs_2022_10, "graph")),
  
  tar_target(metrics_2021_3, map(graphs_2021_3, network_metrics, weight = "sri")),
  tar_target(metrics_2022_3, map(graphs_2022_3, network_metrics, weight = "sri")),
  
  tar_target(metrics_2021_5, map(graphs_2021_5, network_metrics, weight = "sri")),
  tar_target(metrics_2022_5, map(graphs_2022_5, network_metrics, weight = "sri")),
  
  tar_target(metrics_2021_10, map(graphs_2021_10, network_metrics, weight = "sri")),
  tar_target(metrics_2022_10, map(graphs_2022_10, network_metrics, weight = "sri")),
  # XXX start here transferring over from explore_networks.R
)
