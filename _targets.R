library(targets)
library(tarchetypes)
library(crew)

# Set target options:
tar_option_set(
  packages = c("readxl", "dplyr", "tidyr", "move2", "here", "sf", "purrr", "lubridate", "vultureUtils", "tidygraph", "ggraph"),
  controller = crew_controller_local(workers = 10)
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
  tar_target(inpa_data, mutate(readRDS(here("data/created/download_gps_data/inpa_data_version2026-01-29.RDS")), source = "inpa")),
  tar_target(ornitela_data, mutate(readRDS(here("data/created/download_gps_data/ornitela_data_version2026-01-29.RDS")), source = "ornitela")),
  
  tar_target(gps_1, sf::st_as_sf(bind_rows(as.data.frame(ornitela_data), as.data.frame(inpa_data)), crs = "WGS84")),
  
  tar_target(gps, dplyr::bind_cols(gps_1, setNames(as.data.frame(sf::st_coordinates(gps_1)), c("location_long", "location_lat")))),
  
  tar_target(gps_fixed, fix_names_ages(gps, ww_file_new)),

  # Data cleaning -----------------------------------------------------------
  tar_target(ww_new, readxl::read_excel(ww_file_new, sheet = "all gps tags")),
  ## Remove dates before/after the deploy period
  tar_target(removed_deploy, process_deployments(ww_file_new,
                                                      gps_fixed,
                                                      default_end_date = as.Date("2026-01-01"),
                                                      verbose = TRUE)),
  ## Remove hospital/invalid periods (# XXX COME BACK TO THIS)
  # tar_target(removed_periods, remove_periods(ww_file, removed_beforeafter_deploy)),
  ## Clean the data with the various steps in the vultureUtils::cleanData function
  tar_target(cleaned, clean_data(removed_deploy)),
  
  tar_target(downsampled, mutate(sf::st_transform(sf::st_as_sf(downsample_10min(cleaned), coords = c("location_long", "location_lat"), crs = "WGS84", remove = F), 32636), timestamp_il = lubridate::with_tz(timestamp, tzone = "Israel"), date_il = lubridate::date(timestamp_il))),
  tar_target(bbox_south_big, sf::st_transform(
    st_as_sfc(st_set_crs(st_bbox(c("xmin" = 34.205, "xmax" = 35.787,
                                   "ymin" = 29.478, "ymax" = 31.775)), 
                         "WGS84")), 32636)),
  tar_target(downsampled_masked, st_crop(downsampled, bbox_south_big)),
  tar_target(mm_sub_recent, readRDS("data/created/mm_sub_recent.RDS")),
  tar_target(cluster21dates, readRDS("data/created/cluster21dates.RDS")),
  tar_target(cluster22dates, readRDS("data/created/cluster22dates.RDS")),
  tar_target(rp, sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")),
  tar_target(windows_3, split_overlapping(downsampled_masked, days = 3)),
  tar_target(windows_5, split_overlapping(downsampled_masked, days = 5)),
  tar_target(windows_10, split_overlapping(downsampled_masked, days = 10)),
  
  ## Get roost locations for each vulture on each night.
  tar_target(roosts, vultureUtils::get_roosts_df(downsampled_masked, id = "individual_local_identifier", timestamp = "timestamp_il")),
  tar_target(roosts_windows_3, split_overlapping_roosts(roosts, days = 3)),
  tar_target(roosts_windows_5, split_overlapping_roosts(roosts, days = 5)),
  tar_target(roosts_windows_10, split_overlapping_roosts(roosts, days = 10)),
  
  tar_target(ct, 1),
  tar_target(dt, 1000),
  tar_target(stl, 4),
  tar_target(idc, "individual_local_identifier"),
  tar_target(r, "sri"),
  tar_target(tc, "timestamp_il"),
  
  tar_target(sris_3, purrr::map(windows_3, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))),
  
  tar_target(sris_5, purrr::map(windows_5, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))),
  
  tar_target(sris_10, purrr::map(windows_10, ~getEdges_new(dataset = .x, consecThreshold = ct, distThreshold = dt, speedThreshLower = stl, idCol = idc, return = r, timestampCol = tc, roostPolygons = rp))),
  
  tar_target(sris_df_3, make_sri_df(sris_3)),
  tar_target(sris_df_5, make_sri_df(sris_5)),
  tar_target(sris_df_10, make_sri_df(sris_10)),

  tar_target(gs_3, map(sris_3, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  tar_target(gs_5, map(sris_5, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  tar_target(gs_10, map(sris_10, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  
  tar_target(layout_3, get_layout(sris_3)),
  tar_target(layout_5, get_layout(sris_5)),
  tar_target(layout_10, get_layout(sris_10)),

  tar_target(plots_graphs_3, map2(sris_3, names(sris_3), ~plot_network_fixed(.x, layout_3, title = .y))),
  tar_target(plots_3, map(plots_graphs_3, "plot")),
  tar_target(graphs_3, map(plots_graphs_3, "graph")),
  
  tar_target(plots_graphs_5, map2(sris_5, names(sris_5), ~plot_network_fixed(.x, layout_5, title = .y))),
  tar_target(plots_5, map(plots_graphs_5, "plot")),
  tar_target(graphs_5, map(plots_graphs_5, "graph")),
  
  tar_target(plots_graphs_10, map2(sris_10, names(sris_10), ~plot_network_fixed(.x, layout_10, title = .y))),
  tar_target(plots_10, map(plots_graphs_10, "plot")),
  tar_target(graphs_10, map(plots_graphs_10, "graph")),
  
  tar_target(metrics_3, map(graphs_3, network_metrics, weight = "sri")),
  tar_target(metrics_5, map(graphs_5, network_metrics, weight = "sri")),
  tar_target(metrics_10, map(graphs_10, network_metrics, weight = "sri")),

  tar_target(network_metrics_5, get_network_metrics_df(metrics_5, dys = 5)),
  tar_target(network_metrics_3, get_network_metrics_df(metrics_3, dys = 3)),
  tar_target(network_metrics_10, get_network_metrics_df(metrics_10, dys = 10)),

  tar_target(node_metrics_5, get_node_metrics_df(metrics_5, dys = 5)),
  tar_target(node_metrics_3, get_node_metrics_df(metrics_3, dys = 3)),
  tar_target(node_metrics_10, get_node_metrics_df(metrics_10, dys = 10)),

  tar_target(death_date_2021_min, lubridate::ymd("2021-10-24")),
  tar_target(death_date_2021_max, lubridate::ymd("2021-10-24")), 
  tar_target(death_date_2022_min, lubridate::ymd("2022-10-12")),
  tar_target(death_date_2022_max, lubridate::ymd("2022-10-15")),
  tar_target(death_df, data.frame(death_min = c(death_date_2021_min, death_date_2022_min),
                         death_max = c(death_date_2021_max, death_date_2022_max),
                         year = c(2021, 2022))),
  
  tar_target(node_metrics_all, purrr::list_rbind(list(node_metrics_3, node_metrics_5, node_metrics_10))),
  tar_target(network_metrics_all, purrr::list_rbind(list(network_metrics_3, network_metrics_5, network_metrics_10))),
  
  # Roost networks
  tar_target(roosts_sris_3, purrr::map(roosts_windows_3, ~getRoostEdges_new(dataset = .x, mode = "distance", distThreshold = 500, idCol = idc, dateCol = "roost_date", roostCol = "roostID", crsToSet = "WGS84", crsToTransform = 32636, return = r, getLocs = F))),
  
  tar_target(roosts_sris_5, purrr::map(roosts_windows_5, ~getRoostEdges_new(dataset = .x, mode = "distance", distThreshold = 500, idCol = idc, dateCol = "roost_date", roostCol = "roostID", crsToSet = "WGS84", crsToTransform = 32636, return = r, getLocs = F))),
  
  tar_target(roosts_sris_10, purrr::map(roosts_windows_10, ~getRoostEdges_new(dataset = .x, mode = "distance", distThreshold = 500, idCol = idc, dateCol = "roost_date", roostCol = "roostID", crsToSet = "WGS84", crsToTransform = 32636, return = r, getLocs = F))),
  
  tar_target(gs_3_roosts, map(roosts_sris_3, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  tar_target(gs_5_roosts, map(roosts_sris_5, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  tar_target(gs_10_roosts, map(roosts_sris_10, ~as_tbl_graph(select(.x, ID1, ID2, sri), directed = F))),
  
  tar_target(layout_3_roosts, get_layout(roosts_sris_3)),
  tar_target(layout_5_roosts, get_layout(roosts_sris_5)),
  tar_target(layout_10_roosts, get_layout(roosts_sris_10)),
  
  tar_target(plots_graphs_3_roosts, map2(roosts_sris_3, names(roosts_sris_3), ~plot_network_fixed(.x, layout_3_roosts, title = .y))),
  tar_target(plots_3_roosts, map(plots_graphs_3_roosts, "plot")),
  tar_target(graphs_3_roosts, map(plots_graphs_3_roosts, "graph")),
  
  tar_target(plots_graphs_5_roosts, map2(roosts_sris_5, names(roosts_sris_5), ~plot_network_fixed(.x, layout_5_roosts, title = .y))),
  tar_target(plots_5_roosts, map(plots_graphs_5_roosts, "plot")),
  tar_target(graphs_5_roosts, map(plots_graphs_5_roosts, "graph")),
  
  tar_target(plots_graphs_10_roosts, map2(roosts_sris_10, names(roosts_sris_10), ~plot_network_fixed(.x, layout_10_roosts, title = .y))),
  tar_target(plots_10_roosts, map(plots_graphs_10_roosts, "plot")),
  tar_target(graphs_10_roosts, map(plots_graphs_10_roosts, "graph")),
  
  tar_target(metrics_3_roosts, map(graphs_3_roosts, network_metrics, weight = "sri")),
  tar_target(metrics_5_roosts, map(graphs_5_roosts, network_metrics, weight = "sri")),
  tar_target(metrics_10_roosts, map(graphs_10_roosts, network_metrics, weight = "sri")),
  
  tar_target(network_metrics_5_roosts, get_network_metrics_df(metrics_5_roosts, dys = 5)),
  tar_target(network_metrics_3_roosts, get_network_metrics_df(metrics_3_roosts, dys = 3)),
  tar_target(network_metrics_10_roosts, get_network_metrics_df(metrics_10_roosts, dys = 10)),
  
  tar_target(node_metrics_5_roosts, get_node_metrics_df(metrics_5_roosts, dys = 5)),
  tar_target(node_metrics_3_roosts, get_node_metrics_df(metrics_3_roosts, dys = 3)),
  tar_target(node_metrics_10_roosts, get_node_metrics_df(metrics_10_roosts, dys = 10)),
  
  tar_target(node_metrics_all_roosts, purrr::list_rbind(list(node_metrics_3_roosts, node_metrics_5_roosts, node_metrics_10_roosts))),
  tar_target(network_metrics_all_roosts, purrr::list_rbind(list(network_metrics_3_roosts, network_metrics_5_roosts, network_metrics_10_roosts)))
)
