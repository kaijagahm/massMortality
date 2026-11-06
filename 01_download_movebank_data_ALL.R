# Script to download the GPS data with interactive auth
library(move2)
library(targets)
library(readr)
library(here)

deaths_clustered <- readRDS("data/created/mm_sub_recent.RDS")

date_early <- min(deaths_clustered$date)-lubridate::days(180)
date_late <- max(deaths_clustered$date)+lubridate::days(180)

# these are actually expressed in israel time, since the death dates in the record are given in israel time
date_early_il <- lubridate::force_tz(date_early, tzone = "Israel")
date_late_il <- lubridate::force_tz(date_late, tzone = "Israel")

to_movebank_string <- function(x) {
  format(with_tz(x, "UTC"), "%Y%m%d%H%M%S000", tz = "UTC")
}

# now let's get the utc equivalents and format for movebank
date_early_toutc <- map(list(date_early_il), ~to_movebank_string(with_tz(.x, tzone = "UTC")))
date_late_toutc <- map(list(date_late_il), ~to_movebank_string(with_tz(.x, tzone = "UTC")))

# don't forget to interactively authenticate!
# Note on timestamps: Movebank data is stored in UTC (or GPS time, which is just a few leapseconds off from UTC). So for our purposes, we are considering these data to be in UTC.

ornitela_data <- move2::movebank_download_study(1252551761,
                                                     sensor_type_id = "gps",
                                                     timestamp_start = date_early_toutc[[1]],
                                                     timestamp_end = date_late_toutc[[1]])


inpa_data <- move2::movebank_download_study(6071688,
                                                sensor_type_id = "gps",
                                                timestamp_start = date_early_toutc[[1]],
                                                timestamp_end = date_late_toutc[[1]])

od <- left_join(ornitela_data, mt_track_data(ornitela_data))
id <- left_join(inpa_data, mt_track_data(inpa_data))

toremove <- c("sensor_type_id", "acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z", 
              "gps_hdop", "import_marked_outlier", "light_level", "magnetic_field_raw_x", "magnetic_field_raw_y", "tag_voltage", "event_id", "visible", "deployment_comments", "deploy_off_person", "deploy_off_timestamp", "deploy_on_person", "deploy_on_timestamp", "capture_location", "deploy_on_location", "deploy_off_location", "individual_comments", "death_comments", "taxon_canonical_name", "acknowledgements", "citation", "grants_used", "has_quota", "i_am_owner", "is_test", "license_type", "name", "study_number_of_deployments", "number_of_individuals", "number_of_tags", "principal_investigator_name", "study_objective", "study_type", "suspend_license_terms", "i_can_see_data", "there_are_data_which_i_cannot_see", "i_have_download_access", "i_am_collaborator", "study_permission", "number_of_deployed_locations", "timestamp_first_deployed_location", "timestamp_last_deployed_location", "taxon_ids", "contact_person_name", "main_location", "data_decoding_software", "eobs_activity", "eobs_activity_samples", "eobs_battery_voltage", "eobs_fix_battery_voltage", "eobs_horizontal_accuracy_estimate", "eobs_key_bin_checksum", "eobs_speed_accuracy_estimate", "eobs_start_timestamp", "eobs_status", "eobs_temperature", "eobs_type_of_fix", "eobs_used_time_to_get_fixed", "gps_dop", "gps_vdop", "height_above_ellipsoid", "height_raw", "magnetic_field_raw_z", "manually_marked_outlier", "orientation_quaternion_raw_w", "orientation_quaternion_raw_x", "orientation_quaternion_raw_y", "orientation_quaternion_raw_z", "barometric_pressure", "eobs_used_time_to_get_fix", "siblings", "manufacturer_name", "model", "processing_type", "serial_no", "weight")

od <- od %>% dplyr::select(-any_of(toremove))
id <- id %>% dplyr::select(-any_of(toremove))

write_rds(od, file = here("data/created/download_gps_data/ornitela_data_version2026-11-06.RDS"))
write_rds(id, file = here("data/created/download_gps_data/inpa_data_version2026-11-06.RDS"))
