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

# Split gps data into overlapping increments
split_overlapping <- function(gps, days){
  start_date <- as.Date(min(gps$timestamp))
  end_date   <- as.Date(max(gps$timestamp))
  
  window_starts <- seq(from = start_date, to   = end_date - days(days-1),  # last full window
                       by = "1 day")
  
  # make list of 5-day windows
  gps_windows <- purrr::map(
    window_starts, function(win_start){
      win_end <- win_start + days(days-1)
      gps %>% filter(timestamp >= as.POSIXct(win_start) &
                       timestamp <  as.POSIXct(win_end + days(1)))}
  )
  
  # name list elements with their cutoff dates
  names(gps_windows) <- map_chr(
    window_starts,
    function(win_start) {
      win_end <- win_start + days(days-1)
      paste0(
        format(win_start, "%Y.%m.%d"),
        "_",
        format(win_end, "%Y.%m.%d")
      )
    }
  )
  return(gps_windows)
}

make_sri_df <- function(mylist){
  out <- purrr::list_rbind(mylist, names_to = "period") %>% mutate(year = 2021) %>% separate_wider_delim(cols = "period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd))
  return(out)
}

get_layout <- function(dfs){
  all_nodes <- purrr::list_rbind(dfs) %>%
    select(ID1, ID2) %>%
    pivot_longer(everything()) %>%
    distinct(value) %>%
    rename(name = value)
  
  edges_union <- purrr::list_rbind(dfs) %>%
    filter(sri > 0 & !is.na(sri)) %>%
    group_by(ID1, ID2) %>%
    summarise(sri = mean(sri), .groups = "drop")
  
  g_union <- tbl_graph(nodes = all_nodes, edges = edges_union, directed = FALSE)
  
  g_layout <- g_union %>% activate(edges) %>%
    mutate(layout_weight = sri)
  
  layout_tbl <- create_layout(g_layout, layout  = "fr",
                              weights = layout_weight)
  return(layout_tbl)
}

plot_network_fixed <- function(edges, layout_tbl, title = NULL) {
  
  # which nodes are present in THIS network?
  present_nodes <- unique(c(edges$ID1, edges$ID2))
  
  g <- tbl_graph(
    nodes = layout_tbl %>% select(name),
    edges = edges %>% filter(sri > 0 & !is.na(sri)),
    directed = FALSE
  ) %>%
    activate(nodes) %>%
    left_join(layout_tbl, by = "name") %>%
    mutate(present = name %in% present_nodes)
  
  plot <- ggraph(g, layout = "manual", x = x, y = y) +
    geom_edge_link(aes(alpha = sri)) +
    
    ## hide absent nodes
    geom_node_point(aes(alpha = present), size = 2, show.legend = F) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
    
    ## hide labels for absent nodes
    geom_node_text(
      family = "Arial",
      aes(label = if_else(present, name, "")),
      repel = TRUE,
      size = 3,
      color = "blue"
    ) +
    
    ggtitle(title)
  out <- list("graph" = g, "plot" = plot)
  return(out)
}

network_metrics <- function(g, weight = "sri", loops = FALSE) {
  
  nodes <- c((g %>% activate(edges) %>% filter(sri > 0) %>% pull(from)),
             (g %>% activate(edges) %>% filter(sri > 0) %>% pull(to))) %>% unique()
  n <- length(nodes)
  
  ## ---- Global metrics ----
  density <- g %>%
    activate(nodes) %>%
    igraph::edge_density(loops = loops)
  
  assort_degree <- igraph::assortativity(g, values = igraph::degree(g))
  avg_path_length <- igraph::mean_distance(g)
  
  ## ---- Node-level metrics ----
  node_tbl <- g %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree(),
           degree_rel = centrality_degree()/n,
           strength = centrality_degree(weights = sri),
           strength_rel = centrality_degree(weights = sri)/n,
           n = n) %>%
    as_tibble()
  
  ## ---- Network-level metrics ----
  network_tbl <- tibble(density = density, 
                        assort_degree = assort_degree,
                        avg_path_length = avg_path_length) 
  
  ## ---- Output ----
  list(
    network_metrics = network_tbl,
    node_metrics  = node_tbl
  )
}

get_network_metrics_df <- function(metrics, yr, dys){
  out <- purrr::list_rbind(map(metrics, "network_metrics"), names_to = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), year = yr, days = dys)
  return(out)
}

get_node_metrics_df <- function(metrics, yr, dys){
  out <- map_dfr(metrics, "node_metrics", .id = "time_period") %>% separate_wider_delim(cols = "time_period", delim = "_", names = c("period_start", "period_end")) %>% mutate(across(starts_with("period_"), lubridate::ymd), year = yr, days = dys)
  return(out)
}