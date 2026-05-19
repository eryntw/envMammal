library(targets)
library(tarchetypes)
library(crew)

tar_option_set(
  packages = yaml::read_yaml("settings/packages.yaml")$packages, 
  controller = crew_controller_local(workers = 20)
)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

pilot_subset <- tar_read(pilot_subset, store = tars$database$store)

tar_plan(
  
  ## API ------
  api = Sys.getenv("IUCN_REDLIST_KEY"),
  
  ## Get IUCN data for pilot_subset -------
  targets::tar_target(name = iucn_data,
                      command = map_iucn_data(pilot_subset, api = iucnredlist::init_api(api))
  ),
  
  ## Extract threats ------
  targets::tar_target(name = iucn_threat,
                      command = iucn_data$iucn_data %>% 
                        dplyr::select(scientific_name, common, code, starts_with("threat_")) %>% 
                        dplyr::distinct()
  ),
  
  ## Score threats ------
  scored_threat = score_threat(iucn_threat, score_system = "ward_modified"),
  
  
  # Summarise threats ------
  threatsum = summarise_iucn_threat(scored_threat),
  
  # Score trend and status ------
  targets::tar_target(name = scored_trendstatus,
                      command = iucn_data$iucn_data %>% 
                        dplyr::select(scientific_name, common, code, poptrend_description) %>% 
                        dplyr::distinct() %>% 
                        score_trend_status(trend_col = "poptrend_description",
                                           status_col = "code")
  ),
  
  # Join tables -----
  targets::tar_target(name = threats,
                      command = scored_trendstatus %>%
                        dplyr::left_join(threatsum$species_summary, 
                                         by=c("scientific_name","common")) %>% 
                        dplyr::mutate(dplyr::across(where(is.numeric),~ round(.x, 2)))
  )
)


# tar_load everything ----------
stores <- purrr::map(tars_local, "store")
purrr::iwalk(stores, function(store_path, store_name) {
  
  message("Loading store: ", store_name)
  
  objs <- targets::tar_objects(store = store_path)
  
  targets::tar_load(
    names = tidyselect::all_of(objs),
    store = store_path,
    envir = .GlobalEnv
  )
  
})

########## ---------- ##########

if(FALSE) {
  
  # individual tar_make-------
  
  script <- "setup"
  
  tar_visnetwork(script = tars_local[[script]]$script
                 , store = tars_local[[script]]$store
                 , label = "time"
                 , physics = TRUE
                 #, name = matches("Melithreptus_gularis")
  )
  
  tar_make(script = tars_local[[script]]$script
           , store = tars_local[[script]]$store
  )
  
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars_local[[script]]$store) |>
    dplyr::inner_join(tibble::tibble(name = tar_errored(store = tars[[script]]$store)))
  
  tar_meta(fields = any_of("warnings"), complete_only = TRUE, store = tars_local[[script]]$store)
  
  # Invalidate (i.e. to rerun) some targets
  if(FALSE) {
    
    # purrr::map(tars_local
    #            , \(x) tar_invalidate(matches("prep_|predict_|thresh_"), store = x$store)
    #            )
    
  }
  
}

if(FALSE) {
  
  # current run errors ------
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars[[script]]$store) |>
    dplyr::inner_join(tibble::tibble(name = tar_errored(store = tars[[script]]$store)))
  
}


if(FALSE) {
  
  # check crew logs ------
  log_directory <- fs::path(tars[[script]]$store, "log")
  
  logs <- fs::dir_ls(log_directory
                     , recurse = TRUE
                     , regexp = "log$"
  ) |>
    tibble::enframe(name = NULL, value = "path") |>
    dplyr::mutate(run = basename(dirname(dirname(path)))
                  , controller = basename(dirname(path))
                  , run_date = lubridate::ymd_hm(run)
    ) |>
    dplyr::filter(run_date == max(run_date)) |>
    dplyr::mutate(log = purrr::map(path, \(x) readLines(x) |>
                                     paste0(collapse = "\n")
    )
    , has_error = purrr::map_lgl(log, \(x) grepl("error", tolower(x)))
    )
  
  error_lines <- logs |>
    dplyr::filter(has_error) |>
    dplyr::mutate(time = purrr::map(path, \(x) fs::file_info(x)$modification_time)) |>
    tidyr::unnest(cols = c(time)) |>
    dplyr::arrange(time)
  
  # View logs that had errors
  error_lines |>
    dplyr::pull(log) |>
    purrr::map(\(x) cat(x))
  
}

if(FALSE) {
  
  # prop done ------
  
  ## overall -------
  prop_done <- (tar_completed(store = tars$sdm$store) |>
                  length() +
                  (tar_skipped(store = tars$sdm$store) |>
                     length()
                  )) /
    (nrow(tar_manifest(script = tars$sdm$script)))
  
  ## predict -------
  (done <- tar_completed(names = matches("predict_fine"), store = tars$sdm$store) |>
     length() +
     (tar_skipped(names = matches("predict_fine"), store = tars$sdm$store) |>
        length()
     ))
  
  (todo <- nrow(tar_manifest(names = matches("predict_fine"), script = tars$sdm$script)))
  
  done / todo
  
}
