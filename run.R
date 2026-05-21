library(dplyr)
library(targets)

# packages --------
envFunc::check_packages(yaml::read_yaml("settings/packages.yaml") |>
                          unlist() |> unname() |> unique()
                        , update_env = TRUE)

# tars --------
## local ------
store_base <- envFunc::get_env_dir() |>
  fs::path_rel() |>
  fs::path(if(grepl("\\/prod\\/", here::here())) "prod" else "dev"
           , "out")

tars_local <- envTargets::make_tars(settings = envFunc::extract_scale("envMammal")
                                    , store_base = store_base
                                    , save_yaml = FALSE
                                    , list_names = "store")

## Bird
tars_bird <- envTargets::make_tars(
  # same structure as local path
  settings = envFunc::extract_scale(),
  project_base = fs::path("..", "envBird"),
  store_base = store_base,
  local = FALSE,
  list_names = "store"
)

tars <- c(tars_local, tars_bird)
envTargets::write_tars(tars)

# run everything ----------
# in _targets.yaml
purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_make(script = x, store = y)
)

# prune everything ----------
purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_prune(script = x, store = y)
)
