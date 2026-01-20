# Tests for hysplit_dispersion()
# Integration tests are skipped on CRAN and when splitr is not available

splitr_available <- function() {
  requireNamespace("splitr", quietly = TRUE)
}

test_that("hysplit_dispersion requires splitr", {
  skip_on_cran()
  skip_if(splitr_available(),
          "splitr is installed, skipping missing-package test")
  
  met_dir <- file.path(tempdir(), paste0("disperseR_met_", Sys.getpid()))
  run_dir <- file.path(tempdir(), paste0("disperseR_run_", Sys.getpid()))
  dir.create(met_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(c(met_dir, run_dir), recursive = TRUE), add = TRUE)

  # Create minimal inputs to reach the splitr check without downloading met files
  model <- disperseR::create_disp_model()
  model <- disperseR::add_params(
    model,
    lat = 39.9,
    lon = -75.1,
    height = 50,
    start_day = "2005-06-15",
    start_hour = 0,
    duration = 1,
    met_type = "reanalysis"
  )
  model <- disperseR::add_emissions(
    model,
    rate = 1,
    duration = 1,
    start_day = "2005-06-15",
    start_hour = 0
  )
  model <- disperseR::add_species(model, name = "so2")
  model <- disperseR::add_grid(
    model,
    lat = 39.9,
    lon = -75.1,
    range = c(1, 1),
    division = c(1, 1)
  )

  met_files <- c("RP200505.gbl", "RP200506.gbl", "RP200507.gbl")
  file.create(file.path(met_dir, met_files))

  expect_error(
    disperseR::hysplit_dispersion(
      lat = model$lat,
      lon = model$lon,
      height = model$height,
      duration = model$duration,
      start_day = model$start_day,
      start_hour = model$start_hour,
      met_type = "reanalysis",
      met_dir = met_dir,
      emissions = model$emissions,
      species = model$species,
      grids = model$grids,
      return_disp_df = FALSE,
      write_disp_CSV = FALSE,
      run_dir = run_dir
    ),
    "splitr"
  )
})

test_that("hysplit_dispersion validates run_dir", {
  skip_on_cran()
  skip_if(!splitr_available(), "splitr not installed")
  
  expect_error(
    disperseR::hysplit_dispersion(run_dir = NULL),
    "run_dir must be specified"
  )
  
  expect_error(
    disperseR::hysplit_dispersion(run_dir = ""),
    "run_dir must be specified"
  )
})

test_that("hysplit_dispersion validates start_day format", {
  skip_on_cran()
  skip_if(!splitr_available(), "splitr not installed")
  
  temp_dir <- file.path(tempdir(), "hysplit_test")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Invalid date format
  expect_error(
    disperseR::hysplit_dispersion(
      start_day = "invalid",
      run_dir = temp_dir
    ),
    "YYYY-MM-DD"
  )
  
  # NA should error
  expect_error(
    disperseR::hysplit_dispersion(
      start_day = NA,
      run_dir = temp_dir
    ),
    "YYYY-MM-DD"
  )
})

test_that("hysplit_dispersion accepts Date objects", {
  skip_on_cran()
  skip_if(!splitr_available(), "splitr not installed")
  
  temp_dir <- file.path(tempdir(), "hysplit_test2")
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
 # Should not error on start_day validation when given Date
  # (will error later due to missing emissions/species/grids)
  result <- tryCatch(
    disperseR::hysplit_dispersion(
      start_day = as.Date("2005-06-15"),
      run_dir = temp_dir
    ),
    error = function(e) e
  )
  
  # Should NOT be a date format error
  if (inherits(result, "error")) {
    expect_false(grepl("YYYY-MM-DD", result$message))
  }
})
