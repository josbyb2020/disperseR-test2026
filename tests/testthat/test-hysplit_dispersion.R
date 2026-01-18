# Tests for hysplit_dispersion()
# Integration tests are skipped on CRAN and when SplitR is not available

test_that("hysplit_dispersion requires SplitR", {
  skip_on_cran()
  skip_if(requireNamespace("SplitR", quietly = TRUE),
          "SplitR is installed, skipping missing-package test")
  
  expect_error(
    disperseR::hysplit_dispersion(run_dir = tempdir()),
    "SplitR"
  )
})

test_that("hysplit_dispersion validates run_dir", {
  skip_on_cran()
  skip_if_not_installed("SplitR")
  
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
  skip_if_not_installed("SplitR")
  
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
  skip_if_not_installed("SplitR")
  
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
