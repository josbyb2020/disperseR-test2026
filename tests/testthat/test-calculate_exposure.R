# Tests for calculate_exposure()
# Focus on parameter validation and logic, not full workflow

test_that("calculate_exposure validates source.agg", {
  skip_on_cran()
  
  # Create a minimal mock to avoid file loading issues
  # The function should validate source.agg before trying to load files
  expect_error(
    disperseR::calculate_exposure(
      year.E = 2005,
      year.D = 2005,
      source.agg = "invalid",
      time.agg = "year",
      rda_file = "loaded"  # avoid file loading
    ),
    "source.agg not recognized"
  )
})

test_that("calculate_exposure validates time.agg", {
  skip_on_cran()
  
  expect_error(
    disperseR::calculate_exposure(
      year.E = 2005,
      year.D = 2005,
      source.agg = "total",
      time.agg = "invalid",
      rda_file = "loaded"
    ),
    "time.agg not recognized"
  )
})

test_that("calculate_exposure accepts valid source.agg values without validation error", {
  skip_on_cran()
  
  # These should pass validation (may error later due to missing data)
  for (agg in c("total", "facility", "unit")) {
    result <- tryCatch(
      disperseR::calculate_exposure(
        year.E = 2005,
        year.D = 2005,
        source.agg = agg,
        time.agg = "year",
        rda_file = "loaded"  # avoid file loading
      ),
      error = function(e) e
    )
    # Should NOT be a source.agg validation error
    if (inherits(result, "error")) {
      expect_false(
        grepl("source.agg not recognized", result$message),
        info = paste("source.agg =", agg, "should be valid")
      )
    }
  }
})

test_that("calculate_exposure accepts valid time.agg values without validation error", {
  skip_on_cran()
  
  for (agg in c("year", "month")) {
    result <- tryCatch(
      disperseR::calculate_exposure(
        year.E = 2005,
        year.D = 2005,
        source.agg = "total",
        time.agg = agg,
        rda_file = "loaded"
      ),
      error = function(e) e
    )
    # Should NOT be a time.agg validation error
    if (inherits(result, "error")) {
      expect_false(
        grepl("time.agg not recognized", result$message),
        info = paste("time.agg =", agg, "should be valid")
      )
    }
  }
})
