# Tests for calculate_exposure()
# Focus on parameter validation and logic, not full workflow

test_that("calculate_exposure validates source.agg", {
  skip_on_cran()
  
  expect_error(
    disperseR::calculate_exposure(
      year.E = 2005,
      year.D = 2005,
      source.agg = "invalid"
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
      time.agg = "invalid"
    ),
    "time.agg not recognized"
  )
})

test_that("calculate_exposure accepts valid source.agg values", {
  skip_on_cran()
  
  # These should not error on the source.agg validation
  # (they may error later due to missing data, but that's expected)
  for (agg in c("total", "facility", "unit")) {
    result <- tryCatch(
      disperseR::calculate_exposure(
        year.E = 2005,
        year.D = 2005,
        source.agg = agg,
        time.agg = "year",
        rda_file = "nonexistent"
      ),
      error = function(e) e
    )
    # Should NOT contain "source.agg not recognized"
    if (inherits(result, "error")) {
      expect_false(grepl("source.agg not recognized", result$message))
    }
  }
})

test_that("calculate_exposure accepts valid time.agg values", {
  skip_on_cran()
  
  for (agg in c("year", "month")) {
    result <- tryCatch(
      disperseR::calculate_exposure(
        year.E = 2005,
        year.D = 2005,
        source.agg = "total",
        time.agg = agg,
        rda_file = "nonexistent"
      ),
      error = function(e) e
    )
    # Should NOT contain "time.agg not recognized"
    if (inherits(result, "error")) {
      expect_false(grepl("time.agg not recognized", result$message))
    }
  }
})
