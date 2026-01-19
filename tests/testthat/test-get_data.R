# Tests for get_data()
# These tests avoid network calls and focus on validation logic

test_that("get_data rejects unknown data types", {
  expect_error(
    disperseR::get_data(data = "invalid_data_type"),
    "Unknown data type"
  )
  
  expect_error(
    disperseR::get_data(data = "CROSSWALK"),  # case-sensitive

"Unknown data type"
  )
})

test_that("get_data returns crosswalk without network", {
  # crosswalk is bundled with package, no network needed
  result <- disperseR::get_data(data = "crosswalk")
  
  expect_s3_class(result, "data.table")
  expect_true("ZIP" %in% names(result) || "ZCTA" %in% names(result))
  expect_gt(nrow(result), 0)
})

test_that("get_data metfiles requires date parameters", {
  skip_on_cran()
  
  expect_error(
    disperseR::get_data(data = "metfiles"),
    "start.year|start.month|end.year|end.month"
  )
  
  expect_error(
    disperseR::get_data(
      data = "metfiles",
      start.year = "2005"
      # missing other params
    ),
    "start.year|start.month|end.year|end.month"
  )
})

test_that("get_data pblheight requires hpbl_dir", {
  skip_on_cran()
  
 # Should error if hpbl_dir not set and file doesn't exist
  # This tests the validation, not the download
  expect_error(
    disperseR::get_data(data = "pblheight"),
    regexp = NULL
  )
})
