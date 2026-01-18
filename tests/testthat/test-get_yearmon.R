# Tests for get_yearmon()

test_that("get_yearmon returns correct months for single month", {
  result <- disperseR::get_yearmon(
    start.year = "2005",
    start.month = "01",
    end.year = "2005",
    end.month = "01"
  )
  
  expect_equal(result, "20051")
  expect_length(result, 1)
})

test_that("get_yearmon returns correct sequence for multiple months", {
  result <- disperseR::get_yearmon(
    start.year = "2005",
    start.month = "01",
    end.year = "2005",
    end.month = "03"
  )
  
  expect_equal(result, c("20051", "20052", "20053"))
  expect_length(result, 3)
})

test_that("get_yearmon handles year boundaries", {
  result <- disperseR::get_yearmon(
    start.year = "2004",
    start.month = "11",
    end.year = "2005",
    end.month = "02"
  )
  
  expect_equal(result, c("200411", "200412", "20051", "20052"))
  expect_length(result, 4)
})

test_that("get_yearmon requires character inputs", {
  expect_error(
    disperseR::get_yearmon(
      start.year = 2005,
      start.month = "01",
      end.year = "2005",
      end.month = "03"
    ),
    "should all be provided as characters"
  )
})

test_that("get_yearmon handles full year", {
  result <- disperseR::get_yearmon(
    start.year = "2005",
    start.month = "01",
    end.year = "2005",
    end.month = "12"
  )
  
  expect_length(result, 12)
  expect_equal(result[1], "20051")
  expect_equal(result[12], "200512")
})
