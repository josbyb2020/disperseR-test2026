# Tests for get_os()

test_that("get_os returns valid platform identifier", {
  os <- disperseR::get_os()
  

  expect_true(os %in% c("win", "mac", "unix"))
  expect_type(os, "character")
  expect_length(os, 1)
})

test_that("get_os matches .Platform on Windows", {
  skip_if_not(.Platform$OS.type == "windows")
  expect_equal(disperseR::get_os(), "win")
})

test_that("get_os returns mac on macOS", {
 skip_if_not(Sys.info()["sysname"] == "Darwin")
  expect_equal(disperseR::get_os(), "mac")
})

test_that("get_os returns unix on Linux", {
  skip_if_not(.Platform$OS.type == "unix" && Sys.info()["sysname"] != "Darwin")
  expect_equal(disperseR::get_os(), "unix")
})
