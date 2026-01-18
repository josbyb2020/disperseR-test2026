# Tests for create_dirs()

test_that("create_dirs creates expected directory structure", {
  # Use temp directory to avoid side effects
  temp_loc <- file.path(tempdir(), paste0("disperseR_test_", Sys.getpid()))
  on.exit(unlink(temp_loc, recursive = TRUE), add = TRUE)
  
  # Should not error
  expect_no_error(disperseR::create_dirs(location = temp_loc))
  
  # Check main directories exist
  expect_true(dir.exists(file.path(temp_loc, "main")))
  expect_true(dir.exists(file.path(temp_loc, "main", "input")))
  expect_true(dir.exists(file.path(temp_loc, "main", "output")))
  expect_true(dir.exists(file.path(temp_loc, "main", "process")))
})

test_that("create_dirs creates input subdirectories", {
  temp_loc <- file.path(tempdir(), paste0("disperseR_test2_", Sys.getpid()))
  on.exit(unlink(temp_loc, recursive = TRUE), add = TRUE)
  
  disperseR::create_dirs(location = temp_loc)
  
  expect_true(dir.exists(file.path(temp_loc, "main", "input", "zcta_500k")))
  expect_true(dir.exists(file.path(temp_loc, "main", "input", "hpbl")))
  expect_true(dir.exists(file.path(temp_loc, "main", "input", "meteo")))
})

test_that("create_dirs creates output subdirectories", {
  temp_loc <- file.path(tempdir(), paste0("disperseR_test3_", Sys.getpid()))
  on.exit(unlink(temp_loc, recursive = TRUE), add = TRUE)
  
  disperseR::create_dirs(location = temp_loc)
  
  # Use actual directory names from create_dirs()
  expect_true(dir.exists(file.path(temp_loc, "main", "output", "hysplit")))
  expect_true(dir.exists(file.path(temp_loc, "main", "output", "ziplinks")))
  expect_true(dir.exists(file.path(temp_loc, "main", "output", "rdata")))
})

test_that("create_dirs is idempotent (doesn't error on existing dirs)", {
  temp_loc <- file.path(tempdir(), paste0("disperseR_test4_", Sys.getpid()))
  on.exit(unlink(temp_loc, recursive = TRUE), add = TRUE)
  
  # Create twice
  disperseR::create_dirs(location = temp_loc)
  expect_no_error(disperseR::create_dirs(location = temp_loc))
})
