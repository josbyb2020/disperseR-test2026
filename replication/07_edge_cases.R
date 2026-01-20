# =============================================================================
# 07_edge_cases.R — Phase 7: Edge-Case Lab
# HyADS Exposure Mapping of U.S. Power Plants (2005): Replication + Sensitivity Study
# =============================================================================
#
# Goal: Test error handling and edge cases using the ACTUAL disperseR API.
#
# Tests:
#   1. run_disperser_parallel() with invalid input.refs
#   2. run_disperser_parallel() with missing required columns
#   3. link_all_units() with missing crosswalk
#   4. link_all_units() with invalid year.mons
#   5. calculate_exposure() with missing units.mo
#   6. get_met_reanalysis() with invalid path
#   7. define_inputs() with invalid date format
#   8. create_dirs() with restricted path
#
# Deliverable: Edge-Case Report with actual error messages.
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({
    library(disperseR)
    library(data.table)
})
source("helpers.R")

# Ensure output directory exists and initialize cache
dirs <- replication_init_dirs(require_existing = FALSE)

# Edge case results storage
edge_cases <- data.frame(
    test_id = character(),
    function_name = character(),
    test_description = character(),
    expected_behavior = character(),
    actual_result = character(),
    error_message = character(),
    pass_fail = character(),
    stringsAsFactors = FALSE
)

add_result <- function(df, test_id, func, desc, expected, result, error, pass) {
    rbind(df, data.frame(
        test_id = test_id,
        function_name = func,
        test_description = desc,
        expected_behavior = expected,
        actual_result = result,
        error_message = error,
        pass_fail = pass,
        stringsAsFactors = FALSE
    ))
}

cat("=== EDGE-CASE TESTING LAB ===\n")
cat("Testing error handling using the actual disperseR API\n")
cat("================================================================\n\n")

# -----------------------------------------------------------------------------
# Test 1: run_disperser_parallel() with NULL input.refs
# -----------------------------------------------------------------------------
cat("=== Test 1: NULL input.refs ===\n")

test_id <- "E1"
tryCatch({
    result <- run_disperser_parallel(
        input.refs = NULL,
        pbl.height = NULL,
        species = "so2",
        proc_dir = tempdir(),
        hysp_dir = tempdir(),
        meteo_dir = tempdir(),
        mc.cores = 1
    )
    
    edge_cases <- add_result(
        edge_cases, test_id, "run_disperser_parallel",
        "NULL input.refs",
        "Should fail with error about NULL or empty input",
        "Did NOT fail",
        "None",
        "UNEXPECTED"
    )
}, error = function(e) {
    cat("CAUGHT ERROR:", conditionMessage(e), "\n")
    msg <- conditionMessage(e)
    is_clear <- grepl("input.refs|NULL|empty|non-empty", msg, ignore.case = TRUE)
    
    edge_cases <<- add_result(
        edge_cases, test_id, "run_disperser_parallel",
        "NULL input.refs",
        "Should fail with error about NULL or empty input",
        if (is_clear) "Failed with clear error" else "Failed with unclear error",
        msg,
        if (is_clear) "PASS" else "NEEDS IMPROVEMENT"
    )
})

# -----------------------------------------------------------------------------
# Test 2: run_disperser_parallel() with missing required columns
# -----------------------------------------------------------------------------
cat("\n=== Test 2: Missing required columns ===\n")

test_id <- "E2"
tryCatch({
    # Create input.refs missing required columns
    bad_input <- data.table(
        ID = "test_unit",
        Latitude = 40.0
        # Missing: Longitude, Height, start_day, start_hour, duration_emiss_hours, duration_run_hours
    )
    
    result <- run_disperser_parallel(
        input.refs = bad_input,
        pbl.height = NULL,
        species = "so2",
        proc_dir = tempdir(),
        hysp_dir = tempdir(),
        meteo_dir = tempdir(),
        mc.cores = 1
    )
    
    edge_cases <- add_result(
        edge_cases, test_id, "run_disperser_parallel",
        "input.refs missing required columns",
        "Should fail listing missing columns",
        "Did NOT fail",
        "None",
        "UNEXPECTED"
    )
}, error = function(e) {
    cat("CAUGHT ERROR:", conditionMessage(e), "\n")
    msg <- conditionMessage(e)
    is_clear <- grepl("missing|column|required", msg, ignore.case = TRUE)
    
    edge_cases <<- add_result(
        edge_cases, test_id, "run_disperser_parallel",
        "input.refs missing required columns",
        "Should fail listing missing columns",
        if (is_clear) "Failed with clear error" else "Failed with unclear error",
        msg,
        if (is_clear) "PASS" else "NEEDS IMPROVEMENT"
    )
})

# -----------------------------------------------------------------------------
# Test 3: run_disperser_parallel() with invalid start_hour
# -----------------------------------------------------------------------------
cat("\n=== Test 3: Invalid start_hour ===\n")

test_id <- "E3"
tryCatch({
    bad_input <- data.table(
        ID = "test",
        uID = "test_u",
        Latitude = 40.0,
        Longitude = -80.0,
        Height = 100,
        start_day = as.Date("2005-01-15"),
        start_hour = 25,  # Invalid: should be 0-23
        duration_emiss_hours = 1,
        duration_run_hours = 24
    )
    
    result <- run_disperser_parallel(
        input.refs = bad_input,
        pbl.height = NULL,
        species = "so2",
        proc_dir = tempdir(),
        hysp_dir = tempdir(),
        meteo_dir = tempdir(),
        mc.cores = 1
    )
    
    edge_cases <- add_result(
        edge_cases, test_id, "run_disperser_parallel",
        "start_hour = 25 (out of range)",
        "Should fail with error about valid hour range",
        "Did NOT fail",
        "None",
        "UNEXPECTED"
    )
}, error = function(e) {
    cat("CAUGHT ERROR:", conditionMessage(e), "\n")
    msg <- conditionMessage(e)
    is_clear <- grepl("start_hour|0|23|between", msg, ignore.case = TRUE)
    
    edge_cases <<- add_result(
        edge_cases, test_id, "run_disperser_parallel",
        "start_hour = 25 (out of range)",
        "Should fail with error about valid hour range",
        if (is_clear) "Failed with clear error" else "Failed with unclear error",
        msg,
        if (is_clear) "PASS" else "NEEDS IMPROVEMENT"
    )
})

# -----------------------------------------------------------------------------
# Test 4: link_all_units() with NULL crosswalk
# -----------------------------------------------------------------------------
cat("\n=== Test 4: Missing crosswalk ===\n")

test_id <- "E4"
tryCatch({
    test_units <- data.table(
        ID = "test",
        uID = "test_u",
        Latitude = 40.0,
        Longitude = -80.0,
        year = 2005
    )
    
    result <- link_all_units(
        units.run = test_units,
        link.to = "zips",
        year.mons = "200501",
        crosswalk. = NULL,  # Missing required crosswalk
        pbl_trim = FALSE,
        hysp_dir = tempdir(),
        ziplink_dir = tempdir()
    )
    
    edge_cases <- add_result(
        edge_cases, test_id, "link_all_units",
        "NULL crosswalk for link.to='zips'",
        "Should fail with error about missing crosswalk",
        "Did NOT fail",
        "None",
        "UNEXPECTED"
    )
}, error = function(e) {
    cat("CAUGHT ERROR:", conditionMessage(e), "\n")
    msg <- conditionMessage(e)
    is_clear <- grepl("crosswalk|must be provided", msg, ignore.case = TRUE)
    
    edge_cases <<- add_result(
        edge_cases, test_id, "link_all_units",
        "NULL crosswalk for link.to='zips'",
        "Should fail with error about missing crosswalk",
        if (is_clear) "Failed with clear error" else "Failed with unclear error",
        msg,
        if (is_clear) "PASS" else "NEEDS IMPROVEMENT"
    )
})

# -----------------------------------------------------------------------------
# Test 5: link_all_units() with invalid link.to value
# -----------------------------------------------------------------------------
cat("\n=== Test 5: Invalid link.to ===\n")

test_id <- "E5"
tryCatch({
    test_units <- data.table(
        ID = "test",
        uID = "test_u"
    )
    
    result <- link_all_units(
        units.run = test_units,
        link.to = "invalid_type",  # Not 'zips', 'counties', or 'grids'
        year.mons = "200501"
    )
    
    edge_cases <- add_result(
        edge_cases, test_id, "link_all_units",
        "link.to = 'invalid_type'",
        "Should fail with error about valid options",
        "Did NOT fail",
        "None",
        "UNEXPECTED"
    )
}, error = function(e) {
    cat("CAUGHT ERROR:", conditionMessage(e), "\n")
    msg <- conditionMessage(e)
    is_clear <- grepl("zips|counties|grids|link.to", msg, ignore.case = TRUE)
    
    edge_cases <<- add_result(
        edge_cases, test_id, "link_all_units",
        "link.to = 'invalid_type'",
        "Should fail with error about valid options",
        if (is_clear) "Failed with clear error" else "Failed with unclear error",
        msg,
        if (is_clear) "PASS" else "NEEDS IMPROVEMENT"
    )
})

# -----------------------------------------------------------------------------
# Test 6: calculate_exposure() with NULL units.mo
# -----------------------------------------------------------------------------
cat("\n=== Test 6: NULL units.mo ===\n")

test_id <- "E6"
tryCatch({
    result <- calculate_exposure(
        year.E = 2005,
        year.D = 2005,
        link.to = "zips",
        units.mo = NULL,  # Will try to use cache, which may be empty
        rda_file = NULL,
        source.agg = "total",
        time.agg = "year"
    )
    
    # If it reaches here, check if it used cached data
    edge_cases <- add_result(
        edge_cases, test_id, "calculate_exposure",
        "NULL units.mo and no rda_file",
        "Should fail or use cached PP.units data",
        "Returned without error (may have used cache)",
        "None",
        "CHECK MANUALLY"
    )
}, error = function(e) {
    cat("CAUGHT ERROR:", conditionMessage(e), "\n")
    msg <- conditionMessage(e)
    is_clear <- grepl("units.mo|must be provided|rda_file", msg, ignore.case = TRUE)
    
    edge_cases <<- add_result(
        edge_cases, test_id, "calculate_exposure",
        "NULL units.mo and no rda_file",
        "Should fail or use cached PP.units data",
        if (is_clear) "Failed with clear error" else "Failed with unclear error",
        msg,
        if (is_clear) "PASS" else "NEEDS IMPROVEMENT"
    )
})

# -----------------------------------------------------------------------------
# Test 7: get_met_reanalysis() with non-existent path
# -----------------------------------------------------------------------------
cat("\n=== Test 7: Invalid met path ===\n")

test_id <- "E7"
tryCatch({
    result <- get_met_reanalysis(
        files = "RP200501.gbl",
        path_met_files = "/nonexistent/directory/path"
    )
    
    edge_cases <- add_result(
        edge_cases, test_id, "get_met_reanalysis",
        "Non-existent destination path",
        "Should fail with error about path",
        "Did NOT fail",
        "None",
        "UNEXPECTED"
    )
}, error = function(e) {
    cat("CAUGHT ERROR:", conditionMessage(e), "\n")
    msg <- conditionMessage(e)
    is_clear <- grepl("path|directory|exist|create", msg, ignore.case = TRUE)
    
    edge_cases <<- add_result(
        edge_cases, test_id, "get_met_reanalysis",
        "Non-existent destination path",
        "Should fail with error about path",
        if (is_clear) "Failed with clear error" else "Failed with unclear error",
        msg,
        if (is_clear) "PASS" else "NEEDS IMPROVEMENT"
    )
})

# -----------------------------------------------------------------------------
# Test 8: define_inputs() with invalid date format
# -----------------------------------------------------------------------------
cat("\n=== Test 8: Invalid date in define_inputs ===\n")

test_id <- "E8"
tryCatch({
    test_units <- data.table(
        ID = "test",
        Latitude = 40.0,
        Longitude = -80.0,
        Height = 100,
        year = 2005
    )
    
    result <- define_inputs(
        units = test_units,
        startday = "invalid-date",  # Not a valid date
        endday = "2005-01-15",
        start.hours = c(0),
        duration = 24
    )
    
    edge_cases <- add_result(
        edge_cases, test_id, "define_inputs",
        "Invalid startday format",
        "Should fail with error about date format",
        "Did NOT fail",
        "None",
        "UNEXPECTED"
    )
}, error = function(e) {
    cat("CAUGHT ERROR:", conditionMessage(e), "\n")
    msg <- conditionMessage(e)
    is_clear <- grepl("date|format|character string|unambiguous", msg, ignore.case = TRUE)
    
    edge_cases <<- add_result(
        edge_cases, test_id, "define_inputs",
        "Invalid startday format",
        "Should fail with error about date format",
        if (is_clear) "Failed with clear error" else "Failed with unclear error",
        msg,
        if (is_clear) "PASS" else "NEEDS IMPROVEMENT"
    )
})

# -----------------------------------------------------------------------------
# Summary and Report
# -----------------------------------------------------------------------------
cat("\n================================================================\n")
cat("=== EDGE-CASE TEST SUMMARY ===\n")
cat("================================================================\n\n")

print(edge_cases[, c("test_id", "function_name", "pass_fail")])

# Summary statistics
cat("\n--- Results ---\n")
cat("Total tests:", nrow(edge_cases), "\n")
cat("PASS:", sum(edge_cases$pass_fail == "PASS"), "\n")
cat("NEEDS IMPROVEMENT:", sum(edge_cases$pass_fail == "NEEDS IMPROVEMENT"), "\n")
cat("CHECK MANUALLY:", sum(edge_cases$pass_fail == "CHECK MANUALLY"), "\n")
cat("UNEXPECTED:", sum(edge_cases$pass_fail == "UNEXPECTED"), "\n")

# Save detailed report
write.csv(edge_cases, "outputs/tables/edge_case_report.csv", row.names = FALSE)
cat("\nFull report saved to outputs/tables/edge_case_report.csv\n")

# Generate markdown appendix
appendix <- sprintf("
# Edge-Case Report Appendix

Generated: %s

## Testing Summary

| Metric | Count |
|--------|-------|
| Total Tests | %d |
| PASS | %d |
| NEEDS IMPROVEMENT | %d |
| CHECK MANUALLY | %d |
| UNEXPECTED | %d |

## Detailed Results

", format(Sys.time(), "%Y-%m-%d %H:%M"),
    nrow(edge_cases),
    sum(edge_cases$pass_fail == "PASS"),
    sum(edge_cases$pass_fail == "NEEDS IMPROVEMENT"),
    sum(edge_cases$pass_fail == "CHECK MANUALLY"),
    sum(edge_cases$pass_fail == "UNEXPECTED")
)

for (i in seq_len(nrow(edge_cases))) {
    row <- edge_cases[i, ]
    appendix <- paste0(appendix, sprintf("
### Test %s: %s

**Description:** %s

**Expected:** %s

**Result:** %s

**Error Message:**
```
%s
```

**Status:** %s

---
", row$test_id, row$function_name, row$test_description,
        row$expected_behavior, row$actual_result,
        row$error_message, row$pass_fail))
}

writeLines(appendix, "outputs/logs/edge_case_appendix.md")
cat("Markdown appendix saved to outputs/logs/edge_case_appendix.md\n")

cat("\n=== Phase 7 Complete ===\n")
