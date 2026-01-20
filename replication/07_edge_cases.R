# 07_edge_cases.R - Phase 7: Error Handling Tests
# Validates that disperseR functions fail gracefully with clear errors

rm(list = ls())
suppressPackageStartupMessages({
  library(disperseR)
  library(data.table)
})
source("helpers.R")

dirs <- replication_init_dirs(require_existing = FALSE)

# Results storage
edge_cases <- data.frame(
  test_id = character(), function_name = character(),
  test_description = character(), expected_behavior = character(),
  actual_result = character(), error_message = character(),
  pass_fail = character(), stringsAsFactors = FALSE
)

add_result <- function(df, id, func, desc, expected, result, error, pass) {
  rbind(df, data.frame(
    test_id = id, function_name = func, test_description = desc,
    expected_behavior = expected, actual_result = result,
    error_message = error, pass_fail = pass, stringsAsFactors = FALSE
  ))
}

message("Edge-case testing (8 tests)")

# --- Test 1: NULL input.refs ---
tryCatch({
  run_disperser_parallel(
    input.refs = NULL, pbl.height = NULL, species = "so2",
    proc_dir = tempdir(), hysp_dir = tempdir(), meteo_dir = tempdir(), mc.cores = 1
  )
  edge_cases <- add_result(edge_cases, "E1", "run_disperser_parallel",
    "NULL input.refs", "Should fail", "Did NOT fail", "None", "UNEXPECTED")
}, error = function(e) {
  msg <- conditionMessage(e)
  pass <- grepl("input.refs|NULL|empty", msg, ignore.case = TRUE)
  edge_cases <<- add_result(edge_cases, "E1", "run_disperser_parallel",
    "NULL input.refs", "Error about NULL/empty input",
    if (pass) "Clear error" else "Unclear error", msg,
    if (pass) "PASS" else "NEEDS IMPROVEMENT")
})

# --- Test 2: Missing columns ---
tryCatch({
  run_disperser_parallel(
    input.refs = data.table(ID = "test", Latitude = 40.0),
    pbl.height = NULL, species = "so2",
    proc_dir = tempdir(), hysp_dir = tempdir(), meteo_dir = tempdir(), mc.cores = 1
  )
  edge_cases <- add_result(edge_cases, "E2", "run_disperser_parallel",
    "Missing columns", "Should fail", "Did NOT fail", "None", "UNEXPECTED")
}, error = function(e) {
  msg <- conditionMessage(e)
  pass <- grepl("missing|column|required", msg, ignore.case = TRUE)
  edge_cases <<- add_result(edge_cases, "E2", "run_disperser_parallel",
    "Missing columns", "List missing columns",
    if (pass) "Clear error" else "Unclear error", msg,
    if (pass) "PASS" else "NEEDS IMPROVEMENT")
})

# --- Test 3: Invalid start_hour ---
tryCatch({
  run_disperser_parallel(
    input.refs = data.table(
      ID = "test", uID = "test_u", Latitude = 40.0, Longitude = -80.0,
      Height = 100, start_day = as.Date("2005-01-15"), start_hour = 25,
      duration_emiss_hours = 1, duration_run_hours = 24
    ),
    pbl.height = NULL, species = "so2",
    proc_dir = tempdir(), hysp_dir = tempdir(), meteo_dir = tempdir(), mc.cores = 1
  )
  edge_cases <- add_result(edge_cases, "E3", "run_disperser_parallel",
    "start_hour=25", "Should fail", "Did NOT fail", "None", "UNEXPECTED")
}, error = function(e) {
  msg <- conditionMessage(e)
  pass <- grepl("start_hour|0|23|between", msg, ignore.case = TRUE)
  edge_cases <<- add_result(edge_cases, "E3", "run_disperser_parallel",
    "start_hour=25", "Error about valid range",
    if (pass) "Clear error" else "Unclear error", msg,
    if (pass) "PASS" else "NEEDS IMPROVEMENT")
})

# --- Test 4: NULL crosswalk ---
tryCatch({
  link_all_units(
    units.run = data.table(ID = "test", uID = "test_u", Latitude = 40.0, Longitude = -80.0),
    link.to = "zips", year.mons = "200501", crosswalk. = NULL,
    pbl_trim = FALSE, hysp_dir = tempdir(), ziplink_dir = tempdir()
  )
  edge_cases <- add_result(edge_cases, "E4", "link_all_units",
    "NULL crosswalk", "Should fail", "Did NOT fail", "None", "UNEXPECTED")
}, error = function(e) {
  msg <- conditionMessage(e)
  pass <- grepl("crosswalk|must be provided", msg, ignore.case = TRUE)
  edge_cases <<- add_result(edge_cases, "E4", "link_all_units",
    "NULL crosswalk", "Error about missing crosswalk",
    if (pass) "Clear error" else "Unclear error", msg,
    if (pass) "PASS" else "NEEDS IMPROVEMENT")
})

# --- Test 5: Invalid link.to ---
tryCatch({
  link_all_units(
    units.run = data.table(ID = "test", uID = "test_u"),
    link.to = "invalid_type", year.mons = "200501"
  )
  edge_cases <- add_result(edge_cases, "E5", "link_all_units",
    "link.to='invalid'", "Should fail", "Did NOT fail", "None", "UNEXPECTED")
}, error = function(e) {
  msg <- conditionMessage(e)
  pass <- grepl("zips|counties|grids|link.to", msg, ignore.case = TRUE)
  edge_cases <<- add_result(edge_cases, "E5", "link_all_units",
    "link.to='invalid'", "List valid options",
    if (pass) "Clear error" else "Unclear error", msg,
    if (pass) "PASS" else "NEEDS IMPROVEMENT")
})

# --- Test 6: NULL units.mo ---
tryCatch({
  calculate_exposure(
    year.E = 2005, year.D = 2005, link.to = "zips",
    units.mo = NULL, rda_file = NULL, source.agg = "total", time.agg = "year"
  )
  edge_cases <- add_result(edge_cases, "E6", "calculate_exposure",
    "NULL units.mo", "Should fail or use cache",
    "Returned (may use cache)", "None", "CHECK MANUALLY")
}, error = function(e) {
  msg <- conditionMessage(e)
  pass <- grepl("units.mo|must be provided|rda_file", msg, ignore.case = TRUE)
  edge_cases <<- add_result(edge_cases, "E6", "calculate_exposure",
    "NULL units.mo", "Error or cache fallback",
    if (pass) "Clear error" else "Unclear error", msg,
    if (pass) "PASS" else "NEEDS IMPROVEMENT")
})

# --- Test 7: Non-existent path ---
tryCatch({
  get_met_reanalysis(files = "RP200501.gbl", path_met_files = "/nonexistent/path")
  edge_cases <- add_result(edge_cases, "E7", "get_met_reanalysis",
    "Invalid path", "Should fail", "Did NOT fail", "None", "UNEXPECTED")
}, error = function(e) {
  msg <- conditionMessage(e)
  pass <- grepl("path|directory|exist|create", msg, ignore.case = TRUE)
  edge_cases <<- add_result(edge_cases, "E7", "get_met_reanalysis",
    "Invalid path", "Error about path",
    if (pass) "Clear error" else "Unclear error", msg,
    if (pass) "PASS" else "NEEDS IMPROVEMENT")
})

# --- Test 8: Invalid date ---
tryCatch({
  define_inputs(
    units = data.table(ID = "test", Latitude = 40, Longitude = -80, Height = 100, year = 2005),
    startday = "invalid-date", endday = "2005-01-15", start.hours = c(0), duration = 24
  )
  edge_cases <- add_result(edge_cases, "E8", "define_inputs",
    "Invalid date", "Should fail", "Did NOT fail", "None", "UNEXPECTED")
}, error = function(e) {
  msg <- conditionMessage(e)
  pass <- grepl("date|format|unambiguous", msg, ignore.case = TRUE)
  edge_cases <<- add_result(edge_cases, "E8", "define_inputs",
    "Invalid date", "Error about date format",
    if (pass) "Clear error" else "Unclear error", msg,
    if (pass) "PASS" else "NEEDS IMPROVEMENT")
})

# --- Summary ---
message("\nResults:")
message("  PASS: ", sum(edge_cases$pass_fail == "PASS"))
message("  NEEDS IMPROVEMENT: ", sum(edge_cases$pass_fail == "NEEDS IMPROVEMENT"))
message("  CHECK MANUALLY: ", sum(edge_cases$pass_fail == "CHECK MANUALLY"))
message("  UNEXPECTED: ", sum(edge_cases$pass_fail == "UNEXPECTED"))

write.csv(edge_cases, "outputs/tables/edge_case_report.csv", row.names = FALSE)
message("\nReport: outputs/tables/edge_case_report.csv")
message("Phase 7 complete.")
