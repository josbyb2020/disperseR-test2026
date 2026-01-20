# 05_exposure.R - Phase 5: Exposure Calculation
# Compute HyADS exposure using calculate_exposure()

rm(list = ls())
suppressPackageStartupMessages({
  library(disperseR)
  library(data.table)
})
source("helpers.R")

dirs <- replication_init_dirs(require_existing = TRUE)

# --- Check for Linked Data ---
linked_file <- "outputs/logs/linked_zips.fst"
rdata_file <- file.path(dirs$rdata_dir, "hyads_linked_zips.RData")
has_linked_data <- file.exists(linked_file) || file.exists(rdata_file)

if (!has_linked_data) {
  message("No linked data found. Run 04_linking.R first.")
  exposure_results <- data.frame(
    time_agg = c("year", "month"), source_agg = c("total", "total"),
    allow_partial = c(FALSE, FALSE), n_records = NA_integer_,
    total_exposure = NA_real_, status = "SKIPPED (no linked data)"
  )
  write.csv(exposure_results, "outputs/tables/exposure_results.csv", row.names = FALSE)
  stop("Phase 5 requires linked data. See 04_linking.R.", call. = FALSE)
}

message("Linked data found.")
if (file.exists(linked_file)) {
  linked_zips <- fst::read_fst(linked_file)
  message("  Records: ", nrow(linked_zips))
}

# --- Emissions Data ---
units_monthly <- disperseR::PP.units.monthly1995_2017
message("PP.units.monthly1995_2017: ", nrow(units_monthly), " rows (",
        min(units_monthly$year), "-", max(units_monthly$year), ")")

# --- Calculate Exposure ---
exposure_results <- data.frame(
  time_agg = character(), source_agg = character(), allow_partial = logical(),
  n_records = integer(), total_exposure = numeric(), status = character(),
  stringsAsFactors = FALSE
)

if (!file.exists(rdata_file)) {
  message("No combined RData file. Run 04_linking.R with combine_monthly_links().")
  exposure_results <- data.frame(
    time_agg = c("year", "month"), source_agg = c("total", "total"),
    allow_partial = c(TRUE, TRUE), n_records = NA, total_exposure = NA,
    status = "SKIPPED (no RData file)"
  )
} else {
  message("\nUsing: ", basename(rdata_file))
  
  # Annual, total sources
  message("\nCalculating annual total exposure...")
  exp_annual_total <- tryCatch({
    calculate_exposure(
      year.E = 2005, year.D = 2005, link.to = "zips",
      pollutant = "SO2.tons", units.mo = disperseR::PP.units.monthly1995_2017,
      rda_file = rdata_file, exp_dir = dirs$exp_dir,
      source.agg = "total", time.agg = "year",
      return.monthly.data = FALSE, allow.partial = TRUE
    )
  }, error = function(e) { message("Error: ", e$message); NULL })
  
  if (!is.null(exp_annual_total) && nrow(exp_annual_total) > 0) {
    message("  Records: ", nrow(exp_annual_total))
    exposure_results <- rbind(exposure_results, data.frame(
      time_agg = "year", source_agg = "total", allow_partial = TRUE,
      n_records = nrow(exp_annual_total),
      total_exposure = sum(exp_annual_total$hyads, na.rm = TRUE), status = "SUCCESS"
    ))
    write.csv(exp_annual_total, "outputs/tables/exposure_annual_total.csv", row.names = FALSE)
  } else {
    exposure_results <- rbind(exposure_results, data.frame(
      time_agg = "year", source_agg = "total", allow_partial = TRUE,
      n_records = 0, total_exposure = 0, status = "EMPTY"
    ))
  }
  
  # Monthly exposure
  message("\nCalculating monthly exposure...")
  exp_monthly <- tryCatch({
    calculate_exposure(
      year.E = 2005, year.D = 2005, link.to = "zips",
      pollutant = "SO2.tons", units.mo = disperseR::PP.units.monthly1995_2017,
      rda_file = rdata_file, exp_dir = dirs$exp_dir,
      source.agg = "total", time.agg = "month",
      return.monthly.data = TRUE, allow.partial = TRUE
    )
  }, error = function(e) { message("Error: ", e$message); NULL })
  
  if (!is.null(exp_monthly) && nrow(exp_monthly) > 0) {
    message("  Records: ", nrow(exp_monthly))
    exposure_results <- rbind(exposure_results, data.frame(
      time_agg = "month", source_agg = "total", allow_partial = TRUE,
      n_records = nrow(exp_monthly),
      total_exposure = sum(exp_monthly$hyads, na.rm = TRUE), status = "SUCCESS"
    ))
    write.csv(exp_monthly, "outputs/tables/exposure_monthly.csv", row.names = FALSE)
  } else {
    exposure_results <- rbind(exposure_results, data.frame(
      time_agg = "month", source_agg = "total", allow_partial = TRUE,
      n_records = 0, total_exposure = 0, status = "EMPTY"
    ))
  }
  
  # By unit
  message("\nCalculating unit-level exposure...")
  exp_by_unit <- tryCatch({
    calculate_exposure(
      year.E = 2005, year.D = 2005, link.to = "zips",
      pollutant = "SO2.tons", units.mo = disperseR::PP.units.monthly1995_2017,
      rda_file = rdata_file, exp_dir = dirs$exp_dir,
      source.agg = "unit", time.agg = "year",
      return.monthly.data = FALSE, allow.partial = TRUE
    )
  }, error = function(e) { message("Error: ", e$message); NULL })
  
  if (!is.null(exp_by_unit) && nrow(exp_by_unit) > 0) {
    message("  Records: ", nrow(exp_by_unit))
    exposure_results <- rbind(exposure_results, data.frame(
      time_agg = "year", source_agg = "unit", allow_partial = TRUE,
      n_records = nrow(exp_by_unit), total_exposure = NA, status = "SUCCESS"
    ))
    write.csv(exp_by_unit, "outputs/tables/exposure_by_unit.csv", row.names = FALSE)
  } else {
    exposure_results <- rbind(exposure_results, data.frame(
      time_agg = "year", source_agg = "unit", allow_partial = TRUE,
      n_records = 0, total_exposure = 0, status = "EMPTY"
    ))
  }
}

write.csv(exposure_results, "outputs/tables/exposure_results.csv", row.names = FALSE)
message("\nPhase 5 complete. Next: source('06_plots.R')")
