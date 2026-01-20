# 03_batch_run.R - Phase 3: Batch Processing
# Multiple units with parallel timing comparison

rm(list = ls())
library(disperseR)
source("helpers.R")

dirs <- replication_init_dirs(require_existing = TRUE)

if (!file.exists("outputs/logs/units_2005.rds")) {
  stop("Run 01_data.R first to prepare unit data.", call. = FALSE)
}
units_2005 <- readRDS("outputs/logs/units_2005.rds")

# --- Select Units ---
n_units <- 5
batch_units <- head(units_2005, n_units)
message("Selected ", nrow(batch_units), " units for batch processing")
saveRDS(batch_units, "outputs/logs/batch_units.rds")

# --- Build Input References ---
input.refs <- define_inputs(
  units = batch_units,
  startday = "2005-01-01",
  endday = "2005-01-03",
  start.hours = c(0, 6, 12, 18),
  duration = 240
)
message("Input refs: ", nrow(input.refs), " events over ",
        length(unique(input.refs$start_day)), " days")
saveRDS(input.refs, "outputs/logs/input_refs.rds")

# --- Load PBL ---
pblheight <- tryCatch(get_data(data = "pblheight"), error = function(e) NULL)

# --- Timing Results ---
timing_results <- data.frame(
  cores = integer(), units = integer(), days = integer(),
  runtime_sec = numeric(), status = character(), stringsAsFactors = FALSE
)

# Note: Uncomment the run_disperser_parallel() calls when met data is available
# Single-core run (baseline)
timing_results <- rbind(timing_results, data.frame(
  cores = 1, units = n_units, days = 3,
  runtime_sec = NA, status = "SKIPPED (uncomment when met data available)"
))

# Multi-core run
timing_results <- rbind(timing_results, data.frame(
  cores = 2, units = n_units, days = 3,
  runtime_sec = NA, status = "SKIPPED (uncomment when met data available)"
))

# --- Verify Outputs ---
output_dir <- dirs$hysp_dir
if (dir.exists(output_dir)) {
  fst_files <- list.files(output_dir, pattern = "\\.fst$", full.names = TRUE)
  message("Found ", length(fst_files), " .fst output files")
  
  if (length(fst_files) > 0) {
    file_info <- file.info(fst_files)
    empty_count <- sum(file_info$size == 0)
    if (empty_count > 0) {
      message("Warning: ", empty_count, " empty files found")
    }
  }
}

# --- Save Results ---
write.csv(timing_results, "outputs/tables/runtime_comparison.csv", row.names = FALSE)
message("\nPhase 3 complete. Next: source('04_linking.R')")
