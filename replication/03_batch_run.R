# =============================================================================
# 03_batch_run.R — Phase 3: Batch Runs (Realistic Workload)
# HyADS Exposure Mapping of U.S. Power Plants (2005): Replication + Sensitivity Study
# =============================================================================
#
# Goal: simulate multiple sources with parallel processing.
#
# Uses: run_disperser_parallel()
#
# Deliverable: Runtime table by core count.
# =============================================================================

rm(list = ls())
library(disperseR)
source("helpers.R")

# Load configurations and re-initialize cache
dirs <- replication_init_dirs(require_existing = TRUE)

units_2005 <- readRDS("outputs/logs/units_2005.rds")

# -----------------------------------------------------------------------------
# 1. Select Multiple Units for Batch Processing
# -----------------------------------------------------------------------------
cat("=== Selecting Units for Batch Run ===\n")

# Select 5-10 units (adjust based on available compute time)
# For testing: use 5 units
# For full study: can expand to 10+

n_units <- 5
batch_units <- head(units_2005, n_units)

cat("Selected", nrow(batch_units), "units for batch processing:\n")
print(batch_units[, c("ID", "Latitude", "Longitude", "SOx", "Height")])

# Save batch units list
saveRDS(batch_units, "outputs/logs/batch_units.rds")

# -----------------------------------------------------------------------------
# 2. Build Input References (input.refs)
# -----------------------------------------------------------------------------
cat("\n=== Building Input References ===\n")

# Build input.refs using the actual disperseR API
# This will create the required columns for run_disperser_parallel()
input.refs <- define_inputs(
    units = batch_units,
    startday = "2005-01-01",
    endday = "2005-01-03", # First 3 days for a quick test
    start.hours = c(0, 6, 12, 18),
    duration = 240
)

cat("Simulation dates:", format(range(input.refs$start_day)), "\n")
cat("Total run days:", length(unique(input.refs$start_day)), "\n")

print(input.refs)
saveRDS(input.refs, "outputs/logs/input_refs.rds")

# -----------------------------------------------------------------------------
# 3. Load PBL Height Data (Required for run_disperser_parallel)
# -----------------------------------------------------------------------------
cat("\n=== Loading PBL Height Data ===\n")
pblheight <- tryCatch({
    get_data(data = "pblheight")
}, error = function(e) {
    cat("Warning: Could not load PBL:", e$message, "\n")
    NULL
})

# -----------------------------------------------------------------------------
# 4. Run with Single Core (Baseline Timing)
# -----------------------------------------------------------------------------
cat("\n=== Batch Run with mc.cores = 1 ===\n")
cat("NOTE: Requires meteorological data. Will fail if not available.\n\n")

timing_results <- data.frame(
    cores = integer(),
    units = integer(),
    days = integer(),
    runtime_sec = numeric(),
    status = character(),
    stringsAsFactors = FALSE
)

tryCatch(
    {
        start_time <- Sys.time()

        # Single-core run
        # result_1core <- run_disperser_parallel(
        #   input.refs = input.refs,
        #   pbl.height = pblheight,
        #   species = "so2",
        #   proc_dir = dirs$proc_dir,
        #   hysp_dir = dirs$hysp_dir,
        #   meteo_dir = dirs$meteo_dir,
        #   overwrite = TRUE,
        #   npart = 100,
        #   mc.cores = 1
        # )

        end_time <- Sys.time()
        runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

        cat("Single-core run skipped - uncomment when met data available\n")

        timing_results <- rbind(timing_results, data.frame(
            cores = 1,
            units = n_units,
            days = 3,
            runtime_sec = NA,
            status = "SKIPPED (no met data)"
        ))
    },
    error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        timing_results <- rbind(timing_results, data.frame(
            cores = 1,
            units = n_units,
            days = 3,
            runtime_sec = NA,
            status = paste("FAILED:", e$message)
        ))
    }
)

# -----------------------------------------------------------------------------
# 5. Run with Multiple Cores (Performance Comparison)
# -----------------------------------------------------------------------------
cat("\n=== Batch Run with mc.cores = 2 ===\n")

tryCatch(
    {
        start_time <- Sys.time()

        # Multi-core run
        # result_2core <- run_disperser_parallel(
        #   input.refs = input.refs,
        #   pbl.height = pblheight,
        #   species = "so2",
        #   proc_dir = dirs$proc_dir,
        #   hysp_dir = dirs$hysp_dir,
        #   meteo_dir = dirs$meteo_dir,
        #   overwrite = TRUE,
        #   npart = 100,
        #   mc.cores = 2
        # )

        end_time <- Sys.time()
        runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

        cat("Multi-core run skipped - uncomment when met data available\n")

        timing_results <- rbind(timing_results, data.frame(
            cores = 2,
            units = n_units,
            days = 3,
            runtime_sec = NA,
            status = "SKIPPED (no met data)"
        ))
    },
    error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        timing_results <- rbind(timing_results, data.frame(
            cores = 2,
            units = n_units,
            days = 3,
            runtime_sec = NA,
            status = paste("FAILED:", e$message)
        ))
    }
)

# -----------------------------------------------------------------------------
# 5. Verify Output Files
# -----------------------------------------------------------------------------
cat("\n=== Verifying Output Files ===\n")

output_dir <- dirs$output_dir
if (dir.exists(output_dir)) {
    fst_files <- list.files(output_dir, pattern = "\\.fst$", full.names = TRUE)
    cat("Found", length(fst_files), ".fst output files\n")

    if (length(fst_files) > 0) {
        # Check file sizes (should be non-empty)
        file_info <- file.info(fst_files)
        file_info$filename <- basename(rownames(file_info))

        cat("\nOutput file summary:\n")
        print(file_info[, c("filename", "size")])

        # Flag any empty files
        empty_files <- file_info[file_info$size == 0, ]
        if (nrow(empty_files) > 0) {
            cat("\nWARNING: Found", nrow(empty_files), "empty files!\n")
            print(empty_files$filename)
        }

        # Read and inspect first file
        if (file_info$size[1] > 0) {
            library(fst)
            sample_output <- read.fst(fst_files[1])
            cat("\nSample output structure:\n")
            str(sample_output)
        }
    }
} else {
    cat("Output directory not found. Run dispersion model first.\n")
}

# -----------------------------------------------------------------------------
# 6. Save Runtime Results
# -----------------------------------------------------------------------------
cat("\n=== Runtime Comparison ===\n")
print(timing_results)

write.csv(timing_results, "outputs/tables/runtime_comparison.csv", row.names = FALSE)
cat("\nRuntime table saved to outputs/tables/runtime_comparison.csv\n")

# Calculate speedup if both runs completed
if (all(!is.na(timing_results$runtime_sec))) {
    speedup <- timing_results$runtime_sec[1] / timing_results$runtime_sec[2]
    cat("\nSpeedup with 2 cores:", round(speedup, 2), "x\n")
}

cat("\n=== Phase 3 Complete ===\n")
cat("Next: Run 04_linking.R to link dispersion outputs to geography\n")
