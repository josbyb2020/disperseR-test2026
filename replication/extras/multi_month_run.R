# =============================================================================
# multi_month_run.R — Run HYSPLIT for Feb-Mar 2005
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({
    library(disperseR)
    library(data.table)
    library(ggplot2)
})
source("helpers.R")

cat("=== MULTI-MONTH HYSPLIT RUN ===\n\n")

# Setup
dirs <- replication_init_dirs(require_existing = TRUE)

# Check met files
met_files <- list.files(dirs$meteo_dir, pattern = "\\.gbl$")
cat("Met files available:", paste(met_files, collapse = ", "), "\n")

# Load units
units_dt <- data.table(disperseR::units)
unitsrun <- units_dt[year == 2005][order(-SOx)][1:2]
cat("Selected", nrow(unitsrun), "units\n\n")

# Define multi-month inputs (Jan-Mar 2005, one day per month for comparison)
cat("=== Running Jan vs Feb vs Mar comparison ===\n")

months <- c("01", "02", "03")
monthly_results <- list()

for (mo in months) {
    startday <- paste0("2005-", mo, "-15")
    cat("\nProcessing month:", mo, "- Date:", startday, "\n")

    input_refs <- tryCatch(
        {
            define_inputs(
                units = unitsrun[1], # Just one unit
                startday = startday,
                endday = startday, # Single day
                start.hours = c(0, 12), # 2 runs per day
                duration = 120
            )
        },
        error = function(e) {
            cat("  Error defining inputs:", e$message, "\n")
            NULL
        }
    )

    if (!is.null(input_refs) && nrow(input_refs) > 0) {
        cat("  Running", nrow(input_refs), "simulations...\n")

        start_time <- Sys.time()

        result <- tryCatch(
            {
                run_disperser_parallel(
                    input.refs = input_refs,
                    pbl.height = NULL,
                    species = "so2",
                    proc_dir = dirs$proc_dir,
                    hysp_dir = file.path(dirs$hysp_dir, paste0("month_", mo)),
                    meteo_dir = dirs$meteo_dir,
                    overwrite = TRUE,
                    npart = 100,
                    mc.cores = 1
                )
            },
            error = function(e) {
                cat("  HYSPLIT error:", e$message, "\n")
                NULL
            }
        )

        runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

        # Check outputs
        fst_files <- list.files(file.path(dirs$hysp_dir, paste0("month_", mo)),
            pattern = "\\.fst$", recursive = TRUE
        )

        monthly_results[[mo]] <- data.frame(
            month = mo,
            date = startday,
            n_runs = nrow(input_refs),
            n_outputs = length(fst_files),
            runtime_sec = round(runtime, 1),
            status = ifelse(length(fst_files) > 0, "SUCCESS", "FAILED")
        )

        cat("  Completed:", length(fst_files), "outputs in", round(runtime, 1), "sec\n")
    }
}

# Combine results
if (length(monthly_results) > 0) {
    monthly_df <- rbindlist(monthly_results)
    cat("\n=== Monthly Results Summary ===\n")
    print(monthly_df)

    write.csv(monthly_df, "outputs/tables/monthly_comparison.csv", row.names = FALSE)
    cat("\nSaved: monthly_comparison.csv\n")

    # Compare parcel statistics across months
    cat("\n=== Monthly Parcel Statistics ===\n")

    stats_list <- list()
    for (mo in months) {
        fst_dir <- file.path(dirs$hysp_dir, paste0("month_", mo))
        fst_files <- list.files(fst_dir, pattern = "\\.fst$", recursive = TRUE, full.names = TRUE)

        if (length(fst_files) > 0) {
            parcels <- rbindlist(lapply(fst_files, fst::read_fst))

            stats_list[[mo]] <- data.frame(
                month = mo,
                n_parcels = nrow(parcels),
                mean_height = round(mean(parcels$height, na.rm = TRUE), 1),
                max_height = round(max(parcels$height, na.rm = TRUE), 1),
                mean_lat = round(mean(parcels$lat, na.rm = TRUE), 2),
                lat_spread = round(diff(range(parcels$lat, na.rm = TRUE)), 2)
            )
        }
    }

    if (length(stats_list) > 0) {
        stats_df <- rbindlist(stats_list)
        print(stats_df)
        write.csv(stats_df, "outputs/tables/monthly_parcel_stats.csv", row.names = FALSE)

        # Create monthly comparison plot
        p_monthly <- ggplot(stats_df, aes(x = month, y = mean_height)) +
            geom_bar(stat = "identity", fill = "#3182bd", width = 0.6) +
            geom_text(aes(label = round(mean_height, 0)), vjust = -0.5) +
            labs(
                title = "Monthly Comparison: Mean Parcel Height",
                subtitle = "Jan vs Feb vs Mar 2005",
                x = "Month",
                y = "Mean Height (m AGL)"
            ) +
            theme_minimal(base_size = 12)

        ggsave("outputs/figures/monthly_comparison.png", p_monthly,
            width = 7, height = 5, dpi = 300
        )
        cat("Saved: monthly_comparison.png\n")
    }
}

cat("\n=== Multi-Month Run Complete ===\n")
