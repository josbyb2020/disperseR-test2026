# =============================================================================
# docker_test.R - Cross-Platform disperseR Compatibility Test
# =============================================================================

cat("=== disperseR Linux Compatibility Test ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("OS:", Sys.info()["sysname"], Sys.info()["release"], "\n")
cat("R Version:", R.version.string, "\n\n")

# Test results tracking
results <- list()

# -----------------------------------------------------------------------------
# Test 1: Package Loading
# -----------------------------------------------------------------------------
cat("=== Test 1: Package Loading ===\n")

test_packages <- c("disperseR", "splitr", "data.table", "sf", "terra", "fst")
for (pkg in test_packages) {
    status <- tryCatch(
        {
            library(pkg, character.only = TRUE)
            "PASS"
        },
        error = function(e) {
            paste("FAIL:", e$message)
        }
    )
    cat(sprintf("  %-12s: %s\n", pkg, status))
    results[[paste0("load_", pkg)]] <- status
}

# -----------------------------------------------------------------------------
# Test 2: HYSPLIT Binary Check
# -----------------------------------------------------------------------------
cat("\n=== Test 2: HYSPLIT Binary ===\n")

hysplit_check <- tryCatch(
    {
        # splitr stores binaries in specific location
        bin_path <- system.file("bin", package = "splitr")
        files <- list.files(bin_path, recursive = TRUE)

        if (length(files) > 0) {
            cat("  Binary path:", bin_path, "\n")
            cat("  Files found:", length(files), "\n")

            # Check for hycs_std (main dispersion binary)
            hycs <- grep("hycs_std", files, value = TRUE)
            if (length(hycs) > 0) {
                cat("  hycs_std found:", hycs[1], "\n")
                "PASS"
            } else {
                "FAIL: hycs_std not found"
            }
        } else {
            "FAIL: No binaries in splitr"
        }
    },
    error = function(e) {
        paste("FAIL:", e$message)
    }
)
cat("  HYSPLIT binary:", hysplit_check, "\n")
results[["hysplit_binary"]] <- hysplit_check

# -----------------------------------------------------------------------------
# Test 3: Directory Creation
# -----------------------------------------------------------------------------
cat("\n=== Test 3: create_dirs() ===\n")

dirs_check <- tryCatch(
    {
        dirs <- disperseR::create_dirs("/tmp/disperser_test")

        if (is.list(dirs) && "main_dir" %in% names(dirs)) {
            cat("  Main dir:", dirs$main_dir, "\n")
            cat("  Keys:", length(names(dirs)), "\n")
            "PASS"
        } else {
            "FAIL: Invalid return structure"
        }
    },
    error = function(e) {
        paste("FAIL:", e$message)
    }
)
cat("  create_dirs:", dirs_check, "\n")
results[["create_dirs"]] <- dirs_check

# -----------------------------------------------------------------------------
# Test 4: Units Data
# -----------------------------------------------------------------------------
cat("\n=== Test 4: Units Dataset ===\n")

units_check <- tryCatch(
    {
        data("units", package = "disperseR")

        if (exists("units") && nrow(units) > 0) {
            cat("  Rows:", nrow(units), "\n")
            cat("  Years:", range(units$year), "\n")
            cat("  Columns:", paste(names(units)[1:5], collapse = ", "), "...\n")
            "PASS"
        } else {
            "FAIL: units dataset empty"
        }
    },
    error = function(e) {
        paste("FAIL:", e$message)
    }
)
cat("  units data:", units_check, "\n")
results[["units_data"]] <- units_check

# -----------------------------------------------------------------------------
# Test 5: Meteorology Download (small test file)
# -----------------------------------------------------------------------------
cat("\n=== Test 5: Met Data Download ===\n")

met_check <- tryCatch(
    {
        # Try downloading a small met file
        met_dir <- file.path(dirs$meteo_dir)

        # Use get_met_reanalysis for a single month
        result <- disperseR::get_met_reanalysis(
            files = "RP200501.gbl",
            path_met_files = met_dir
        )

        # Check if file exists
        if (file.exists(file.path(met_dir, "RP200501.gbl"))) {
            fsize <- file.size(file.path(met_dir, "RP200501.gbl"))
            cat("  Downloaded:", round(fsize / 1e6, 1), "MB\n")
            "PASS"
        } else {
            "PARTIAL: Download attempted but file not verified"
        }
    },
    error = function(e) {
        paste("FAIL:", e$message)
    }
)
cat("  met download:", met_check, "\n")
results[["met_download"]] <- met_check

# -----------------------------------------------------------------------------
# Test 6: HYSPLIT Execution
# -----------------------------------------------------------------------------
cat("\n=== Test 6: HYSPLIT Execution ===\n")

hysplit_run <- tryCatch(
    {
        # Define minimal inputs
        library(data.table)
        units_dt <- data.table(units)
        test_unit <- units_dt[year == 2005][order(-SOx)][1]

        input_refs <- disperseR::define_inputs(
            units = test_unit,
            startday = "2005-01-15",
            endday = "2005-01-15",
            start.hours = 12,
            duration = 24
        )

        if (nrow(input_refs) > 0) {
            cat("  Input refs created:", nrow(input_refs), "\n")

            # Try running HYSPLIT
            run_result <- disperseR::run_disperser_parallel(
                input.refs = input_refs,
                pbl.height = NULL,
                species = "so2",
                proc_dir = dirs$proc_dir,
                hysp_dir = dirs$hysp_dir,
                meteo_dir = dirs$meteo_dir,
                overwrite = TRUE,
                npart = 50,
                mc.cores = 1
            )

            # Check for output files
            fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE)
            if (length(fst_files) > 0) {
                cat("  Output files:", length(fst_files), "\n")
                "PASS"
            } else {
                "PARTIAL: Ran but no output files"
            }
        } else {
            "FAIL: Could not create input refs"
        }
    },
    error = function(e) {
        paste("FAIL:", e$message)
    }
)
cat("  HYSPLIT run:", hysplit_run, "\n")
results[["hysplit_run"]] <- hysplit_run

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("SUMMARY - disperseR Linux Compatibility\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

pass_count <- sum(grepl("^PASS", unlist(results)))
fail_count <- sum(grepl("^FAIL", unlist(results)))
partial_count <- sum(grepl("^PARTIAL", unlist(results)))

cat("Results:\n")
cat("  PASS:   ", pass_count, "\n")
cat("  PARTIAL:", partial_count, "\n")
cat("  FAIL:   ", fail_count, "\n")
cat("  Total:  ", length(results), "\n\n")

# Print all results
cat("Detailed Results:\n")
for (name in names(results)) {
    status <- if (grepl("^PASS", results[[name]])) "✓" else if (grepl("^PARTIAL", results[[name]])) "~" else "✗"
    cat(sprintf("  %s %-20s: %s\n", status, name, results[[name]]))
}

# Exit code based on failures
if (fail_count > 0) {
    cat("\n⚠ Some tests failed. See details above.\n")
    quit(status = 1)
} else {
    cat("\n✓ All critical tests passed on Linux.\n")
    quit(status = 0)
}
