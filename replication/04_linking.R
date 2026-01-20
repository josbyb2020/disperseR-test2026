# =============================================================================
# 04_linking.R — Phase 4: Linking to Geography
# HyADS Exposure Mapping of U.S. Power Plants (2005): Replication + Sensitivity Study
# =============================================================================
#
# Goal: Link dispersion outputs to ZIPs using link_all_units().
#
# Uses: link_all_units(), combine_monthly_links(), get_yearmon()
#
# Deliverable: Linked parcel data aggregated to ZIP codes.
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({
    library(disperseR)
    library(data.table)
})
source("helpers.R")

# -----------------------------------------------------------------------------
# 1. Load Configuration and Re-initialize Cache
# -----------------------------------------------------------------------------
cat("=== Loading Configuration ===\n")

dirs <- replication_init_dirs(require_existing = TRUE)
project_path <- dirname(dirs$main_dir)
cat("Project directory:", project_path, "\n")

# -----------------------------------------------------------------------------
# 2. Load Required Data
# -----------------------------------------------------------------------------
cat("\n=== Loading Required Data ===\n")

# Load units data
if (!file.exists("outputs/logs/units_2005.rds")) {
    stop("Run 01_data.R first to prepare unit data.")
}
units_2005 <- data.table(readRDS("outputs/logs/units_2005.rds"))

# Select top 2 emitters (matching real_run.R)
units_run <- units_2005[order(-SOx)][1:2]
cat("Units to link:", nrow(units_run), "\n")
print(units_run[, .(ID, Latitude, Longitude, SOx)])

# Ensure uID column exists (required by link_all_units)
if (!"uID" %in% names(units_run)) {
    units_run[, uID := ID]
}

# Load PBL height data using get_data() (ensures proper caching and CRS)
cat("Loading PBL height data...\n")
pblheight <- tryCatch({
    get_data(data = "pblheight")
}, error = function(e) {
    cat("Warning: Could not load PBL:", e$message, "\n")
    # Fallback to direct file load with CRS correction
    pbl_file <- file.path(dirs$hpbl_dir, "hpbl.mon.mean.nc")
    if (!file.exists(pbl_file)) return(NULL)
    pbl <- terra::rast(pbl_file)
    terra::crs(pbl) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
    pbl
})
cat("PBL loaded:", !is.null(pblheight), "\n")

# Load crosswalk data (built-in dataset in disperseR)
cat("Loading crosswalk data...\n")
crosswalk <- tryCatch({
    get_data(data = "crosswalk")
}, error = function(e) {
    cat("Warning: Could not load crosswalk:", e$message, "\n")
    # Fallback to direct access
    disperseR::crosswalk
})
cat("Crosswalk loaded:", !is.null(crosswalk), "\n")

# -----------------------------------------------------------------------------
# 3. Check HYSPLIT Outputs
# -----------------------------------------------------------------------------
cat("\n=== Checking HYSPLIT Outputs ===\n")

fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE)
cat("Found", length(fst_files), ".fst files in hysp_dir\n")
skip_linking <- length(fst_files) == 0

if (skip_linking) {
    cat("\nNo dispersion outputs found.\n")
    cat("Run 02_single_run.R or 03_batch_run.R first.\n")
    cat("Skipping linking phase.\n")
    
    # Create empty results
    link_results <- data.frame(
        link_type = "zips",
        pbl_trim = c(FALSE, TRUE),
        n_records = NA_integer_,
        runtime_sec = NA_real_,
        status = "SKIPPED (no HYSPLIT outputs)"
    )
    write.csv(link_results, "outputs/tables/link_results.csv", row.names = FALSE)
    
    cat("\n=== Phase 4 Skipped ===\n")
    cat("Next: Run 02_single_run.R or 03_batch_run.R to generate outputs\n")
}

if (!skip_linking) {
    # Sample one output to verify structure
    sample_file <- file.path(dirs$hysp_dir, fst_files[1])
    sample_data <- fst::read_fst(sample_file)
    cat("Sample output columns:", paste(names(sample_data), collapse = ", "), "\n")

    # -----------------------------------------------------------------------------
    # 4. Prepare Year-Months for Linking
    # -----------------------------------------------------------------------------
    cat("\n=== Preparing Year-Months ===\n")

# Get year-months from the output files
yearmons <- get_yearmon(
    start.year = "2005",
    start.month = "01",
    end.year = "2005",
    end.month = "01"
)
cat("Year-months to link:", paste(yearmons, collapse = ", "), "\n")

    # -----------------------------------------------------------------------------
    # 5. Link to ZIP Codes
    # -----------------------------------------------------------------------------
    cat("\n=== Linking to ZIP Codes ===\n")

link_results <- data.frame(
    link_type = character(),
    pbl_trim = logical(),
    n_records = integer(),
    runtime_sec = numeric(),
    status = character(),
    stringsAsFactors = FALSE
)

# Check requirements
can_link <- !is.null(pblheight) && !is.null(crosswalk)

if (!can_link) {
    cat("Cannot link: missing PBL or crosswalk data.\n")
    link_results <- rbind(link_results, data.frame(
        link_type = "zips",
        pbl_trim = TRUE,
        n_records = NA,
        runtime_sec = NA,
        status = "SKIPPED (missing data)"
    ))
} else {
    cat("Running link_all_units(link.to = 'zips', pbl_trim = TRUE)...\n")
    
    start_time <- Sys.time()
    
    linked_zips <- tryCatch({
        link_all_units(
            units.run = units_run,
            link.to = "zips",
            mc.cores = 1,
            year.mons = yearmons,
            pbl_trim = TRUE,
            pbl.height = pblheight,
            crosswalk. = crosswalk,
            hysp_dir = dirs$hysp_dir,
            ziplink_dir = dirs$ziplink_dir,
            duration.run.hours = 120,
            res.link = 12000,
            overwrite = TRUE,
            return.linked.data = TRUE
        )
    }, error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        NULL
    })
    
    runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    if (!is.null(linked_zips) && nrow(linked_zips) > 0) {
        cat("Linked", nrow(linked_zips), "records in", round(runtime, 1), "seconds\n")
        
        link_results <- rbind(link_results, data.frame(
            link_type = "zips",
            pbl_trim = TRUE,
            n_records = nrow(linked_zips),
            runtime_sec = round(runtime, 2),
            status = "SUCCESS"
        ))
        
        # Save linked data
        fst::write_fst(linked_zips, "outputs/logs/linked_zips.fst")
        cat("Saved to outputs/logs/linked_zips.fst\n")
        
        # Sample output
        cat("\nSample linked data:\n")
        print(head(linked_zips))
        
        # Summary by ZIP
        zip_summary <- linked_zips[, .(
            total_N = sum(N, na.rm = TRUE),
            n_units = uniqueN(ID)
        ), by = .(ZIP)]
        
        write.csv(
            head(zip_summary[order(-total_N)], 50),
            "outputs/tables/top_50_zips_exposure.csv",
            row.names = FALSE
        )
        cat("\nTop 50 ZIPs by exposure saved.\n")
    } else {
        cat("Linking returned no records.\n")
        link_results <- rbind(link_results, data.frame(
            link_type = "zips",
            pbl_trim = TRUE,
            n_records = 0,
            runtime_sec = round(runtime, 2),
            status = "EMPTY"
        ))
    }
}

    # -----------------------------------------------------------------------------
    # 6. Combine Monthly Links
    # -----------------------------------------------------------------------------
    cat("\n=== Combining Monthly Links ===\n")

if (exists("linked_zips") && !is.null(linked_zips) && nrow(linked_zips) > 0) {
    combined <- tryCatch({
        combine_monthly_links(
            month_YYYYMMs = yearmons,
            link.to = "zips",
            filename = "hyads_linked_zips.RData"
        )
    }, error = function(e) {
        cat("combine_monthly_links error:", conditionMessage(e), "\n")
        NULL
    })
    
    if (!is.null(combined)) {
        cat("Combined links created.\n")
    }
} else {
    cat("No linked data to combine.\n")
}

    # -----------------------------------------------------------------------------
    # 7. Summary
    # -----------------------------------------------------------------------------
    cat("\n=== Link Results Summary ===\n")
    print(link_results)

    write.csv(link_results, "outputs/tables/link_results.csv", row.names = FALSE)

# PBL comparison notes
pbl_notes <- "
=== Planetary Boundary Layer (PBL) Trimming ===

When pbl_trim = TRUE, parcels above the monthly mean PBL height are excluded.
This is physically more realistic because:
- Surface exposure primarily comes from pollutants within the boundary layer
- Above-PBL parcels are less likely to affect ground-level concentrations
- Seasonal/geographic variations in PBL height affect exposure patterns

Typical effects:
- Reduces total exposure estimates by 10-30%
- Effect is stronger in winter (lower PBL) than summer
- Inland/nighttime parcels more affected than coastal/daytime
"
    writeLines(pbl_notes, "outputs/logs/pbl_notes.txt")

    cat("\n=== Phase 4 Complete ===\n")
    cat("Next: Run 05_exposure.R to calculate HyADS exposure\n")
}
