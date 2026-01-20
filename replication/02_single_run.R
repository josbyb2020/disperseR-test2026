# =============================================================================
# 02_single_run.R — Phase 2: Single Dispersion Run (HYSPLIT)
# HyADS Exposure Mapping of U.S. Power Plants (2005): Replication + Sensitivity Study
# =============================================================================
#
# Goal: Demonstrate a minimal single-unit HYSPLIT run using the actual API.
#
# Uses: define_inputs(), run_disperser_parallel(), get_data(), create_dirs()
#
# Deliverable: A short note explaining what each input parameter does and
#              proof-of-concept HYSPLIT output from one unit/date.
# =============================================================================

rm(list = ls())
suppressPackageStartupMessages({
    library(disperseR)
    library(data.table)
})
source("helpers.R")

# -----------------------------------------------------------------------------
# 1. Load Configuration from Previous Setup
# -----------------------------------------------------------------------------
cat("=== Loading Configuration ===\n")

dirs <- replication_init_dirs(require_existing = TRUE)
project_path <- dirname(dirs$main_dir)
cat("Project directory:", project_path, "\n")

# Load units data
if (!file.exists("outputs/logs/units_2005.rds")) {
    stop("Run 01_data.R first to prepare unit data.")
}
units_2005 <- readRDS("outputs/logs/units_2005.rds")

# -----------------------------------------------------------------------------
# 2. Select a Single Unit for Testing
# -----------------------------------------------------------------------------
cat("\n=== Selecting Test Unit ===\n")

# Pick one unit from 2005 data (top SOx emitter)
units_dt <- data.table(units_2005)
test_unit <- units_dt[order(-SOx)][1, ]
cat("Selected Unit:\n")
print(test_unit[, .(ID, Latitude, Longitude, Height, SOx)])

# Document unit properties
unit_doc <- data.frame(
    Parameter = c("ID", "Latitude", "Longitude", "Stack Height (m)", "SOx (tons/year)"),
    Value = c(
        test_unit$ID,
        round(test_unit$Latitude, 4),
        round(test_unit$Longitude, 4),
        test_unit$Height,
        round(test_unit$SOx, 1)
    )
)
write.csv(unit_doc, "outputs/tables/test_unit_properties.csv", row.names = FALSE)
saveRDS(test_unit, "outputs/logs/test_unit.rds")
cat("Test unit saved to outputs/logs/test_unit.rds\n")

# -----------------------------------------------------------------------------
# 3. Parameter Reference (Educational Documentation)
# -----------------------------------------------------------------------------
param_notes <- "
=== disperseR API Parameter Reference ===

1. define_inputs() — Creates the emission event table
   - units: data.table with ID, Latitude, Longitude, Height columns
   - startday: First date to simulate (YYYY-MM-DD string)
   - endday: Last date to simulate  
   - start.hours: Vector of hours (0-23) to release parcels
   - duration: Forward run duration in hours (e.g., 120 = 5 days)

2. run_disperser_parallel() — Executes HYSPLIT for each input ref
   - input.refs: Output from define_inputs()
   - pbl.height: Planetary boundary layer raster (from get_data)
   - species: 'so2' (gas) or 'so4p' (particulate sulfate)
   - proc_dir: Temporary processing directory
   - hysp_dir: Output directory for .fst parcel files
   - meteo_dir: Directory with met files (RP*.gbl)
   - npart: Number of air parcels (default 100)
   - mc.cores: Parallel cores (2 for CRAN compliance)
   - overwrite: Whether to rerun existing outputs

3. HYSPLIT Internal Parameters (set by add_params/add_emissions/add_species)
   - lat/lon/height: Source location
   - duration: Run length in hours
   - direction: 'forward' or 'backward'
   - met_type: 'reanalysis' (NCEP/NCAR) or other
   - npart: Number of parcels tracked

4. Output Format
   - .fst files with columns: lon, lat, height, Pdate, hour
   - Each file = one unit + start_day + start_hour combination
"

cat(param_notes)
writeLines(param_notes, "outputs/logs/parameter_reference.txt")
cat("\nParameter reference saved to outputs/logs/parameter_reference.txt\n")

# -----------------------------------------------------------------------------
# 4. Download Required Data (if not already present)
# -----------------------------------------------------------------------------
cat("\n=== Checking Required Data ===\n")

# Check for met files
met_files <- list.files(dirs$meteo_dir, pattern = "\\.gbl$", full.names = TRUE)
cat("Met files found:", length(met_files), "\n")

if (length(met_files) == 0) {
    cat("\nDownloading met data for January 2005...\n")
    get_data(
        data = "metfiles",
        start.year = "2005",
        start.month = "01",
        end.year = "2005",
        end.month = "01"
    )
    met_files <- list.files(dirs$meteo_dir, pattern = "\\.gbl$", full.names = TRUE)
}

# Load PBL data using get_data() (ensures proper caching and CRS)
cat("Loading PBL height data...\n")
pblheight <- tryCatch({
    get_data(data = "pblheight")
}, error = function(e) {
    cat("Warning: Could not load PBL via get_data():", e$message, "\n")
    # Fallback to direct file load with CRS correction
    pbl_file <- file.path(dirs$hpbl_dir, "hpbl.mon.mean.nc")
    if (!file.exists(pbl_file)) return(NULL)
    pbl <- terra::rast(pbl_file)
    terra::crs(pbl) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
    pbl
})

cat("PBL data loaded:", !is.null(pblheight), "\n")

# -----------------------------------------------------------------------------
# 5. Create Input References for Single Unit/Date
# -----------------------------------------------------------------------------
cat("\n=== Creating Input References ===\n")

# Single day, single hour for minimal test
input_refs <- define_inputs(
    units = test_unit,
    startday = "2005-01-15",
    endday = "2005-01-15",
    start.hours = c(0),       # Just midnight
    duration = 24             # 1 day forward (quick test)
)

cat("Input refs created:", nrow(input_refs), "emission events\n")
print(input_refs)

saveRDS(input_refs, "outputs/logs/single_run_input_refs.rds")

# -----------------------------------------------------------------------------
# 6. Run HYSPLIT Dispersion (Single Event)
# -----------------------------------------------------------------------------
cat("\n=== Running HYSPLIT Dispersion ===\n")

# Check if HYSPLIT binaries are available via splitr
splitr_available <- requireNamespace("splitr", quietly = TRUE)

if (!splitr_available) {
    cat("WARNING: splitr not installed. HYSPLIT binaries unavailable.\n")
    cat("Install with: remotes::install_github('rich-iannone/splitr')\n")
    cat("Skipping actual HYSPLIT run.\n\n")
    hysplit_ran <- FALSE
} else if (length(met_files) == 0) {
    cat("WARNING: No met files found. Cannot run HYSPLIT.\n")
    hysplit_ran <- FALSE
} else if (is.null(pblheight)) {
    cat("WARNING: PBL data not loaded. Cannot run HYSPLIT.\n")
    hysplit_ran <- FALSE
} else {
    cat("All requirements met. Running HYSPLIT...\n\n")
    
    start_time <- Sys.time()
    
    result <- tryCatch({
        run_disperser_parallel(
            input.refs = input_refs,
            pbl.height = pblheight,
            species = "so2",
            proc_dir = dirs$proc_dir,
            hysp_dir = dirs$hysp_dir,
            meteo_dir = dirs$meteo_dir,
            overwrite = FALSE,
            npart = 100,
            mc.cores = 1,
            keep.hysplit.files = FALSE
        )
    }, error = function(e) {
        cat("HYSPLIT ERROR:", conditionMessage(e), "\n")
        NULL
    })
    
    end_time <- Sys.time()
    runtime <- difftime(end_time, start_time, units = "secs")
    
    hysplit_ran <- !is.null(result)
    
    if (hysplit_ran) {
        cat("HYSPLIT completed in", round(as.numeric(runtime), 1), "seconds\n")
    }
}

# -----------------------------------------------------------------------------
# 7. Inspect Output (if available)
# -----------------------------------------------------------------------------
cat("\n=== Inspecting Output ===\n")

fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE)
cat("Output .fst files found:", length(fst_files), "\n")

if (length(fst_files) > 0) {
    # Read first output file
    sample_file <- file.path(dirs$hysp_dir, fst_files[1])
    parcels <- fst::read_fst(sample_file)
    
    cat("\nOutput file:", basename(sample_file), "\n")
    cat("Rows:", nrow(parcels), "\n")
    cat("Columns:", paste(names(parcels), collapse = ", "), "\n\n")
    
    cat("Sample data (first 10 rows):\n")
    print(head(parcels, 10))
    
    # Summary statistics
    cat("\nHeight statistics (m):\n")
    print(summary(parcels$height))
    
    # Save sample for reference
    write.csv(head(parcels, 100), "outputs/tables/single_run_sample.csv", row.names = FALSE)
    cat("\nSample output saved to outputs/tables/single_run_sample.csv\n")
} else {
    cat("No output files generated.\n")
    if (!hysplit_ran) {
        cat("This is expected if HYSPLIT prerequisites were not met.\n")
    }
}

# -----------------------------------------------------------------------------
# 8. Run Summary
# -----------------------------------------------------------------------------
run_summary <- data.frame(
    Step = c(
        "Unit selected", "Met files available", "PBL data loaded",
        "Input refs created", "HYSPLIT ran", "Output files generated"
    ),
    Status = c(
        "Yes",
        ifelse(length(met_files) > 0, paste("Yes (", length(met_files), " files)"), "No"),
        ifelse(!is.null(pblheight), "Yes", "No"),
        paste("Yes (", nrow(input_refs), " events)"),
        ifelse(hysplit_ran, "Yes", "No"),
        ifelse(length(fst_files) > 0, paste("Yes (", length(fst_files), " files)"), "No")
    )
)

cat("\n=== Run Summary ===\n")
print(run_summary, row.names = FALSE)
write.csv(run_summary, "outputs/tables/single_run_summary.csv", row.names = FALSE)

cat("\n=== Phase 2 Complete ===\n")
cat("Next: Run 03_batch_run.R to process multiple units/dates\n")
