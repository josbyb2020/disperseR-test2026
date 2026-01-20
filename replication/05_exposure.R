# =============================================================================
# 05_exposure.R — Phase 5: Exposure Calculation
# HyADS Exposure Mapping of U.S. Power Plants (2005): Replication + Sensitivity Study
# =============================================================================
#
# Goal: Compute HyADS exposure using calculate_exposure().
#
# Uses: calculate_exposure(), combine_monthly_links(), PP.units.monthly1995_2017
#
# Deliverable: Exposure tables and aggregation comparison.
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
# 2. Check for Linked Data
# -----------------------------------------------------------------------------
cat("\n=== Checking for Linked Data ===\n")

linked_file <- "outputs/logs/linked_zips.fst"
rdata_file <- file.path(dirs$rdata_dir, "hyads_linked_zips.RData")

# Check if linked data exists
has_linked_data <- file.exists(linked_file) || file.exists(rdata_file)

if (!has_linked_data) {
    cat("No linked data found.\n")
    cat("Run 04_linking.R first to generate linked ZIP data.\n\n")
    
    # Create placeholder results
    exposure_results <- data.frame(
        time_agg = c("year", "month"),
        source_agg = c("total", "total"),
        allow_partial = c(FALSE, FALSE),
        n_records = NA_integer_,
        total_exposure = NA_real_,
        status = "SKIPPED (no linked data)"
    )
    write.csv(exposure_results, "outputs/tables/exposure_results.csv", row.names = FALSE)
    
    cat("=== Phase 5 Incomplete ===\n")
}

if (has_linked_data) {
    cat("Linked data found.\n")

# Load linked data if available
if (file.exists(linked_file)) {
    linked_zips <- fst::read_fst(linked_file)
    cat("Loaded", nrow(linked_zips), "linked records from fst file.\n")
}

# -----------------------------------------------------------------------------
# 3. Load Emissions Data
# -----------------------------------------------------------------------------
cat("\n=== Loading Emissions Data ===\n")

# The calculate_exposure function uses PP.units.monthly1995_2017
# This is a built-in dataset in disperseR
units_monthly <- disperseR::PP.units.monthly1995_2017

cat("PP.units.monthly1995_2017 loaded:", nrow(units_monthly), "rows\n")
cat("Year range:", min(units_monthly$year), "-", max(units_monthly$year), "\n")
cat("Columns:", paste(names(units_monthly), collapse = ", "), "\n")

# Filter to 2005
units_2005_monthly <- units_monthly[year == 2005]
cat("2005 records:", nrow(units_2005_monthly), "\n")

# -----------------------------------------------------------------------------
# 4. Calculate Exposure
# -----------------------------------------------------------------------------
cat("\n=== Calculating Exposure ===\n")

exposure_results <- data.frame(
    time_agg = character(),
    source_agg = character(),
    allow_partial = logical(),
    n_records = integer(),
    total_exposure = numeric(),
    status = character(),
    stringsAsFactors = FALSE
)

# Check if RData file exists (from combine_monthly_links)
if (file.exists(rdata_file)) {
    cat("Using combined links from:", rdata_file, "\n\n")
    
    # Annual exposure, total sources
    cat("--- time.agg='year', source.agg='total' ---\n")
    exp_annual_total <- tryCatch({
        calculate_exposure(
            year.E = 2005,
            year.D = 2005,
            link.to = "zips",
            pollutant = "SO2.tons",
            units.mo = disperseR::PP.units.monthly1995_2017,
            rda_file = rdata_file,
            exp_dir = dirs$exp_dir,
            source.agg = "total",
            time.agg = "year",
            return.monthly.data = FALSE,
            allow.partial = TRUE
        )
    }, error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        NULL
    })
    
    if (!is.null(exp_annual_total) && nrow(exp_annual_total) > 0) {
        cat("Exposure calculated:", nrow(exp_annual_total), "records\n")
        
        exposure_results <- rbind(exposure_results, data.frame(
            time_agg = "year",
            source_agg = "total",
            allow_partial = TRUE,
            n_records = nrow(exp_annual_total),
            total_exposure = sum(exp_annual_total$hyads, na.rm = TRUE),
            status = "SUCCESS"
        ))
        
        # Save results
        write.csv(exp_annual_total, "outputs/tables/exposure_annual_total.csv", row.names = FALSE)
        cat("Saved to outputs/tables/exposure_annual_total.csv\n")
        
        # Top 20 ZIPs
        top_zips <- head(exp_annual_total[order(-hyads)], 20)
        cat("\nTop 20 ZIPs by exposure:\n")
        print(top_zips)
    } else {
        cat("No exposure records calculated.\n")
        exposure_results <- rbind(exposure_results, data.frame(
            time_agg = "year",
            source_agg = "total",
            allow_partial = TRUE,
            n_records = 0,
            total_exposure = 0,
            status = "EMPTY"
        ))
    }
    
    # Monthly exposure
    cat("\n--- time.agg='month', source.agg='total' ---\n")
    exp_monthly <- tryCatch({
        calculate_exposure(
            year.E = 2005,
            year.D = 2005,
            link.to = "zips",
            pollutant = "SO2.tons",
            units.mo = disperseR::PP.units.monthly1995_2017,
            rda_file = rdata_file,
            exp_dir = dirs$exp_dir,
            source.agg = "total",
            time.agg = "month",
            return.monthly.data = TRUE,
            allow.partial = TRUE
        )
    }, error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        NULL
    })
    
    if (!is.null(exp_monthly) && nrow(exp_monthly) > 0) {
        cat("Monthly exposure:", nrow(exp_monthly), "records\n")
        
        exposure_results <- rbind(exposure_results, data.frame(
            time_agg = "month",
            source_agg = "total",
            allow_partial = TRUE,
            n_records = nrow(exp_monthly),
            total_exposure = sum(exp_monthly$hyads, na.rm = TRUE),
            status = "SUCCESS"
        ))
        
        write.csv(exp_monthly, "outputs/tables/exposure_monthly.csv", row.names = FALSE)
    } else {
        exposure_results <- rbind(exposure_results, data.frame(
            time_agg = "month",
            source_agg = "total",
            allow_partial = TRUE,
            n_records = 0,
            total_exposure = 0,
            status = "EMPTY"
        ))
    }
    
    # By unit aggregation
    cat("\n--- time.agg='year', source.agg='unit' ---\n")
    exp_by_unit <- tryCatch({
        calculate_exposure(
            year.E = 2005,
            year.D = 2005,
            link.to = "zips",
            pollutant = "SO2.tons",
            units.mo = disperseR::PP.units.monthly1995_2017,
            rda_file = rdata_file,
            exp_dir = dirs$exp_dir,
            source.agg = "unit",
            time.agg = "year",
            return.monthly.data = FALSE,
            allow.partial = TRUE
        )
    }, error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        NULL
    })
    
    if (!is.null(exp_by_unit) && nrow(exp_by_unit) > 0) {
        cat("Unit-level exposure:", nrow(exp_by_unit), "records\n")
        
        exposure_results <- rbind(exposure_results, data.frame(
            time_agg = "year",
            source_agg = "unit",
            allow_partial = TRUE,
            n_records = nrow(exp_by_unit),
            total_exposure = NA,  # Multiple columns
            status = "SUCCESS"
        ))
        
        write.csv(exp_by_unit, "outputs/tables/exposure_by_unit.csv", row.names = FALSE)
    } else {
        exposure_results <- rbind(exposure_results, data.frame(
            time_agg = "year",
            source_agg = "unit",
            allow_partial = TRUE,
            n_records = 0,
            total_exposure = 0,
            status = "EMPTY"
        ))
    }
    
} else {
    cat("No combined RData file found.\n")
    cat("Run 04_linking.R with combine_monthly_links() first.\n")
    
    exposure_results <- data.frame(
        time_agg = c("year", "month"),
        source_agg = c("total", "total"),
        allow_partial = c(TRUE, TRUE),
        n_records = NA,
        total_exposure = NA,
        status = "SKIPPED (no RData file)"
    )
}

# -----------------------------------------------------------------------------
# 5. Aggregation Parameter Reference
# -----------------------------------------------------------------------------
agg_notes <- "
=== calculate_exposure() Parameter Reference ===

REQUIRED PARAMETERS:
- year.E: Emissions year (numeric, e.g., 2005)
- year.D: Dispersion year (usually same as year.E)
- link.to: 'zips', 'counties', or 'grids'

DATA SOURCES:
- units.mo: Monthly emissions data (use PP.units.monthly1995_2017)
- rda_file: Path to RData from combine_monthly_links()
- pollutant: Column name in units.mo (e.g., 'SO2.tons')

AGGREGATION OPTIONS:
- source.agg: 'total' | 'facility' | 'unit'
  - 'total': Single exposure column summing all sources
  - 'facility': One column per facility
  - 'unit': One column per unit

- time.agg: 'year' | 'month'
  - 'year': Annual totals
  - 'month': Monthly breakdowns

ROBUSTNESS:
- allow.partial: If TRUE, continues with available months
  (warns about missing months, attaches 'missing_maps' attribute)

OUTPUT:
- data.table with ZIP/county/grid identifiers and exposure values
- When source.agg != 'total', output has multiple exposure columns
"

cat(agg_notes)
writeLines(agg_notes, "outputs/logs/exposure_reference.txt")

}

# -----------------------------------------------------------------------------
# 6. Summary
# -----------------------------------------------------------------------------
cat("\n=== Exposure Calculation Summary ===\n")
print(exposure_results)

write.csv(exposure_results, "outputs/tables/exposure_results.csv", row.names = FALSE)

cat("\n=== Phase 5 Complete ===\n")
cat("Next: Run 06_plots.R for visualization\n")
