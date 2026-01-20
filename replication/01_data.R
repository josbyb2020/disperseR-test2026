# =============================================================================
# 01_data.R — Phase 1: Data Acquisition (Real World)
# HyADS Exposure Mapping of U.S. Power Plants (2005): Replication + Sensitivity Study
# =============================================================================
#
# Goal: pull and inspect real public data.
#
# Uses: get_data(), get_met_reanalysis()
#
# Deliverable: Data provenance table (source URLs + date accessed)
# =============================================================================

rm(list = ls())
library(disperseR)
source("helpers.R")

# Re-initialize disperseR cache (required for each new R session)
# This populates the internal cache that get_data() depends on
dirs <- replication_init_dirs(require_existing = TRUE)
project_path <- dirname(dirs$main_dir)
cat("disperseR cache initialized at:", project_path, "\n\n")

# -----------------------------------------------------------------------------
# 1. Data Provenance Tracking
# -----------------------------------------------------------------------------
# Initialize provenance table
provenance <- data.frame(
    dataset = character(),
    source_url = character(),
    date_accessed = character(),
    local_path = character(),
    status = character(),
    stringsAsFactors = FALSE
)

add_provenance <- function(prov_df, dataset, url, path, status) {
    rbind(prov_df, data.frame(
        dataset = dataset,
        source_url = url,
        date_accessed = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        local_path = path,
        status = status,
        stringsAsFactors = FALSE
    ))
}

# -----------------------------------------------------------------------------
# 2. Get Crosswalk Data
# -----------------------------------------------------------------------------
cat("=== Downloading ZCTA-County Crosswalk ===\n")
tryCatch(
    {
        crosswalk <- get_data("crosswalk")
        cat("Crosswalk downloaded successfully\n")
        cat("Dimensions:", nrow(crosswalk), "rows x", ncol(crosswalk), "cols\n")
        print(head(crosswalk))

        provenance <- add_provenance(
            provenance,
            "crosswalk",
            "HUD ZCTA-County Crosswalk (via disperseR)",
            "disperseR::crosswalk (built-in dataset, cached in session)",
            "SUCCESS"
        )
    },
    error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        provenance <- add_provenance(provenance, "crosswalk", "HUD ZCTA-County", "", paste("FAILED:", e$message))
    }
)

# -----------------------------------------------------------------------------
# 3. Get ZCTA Shapefile
# -----------------------------------------------------------------------------
cat("\n=== Downloading ZCTA Shapefile ===\n")
tryCatch(
    {
        zcta_shape <- get_data("zctashapefile")
        cat("ZCTA shapefile downloaded successfully\n")
        cat("Type:", class(zcta_shape)[1], "\n")
        cat("Features:", nrow(zcta_shape), "\n")

        provenance <- add_provenance(
            provenance,
            "zctashapefile",
            "US Census TIGER/Line ZCTA (via disperseR)",
            dirs$zcta_dir,
            "SUCCESS"
        )
    },
    error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        provenance <- add_provenance(provenance, "zctashapefile", "US Census TIGER/Line", "", paste("FAILED:", e$message))
    }
)

# -----------------------------------------------------------------------------
# 4. Get PBL Height Data
# -----------------------------------------------------------------------------
cat("\n=== Downloading PBL Height Data ===\n")
tryCatch(
    {
        pblheight <- get_data("pblheight")
        cat("PBL height data downloaded successfully\n")
        cat("Type:", class(pblheight)[1], "\n")

        provenance <- add_provenance(
            provenance,
            "pblheight",
            "NARR PBL Height (via disperseR)",
            dirs$hpbl_dir,
            "SUCCESS"
        )
    },
    error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        provenance <- add_provenance(provenance, "pblheight", "NARR PBL Height", "", paste("FAILED:", e$message))
    }
)

# -----------------------------------------------------------------------------
# 5. Get Meteorological Data (Optional - Large Download)
# -----------------------------------------------------------------------------
cat("\n=== Downloading Meteorological Data (January 2005) ===\n")
cat("NOTE: This is a large file (~500MB). Skip if bandwidth is limited.\n")

# Uncomment to download met data:
# tryCatch({
#   # Download reanalysis met files for January 2005
#   get_met_reanalysis(
#     files = "RP200501.gbl",
#     path_met_files = dirs$meteo_dir
#   )
#   cat("Met data downloaded successfully\n")
#
#   provenance <- add_provenance(
#     provenance,
#     "met_200501",
#     "NCEP/NCAR Reanalysis (via ARL)",
#     file.path(dirs$meteo_dir, "RP200501.gbl"),
#     "SUCCESS"
#   )
# }, error = function(e) {
#   cat("ERROR:", conditionMessage(e), "\n")
#   provenance <- add_provenance(provenance, "met_200501", "NCEP/NCAR Reanalysis", "", paste("FAILED:", e$message))
# })

# For now, add placeholder for met data
provenance <- add_provenance(
    provenance,
    "met_200501",
    "NCEP/NCAR Reanalysis (https://www.ready.noaa.gov/data/archives/reanalysis/)",
    "Not downloaded (uncomment code to download)",
    "SKIPPED"
)

# -----------------------------------------------------------------------------
# 6. Inspect Built-in Units Data
# -----------------------------------------------------------------------------
cat("\n=== Inspecting Built-in Units Data ===\n")
tryCatch(
    {
        # disperseR should have built-in units data for 2005
        data("units", package = "disperseR")
        cat("Units data loaded\n")
        cat("Dimensions:", nrow(units), "rows x", ncol(units), "cols\n")
        cat("Years available:", unique(units$year), "\n")

        # Filter to 2005
        units_2005 <- units[units$year == 2005, ]
        cat("Units in 2005:", nrow(units_2005), "\n")
        print(head(units_2005))

        # Save for later use
        saveRDS(units_2005, "outputs/logs/units_2005.rds")

        provenance <- add_provenance(
            provenance,
            "units_2005",
            "disperseR built-in (EPA CEMS)",
            "outputs/logs/units_2005.rds",
            "SUCCESS"
        )
    },
    error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        cat("Note: Units data may have different name in this version\n")
    }
)

# -----------------------------------------------------------------------------
# 7. Save Data Provenance Table
# -----------------------------------------------------------------------------
cat("\n=== Saving Data Provenance ===\n")
write.csv(provenance, "outputs/tables/data_provenance.csv", row.names = FALSE)
print(provenance)
cat("\nProvenance table saved to outputs/tables/data_provenance.csv\n")

cat("\n=== Phase 1 Complete ===\n")
cat("Next: Run 02_single_run.R to perform a single dispersion run\n")
