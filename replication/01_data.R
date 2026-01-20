# 01_data.R - Phase 1: Data Acquisition
# Pulls and validates public datasets for 2005 analysis

rm(list = ls())
library(disperseR)
source("helpers.R")

dirs <- replication_init_dirs(require_existing = TRUE)
message("Cache initialized: ", dirname(dirs$main_dir))

# --- Provenance Tracking ---
provenance <- data.frame(
  dataset = character(), source_url = character(),
  date_accessed = character(), local_path = character(),
  status = character(), stringsAsFactors = FALSE
)

add_provenance <- function(prov, dataset, url, path, status) {
  rbind(prov, data.frame(
    dataset = dataset, source_url = url,
    date_accessed = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    local_path = path, status = status, stringsAsFactors = FALSE
  ))
}

# --- Crosswalk ---
message("\nDownloading ZCTA-County crosswalk...")
tryCatch({
  crosswalk <- get_data("crosswalk")
  message("  Rows: ", nrow(crosswalk), " | Cols: ", ncol(crosswalk))
  provenance <- add_provenance(provenance, "crosswalk",
    "HUD ZCTA-County (via disperseR)", "disperseR::crosswalk", "SUCCESS")
}, error = function(e) {
  message("  ERROR: ", e$message)
  provenance <<- add_provenance(provenance, "crosswalk", "HUD", "", paste("FAILED:", e$message))
})

# --- ZCTA Shapefile ---
message("\nDownloading ZCTA shapefile...")
tryCatch({
  zcta_shape <- get_data("zctashapefile")
  message("  Features: ", nrow(zcta_shape))
  provenance <- add_provenance(provenance, "zctashapefile",
    "US Census TIGER/Line ZCTA", dirs$zcta_dir, "SUCCESS")
}, error = function(e) {
  message("  ERROR: ", e$message)
  provenance <<- add_provenance(provenance, "zctashapefile", "Census", "", paste("FAILED:", e$message))
})

# --- PBL Height ---
message("\nDownloading PBL height data...")
tryCatch({
  pblheight <- get_data("pblheight")
  message("  Class: ", class(pblheight)[1])
  provenance <- add_provenance(provenance, "pblheight",
    "NARR PBL Height (via disperseR)", dirs$hpbl_dir, "SUCCESS")
}, error = function(e) {
  message("  ERROR: ", e$message)
  provenance <<- add_provenance(provenance, "pblheight", "NARR", "", paste("FAILED:", e$message))
})

# --- Met Data (optional large download) ---
# Uncomment to download:
# get_met_reanalysis(files = "RP200501.gbl", path_met_files = dirs$meteo_dir)
provenance <- add_provenance(provenance, "met_200501",
  "NCEP/NCAR Reanalysis (https://www.ready.noaa.gov/data/archives/reanalysis/)",
  "Not downloaded (uncomment to download)", "SKIPPED")

# --- Built-in Units ---
message("\nLoading built-in units data...")
tryCatch({
  data("units", package = "disperseR")
  units_2005 <- units[units$year == 2005, ]
  message("  Units in 2005: ", nrow(units_2005))
  saveRDS(units_2005, "outputs/logs/units_2005.rds")
  provenance <- add_provenance(provenance, "units_2005",
    "disperseR built-in (EPA CEMS)", "outputs/logs/units_2005.rds", "SUCCESS")
}, error = function(e) {
  message("  ERROR: ", e$message)
})

# --- Save Provenance ---
write.csv(provenance, "outputs/tables/data_provenance.csv", row.names = FALSE)
message("\nProvenance saved: outputs/tables/data_provenance.csv")
message("\nPhase 1 complete. Next: source('02_single_run.R')")
