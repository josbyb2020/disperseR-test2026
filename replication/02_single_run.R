# 02_single_run.R - Phase 2: Single Dispersion Run
# Minimal proof-of-concept HYSPLIT execution for one unit/date
# See docs/REFERENCE.md for parameter details

rm(list = ls())
suppressPackageStartupMessages({
  library(disperseR)
  library(data.table)
})
source("helpers.R")

dirs <- replication_init_dirs(require_existing = TRUE)

# --- Load Unit Data ---
if (!file.exists("outputs/logs/units_2005.rds")) {
  stop("Run 01_data.R first to prepare unit data.")
}
units_2005 <- readRDS("outputs/logs/units_2005.rds")

# --- Select Test Unit (top SOx emitter) ---
units_dt <- data.table(units_2005)
test_unit <- units_dt[order(-SOx)][1, ]
message("Selected unit: ", test_unit$ID)
message("  Location: ", round(test_unit$Latitude, 4), ", ", round(test_unit$Longitude, 4))
message("  SOx: ", round(test_unit$SOx, 1), " tons/year")

unit_doc <- data.frame(
  Parameter = c("ID", "Latitude", "Longitude", "Stack Height (m)", "SOx (tons/year)"),
  Value = c(test_unit$ID, round(test_unit$Latitude, 4),
            round(test_unit$Longitude, 4), test_unit$Height, round(test_unit$SOx, 1))
)
write.csv(unit_doc, "outputs/tables/test_unit_properties.csv", row.names = FALSE)
saveRDS(test_unit, "outputs/logs/test_unit.rds")

# --- Parameter Reference ---
param_notes <- "
disperseR API Reference

define_inputs()
  units       data.table with ID, Latitude, Longitude, Height
  startday    First date (YYYY-MM-DD)
  endday      Last date
  start.hours Vector of release hours (0-23)
  duration    Forward run duration in hours

run_disperser_parallel()
  input.refs  Output from define_inputs()
  pbl.height  Planetary boundary layer raster
  species     'so2' (gas) or 'so4p' (particulate)
  proc_dir    Temporary processing directory
  hysp_dir    Output directory for .fst files
  meteo_dir   Met file directory (RP*.gbl)
  npart       Number of parcels (default 100)
  mc.cores    Parallel cores (2 max for CRAN)

Output: .fst files with columns lon, lat, height, Pdate, hour
"
writeLines(param_notes, "outputs/logs/parameter_reference.txt")

# --- Check Prerequisites ---
met_files <- list.files(dirs$meteo_dir, pattern = "\\.gbl$", full.names = TRUE)
if (length(met_files) == 0) {
  message("Downloading met data for January 2005...")
  get_data(data = "metfiles", start.year = "2005", start.month = "01",
           end.year = "2005", end.month = "01")
  met_files <- list.files(dirs$meteo_dir, pattern = "\\.gbl$", full.names = TRUE)
}
message("Met files: ", length(met_files))

pblheight <- tryCatch(get_data(data = "pblheight"), error = function(e) {
  pbl_file <- file.path(dirs$hpbl_dir, "hpbl.mon.mean.nc")
  if (!file.exists(pbl_file)) return(NULL)
  pbl <- terra::rast(pbl_file)
  terra::crs(pbl) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
  pbl
})
message("PBL loaded: ", !is.null(pblheight))

# --- Create Input References ---
input_refs <- define_inputs(
  units = test_unit,
  startday = "2005-01-15",
  endday = "2005-01-15",
  start.hours = c(0),
  duration = 24
)
message("Input refs: ", nrow(input_refs), " events")
saveRDS(input_refs, "outputs/logs/single_run_input_refs.rds")

# --- Run HYSPLIT ---
splitr_available <- requireNamespace("splitr", quietly = TRUE)
hysplit_ran <- FALSE

if (!splitr_available) {
  message("splitr not installed. Install with: remotes::install_github('rich-iannone/splitr')")
} else if (length(met_files) == 0) {
  message("No met files found.")
} else if (is.null(pblheight)) {
  message("PBL data not loaded.")
} else {
  message("\nRunning HYSPLIT...")
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
    message("HYSPLIT error: ", e$message)
    NULL
  })
  
  runtime <- difftime(Sys.time(), start_time, units = "secs")
  hysplit_ran <- !is.null(result)
  if (hysplit_ran) message("  Runtime: ", round(as.numeric(runtime), 1), " sec")
}

if (hysplit_ran) {
  saveRDS(
    list(
      source = "02_single_run.R",
      duration_run_hours = input_refs$duration_run_hours[1],
      created = Sys.time()
    ),
    "outputs/logs/last_run_config.rds"
  )
}

# --- Inspect Output ---
fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE)
message("Output files: ", length(fst_files))

if (length(fst_files) > 0) {
  sample_file <- file.path(dirs$hysp_dir, fst_files[1])
  parcels <- fst::read_fst(sample_file)
  message("  Rows: ", nrow(parcels), " | Cols: ", paste(names(parcels), collapse = ", "))
  write.csv(head(parcels, 100), "outputs/tables/single_run_sample.csv", row.names = FALSE)
}

# --- Summary ---
run_summary <- data.frame(
  Step = c("Unit selected", "Met files", "PBL data", "Input refs", "HYSPLIT ran", "Outputs"),
  Status = c("Yes",
    ifelse(length(met_files) > 0, paste(length(met_files), "files"), "No"),
    ifelse(!is.null(pblheight), "Yes", "No"),
    paste(nrow(input_refs), "events"),
    ifelse(hysplit_ran, "Yes", "No"),
    ifelse(length(fst_files) > 0, paste(length(fst_files), "files"), "No"))
)
write.csv(run_summary, "outputs/tables/single_run_summary.csv", row.names = FALSE)

message("\nPhase 2 complete. Next: source('03_batch_run.R')")
