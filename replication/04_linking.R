# 04_linking.R - Phase 4: Link Dispersion to Geography
# Aggregate parcel data to ZIP codes using link_all_units()

rm(list = ls())
suppressPackageStartupMessages({
  library(disperseR)
  library(data.table)
})
source("helpers.R")

dirs <- replication_init_dirs(require_existing = TRUE)

# --- Load Data ---
if (!file.exists("outputs/logs/units_2005.rds")) {
  stop("Run 01_data.R first to prepare unit data.")
}
units_2005 <- data.table(readRDS("outputs/logs/units_2005.rds"))
units_run <- units_2005[order(-SOx)][1:2]
if (!"uID" %in% names(units_run)) units_run[, uID := ID]
message("Units to link: ", nrow(units_run))

# Load PBL with fallback
pblheight <- tryCatch(get_data(data = "pblheight"), error = function(e) {
  pbl_file <- file.path(dirs$hpbl_dir, "hpbl.mon.mean.nc")
  if (!file.exists(pbl_file)) return(NULL)
  pbl <- terra::rast(pbl_file)
  terra::crs(pbl) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
  pbl
})

crosswalk <- tryCatch(get_data(data = "crosswalk"), error = function(e) disperseR::crosswalk)

# --- Check HYSPLIT Outputs ---
fst_files <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE)
message("HYSPLIT output files: ", length(fst_files))

if (length(fst_files) == 0) {
  message("No dispersion outputs. Run 02_single_run.R or 03_batch_run.R first.")
  link_results <- data.frame(
    link_type = "zips", pbl_trim = TRUE, n_records = NA_integer_,
    runtime_sec = NA_real_, status = "SKIPPED (no outputs)"
  )
  write.csv(link_results, "outputs/tables/link_results.csv", row.names = FALSE)
  stop("Phase 4 requires HYSPLIT outputs. See 02_single_run.R.", call. = FALSE)
}

# --- Link to ZIP Codes ---
yearmons <- get_yearmon("2005", "01", "2005", "01")
message("Year-months: ", paste(yearmons, collapse = ", "))

link_results <- data.frame(
  link_type = character(), pbl_trim = logical(), n_records = integer(),
  runtime_sec = numeric(), status = character(), stringsAsFactors = FALSE
)

can_link <- !is.null(pblheight) && !is.null(crosswalk)
if (!can_link) {
  message("Missing PBL or crosswalk data.")
  link_results <- rbind(link_results, data.frame(
    link_type = "zips", pbl_trim = TRUE, n_records = NA,
    runtime_sec = NA, status = "SKIPPED (missing data)"
  ))
} else {
  message("\nLinking parcels to ZIP codes...")
  
  duration_hours <- 120
  duration_source <- "default"

  last_run_file <- "outputs/logs/last_run_config.rds"
  if (file.exists(last_run_file)) {
    last_run <- readRDS(last_run_file)
    if (!is.null(last_run$duration_run_hours)) {
      duration_hours <- last_run$duration_run_hours[1]
      duration_source <- last_run$source
      message("  Using duration from last run config (", duration_source, "): ",
              duration_hours, " hours")
    }
  }

  if (duration_source == "default") {
    ref_files <- c(
      "outputs/logs/single_run_input_refs.rds",
      "outputs/logs/input_refs_real.rds"
    )
    ref_files <- ref_files[file.exists(ref_files)]
    if (length(ref_files) > 0) {
      latest_file <- ref_files[which.max(file.info(ref_files)$mtime)]
      refs <- readRDS(latest_file)
      if ("duration_run_hours" %in% names(refs)) {
        duration_hours <- refs$duration_run_hours[1]
        message("  Using duration from ", basename(latest_file), ": ",
                duration_hours, " hours")
      }
    }
  }
  
  start_time <- Sys.time()
  
  linked_zips <- tryCatch({
    link_all_units(
      units.run = units_run, link.to = "zips", mc.cores = 1,
      year.mons = yearmons, pbl_trim = TRUE, pbl.height = pblheight,
      crosswalk. = crosswalk, hysp_dir = dirs$hysp_dir,
      ziplink_dir = dirs$ziplink_dir, duration.run.hours = duration_hours,
      res.link = 12000, overwrite = TRUE, return.linked.data = TRUE
    )
  }, error = function(e) {
    message("Error: ", e$message)
    NULL
  })
  
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (!is.null(linked_zips) && nrow(linked_zips) > 0) {
    message("  Records: ", nrow(linked_zips), " (", round(runtime, 1), " sec)")
    link_results <- rbind(link_results, data.frame(
      link_type = "zips", pbl_trim = TRUE, n_records = nrow(linked_zips),
      runtime_sec = round(runtime, 2), status = "SUCCESS"
    ))
    
    fst::write_fst(linked_zips, "outputs/logs/linked_zips.fst")
    
    zip_summary <- linked_zips[, .(total_N = sum(N, na.rm = TRUE)), by = .(ZIP)]
    write.csv(head(zip_summary[order(-total_N)], 50),
              "outputs/tables/top_50_zips_exposure.csv", row.names = FALSE)
  } else {
    link_results <- rbind(link_results, data.frame(
      link_type = "zips", pbl_trim = TRUE, n_records = 0,
      runtime_sec = round(runtime, 2), status = "EMPTY"
    ))
  }
}

# --- Combine Monthly Links ---
if (exists("linked_zips") && !is.null(linked_zips) && nrow(linked_zips) > 0) {
  tryCatch({
    combine_monthly_links(month_YYYYMMs = yearmons, link.to = "zips",
                          filename = "hyads_linked_zips.RData")
    message("Combined links created.")
  }, error = function(e) message("combine_monthly_links error: ", e$message))
}

write.csv(link_results, "outputs/tables/link_results.csv", row.names = FALSE)
message("\nPhase 4 complete. Next: source('05_exposure.R')")
