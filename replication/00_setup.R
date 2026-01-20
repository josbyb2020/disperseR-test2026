# 00_setup.R - Phase 0: Setup & Verification
# HyADS Exposure Mapping of U.S. Power Plants (2005)

rm(list = ls())
source("helpers.R")

# Create output directories
replication_ensure_outputs()

# --- System Environment ---
message("Recording system information...")
system_info <- data.frame(
  item = c("Date", "R_Version", "Platform", "OS", "OS_Version"),
  value = c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    R.version.string,
    R.version$platform,
    as.character(Sys.info()["sysname"]),
    as.character(Sys.info()["release"])
  )
)
write.csv(system_info, "outputs/logs/system_info.csv", row.names = FALSE)
message("  Saved: outputs/logs/system_info.csv")

# --- Load disperseR ---
# Uncomment to install:
# remotes::install_github("josbyb2020/disperseR-test2026")
library(disperseR)

message("Detected OS: ", get_os())

# --- Create Project Directories ---
if (!nzchar(Sys.getenv("DISPERSER_PROJECT"))) {
  message("Tip: set DISPERSER_PROJECT env var to override the default path.")
}

dirs <- replication_init_dirs(require_existing = FALSE)
project_path <- dirname(dirs$main_dir)

message("Project directory: ", project_path)
message("  main_dir:   ", dirs$main_dir)
message("  meteo_dir:  ", dirs$meteo_dir)
message("  hysp_dir:   ", dirs$hysp_dir)

saveRDS(dirs, "outputs/logs/directory_config.rds")

message("\nPhase 0 complete. Next: source('01_data.R')")
