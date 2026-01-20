# =============================================================================
# 00_setup.R — Phase 0: Setup & Verification
# HyADS Exposure Mapping of U.S. Power Plants (2005): Replication + Sensitivity Study
# =============================================================================
#
# Goal: mimic "real user" install and document the system environment.
#
# Uses: create_dirs(), get_os()
# =============================================================================

# Clear environment
rm(list = ls())

source("helpers.R")

# -----------------------------------------------------------------------------
# 1. Create output directories first (before any file writes)
# -----------------------------------------------------------------------------
replication_ensure_outputs()

# -----------------------------------------------------------------------------
# 2. Document System Environment
# -----------------------------------------------------------------------------
cat("\n=== System Information ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R Version:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("OS:", Sys.info()["sysname"], Sys.info()["release"], "\n")
cat("Working Directory:", getwd(), "\n")

# Save system info to log
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
cat("\nSystem info saved to outputs/logs/system_info.csv\n")

# -----------------------------------------------------------------------------
# 3. Install disperseR from GitHub (if needed)
# -----------------------------------------------------------------------------
# Uncomment to install:
# if (!require("remotes")) install.packages("remotes")
# remotes::install_github("josbyb2020/disperseR-test2026")

# Load disperseR
library(disperseR)

# Verify OS detection
cat("\n=== disperseR OS Detection ===\n")
detected_os <- get_os()
cat("Detected OS:", detected_os, "\n")

# -----------------------------------------------------------------------------
# 4. Create disperseR Project Directory Structure
# -----------------------------------------------------------------------------
cat("\n=== Creating disperseR Directories ===\n")

if (!nzchar(Sys.getenv("DISPERSER_PROJECT"))) {
    cat("Tip: set DISPERSER_PROJECT to override the default project path.\n")
}

dirs <- replication_init_dirs(require_existing = FALSE)
project_path <- dirname(dirs$main_dir)

cat("Directories created at:", project_path, "\n")
print(dirs)

# Save directory configuration
saveRDS(dirs, "outputs/logs/directory_config.rds")
cat("Directory configuration saved to outputs/logs/directory_config.rds\n")

# -----------------------------------------------------------------------------
# 5. Run Verification Harness (Optional)
# -----------------------------------------------------------------------------
# If you have access to the verification folder, run:
# source("../verification/run_all.R")

cat("\n=== Phase 0 Complete ===\n")
cat("Next: Run 01_data.R to acquire datasets\n")
