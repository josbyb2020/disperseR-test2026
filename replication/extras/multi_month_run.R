# multi_month_run.R - Multi-month HYSPLIT comparison
# Runs Jan vs Feb vs Mar 2005 for seasonal pattern analysis
# Requires: met files for all three months

rm(list = ls())
suppressPackageStartupMessages({
  library(disperseR)
  library(data.table)
  library(ggplot2)
})
source("../helpers.R")

dirs <- replication_init_dirs(require_existing = TRUE)

met_files <- list.files(dirs$meteo_dir, pattern = "\\.gbl$")
message("Met files: ", paste(met_files, collapse = ", "))

units_dt <- data.table(disperseR::units)
test_unit <- units_dt[year == 2005][order(-SOx)][1]
message("Unit: ", test_unit$ID)

# Run mid-month for Jan, Feb, Mar
months <- c("01", "02", "03")
monthly_results <- list()

for (mo in months) {
  startday <- paste0("2005-", mo, "-15")
  message("\nMonth ", mo, ": ", startday)
  
  input_refs <- tryCatch({
    define_inputs(
      units = test_unit, startday = startday, endday = startday,
      start.hours = c(0, 12), duration = 120
    )
  }, error = function(e) NULL)
  
  if (is.null(input_refs)) {
    message("  Skipped (define_inputs failed)")
    next
  }
  
  # Check for required met file
  required_met <- paste0("RP2005", mo, ".gbl")
  if (!required_met %in% met_files) {
    message("  Skipped (missing ", required_met, ")")
    monthly_results[[mo]] <- data.frame(month = mo, status = "NO_MET")
    next
  }
  
  # Note: Uncomment to actually run HYSPLIT
  # result <- run_disperser_parallel(...)
  monthly_results[[mo]] <- data.frame(month = mo, status = "READY", n_refs = nrow(input_refs))
}

results_df <- do.call(rbind, monthly_results)
write.csv(results_df, "../outputs/tables/multi_month_status.csv", row.names = FALSE)
message("\nStatus saved to outputs/tables/multi_month_status.csv")
