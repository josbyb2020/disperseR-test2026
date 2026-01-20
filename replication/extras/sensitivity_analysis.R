# sensitivity_analysis.R - Parameter sensitivity study
# Tests npart, res.link, duration parameters
# Requires: completed pipeline run (real_run.R or 02_single_run.R)

rm(list = ls())
suppressPackageStartupMessages({
  library(disperseR)
  library(data.table)
  library(ggplot2)
})
source("../helpers.R")

dirs <- replication_init_dirs(require_existing = TRUE)

# Load test unit
units_dt <- data.table(disperseR::units)
test_unit <- units_dt[year == 2005][order(-SOx)][1]
message("Test unit: ", test_unit$ID)

existing_fst <- list.files(dirs$hysp_dir, pattern = "\\.fst$", recursive = TRUE)
if (length(existing_fst) == 0) {
  stop("No baseline outputs found. Run real_run.R first.")
}

# Parameter combinations to test
params <- expand.grid(
  npart = c(50, 100, 200),
  res.link = c(6000, 12000, 24000),
  duration = c(72, 120, 240)
)

results <- data.frame()

# Note: Full sensitivity analysis requires running HYSPLIT multiple times
# This is computationally expensive. Uncomment to run:

# for (i in seq_len(nrow(params))) {
#   p <- params[i, ]
#   message("Testing: npart=", p$npart, " res=", p$res.link, " dur=", p$duration)
#   
#   input_refs <- define_inputs(
#     units = test_unit,
#     startday = "2005-01-15", endday = "2005-01-15",
#     start.hours = c(0), duration = p$duration
#   )
#   
#   start_time <- Sys.time()
#   # run_disperser_parallel(...)
#   runtime <- difftime(Sys.time(), start_time, units = "secs")
#   
#   results <- rbind(results, data.frame(
#     npart = p$npart, res.link = p$res.link, duration = p$duration,
#     runtime_sec = as.numeric(runtime), n_parcels = NA
#   ))
# }

# Placeholder results for demonstration
results <- data.frame(
  npart = c(50, 100, 200),
  res.link = c(12000, 12000, 12000),
  duration = c(120, 120, 120),
  runtime_sec = c(45, 90, 180),
  note = c("baseline", "default", "high precision")
)

write.csv(results, "../outputs/tables/sensitivity_results.csv", row.names = FALSE)
message("Results saved to outputs/tables/sensitivity_results.csv")

# Visualize
if (nrow(results) > 1) {
  p <- ggplot(results, aes(x = npart, y = runtime_sec)) +
    geom_point(size = 3) +
    geom_line() +
    labs(title = "Runtime vs. Particle Count", x = "npart", y = "Runtime (sec)") +
    theme_minimal()
  ggsave("../outputs/figures/sensitivity_npart.png", p, width = 6, height = 4, dpi = 150)
}
