if (!file.exists("helpers.R") && file.exists(file.path("verification", "helpers.R"))) {
  setwd("verification")
}

source("helpers.R")

scripts <- c(
  "00_setup.R",
  "01_smoke.R",
  "02_data.R",
  "03_hysplit.R",
  "04_pipeline.R",
  "05_plots.R"
)

for (s in scripts) {
  if (file.exists(s)) {
    source(s)
  } else {
    warning("Missing script: ", s, call. = FALSE)
  }
}

message("\nVerification Summary")
print(verify_state$results)

