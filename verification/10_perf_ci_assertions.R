if (!file.exists("helpers.R") && file.exists(file.path("verification", "helpers.R"))) {
  setwd("verification")
}

source("helpers.R")

cfg <- verify_read_config()
perf_dir <- file.path(cfg$base_dir, "perf")
summary_file <- file.path(perf_dir, "crossplatform_ci_summary.csv")

read_threshold <- function(env_name, default_value) {
  raw <- suppressWarnings(as.numeric(Sys.getenv(env_name, as.character(default_value))))
  if (!is.finite(raw) || is.na(raw) || raw <= 0) {
    return(as.numeric(default_value))
  }
  as.numeric(raw)
}

write_job_summary <- function(lines) {
  summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
  if (!nzchar(summary_path)) {
    return(invisible(FALSE))
  }
  cat(paste0(lines, collapse = "\n"), file = summary_path, append = TRUE)
  cat("\n", file = summary_path, append = TRUE)
  invisible(TRUE)
}

verify_step("load_crossplatform_summary", {
  verify_expect(
    file.exists(summary_file),
    paste0("Missing CI summary file: ", summary_file)
  )
  summary_dt <- data.table::fread(summary_file)
  verify_expect(nrow(summary_dt) > 0, "crossplatform_ci_summary.csv is empty.")
  assign("crossplatform_summary_dt", summary_dt, envir = verify_state)
  invisible(summary_dt)
})

verify_step("assert_crossplatform_perf_contract", {
  summary_dt <- get("crossplatform_summary_dt", envir = verify_state)

  verify_expect("speedup_x" %in% names(summary_dt), "speedup_x column is missing from summary output.")
  summary_dt[, speedup_x := suppressWarnings(as.numeric(speedup_x))]
  verify_expect(
    all(is.finite(summary_dt$speedup_x) & summary_dt$speedup_x > 0),
    "speedup_x contains non-finite or non-positive values."
  )

  if ("parity_ok" %in% names(summary_dt)) {
    summary_dt[, parity_ok := as.logical(parity_ok)]
    bad_parity <- summary_dt[!is.na(parity_ok) & !parity_ok]
    verify_expect(
      nrow(bad_parity) == 0,
      paste0("Parity check failed for ", nrow(bad_parity), " benchmark row(s).")
    )
  }

  min_engine <- read_threshold("DISPERSER_MIN_SPEEDUP_ENGINE", 1.05)
  min_userflow <- read_threshold("DISPERSER_MIN_SPEEDUP_USERFLOW", 0.75)
  min_heavy <- read_threshold("DISPERSER_MIN_SPEEDUP_HEAVY", 1.05)
  min_median <- read_threshold("DISPERSER_MIN_SPEEDUP_MEDIAN", 1.00)

  check_source_floor <- function(source_pattern, label, floor_value) {
    sub <- summary_dt[grepl(source_pattern, source_file, fixed = TRUE)]
    if (nrow(sub) == 0) {
      return(sprintf("- %s: no rows (skipped)", label))
    }
    min_seen <- suppressWarnings(min(sub$speedup_x, na.rm = TRUE))
    verify_expect(
      is.finite(min_seen) && min_seen >= floor_value,
      paste0(
        label, " minimum speedup below floor. ",
        "Required >= ", sprintf("%.3f", floor_value),
        ", observed ", sprintf("%.3f", min_seen), "."
      )
    )
    sprintf("- %s: min speedup %.3fx (floor %.2fx)", label, min_seen, floor_value)
  }

  checks <- c(
    check_source_floor("linking_engine_benchmark.csv", "Engine benchmark", min_engine),
    check_source_floor("userflow_link_all_units_", "User-flow benchmark", min_userflow),
    check_source_floor("heavy_user_flow_link_all_units.csv", "Heavy benchmark", min_heavy)
  )

  median_speedup <- suppressWarnings(stats::median(summary_dt$speedup_x, na.rm = TRUE))
  verify_expect(
    is.finite(median_speedup) && median_speedup >= min_median,
    paste0(
      "Median speedup below floor. Required >= ",
      sprintf("%.3f", min_median),
      ", observed ", sprintf("%.3f", median_speedup), "."
    )
  )

  os_name <- if ("os" %in% names(summary_dt)) unique(summary_dt$os) else "unknown"
  os_name <- os_name[!is.na(os_name)]
  summary_lines <- c(
    "## Cross-platform perf gate",
    paste0(
      "- Rows checked: ", nrow(summary_dt),
      " | OS: ", paste(os_name, collapse = ", ")
    ),
    checks,
    paste0("- Median speedup: ", sprintf("%.3fx", median_speedup), " (floor ", sprintf("%.2fx", min_median), ")")
  )
  write_job_summary(summary_lines)
  message(paste(summary_lines, collapse = "\n"))
  invisible(summary_dt)
})

verify_stop_on_failures("10_perf_ci_assertions")
