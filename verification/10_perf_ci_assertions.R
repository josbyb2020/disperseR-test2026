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

detect_os_name <- function(summary_dt) {
  if ("os" %in% names(summary_dt)) {
    vals <- unique(tolower(as.character(summary_dt$os)))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) > 0) {
      return(vals[[1]])
    }
  }
  tolower(Sys.info()[["sysname"]])
}

default_speed_floor <- function(metric, os_name, profile_name) {
  is_windows <- identical(tolower(os_name), "windows")
  is_full <- identical(tolower(profile_name), "full")

  if (!is_windows) {
    return(switch(
      metric,
      engine = 1.05,
      userflow = 0.75,
      userflow_expected_fast = 0.95,
      userflow_non_extract = 0.70,
      heavy = 1.05,
      heavy_non_strict = 0.95,
      median = 1.00,
      1.00
    ))
  }

  switch(
    metric,
    engine = if (is_full) 0.95 else 0.90,
    userflow = if (is_full) 0.75 else 0.70,
    userflow_expected_fast = if (is_full) 0.95 else 0.90,
    userflow_non_extract = if (is_full) 0.70 else 0.65,
    heavy = if (is_full) 1.00 else 0.90,
    heavy_non_strict = if (is_full) 0.90 else 0.85,
    median = if (is_full) 0.95 else 0.90,
    0.90
  )
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
  os_name <- detect_os_name(summary_dt)
  profile_name <- tolower(Sys.getenv("DISPERSER_PERF_PROFILE", "smoke"))
  if (!(profile_name %in% c("smoke", "full"))) {
    profile_name <- "smoke"
  }

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

  min_engine <- read_threshold("DISPERSER_MIN_SPEEDUP_ENGINE", default_speed_floor("engine", os_name, profile_name))
  min_userflow <- read_threshold("DISPERSER_MIN_SPEEDUP_USERFLOW", default_speed_floor("userflow", os_name, profile_name))
  min_userflow_expected_fast <- read_threshold(
    "DISPERSER_MIN_SPEEDUP_USERFLOW_EXPECTED_FAST",
    default_speed_floor("userflow_expected_fast", os_name, profile_name)
  )
  min_userflow_non_extract <- read_threshold(
    "DISPERSER_MIN_SPEEDUP_USERFLOW_NON_EXTRACT",
    default_speed_floor("userflow_non_extract", os_name, profile_name)
  )
  min_userflow_expected_fast_rows <- as.integer(
    read_threshold("DISPERSER_MIN_ROWS_EXPECTED_FAST", 50000)
  )
  if (!is.finite(min_userflow_expected_fast_rows) ||
      is.na(min_userflow_expected_fast_rows) ||
      min_userflow_expected_fast_rows < 1) {
    min_userflow_expected_fast_rows <- 50000L
  }
  min_heavy <- read_threshold("DISPERSER_MIN_SPEEDUP_HEAVY", default_speed_floor("heavy", os_name, profile_name))
  min_heavy_non_strict <- read_threshold(
    "DISPERSER_MIN_SPEEDUP_HEAVY_NON_STRICT",
    default_speed_floor("heavy_non_strict", os_name, profile_name)
  )
  heavy_strict_min_legacy_sec <- read_threshold("DISPERSER_MIN_HEAVY_LEGACY_SEC_STRICT", 5)
  min_median <- read_threshold("DISPERSER_MIN_SPEEDUP_MEDIAN", default_speed_floor("median", os_name, profile_name))

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

  check_userflow_floor <- function(source_pattern,
                                   floor_all,
                                   floor_expected_fast,
                                   floor_non_extract,
                                   expected_fast_min_rows) {
    sub <- summary_dt[grepl(source_pattern, source_file, fixed = TRUE)]
    if (nrow(sub) == 0) {
      return("- User-flow benchmark: no rows (skipped)")
    }

    if (!("expected_fast_extract" %in% names(sub))) {
      min_seen <- suppressWarnings(min(sub$speedup_x, na.rm = TRUE))
      verify_expect(
        is.finite(min_seen) && min_seen >= floor_all,
        paste0(
          "User-flow benchmark minimum speedup below floor. ",
          "Required >= ", sprintf("%.3f", floor_all),
          ", observed ", sprintf("%.3f", min_seen), "."
        )
      )
      return(sprintf("- User-flow benchmark: min speedup %.3fx (floor %.2fx)", min_seen, floor_all))
    }

    sub[, expected_fast_extract := as.logical(expected_fast_extract)]
    has_rows <- "rows_per_file" %in% names(sub)
    if (has_rows) {
      sub[, rows_per_file := suppressWarnings(as.numeric(rows_per_file))]
      sub[, expected_fast_rows_ok := is.finite(rows_per_file) & rows_per_file >= expected_fast_min_rows]
      expected_dt <- sub[!is.na(expected_fast_extract) & expected_fast_extract & expected_fast_rows_ok]
      fallback_dt <- sub[is.na(expected_fast_extract) | !expected_fast_extract | !expected_fast_rows_ok]
    } else {
      expected_dt <- sub[!is.na(expected_fast_extract) & expected_fast_extract]
      fallback_dt <- sub[is.na(expected_fast_extract) | !expected_fast_extract]
    }
    lines <- character(0)

    if (nrow(expected_dt) > 0) {
      min_expected <- suppressWarnings(min(expected_dt$speedup_x, na.rm = TRUE))
      verify_expect(
        is.finite(min_expected) && min_expected >= floor_expected_fast,
        paste0(
          "User-flow benchmark (expected fast-extract scenarios",
          if (has_rows) paste0(", rows_per_file >= ", expected_fast_min_rows) else "",
          ") minimum speedup below floor. ",
          "Required >= ", sprintf("%.3f", floor_expected_fast),
          ", observed ", sprintf("%.3f", min_expected), "."
        )
      )
      lines <- c(
        lines,
        sprintf(
          "- User-flow (expected fast-extract%s): min speedup %.3fx (floor %.2fx)",
          if (has_rows) paste0(", rows_per_file >= ", expected_fast_min_rows) else "",
          min_expected,
          floor_expected_fast
        )
      )
    }

    if (nrow(fallback_dt) > 0) {
      min_fallback <- suppressWarnings(min(fallback_dt$speedup_x, na.rm = TRUE))
      verify_expect(
        is.finite(min_fallback) && min_fallback >= floor_non_extract,
        paste0(
          "User-flow benchmark (fallback scenarios) minimum speedup below floor. ",
          "Required >= ", sprintf("%.3f", floor_non_extract),
          ", observed ", sprintf("%.3f", min_fallback), "."
        )
      )
      lines <- c(
        lines,
        sprintf(
          "- User-flow (fallback scenarios): min speedup %.3fx (floor %.2fx)",
          min_fallback,
          floor_non_extract
        )
      )
    }

    if (length(lines) == 0) {
      min_seen <- suppressWarnings(min(sub$speedup_x, na.rm = TRUE))
      lines <- sprintf("- User-flow benchmark: min speedup %.3fx (floor %.2fx)", min_seen, floor_all)
    }
    lines
  }

  check_heavy_floor <- function(source_pattern,
                                floor_strict,
                                floor_non_strict,
                                strict_min_legacy_sec) {
    sub <- summary_dt[grepl(source_pattern, source_file, fixed = TRUE)]
    if (nrow(sub) == 0) {
      return("- Heavy benchmark: no rows (skipped)")
    }

    if (!("legacy_elapsed_sec" %in% names(sub))) {
      min_seen <- suppressWarnings(min(sub$speedup_x, na.rm = TRUE))
      verify_expect(
        is.finite(min_seen) && min_seen >= floor_strict,
        paste0(
          "Heavy benchmark minimum speedup below floor. ",
          "Required >= ", sprintf("%.3f", floor_strict),
          ", observed ", sprintf("%.3f", min_seen), "."
        )
      )
      return(sprintf("- Heavy benchmark: min speedup %.3fx (floor %.2fx)", min_seen, floor_strict))
    }

    sub[, legacy_elapsed_sec := suppressWarnings(as.numeric(legacy_elapsed_sec))]
    strict_dt <- sub[is.finite(legacy_elapsed_sec) & legacy_elapsed_sec >= strict_min_legacy_sec]
    non_strict_dt <- sub[is.na(legacy_elapsed_sec) | !is.finite(legacy_elapsed_sec) | legacy_elapsed_sec < strict_min_legacy_sec]

    lines <- character(0)
    if (nrow(strict_dt) > 0) {
      min_strict <- suppressWarnings(min(strict_dt$speedup_x, na.rm = TRUE))
      verify_expect(
        is.finite(min_strict) && min_strict >= floor_strict,
        paste0(
          "Heavy benchmark (legacy_elapsed_sec >= ", sprintf("%.1f", strict_min_legacy_sec),
          ") minimum speedup below floor. ",
          "Required >= ", sprintf("%.3f", floor_strict),
          ", observed ", sprintf("%.3f", min_strict), "."
        )
      )
      lines <- c(
        lines,
        sprintf(
          "- Heavy (legacy_elapsed_sec >= %.1fs): min speedup %.3fx (floor %.2fx)",
          strict_min_legacy_sec,
          min_strict,
          floor_strict
        )
      )
    }

    if (nrow(non_strict_dt) > 0) {
      min_non_strict <- suppressWarnings(min(non_strict_dt$speedup_x, na.rm = TRUE))
      verify_expect(
        is.finite(min_non_strict) && min_non_strict >= floor_non_strict,
        paste0(
          "Heavy benchmark (legacy_elapsed_sec < ", sprintf("%.1f", strict_min_legacy_sec),
          ") minimum speedup below floor. ",
          "Required >= ", sprintf("%.3f", floor_non_strict),
          ", observed ", sprintf("%.3f", min_non_strict), "."
        )
      )
      lines <- c(
        lines,
        sprintf(
          "- Heavy (legacy_elapsed_sec < %.1fs): min speedup %.3fx (floor %.2fx)",
          strict_min_legacy_sec,
          min_non_strict,
          floor_non_strict
        )
      )
    }

    if (length(lines) == 0) {
      min_seen <- suppressWarnings(min(sub$speedup_x, na.rm = TRUE))
      lines <- sprintf("- Heavy benchmark: min speedup %.3fx (floor %.2fx)", min_seen, floor_strict)
    }
    lines
  }

  checks <- c(
    check_source_floor("linking_engine_benchmark.csv", "Engine benchmark", min_engine),
    check_userflow_floor(
      "userflow_link_all_units_",
      floor_all = min_userflow,
      floor_expected_fast = min_userflow_expected_fast,
      floor_non_extract = min_userflow_non_extract,
      expected_fast_min_rows = min_userflow_expected_fast_rows
    ),
    check_heavy_floor(
      "heavy_user_flow_link_all_units.csv",
      floor_strict = min_heavy,
      floor_non_strict = min_heavy_non_strict,
      strict_min_legacy_sec = heavy_strict_min_legacy_sec
    )
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

  os_vals <- if ("os" %in% names(summary_dt)) unique(summary_dt$os) else "unknown"
  os_vals <- os_vals[!is.na(os_vals)]
  summary_lines <- c(
    "## Cross-platform perf gate",
    paste0(
      "- Rows checked: ", nrow(summary_dt),
      " | OS: ", paste(os_vals, collapse = ", "),
      " | Profile: ", profile_name
    ),
    checks,
    paste0("- Median speedup: ", sprintf("%.3fx", median_speedup), " (floor ", sprintf("%.2fx", min_median), ")")
  )
  write_job_summary(summary_lines)
  message(paste(summary_lines, collapse = "\n"))
  invisible(summary_dt)
})

verify_stop_on_failures("10_perf_ci_assertions")
