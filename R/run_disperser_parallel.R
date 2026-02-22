#' Compute required reanalysis met files for a set of input refs
#'
#' Replicates the prev/current/next month logic from \code{hysplit_dispersion}
#' across all unique start months in \code{input.refs}.
#'
#' @param input.refs data.table with \code{start_day} (Date) column.
#' @return Character vector of unique required met filenames (e.g. "RP200501.gbl").
#' @keywords internal
.compute_required_met_files <- function(input.refs) {
  start_dates <- as.Date(input.refs$start_day)
  # unique year-months across all runs
  ym <- unique(format(start_dates, "%Y-%m"))
  all_files <- character(0)

  for (ym_str in ym) {
    d <- as.Date(paste0(ym_str, "-01"))
    yr <- as.integer(format(d, "%Y"))
    mo <- as.integer(format(d, "%m"))

    # Previous month
    if (mo == 1L) {
      prev <- paste0("RP", yr - 1L, "12.gbl")
    } else {
      prev <- paste0("RP", yr, formatC(mo - 1L, width = 2, flag = "0"), ".gbl")
    }
    # Current month
    curr <- paste0("RP", yr, formatC(mo, width = 2, flag = "0"), ".gbl")
    # Next month
    if (mo == 12L) {
      nxt <- paste0("RP", yr + 1L, "01.gbl")
    } else {
      nxt <- paste0("RP", yr, formatC(mo + 1L, width = 2, flag = "0"), ".gbl")
    }
    all_files <- c(all_files, prev, curr, nxt)
  }
  unique(all_files)
}

#' Run the dispersion model in parallel
#'
#' @description Runs HYSPLIT dispersion simulations in parallel across multiple
#' emission sources/times. Automatically uses appropriate parallelization method
#' based on the operating system (mclapply on Unix/macOS, parLapplyLB on Windows).
#'
#' @param input.refs A data.table with columns: ID (character), uID (character),
#'   Latitude (numeric), Longitude (numeric), Height (numeric), start_day (Date),
#'   start_hour (numeric), duration_emiss_hours (numeric), duration_run_hours (numeric).
#'   ID values are used in output filenames and must be filesystem-safe across
#'   platforms (avoid `/`, backslash, or `:*?"<>|`).
#' @param pbl.height Monthly mean planetary boundary layer heights
#' @param species Species type: 'so2' (default) or 'so4p' (particulate sulfate)
#' @param proc_dir Directory for temporary files (from create_dirs())
#' @param hysp_dir Directory containing HYSPLIT output files (defaults to
#'   create_dirs()).
#' @param meteo_dir Directory containing meteorological input files (defaults to
#'   create_dirs()).
#' @param overwrite Overwrite existing output files? Default FALSE
#' @param npart Number of air parcels tracked by HYSPLIT. Default 100
#' @param mc.cores Number of cores for parallel computation. Default 2 (CRAN policy).
#'   Set `options(disperseR.mc.cores = parallel::detectCores())` to use all cores.
#'   On Windows, a socket cluster is used instead of forking.
#' @param keep.hysplit.files Keep HYSPLIT run files? Default FALSE
#' @param binary_path Path to HYSPLIT binary (hycs_std). If NULL, uses splitr's bundled binary.
#' @param parhplot_path Path to HYSPLIT parhplot binary. If NULL, uses splitr's bundled binary.
#'
#' @return List of results from each dispersion run
#' @export
#' @importFrom parallel detectCores mclapply makeCluster stopCluster clusterExport parLapplyLB
run_disperser_parallel <- function(input.refs = NULL,
  pbl.height = NULL,
  species = 'so2',
  proc_dir = NULL,
  hysp_dir = NULL,
  meteo_dir = NULL,
  overwrite = FALSE,
  npart = 100,
  mc.cores = getOption("disperseR.mc.cores", 2L),
  keep.hysplit.files = FALSE,
  binary_path = NULL,
  parhplot_path = NULL) {

  if (is.null(input.refs) || nrow(input.refs) == 0) {
    stop("input.refs must be a non-empty data.table")
  }

  input.refs <- data.table::as.data.table(input.refs)
  required_cols <- c(
    "ID", "Latitude", "Longitude", "Height", "start_day", "start_hour",
    "duration_emiss_hours", "duration_run_hours"
  )
  missing_cols <- setdiff(required_cols, names(input.refs))
  if (length(missing_cols) > 0) {
    stop(
      "input.refs is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  input.refs[, ID := as.character(ID)]
  invisible(lapply(input.refs$ID, .disperseR_validate_id_component, arg_name = "input.refs$ID"))

  if (!inherits(input.refs$start_day, "Date")) {
    start_day <- as.Date(input.refs$start_day)
    if (anyNA(start_day)) {
      stop("start_day must be Date or coercible to Date (e.g., '2005-01-02').",
           call. = FALSE)
    }
    input.refs[, start_day := start_day]
  }

  if (!is.numeric(input.refs$start_hour)) {
    input.refs[, start_hour := as.integer(as.character(start_hour))]
    if (anyNA(input.refs$start_hour)) {
      stop("start_hour must be numeric (0-23).", call. = FALSE)
    }
  }
  if (any(input.refs$start_hour < 0 | input.refs$start_hour > 23)) {
    stop("start_hour must be between 0 and 23.", call. = FALSE)
  }

  numeric_cols <- c("Latitude", "Longitude", "Height", "duration_emiss_hours", "duration_run_hours")
  for (col in numeric_cols) {
    if (!is.numeric(input.refs[[col]])) {
      input.refs[, (col) := as.numeric(as.character(get(col)))]
    }
    if (anyNA(input.refs[[col]])) {
      stop(col, " must be numeric and cannot contain NA values.", call. = FALSE)
    }
  }

  if (!"year" %in% names(input.refs)) {
    input.refs[, year := format(start_day, "%Y")]
  } else {
    input.refs[, year := ifelse(is.na(year), format(start_day, "%Y"), as.character(year))]
  }
  
  if (is.null(proc_dir)) {
    stop("proc_dir must be specified")
  }
  if (!dir.exists(proc_dir)) {
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(proc_dir)) {
    stop("proc_dir does not exist and could not be created: ", proc_dir, call. = FALSE)
  }

  # pull dir paths from the cache that create_dirs() set up
  if (is.null(hysp_dir)) {
    hysp_dir <- .disperseR_cache_get("hysp_dir")
  }
  if (is.null(meteo_dir)) {
    meteo_dir <- .disperseR_cache_get("meteo_dir")
  }
  if (is.null(hysp_dir) || !nzchar(hysp_dir)) {
    stop(
      "hysp_dir is not set. Run create_dirs() first or pass hysp_dir explicitly.",
      call. = FALSE
    )
  }
  if (is.null(meteo_dir) || !nzchar(meteo_dir)) {
    stop(
      "meteo_dir is not set. Run create_dirs() first or pass meteo_dir explicitly.",
      call. = FALSE
    )
  }

  # check that all needed met files are on disk before we start anything
  required_met <- .compute_required_met_files(input.refs)
  existing_files <- list.files(meteo_dir)
  met_paths <- file.path(meteo_dir, required_met)
  # zero-size files are useless — treat them as missing
  zero_size <- required_met[required_met %in% existing_files &
                             file.info(met_paths)$size == 0]
  missing_met <- required_met[!required_met %in% existing_files]
  missing_met <- unique(c(missing_met, zero_size))

  if (length(missing_met) > 0) {
    est_mb <- length(missing_met) * 120L
    message(sprintf(
      "Pre-flight check: %d of %d required met file(s) missing (~%d MB):\n  %s",
      length(missing_met), length(required_met), est_mb,
      paste(missing_met, collapse = ", ")
    ))
    # nuke corrupt zero-size files so the re-download doesn't skip them
    for (zf in zero_size) {
      fpath <- file.path(meteo_dir, zf)
      if (file.exists(fpath)) unlink(fpath, force = TRUE)
    }
    if (interactive()) {
      ans <- readline("Download missing met files now? [Y/n] ")
      if (tolower(trimws(ans)) %in% c("n", "no")) {
        stop("Aborting: missing met files. Download them with get_data(data='metfiles', ...).",
             call. = FALSE)
      }
    } else {
      message("Non-interactive session: auto-downloading missing met files...")
    }
    result <- get_met_reanalysis(files = missing_met, path_met_files = meteo_dir)
    still_missing <- missing_met[!missing_met %in% list.files(meteo_dir)]
    if (length(still_missing) > 0) {
      stop("Pre-flight download failed. Still missing: ",
           paste(still_missing, collapse = ", "), call. = FALSE)
    }
    message("Pre-flight download complete: all met files present.")
  } else {
    message(sprintf("Pre-flight check: all %d required met files present.",
                    length(required_met)))
  }

  run_sample <- seq_len(nrow(input.refs))

  # pick parallelization strategy based on OS
  is_windows <- .Platform$OS.type == "windows"
  

  # on windows: kill zombie HYSPLIT processes and warn about antivirus
  if (is_windows) {
    cleanup_hysplit_zombies(verbose = FALSE)
    warn_av_interference(length(run_sample))
  }

  if (is_windows && mc.cores > 1) {
    pkg_path <- system.file(package = "disperseR")
    if (!nzchar(pkg_path)) {
      warning(
        "Windows parallel runs require disperseR to be installed. ",
        "Install the package (not just devtools::load_all) or set mc.cores = 1.",
        call. = FALSE
      )
      mc.cores <- 1
    }
  }

  if (is_windows && mc.cores > 1 && inherits(pbl.height, "SpatRaster")) {
    packed_ok <- TRUE
    pbl.height.packed <- tryCatch(
      terra::wrap(pbl.height, proxy = TRUE),
      error = function(e) {
        packed_ok <<- FALSE
        warning(
          "Could not pack pbl.height for Windows parallel transfer (",
          conditionMessage(e),
          "). Falling back to mc.cores = 1.",
          call. = FALSE
        )
        NULL
      }
    )
    if (packed_ok) {
      pbl.height <- pbl.height.packed
    } else {
      mc.cores <- 1
    }
  }
  
  message(sprintf("Processing %d dispersion runs across %d core(s)...",
                  length(run_sample), mc.cores))
  use_parallel <- mc.cores > 1 && length(run_sample) > 1
  parallel_dt_threads <- getOption("disperseR.parallel.dt.threads", 1L)
  if (!is.numeric(parallel_dt_threads) ||
      length(parallel_dt_threads) != 1 ||
      !is.finite(parallel_dt_threads) ||
      parallel_dt_threads < 1) {
    parallel_dt_threads <- 1L
  }
  parallel_dt_threads <- as.integer(parallel_dt_threads)
  if (use_parallel) {
    old_dt_threads <- data.table::getDTthreads()
    data.table::setDTthreads(parallel_dt_threads)
    on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
  }
  resolve_pbl_height <- local({
    cached <- NULL
    function() {
      if (is.null(cached)) {
        cached <<- if (inherits(pbl.height, "PackedSpatRaster")) {
          tryCatch(
            terra::unwrap(pbl.height),
            error = function(e) {
              stop(
                "Failed to unpack pbl.height for dispersion runs: ",
                conditionMessage(e),
                call. = FALSE
              )
            }
          )
        } else {
          pbl.height
        }
      }
      cached
    }
  })

  if (!use_parallel) {
    # single-core path
    n_total <- length(run_sample)
    results <- lapply(
      X = run_sample,
      FUN = function(i) {
        if (i %% 10 == 0 || i == n_total) {
          message(sprintf("  [%d/%d] runs completed", i, n_total))
        }
        run_fac(
          x = i,
          input.refs = input.refs,
          pbl.height = resolve_pbl_height(),
          species = species,
          proc_dir = proc_dir,
          hysp_dir = hysp_dir,
          meteo_dir = meteo_dir,
          overwrite = overwrite,
          npart = npart,
          keep.hysplit.files = keep.hysplit.files,
          binary_path = binary_path,
          parhplot_path = parhplot_path
        )
      }
    )
  } else if (is_windows) {
    # windows: socket cluster (can't fork on windows)
    message(sprintf("Windows detected: using socket cluster with %d workers", mc.cores))
    
    cl <- parallel::makeCluster(mc.cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # ship everything the workers need
    parallel::clusterExport(cl, c(
      "input.refs", "pbl.height", "species", "proc_dir",
      "hysp_dir", "meteo_dir",
      "overwrite", "npart", "keep.hysplit.files",
      "binary_path", "parhplot_path", "run_fac", "resolve_pbl_height",
      "parallel_dt_threads"
    ), envir = environment())
    
    # load packages on each worker
    parallel::clusterEvalQ(cl, {
      library(disperseR)
      library(data.table)
      library(magrittr)
      data.table::setDTthreads(parallel_dt_threads)
    })
    
    results <- parallel::parLapplyLB(
      cl = cl,
      X = run_sample,
      fun = function(x) {
        run_fac(
          x = x,
          input.refs = input.refs,
          pbl.height = resolve_pbl_height(),
          species = species,
          proc_dir = proc_dir,
          hysp_dir = hysp_dir,
          meteo_dir = meteo_dir,
          overwrite = overwrite,
          npart = npart,
          keep.hysplit.files = keep.hysplit.files,
          binary_path = binary_path,
          parhplot_path = parhplot_path
        )
      }
    )
  } else {
    # unix/mac: fork with mclapply
    results <- parallel::mclapply(
      X = run_sample,
      FUN = run_fac,
      input.refs = input.refs,
      pbl.height = resolve_pbl_height(),
      species = species,
      proc_dir = proc_dir,
      hysp_dir = hysp_dir,
      meteo_dir = meteo_dir,
      overwrite = overwrite,
      npart = npart,
      keep.hysplit.files = keep.hysplit.files,
      binary_path = binary_path,
      parhplot_path = parhplot_path,
      mc.cores = mc.cores,
      mc.preschedule = FALSE
    )
  }
  
  return(results)
}


run_fac <- function(x,
  input.refs,
  pbl.height = NULL,
  species = "so2",
  npart = 100,
  overwrite = FALSE,
  keep.hysplit.files = FALSE,
  proc_dir,
  hysp_dir,
  meteo_dir,
  binary_path = NULL,
  parhplot_path = NULL) {

  subset <- input.refs[x]
  unit_id <- .disperseR_validate_id_component(as.character(subset$ID[[1]]), "input.refs$ID")
  verbose <- isTRUE(getOption("disperseR.verbose", TRUE))
  if (verbose) {
    message("Running ID=", unit_id, " date=", format(subset$start_day, "%Y-%m-%d"),
            " hour=", subset$start_hour)
  }

  # warnings matching these patterns are harmless noise from spatial libs
  benign_patterns <- c(
    "invalid extent",
    "unknown CRS",
    "CRS comment",
    "deprecated",
    "column name",
    "st_crs.*comment",
    "old-style crs",
    "GDAL Message",
    "Column .* lost",
    "attribute variables",
    "already exists",
    "single-line footer"
  )

  captured_warnings <- character(0)

  out <- withCallingHandlers(
    {
      .run_fac_body(
        subset = subset,
        unit_id = unit_id,
        verbose = verbose,
        species = species,
        npart = npart,
        overwrite = overwrite,
        keep.hysplit.files = keep.hysplit.files,
        proc_dir = proc_dir,
        hysp_dir = hysp_dir,
        meteo_dir = meteo_dir,
        binary_path = binary_path,
        parhplot_path = parhplot_path
      )
    },
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # dedupe and print a clean summary instead of 50+ raw warnings
  if (length(captured_warnings) > 0 && verbose) {
    deduped <- unique(captured_warnings)
    counts <- table(captured_warnings)
    is_benign <- vapply(deduped, function(msg) {
      any(vapply(benign_patterns, function(pat) grepl(pat, msg, ignore.case = TRUE),
                 logical(1)))
    }, logical(1))

    benign_msgs <- deduped[is_benign]
    review_msgs <- deduped[!is_benign]

    if (length(benign_msgs) > 0 || length(review_msgs) > 0) {
      message("  -- Warning summary for ID=", unit_id,
              " (", sum(counts), " total, ", length(deduped), " unique) --")
    }
    for (msg in benign_msgs) {
      n <- as.integer(counts[msg])
      message("    [benign] (x", n, ") ", substr(msg, 1, 120))
    }
    for (msg in review_msgs) {
      n <- as.integer(counts[msg])
      message("    [review] (x", n, ") ", substr(msg, 1, 120))
    }
  }

  return(out)
}


# the actual work — pulled out so run_fac() can wrap it in withCallingHandlers
.run_fac_body <- function(subset, unit_id, verbose, species, npart, overwrite,
                           keep.hysplit.files, proc_dir, hysp_dir, meteo_dir,
                           binary_path, parhplot_path) {

  # species parameters

  if (species == 'so2') {
    species_param <-
      data.table(
        name = 'so2',
        pdiam = 0,
        density = 0,
        shape_factor = 0,
        resuspension = 1e-10,
        ddep_vel = 0.002)
  } else if (species %in% c('so4', 'so4p')) {
    # so4p (particulate sulfate)
    species_param <-
      data.table(
        name = 'so4p',
        pdiam = 2.5,
        density = 1,
        shape_factor = 1,
        resuspension = 0,
        ddep_vel = 0.002)
  } else {
    stop("No species or incorrect species defined!")
  }

  # height can't be NA or HYSPLIT will choke
  if (is.na(subset$Height)) {
    stop("Check to make sure your Height is defined in the run_ref_tab!")
  }

  # output goes into hysp_dir/YYYY/MM/
  hysp_dir_yr <- file.path(hysp_dir, subset$year)
  hysp_dir_mo <- file.path( hysp_dir_yr,
                            formatC(
                              month( subset$start_day),
                              width = 2, flag = '0'))
  dir.create( hysp_dir_mo, showWarnings = TRUE, recursive = TRUE)

  output_file <- path.expand(file.path(
    hysp_dir_mo,
    paste0(
      "hyspdisp_",
      unit_id,
      "_",
      subset$start_day,
      "_",
      formatC(
        subset$start_hour,
        width = 2,
        format = "d",
        flag = "0"
      ),
      ".fst"
    )
  ))
  if (verbose) {
    message("output file ", output_file)
  }


  # default message if the file already exists
  out <-
    paste(
      "Partial trimmed parcel locations (below height 0 and the highest PBL height) already exist at",
      output_file
    )

  tmp.exists <- file.exists( file.path(output_file))

  if (!tmp.exists | overwrite == TRUE) {
    message("Defining HYSPLIT model parameters and running the model.")

    run_dir <- file.path(
      proc_dir,
      sprintf(
        "%s_%s_%02d",
        unit_id,
        format(subset$start_day, "%Y-%m-%d"),
        as.integer(subset$start_hour)
      )
    )

    # clean slate — but refuse to nuke anything outside proc_dir
    proc_dir_norm <- normalizePath(proc_dir, winslash = "/", mustWork = TRUE)
    run_dir_norm <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)
    if (!startsWith(run_dir_norm, paste0(proc_dir_norm, "/"))) {
      stop("Refusing to remove run_dir outside proc_dir: ", run_dir, call. = FALSE)
    }
    unlink(run_dir, recursive = TRUE)
    dir.create(run_dir, showWarnings = FALSE)

    # build + run the dispersion model
    dispersion_model <-
      create_disp_model() %>%
      add_emissions(
        rate = 1,
        duration = subset$duration_emiss_hours,
        start_day = as.character(subset$start_day),
        start_hour = subset$start_hour
      ) %>%
      add_species(
        name = species_param$name,
        pdiam = species_param$pdiam,
        density = 0,
        shape_factor = 0,
        ddep_vel = species_param$ddep_vel
      ) %>%
      add_grid(range = c(0.5, 0.5),
        division = c(0.1, 0.1)) %>%
      add_params(
        lat = subset$Latitude,
        lon = subset$Longitude,
        height = subset$Height,
        duration = subset$duration_run_hours,
        start_day = as.character(subset$start_day),
        start_hour = subset$start_hour,
        direction = "forward",
        met_type = "reanalysis",
        met_dir = meteo_dir
      ) %>%
      run_model(npart = npart, run.dir = run_dir,
        binary_path = binary_path, parhplot_path = parhplot_path)


    # extract output, trim bad particles, save to .fst
    dispersion_df <- dispersion_model %>% get_output_df() %>% data.table()
    disp_df <- trim_zero(dispersion_df)
    disp_df$Pdate <- subset$start_day + disp_df$hour / 24
    disp_df_trim <- disp_df[height <= 2665]  # global max PBL cutoff

    save.vars <- c('lon', 'lat', 'height', 'Pdate', 'hour')
    partial_trimmed_parcel_locs <-
      disp_df_trim[, save.vars, with = FALSE]
    write.fst(partial_trimmed_parcel_locs, output_file)
    out <-
      paste(
        "Partial trimmed parcel locations (below height 0 and the highest PBL height) written to",
        output_file
      )

    # clean up temp run files unless user wants to keep them
    if (!keep.hysplit.files)
      unlink(run_dir, recursive = TRUE)
  }

  return(out)
}
