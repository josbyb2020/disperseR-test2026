#' Run the dispersion model in parallel
#'
#' @description Runs HYSPLIT dispersion simulations in parallel across multiple
#' emission sources/times. Automatically uses appropriate parallelization method
#' based on the operating system (mclapply on Unix/macOS, parLapply on Windows).
#'
#' @param input.refs A data.table with columns: ID (character), uID (character),
#'   Latitude (numeric), Longitude (numeric), Height (numeric), start_day (Date),
#'   start_hour (numeric), duration_emiss_hours (numeric), duration_run_hours (numeric).
#' @param pbl.height Monthly mean planetary boundary layer heights
#' @param species Species type: 'so2' (default) or 'so4p' (particulate sulfate)
#' @param proc_dir Directory for temporary files (from create_dirs())
#' @param hysp_dir Directory containing HYSPLIT output files (defaults to
#'   create_dirs()).
#' @param meteo_dir Directory containing meteorological input files (defaults to
#'   create_dirs()).
#' @param overwrite Overwrite existing output files? Default FALSE
#' @param npart Number of air parcels tracked by HYSPLIT. Default 100
#' @param mc.cores Number of cores for parallel computation. Default detectCores().
#'   On Windows, a socket cluster is used instead of forking.
#' @param keep.hysplit.files Keep HYSPLIT run files? Default FALSE
#'
#' @return List of results from each dispersion run
#' @export
#' @importFrom parallel detectCores mclapply makeCluster stopCluster clusterExport parLapply
run_disperser_parallel <- function(input.refs = NULL,
  pbl.height = NULL,
  species = 'so2',
  proc_dir = NULL,
  hysp_dir = NULL,
  meteo_dir = NULL,
  overwrite = FALSE,
  npart = 100,
  mc.cores = parallel::detectCores(),
  keep.hysplit.files = FALSE) {

  if (is.null(input.refs) || nrow(input.refs) == 0) {
    stop("input.refs must be a non-empty data.table")
  }
  
  if (is.null(proc_dir)) {
    stop("proc_dir must be specified")
  }

  # Resolve directory paths (create_dirs() stores these in the caller's .GlobalEnv)
  if (is.null(hysp_dir)) {
    hysp_dir <- get0("hysp_dir", envir = .GlobalEnv, ifnotfound = NULL)
  }
  if (is.null(meteo_dir)) {
    meteo_dir <- get0("meteo_dir", envir = .GlobalEnv, ifnotfound = NULL)
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

  run_sample <- seq_len(nrow(input.refs))
  
  # Detect OS and choose parallelization strategy
 is_windows <- .Platform$OS.type == "windows"
  
  if (mc.cores == 1 || length(run_sample) == 1) {
    # Sequential execution
    results <- lapply(
      X = run_sample,
      FUN = run_fac,
      input.refs = input.refs,
      pbl.height = pbl.height,
      species = species,
      proc_dir = proc_dir,
      hysp_dir = hysp_dir,
      meteo_dir = meteo_dir,
      overwrite = overwrite,
      npart = npart,
      keep.hysplit.files = keep.hysplit.files
    )
  } else if (is_windows) {
    # Windows: use socket cluster with parLapply
    message(sprintf("Windows detected: using socket cluster with %d workers", mc.cores))
    
    cl <- parallel::makeCluster(mc.cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Export required variables and packages to workers
    parallel::clusterExport(cl, c(
      "input.refs", "pbl.height", "species", "proc_dir",
      "hysp_dir", "meteo_dir",
      "overwrite", "npart", "keep.hysplit.files", "run_fac"
    ), envir = environment())
    
    # Load disperseR on each worker
    parallel::clusterEvalQ(cl, {
      library(disperseR)
      library(data.table)
      library(magrittr)
    })
    
    results <- parallel::parLapply(
      cl = cl,
      X = run_sample,
      fun = function(x) {
        run_fac(
          x = x,
          input.refs = input.refs,
          pbl.height = pbl.height,
          species = species,
          proc_dir = proc_dir,
          hysp_dir = hysp_dir,
          meteo_dir = meteo_dir,
          overwrite = overwrite,
          npart = npart,
          keep.hysplit.files = keep.hysplit.files
        )
      }
    )
  } else {
    # Unix/macOS: use mclapply (fork-based)
    results <- parallel::mclapply(
      X = run_sample,
      FUN = run_fac,
      input.refs = input.refs,
      pbl.height = pbl.height,
      species = species,
      proc_dir = proc_dir,
      hysp_dir = hysp_dir,
      meteo_dir = meteo_dir,
      overwrite = overwrite,
      npart = npart,
      keep.hysplit.files = keep.hysplit.files,
      mc.cores = mc.cores
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
  meteo_dir) {

  subset <- input.refs[x]
  print(subset)

  ## function to negate
  '%ni%' <- function(x, y) {
    return(!('%in%'(x, y)))
  }

  #########################################################################################################
  ## Define species parameters

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

  #########################################################################################################
  ## subset the data using the indexes provided.
  print(paste0(
    'Date: ',
    format(subset$start_day, format = "%Y-%m-%d"),
    ', Hour: ',
    subset$start_hour
  ))

  if (is.na(subset$Height)) {
    stop("Check to make sure your Height is defined in the run_ref_tab!")
  }

  #########################################################################################################
  ## Check if Height parameter in unit is NA

  # create sharded directory structure
  hysp_dir_yr <- file.path(hysp_dir, subset$year)
  hysp_dir_mo <- file.path( hysp_dir_yr,
                            formatC(
                              month( subset$start_day),
                              width = 2, flag = '0'))
  dir.create( hysp_dir_mo, showWarnings = TRUE, recursive = TRUE)

  ## Define output file names
  output_file <- path.expand(file.path(
    hysp_dir_mo,
    paste0(
      "hyspdisp_",
      subset$ID,
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
  message(paste("output file", output_file))


  ## Initial output data.table
  out <-
    paste(
      "Partial trimmed parcel locations (below height 0 and the highest PBL height) already exist at",
      output_file
    )

  ## Check if output parcel locations file already exists
  tmp.exists <- file.exists( file.path(output_file))

  if (!tmp.exists | overwrite == TRUE) {
    message("Defining HYSPLIT model parameters and running the model.")

    ## Create run directory
    run_dir <- file.path(proc_dir, paste0(subset$ID, '_', paste(subset[, .(ID, start_day, start_hour)], collapse = '_')))

    ## preemptively remove if run_dir already exists, then create
    unlink(run_dir, recursive = TRUE)
    dir.create(run_dir, showWarnings = FALSE)

    ## Define the dispersion model
    dispersion_model <-
      disperseR::create_disp_model() %>%
      disperseR::add_emissions(
        rate = 1,
        duration = subset$duration_emiss_hours,
        start_day = as.character(subset$start_day),
        start_hour = subset$start_hour
      ) %>%
      disperseR::add_species(
        name = species_param$name,
        pdiam = species_param$pdiam,
        density = 0,
        shape_factor = 0,
        ddep_vel = species_param$ddep_vel
      ) %>%
      disperseR::add_grid(range = c(0.5, 0.5),
        division = c(0.1, 0.1)) %>%
      disperseR::add_params(
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
      disperseR::run_model(npart = npart, run.dir = run_dir)


    ## Extract output from the dispersion model
    dispersion_df <- dispersion_model %>% get_output_df() %>% data.table()

    ## trim particles if they go below zero
    disp_df <- trim_zero(dispersion_df)

    ## Add parcel date and time
    disp_df$Pdate <- subset$start_day + disp_df$hour / 24

    # trims particles that are above the global max boundary value
    disp_df_trim <- disp_df[height <= 2665]

    ## Save R data frame
    save.vars <- c('lon', 'lat', 'height', 'Pdate', 'hour')
    partial_trimmed_parcel_locs <-
      disp_df_trim[, save.vars, with = FALSE]
    write.fst(partial_trimmed_parcel_locs, output_file)
    out <-
      paste(
        "Partial trimmed parcel locations (below height 0 and the highest PBL height) written to",
        output_file
      )

    ## Erase run files
    if (!keep.hysplit.files)
      unlink(run_dir, recursive = TRUE)
  }


  return(out)
}
