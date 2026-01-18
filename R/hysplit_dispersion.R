#' Run HYSPLIT dispersion model
#'
#' @description Executes a single HYSPLIT dispersion simulation. This is a
#' modified version of SplitR's dispersion function, adapted for disperseR
#' workflows.
#'
#' @param lat Numeric. Source latitude in decimal degrees. Default 49.263.
#' @param lon Numeric. Source longitude in decimal degrees. Default -123.250.
#' @param height Numeric. Source height in meters above ground. Default 50.
#' @param duration Numeric. Simulation duration in hours. Default 24.
#' @param start_day Character, Date, or POSIXt. Start date in "YYYY-MM-DD" format.
#' @param start_hour Numeric. Start hour (0-23). Default 0.
#' @param direction Character. "forward" or "backward". Default "forward".
#' @param met_type Character. Meteorology type: "reanalysis" or "gdas1".
#' @param met_dir Character. Path to meteorology files. Defaults to working directory.
#' @param vert_motion Numeric. Vertical motion method (0-4). Default 0.
#' @param model_height Numeric. Top of model domain in meters. Default 20000.
#' @param particle_num Numeric. Number of particles released. Default 2500.
#' @param particle_max Numeric. Maximum particles tracked. Default 10000.
#' @param emissions Data frame. Emission parameters (see add_emissions).
#' @param species Data frame. Species parameters (see add_species).
#' @param grids Data frame. Grid parameters (see add_grid).
#' @param return_disp_df Logical. Return dispersion data frame? Default TRUE.
#' @param write_disp_CSV Logical. Write dispersion CSV to run_dir? Default TRUE.
#' @param disp_name Character. Optional name prefix for output files.
#' @param run_dir Character. Working directory for HYSPLIT output. Required.
#'   Supports ~ expansion. Created if it doesn't exist.
#'
#' @return If return_disp_df is TRUE, returns a data frame with dispersion results.
#'   Otherwise returns invisibly.
#'
#' @details
#' Requires the SplitR package for HYSPLIT binaries. Install with:
#' `remotes::install_github("rich-iannone/SplitR")`
#'
#' Meteorology files are downloaded automatically if not present in met_dir.
#' Reanalysis files are ~120 MB each.
#'
#' @examples
#' \dontrun{
#' # Basic dispersion run
#' result <- hysplit_dispersion(
#'   lat = 39.9,
#'   lon = -75.1,
#'   height = 100,
#'   start_day = "2005-06-15",
#'   emissions = my_emissions,
#'   species = my_species,
#'   grids = my_grids,
#'   run_dir = "~/disperseR_output/run1"
#' )
#' }
#'
#' @seealso [add_emissions()], [add_species()], [add_grid()], [run_model()]
#' @export
hysplit_dispersion <- function(lat = 49.263,
  lon = -123.250,
  height = 50,
  duration = 24,
  start_day = "2015-07-01",
  start_hour = 0,
  direction = "forward",
  met_type = "reanalysis",
  met_dir = NULL,
  vert_motion = 0,
  model_height = 20000,
  particle_num = 2500,
  particle_max = 10000,
  emissions,
  species,
  grids,
  return_disp_df = TRUE,
  write_disp_CSV = TRUE,
  disp_name = NULL,
  run_dir) {

  if (!requireNamespace("SplitR", quietly = TRUE)) {
    stop(
      "HYSPLIT dispersion requires the 'SplitR' package.\n",
      "Install it with: remotes::install_github('rich-iannone/SplitR')",
      call. = FALSE
    )
  }

  # Validate and normalize run_dir
 if (missing(run_dir) || is.null(run_dir) || !nzchar(run_dir)) {
    stop("run_dir must be specified (working directory for HYSPLIT output)",
         call. = FALSE)
  }
  run_dir <- path.expand(run_dir)
  if (!dir.exists(run_dir)) {
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(run_dir)) {
      stop("run_dir '", run_dir, "' does not exist and could not be created",
           call. = FALSE)
    }
  }

  # Normalize met_dir
  if (is.null(met_dir)) met_dir <- getwd()
  met_dir <- path.expand(met_dir)

  # Coerce start_day to character if Date or POSIXct
  if (inherits(start_day, c("Date", "POSIXt"))) {
    start_day <- format(start_day, "%Y-%m-%d")
  }
  
 # Validate start_day format (guard against NA)
  if (length(start_day) != 1 ||
      is.na(start_day) ||
      !is.character(start_day) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", start_day)) {
    stop(
      "start_day must be a single date in 'YYYY-MM-DD' format ",
      "(character, Date, or POSIXt)",
      call. = FALSE
    )
  }
  
  run_type <- "day"
  run_day <- start_day

  # If SETUP.CFG or ASCDATA.CFG do not exist in run_dir,
  # write default versions of those config files
  if (!("SETUP.CFG" %in% list.files(run_dir)) ||
      !("ASCDATA.CFG" %in% list.files(run_dir))) {
    disperseR::hysplit_config_init(run_dir)
  }

  # Set number of particles to 1 in the SETUP.CFG file
  setup.cfg <- readLines(paste0(run_dir, "/SETUP.CFG"))

  setup.cfg <- gsub(" numpar = ([0-9]*),",
    paste0(" numpar = ", particle_num, ","),
    setup.cfg)

  setup.cfg <- gsub(" maxpar = ([0-9]*),",
    paste0(" maxpar = ", particle_max, ","),
    setup.cfg)

  writeLines(setup.cfg, paste0(run_dir, "/SETUP.CFG"))

  rm(setup.cfg)

  # Make a vector list of run days in POSIXct format
  run_day <- as.POSIXct(run_day, origin = "1970-01-01", tz = "UTC")

  # Define starting time parameters
  start_year_GMT <- substr(as.character(year(run_day)), 3, 4)

  start_month_GMT <- formatC(as.numeric(month(run_day)),
    width = 2, format = "d", flag = "0")

  start_day_GMT <- formatC(as.numeric(lubridate::day(run_day)),
    width = 2, format = "d", flag = "0")

  # Format `start_hour` if given as a numeric value
  if (class(start_hour) == "numeric") {
    start_hour <- formatC(sort(start_hour), width = 2, flag = 0)
  }

  # Determine the start time of the model run
  start_time_GMT <-
    lubridate::ymd_hms(paste0(ifelse(start_year_GMT > 50,
      paste0("19",
        start_year_GMT),
      start_year_GMT), "-",
      start_month_GMT, "-",
      start_day_GMT, " ",
      start_hour, ":00:00"))

  # Determine the end time of the model run
  end_time_GMT <- as.POSIXct(ifelse(direction == "backward",
    start_time_GMT - (duration * 3600),
    start_time_GMT + (duration * 3600)),
    origin = "1970-01-01",
    tz = "UTC")

  # Determine whether the start year is a leap year
  leap_year <- lubridate::leap_year(lubridate::ymd(paste0(start_year_GMT,
    "-",
    start_month_GMT, "-",
    start_day_GMT)))

  # Determine whether the beginning and end of the
  # current run crosses over a calendar year
  number_of_calendar_years <- ifelse(year(start_time_GMT) ==  year(end_time_GMT), 1, 2)

  # Determine whether the beginning and end of
  # the current run crosses over a calendar month
  number_of_calendar_months <- ifelse(month(start_time_GMT) == month(end_time_GMT), 1, 2)

  #Divide different requirements for met files
  #    into different cases

  # Set the different cases to FALSE by default
  case_within_month <- FALSE
  case_over_year <- FALSE
  case_over_month <- FALSE

  # Determine which of the three cases is true
  if (number_of_calendar_years == 1 &
      number_of_calendar_months == 1) {
    case_within_month <- TRUE
  } else if (number_of_calendar_years > 1) {
    case_over_year <- TRUE
  } else if (number_of_calendar_months > 1) {
    case_over_month <- TRUE
  } else { NULL }

  #Get vector lists of met files applicable to
  #    run from GDAS 1-degree dataset

  # Trap leap-year condition of missing .w5 met
  # file for February in a '0' list value
  if (case_within_month &
      met_type == "gdas1") met <-
    c(paste0(
      "gdas1.",
      substr(tolower(format(start_time_GMT,
        "%B")), 1, 3),
      substr(year(start_time_GMT), 3, 4), ".w1"),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT,
          "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w2"),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT,
          "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w3"),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT,
          "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w4"),
      ifelse(
        month(start_time_GMT) == 2 &
          leap_year, 0,
        paste0("gdas1.",
          substr(tolower(format(start_time_GMT,
            "%B")), 1, 3),
          substr(year(start_time_GMT), 3, 4),
          ".w5")))

  if (case_over_year &
      met_type == "gdas1") met <-
    c(paste0(
      "gdas1.dec",
      substr(year(end_time_GMT), 3, 4), ".w3"),
      paste0(
        "gdas1.dec",
        substr(year(end_time_GMT), 3, 4), ".w4"),
      paste0(
        "gdas1.dec",
        substr(year(end_time_GMT), 3, 4), ".w5"),
      paste0(
        "gdas1.jan",
        substr(year(start_time_GMT), 3, 4), ".w1"),
      paste0(
        "gdas1.jan",
        substr(year(start_time_GMT), 3, 4), ".w2"),
      paste0(
        "gdas1.jan",
        substr(year(start_time_GMT), 3, 4), ".w3"))

  if (case_over_month == TRUE & met_type == "gdas1") {
    met <- c(paste0("gdas1.",
      substr(tolower(format(end_time_GMT,"%B")), 1, 3),
      substr(year(end_time_GMT), 3, 4), ".w3"),
      paste0(
        "gdas1.",
        substr(tolower(format(end_time_GMT,
          "%B")), 1, 3),
        substr(year(end_time_GMT), 3, 4), ".w4"),
      ifelse(
        month(end_time_GMT) == 2 &
          leap_year == TRUE, 0,
        paste0(
          "gdas1.",
          substr(tolower(format(end_time_GMT,
            "%B")), 1, 3),
          substr(year(end_time_GMT), 3, 4), ".w5")),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT,
          "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w1"),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT,
          "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w2"),
      paste0(
        "gdas1.",
        substr(tolower(format(start_time_GMT,
          "%B")), 1, 3),
        substr(year(start_time_GMT), 3, 4), ".w3"))
  }

  # Get vector lists of met files applicable to run
  # from the NCEP/NCAR reanalysis dataset
  if (met_type == "reanalysis") {
    met <- c(paste0(
      "RP",
      ifelse(start_month_GMT == "01",
        year(start_time_GMT) - 1,
        year(start_time_GMT)),
      ifelse(start_month_GMT == "01", "12",
        formatC(month(start_time_GMT) - 1,
          width = 2,
          format = "d",
          flag = "0")),
      ".gbl"),
      paste0(
        "RP",
        year(start_time_GMT),
        start_month_GMT, ".gbl"),
      paste0(
        "RP",
        ifelse(start_month_GMT == "12",
          year(start_time_GMT) + 1,
          year(start_time_GMT)),
        ifelse(start_month_GMT == "12", "01",
          formatC(month(start_time_GMT) + 1,
            width = 2,
            format = "d",
            flag = "0")),
        ".gbl"))
  }

  # Remove list values containing '0' (representing
  # missing .w5 data files for Feb in leap years)
  if(exists("met")) {
    met <- met[!met %in% c(0)]
  }

  # Are the met files available in the
  # selected path?
  met_file_df <- stats::setNames(data.frame(mat.or.vec(nr = length(met),
    nc = 2)),
    nm = c("file", "available"))

  if (any(c("mac", "unix") %in%  disperseR::get_os())) {
    for (k in 1:length(met)) {
      met_file_df[k, 1] <- met[k]
      met_file_df[k, 2] <- as.character( file.exists(paste0(met_dir, "/", met[k])))
    }

    # Write the met file availability to file
    utils::write.table(
      met_file_df,
      file = paste0(met_dir, "/", "met_file_list"),
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      append = FALSE)

    # Download the missing met files
    if (FALSE %in% met_file_df[,2]) {

      files_to_get <-
        subset(met_file_df,
          available == FALSE)[,1]

      if (met_type == "reanalysis") {
        get_met_reanalysis(files = files_to_get, path_met_files = paste0(met_dir, "/"))
      }

      if (met_type == "gdas1") {
        if (!requireNamespace("SplitR", quietly = TRUE)) {
          stop(
            "GDAS1 meteorological data requires the 'SplitR' package.\n",
            "Install it with: devtools::install_github('rich-iannone/SplitR')\n",
            "Or use met_type = 'reanalysis' instead.",
            call. = FALSE
          )
        }
        SplitR::get_met_gdas1(files = files_to_get, path_met_files = paste0(met_dir, "/"))
      }
    }
  }

  if (disperseR::get_os() == "win") {

    for (k in 1:length(met)) {
      met_file_df[k, 1] <- met[k]
      met_file_df[k, 2] <- as.character( file.exists(paste0(met_dir, "\\", met[k])))}

    # Write the met file availability to file
    utils::write.table(met_file_df,
      file = paste0(met_dir, "\\",
        "met_file_list"),
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      append = FALSE)

    # Download the missing met files
    if (FALSE %in% met_file_df[,2]) {

      files_to_get <- subset(met_file_df, available == FALSE)[,1]

      if (met_type == "reanalysis") {
        get_met_reanalysis(files = files_to_get, path_met_files = met_dir)
      }

      if (met_type == "gdas1") {
        if (!requireNamespace("SplitR", quietly = TRUE)) {
          stop(
            "GDAS1 meteorological data requires the 'SplitR' package.\n",
            "Install it with: devtools::install_github('rich-iannone/SplitR')\n",
            "Or use met_type = 'reanalysis' instead.",
            call. = FALSE
          )
        }
        SplitR::get_met_gdas1(files = files_to_get, path_met_files = met_dir)
      }
    }
  }

  # Construct the output filename string
  # for this model run
  output_filename <-
    paste0("--disp",
      ifelse(direction == "backward",
        "(back)", "(forward)"), "-",
      start_year_GMT, "-",
      start_month_GMT, "-",
      start_day_GMT, "-",
      start_hour, "-",
      "lat_", lat, "_",
      "long_",lon, "-",
      "height_",height, "-",
      duration, "h")

  # Write start year, month, day, hour to 'CONTROL'
  cat(start_year_GMT, " ",
    start_month_GMT, " ",
    start_day_GMT, " ",
    start_hour, "\n",
    file = paste0(run_dir, "/", "CONTROL"),
    sep = '', append = FALSE)

  #Write number of starting locations to 'CONTROL'
  cat("1\n",
    file = paste0(run_dir, "/", "CONTROL"),
    sep = '', append = TRUE)

  # Write starting latitude, longitude, and height
  # AGL to 'CONTROL'
  cat(lat, " ",
    lon, " ",
    height, "\n",
    file = paste0(run_dir, "/", "CONTROL"),
    sep = '', append = TRUE)

  # Write direction and number of simulation hours
  # to 'CONTROL'
  cat(ifelse(direction == "backward", "-", ""),
    duration, "\n",
    file = paste0(run_dir, "/", "CONTROL"),
    sep = '', append = TRUE)

  # Write vertical motion option to 'CONTROL'
  cat(vert_motion, "\n",
    file = paste0(run_dir, "/", "CONTROL"),
    sep = '', append = TRUE)

  # Write top of model domain in meters to 'CONTROL'
  cat(model_height, "\n",
    file = paste0(run_dir, "/", "CONTROL"),
    sep = '', append = TRUE)

  # Write number of met files used to 'CONTROL'
  cat(length(met), "\n",
    file = paste0(run_dir, "/", "CONTROL"),
    sep = '', append = TRUE)

  # Write met file paths to 'CONTROL'
  for (i in 1:length(met)) {
    cat(met_dir, "/\n", met[i], "\n",
      file = paste0(run_dir, "/", "CONTROL"),
      sep = '', append = TRUE)
  }

  # Write emissions blocks to 'CONTROL'
  for (i in 1:nrow(emissions)) {
    cat(c(nrow(emissions), "\n",
      substr(emissions[i, 1], 1, 4), "\n",
      emissions[i, 2], "\n",
      emissions[i, 3], "\n",
      paste0(paste(unlist(strsplit(substr(emissions[i, 4], 3, 10), "-")), collapse = " "), " ",
        formatC(emissions[i, 5], width = 2,format = "d", flag = "0"), " 00")), "\n",
      file = paste0(run_dir, "/", "CONTROL"),
      sep = "", append = TRUE)
  }

  # Get vector text elements through reading
  # selected elements from the 'grids' data frame
  if (any(is.na(grids$lat),
    is.na(grids$lon))) {

    grids$lat <- lat
    grids$lon <- lon
  }

  if (any(is.na(grids$duration),
    is.na(grids$start_day),
    is.na(grids$start_hour),
    is.na(grids$end_day),
    is.na(grids$end_hour))) {

    grids$duration <- duration
    grids$start_day <- start_day
    grids$start_hour <- start_hour

    grids$end_day <-
      format(lubridate::ymd_h(paste(grids$start_day, grids$start_hour)) +
          (duration * 3600),
        "%Y-%m-%d")

    grids$end_hour <-
      as.numeric(
        format(lubridate::ymd_h(paste(grids$start_day, grids$start_hour)) +
            (duration * 3600),
          "%H")
      )
  }

  if (grids[1, 14] == "avg") {
    sampling_type <- "0"
  } else if (grids[1, 14] == "snapshot") {
    sampling_type <- "1"
  } else if (grids[1, 14] == "max") {
    sampling_type <- "2"
  } else {
    sampling_type <- "0"
  }

  grids_text <-
    c("1",
      paste(grids[1, 2],
        grids[1, 3]),
      paste(grids[1, 6],
        grids[1, 7]),
      paste(grids[1, 4],
        grids[1, 5]),
      paste0(run_dir, "/"),
      grids[1,1],
      "1", "50",
      paste0(paste(unlist(strsplit(substr(grids[i, 9], 3, 10), "-")),
        collapse = " "),
        " ",
        formatC(grids[1, 10],
          width = 2,
          format = "d",
          flag = "0"),
        " 00"),
      paste0(paste(unlist(strsplit(substr(grids[i, 11], 3, 10), "-")),
        collapse = " "),
        " ",
        formatC(grids[1, 12],
          width = 2,
          format = "d",
          flag = "0"),
        " 00"),
      paste0(sampling_type, " ",
        formatC(grids[1, 15],
          width = 2,
          format = "d",
          flag = "0"),
        " 00"))

  # Get vector text indices that contain the short
  # name(s) of the grid(s)
  gridnames_indices <-
    seq(from = 1 + 5,
      to = length(grids_text) - 5,
      by = 10)

  # Combine short grid name string with longer
  # output_filename string
  for (i in 1:((length(grids_text) - 1)/10)) {
    grids_text[gridnames_indices[i]] <-
      paste0(grids_text[gridnames_indices[i]],
        output_filename)
  }

  # Write grid blocks to 'CONTROL'
  for (i in 1:length(grids_text)) {
    cat(grids_text[i], "\n",
      file = paste0(run_dir, "/", "CONTROL"),
      sep = '', append = TRUE)
  }



  # Write species blocks to 'CONTROL'
  for (i in 1:nrow(species)) {
    cat(c(nrow(species), "\n",
      paste(species[1, 2],
        species[1, 3],
        species[1, 4]), "\n",
      paste(species[1, 5],
        species[1, 6],
        species[1, 7],
        species[1, 8],
        species[1, 9]), "\n",
      paste(species[1, 10],
        species[1, 11],
        species[1, 12]), "\n",
      species[1, 13], "\n",
      species[1, 14]), "\n",
      file = paste0(run_dir, "/", "CONTROL"),
      sep = "", append = TRUE)
  }

  # CONTROL file is now complete and in the
  # working directory; execute the model run

  # Execute HYSPLIT binary (paths quoted for space safety)
  if (disperseR::get_os() == "mac") {
    binary_path <- system.file("osx/hycs_std", package = "SplitR")
    if (binary_path == "") {
      stop("HYSPLIT binary not found. Ensure SplitR is installed: ",
           "remotes::install_github('rich-iannone/SplitR')",
           call. = FALSE)
    }
    exit_status <- system(paste0("(cd ", shQuote(run_dir), " && ", shQuote(binary_path),
      " >> /dev/null 2>&1)"))
    if (exit_status != 0) {
      stop("HYSPLIT execution failed with exit status ", exit_status, ". ",
           "Check CONTROL file and run_dir '", run_dir, "' for errors.",
           call. = FALSE)
    }
  }

  if (disperseR::get_os() == "unix") {
    binary_path <- system.file("linux-amd64/hycs_std", package = "SplitR")
    if (binary_path == "") {
      stop("HYSPLIT binary not found. Ensure SplitR is installed: ",
           "remotes::install_github('rich-iannone/SplitR')",
           call. = FALSE)
    }
    exit_status <- system(paste0("(cd ", shQuote(run_dir), " && ", shQuote(binary_path),
      " >> /dev/null 2>&1)"))
    if (exit_status != 0) {
      stop("HYSPLIT execution failed with exit status ", exit_status, ". ",
           "Check CONTROL file and run_dir '", run_dir, "' for errors.",
           call. = FALSE)
    }
  }

  if (disperseR::get_os() == "win") {
    binary_path <- system.file("win/hycs_std.exe", package = "SplitR")
    if (binary_path == "") {
      stop("HYSPLIT binary not found. Ensure SplitR is installed: ",
           "remotes::install_github('rich-iannone/SplitR')",
           call. = FALSE)
    }
    exit_status <- shell(paste0("cd /d \"", run_dir, "\" && \"", binary_path, "\""),
                        intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (exit_status != 0) {
      stop("HYSPLIT execution failed with exit status ", exit_status, ". ",
           "Check CONTROL file and run_dir '", run_dir, "' for errors.",
           call. = FALSE)
    }
  }


  # Extract the particle positions at every hour
  if (disperseR::get_os() == "mac") {
    parhplot_path <- system.file("osx/parhplot", package = "SplitR")
    if (parhplot_path == "") {
      stop("parhplot binary not found. Ensure SplitR is installed: ",
           "remotes::install_github('rich-iannone/SplitR')",
           call. = FALSE)
    }
    exit_status <- system(paste0("(cd ", shQuote(run_dir), " && ", shQuote(parhplot_path),
      " -iPARDUMP -a1)"))
    if (exit_status != 0) {
      stop("parhplot execution failed with exit status ", exit_status, ". ",
           "HYSPLIT may not have produced PARDUMP file in '", run_dir, "'.",
           call. = FALSE)
    }
  }

  if (disperseR::get_os() == "unix") {
    parhplot_path <- system.file("linux-amd64/parhplot", package = "SplitR")
    if (parhplot_path == "") {
      stop("parhplot binary not found. Ensure SplitR is installed: ",
           "remotes::install_github('rich-iannone/SplitR')",
           call. = FALSE)
    }
    exit_status <- system(paste0("(cd ", shQuote(run_dir), " && ", shQuote(parhplot_path),
      " -iPARDUMP -a1)"))
    if (exit_status != 0) {
      stop("parhplot execution failed with exit status ", exit_status, ". ",
           "HYSPLIT may not have produced PARDUMP file in '", run_dir, "'.",
           call. = FALSE)
    }
  }

  if (disperseR::get_os() == "win") {
    parhplot_path <- system.file("win/parhplot.exe", package = "SplitR")
    if (parhplot_path == "") {
      stop("parhplot binary not found. Ensure SplitR is installed: ",
           "remotes::install_github('rich-iannone/SplitR')",
           call. = FALSE)
    }
    exit_status <- shell(paste0("cd /d \"", run_dir, "\" && \"", parhplot_path,
      "\" -iPARDUMP -a1"),
      intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (exit_status != 0) {
      stop("parhplot execution failed with exit status ", exit_status, ". ",
           "HYSPLIT may not have produced PARDUMP file in '", run_dir, "'.",
           call. = FALSE)
    }
  }

  # Remove the .att files from run_dir
  if (any(c("mac", "unix") %in% disperseR::get_os())) {
    system(paste0("(cd ", shQuote(run_dir), " && rm -f GIS_part*.att)"))
  }

  if (disperseR::get_os() == "win") {
    shell(paste0("cd /d \"", run_dir, "\" && del /q GIS_part*.att 2>nul"))
  }

  # Remove the postscript plot from run_dir
  if (any(c("mac", "unix") %in% disperseR::get_os())) {
    system(paste0("(cd ", shQuote(run_dir), " && rm -f parhplot.ps)"))
  }

  if (disperseR::get_os() == "win") {
    shell(paste0("cd /d \"", run_dir, "\" && del /q parhplot.ps 2>nul"))
  }
  
  # Rename the TXT files as CSV files
  if (any(c("mac", "unix") %in% disperseR::get_os())) {
    system(paste0(
      "(cd ", shQuote(run_dir),
      " && for f in GIS*.txt; do [ -f \"$f\" ] && mv \"$f\" \"${f%.txt}.csv\"; done)"
    ))
  }

  if (get_os() == "win") {
    temp_file_list <-
      list.files(path = run_dir,
        pattern = "*._ps.txt",
        full.names = TRUE)

    for (i in 1:length(temp_file_list)) {
      temp_lines <- readLines(temp_file_list[i])
      temp_lines <- temp_lines[-(length(temp_lines))]
      utils::write.table(temp_lines,
        file = gsub("txt", "csv",
          temp_file_list[i]),
        col.names = FALSE,
        row.names = FALSE,
        quote = FALSE)
    }
  }

  # # Move the .csv files from the working directory
  # # to the output folder
  # if (any(c("mac", "unix") %in% get_os())) {
  #   if (is.null(disp_name)) {
  #     folder_name <-
  #       paste0("disp--",
  #         format(Sys.time(),
  #           "%Y-%m-%d--%H-%M-%S"))
  #   } else if (!is.null(disp_name)) {
  #     folder_name <-
  #       paste0(disp_name, "--",
  #         format(Sys.time(),
  #           "%Y-%m-%d--%H-%M-%S"))
  #   }
  #
  #   # Perform the movement of all dispersion files
  #   # into a folder residing in the output dir
  #
  #   dir.create(path = paste0(hysp_dir, "/", folder_name))
  #
  #   system(paste0("(cd ", run_dir,
  #     " && mv GIS_part*.csv ",
  #     hysp_dir, "/",
  #     folder_name,
  #     ")"))
  # }
  #
  # if (disperseR::get_os() == "win") {
  #
  #   if (is.null(disp_name)) {
  #     folder_name <-
  #       paste0("disp--",
  #         format(Sys.time(),
  #           "%Y-%m-%d--%H-%M-%S"))
  #   } else if (!is.null(disp_name)) {
  #     folder_name <-
  #       paste0(disp_name, "--",
  #         format(Sys.time(),
  #           "%Y-%m-%d--%H-%M-%S"))
  #   }
  #
  #   # Perform the movement of all dispersion files
  #   # into a folder residing in the output dir
  #   dir.create(path = paste0(hysp_dir, "/",
  #     folder_name))
  #
  #   shell(paste0("(cd \"", run_dir,
  #     "\" && move GIS_part*.csv \"",
  #     hysp_dir, "/",
  #     folder_name,
  #     "\")"))
  # }

  # Write the dispersion data frame to a CSV if
  # it is requested
  if (write_disp_CSV) {
    disp_df <-
      disperseR::dispersion_read(archive_folder = run_dir)
          # paste0(hysp_dir, "/",
          #   folder_name))

    if (any(c("mac", "unix") %in% disperseR::get_os())) {
      utils::write.table(
        disp_df,
        file = file.path(run_dir, #"/", #hysp_dir, "/",
          # folder_name,
          "dispersion.csv"),
        sep = ",",
        row.names = FALSE)
    }

    if (disperseR::get_os() == "win") {
      utils::write.table(
        disp_df,
        file = file.path( run_dir, #hysp_dir, "/",
          # folder_name,
          "dispersion.csv"),
        sep = ",",
        row.names = FALSE)
    }
  }

  # Return a dispersion data frame if it is requested
  if (return_disp_df) {

    disp_df <- dispersion_read(archive_folder = file.path( run_dir)) #file.path(hysp_dir, folder_name))

    invisible(disp_df)
  }
}
