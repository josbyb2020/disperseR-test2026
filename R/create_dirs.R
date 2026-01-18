#' Create project directories for disperseR
#'
#' @description Creates the directory structure required for disperseR analyses.
#'
#' @param location Path where project directories are created. Defaults to
#'   a session-specific temporary directory (CRAN policy). For persistent

#'   storage, provide an explicit path like `"~/disperseR_project"`.
#'
#' @return A named list of directory paths. The paths are also stored in an
#'   internal package cache for use by other functions.
#'
#' @details By default, directories are created in `tempdir()` which is
#'   cleaned up when the R session ends. For persistent projects, pass an
#'   explicit `location` argument.
#'
#' @examples
#' \dontrun{
#' # Temporary session directories (default, CRAN-safe)
#' dirs <- create_dirs()
#'
#' # Persistent project directory
#' dirs <- create_dirs(location = "~/disperseR_project")
#' }
#'
#' @export create_dirs

create_dirs <- function(location = NULL) {

  # Default to tempdir for CRAN compliance (no writing to user home)
  if (is.null(location)) {
    location <- file.path(tempdir(), "disperseR_project")
    message("Using temporary directory: ", location, 
            "\nFor persistent storage, pass an explicit location.")
  }
  
  location <- path.expand(location)
  if (!dir.exists(location)) {
    dir.create(location, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(location)) {
    stop("location does not exist and could not be created: ", location, call. = FALSE)
  }

  message("Creating project setup")

  ## Main - Project Folder
  main_dir <- file.path(location, 'main')
  if( file.exists(main_dir))
    message("main directory exists -- don't worry, I'm not overwriting anything!")
  dir.create(main_dir, showWarnings = FALSE)

  ###### Input
  input_dir <- file.path(main_dir, 'input')
  dir.create(input_dir, showWarnings = FALSE)

  ########### ZCTA
  zcta_dir <- file.path(input_dir, 'zcta_500k')
  dir.create(zcta_dir, showWarnings = FALSE)

  ########### HPBL
  hpbl_dir <- file.path(input_dir, 'hpbl')
  dir.create(hpbl_dir, showWarnings = FALSE)

  ########### Meteo
  meteo_dir <- file.path(input_dir, 'meteo')
  dir.create(meteo_dir, showWarnings = FALSE)

  ###### Process
  proc_dir <- file.path(main_dir, 'process')
  dir.create(proc_dir, showWarnings = FALSE)

  ###### Output
  output_dir <- file.path(main_dir, 'output')
  dir.create(output_dir, showWarnings = FALSE)

  ########### Hysplit
  hysp_dir <- file.path(output_dir, 'hysplit')
  dir.create(hysp_dir, showWarnings = FALSE)

  ########### Ziplinks
  ziplink_dir <- file.path(output_dir, 'ziplinks')
  dir.create(ziplink_dir, showWarnings = FALSE)

  ########### Exposure
  exp_dir <- file.path(output_dir, 'exp')
  dir.create(exp_dir, showWarnings = FALSE)

  ########### Graph
  graph_dir <- file.path(output_dir, 'graph')
  dir.create(graph_dir, showWarnings = FALSE)

  ########### Rdata
  rdata_dir <- file.path(output_dir, 'rdata')
  dir.create(rdata_dir, showWarnings = FALSE)

  dirs <- list(
    main_dir = main_dir,
    input_dir = input_dir,
    zcta_dir = zcta_dir,
    hpbl_dir = hpbl_dir,
    meteo_dir = meteo_dir,
    proc_dir = proc_dir,
    output_dir = output_dir,
    hysp_dir = hysp_dir,
    ziplink_dir = ziplink_dir,
    exp_dir = exp_dir,
    graph_dir = graph_dir,
    rdata_dir = rdata_dir
  )

  list2env(dirs, envir = .disperseR_cache)

  message(paste("Project created in : ", location))

  return(dirs)
}
