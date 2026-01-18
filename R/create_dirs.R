#' create a set of directories to run disperseR
#'
#' \code{create_dirs}
#'
#' @description `create_dirs()` takes 0 or the following argument `location`
#'
#'
#' @param location Path where project directories are created. Defaults to the
#'   Desktop when available, otherwise the home directory. Creates the path
#'   if it does not exist.
#'
#'
#' @return A named list of directory paths. The paths are also stored in an
#' internal package cache for use by other functions.


#' @export create_dirs

create_dirs <- function(location = file.path("~", "Desktop")) {

  is_default <- missing(location)
  location <- path.expand(location)
  if (is_default && !dir.exists(location)) {
    location <- path.expand("~")
    message("Default Desktop path not found; using home directory: ", location)
  }
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
