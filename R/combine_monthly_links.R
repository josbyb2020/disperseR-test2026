#' Combine monthly linked files
#'
#' @description Combines linked files produced with `disperseR::link_all_units()` 
#' into lists of data.tables for easier manipulation.
#'
#' @param month_YYYYMMs Months and years to combine. Format created by 
#'   `disperseR::get_yearmon()`
#' @param link.to Spatial scale for plotting. One of 'zips', 'counties', 
#'   or 'grids' that should match original input to `disperseR::link_all_units()`
#' @param filename Output filename. Defaults to 
#'   `paste0('hyads_unwgted_', link.to, '.RData')`
#' @param ziplink_dir Directory containing linked files from link_all_units().
#'   If NULL, uses the directory cached by create_dirs().
#' @param rdata_dir Directory to save output RData file.
#'   If NULL, uses the directory cached by create_dirs().
#'
#' @return Saves an .RData file to rdata_dir with filename `filename`.
#'   Returns a list of data.tables (one per month).
#'
#' @export
#' @importFrom data.table data.table rbindlist dcast setDT
#' @importFrom fst read.fst
#' @importFrom terra rast ext extend as.data.frame

combine_monthly_links <- function(month_YYYYMMs,
                                   link.to = 'zips',
                                   filename = NULL,
                                   ziplink_dir = NULL,
                                   rdata_dir = NULL) {

  # Resolve directory paths from package cache if not provided
  if (is.null(ziplink_dir)) {
    ziplink_dir <- .disperseR_cache_get("ziplink_dir")
    if (is.null(ziplink_dir)) {
      stop("ziplink_dir not specified and not found in cache.\n",
           "Either pass ziplink_dir explicitly or run create_dirs() first.",
           call. = FALSE)
    }
  }
  if (!dir.exists(ziplink_dir)) {
    stop("ziplink_dir does not exist: ", ziplink_dir, call. = FALSE)
  }
  
  if (is.null(rdata_dir)) {
    rdata_dir <- .disperseR_cache_get("rdata_dir")
    if (is.null(rdata_dir)) {
      stop("rdata_dir not specified and not found in cache.\n",
           "Either pass rdata_dir explicitly or run create_dirs() first.",
           call. = FALSE)
    }
  }
  if (!dir.exists(rdata_dir)) {
    dir.create(rdata_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Use a local list to accumulate results (no assign() side effects)
  monthly_maps <- list()

  for (ym in month_YYYYMMs) {

    year.h <- substr(ym, 1, 4)
    month.m <- as.integer(substr(ym, 5, 6))
    month.h <- formatC(month.m, width = 2, format = "d", flag = "0")

    if (link.to == 'zips') {
      pattern <- paste0('ziplinks.*', year.h, '-', month.h, '.*\\.fst$')
    } else if (link.to == 'grids') {
      pattern <- paste0('gridlinks.*', year.h, '-', month.h, '.*\\.fst$')
    } else if (link.to == 'counties') {
      pattern <- paste0('countylinks.*', year.h, '-', month.h, '.*\\.fst$')
    }

    files.month <- list.files(
      path = ziplink_dir,
      pattern = pattern,
      full.names = TRUE
    )

    if (length(files.month) == 0) {
      message("No data files for month_YYYYMMs ", ym)
    } else {
      message("Reading and merging month ", month.h, " in year ", year.h)

      unitnames <- gsub(
        paste0('.*links_|_', year.h, '-', month.h, '.*fst$'),
        '',
        files.month
      )
      names(files.month) <- unitnames

      if (link.to == 'zips') {
        data.h <- lapply(
          seq_along(files.month),
          disperseR::read_ziplinks_subfun,
          files.month
        )

        MergedDT <- data.table::rbindlist(data.h)
        Merged_cast <- data.table::dcast(
          MergedDT,
          ZIP ~ ID,
          fun.aggregate = sum,
          value.var = "N"
        )
        
      } else if (link.to == 'grids') {
        data.h <- lapply(
          seq_along(files.month),
          disperseR::read_gridlinks_subfun,
          files.month
        )

        MergedDT <- data.table::rbindlist(data.h)
        Merged_cast <- data.table::dcast(
          MergedDT,
          x + y ~ ID,
          fun.aggregate = sum,
          value.var = "N"
        )

      } else if (link.to == 'counties') {
        data.h <- lapply(
          seq_along(files.month),
          disperseR::read_countylinks_subfun,
          files.month
        )

        MergedDT <- data.table::rbindlist(data.h)
        Merged_cast <- data.table::dcast(
          MergedDT,
          statefp + countyfp + state_name + name + geoid ~ ID,
          fun.aggregate = sum,
          value.var = "N"
        )
      }

      name.map <- paste0("MAP", month.m, ".", year.h)
      monthly_maps[[name.map]] <- Merged_cast
      rm("MergedDT", "Merged_cast")
    }
  }

  # Put all grid links on consistent extent using terra
  if (link.to == 'grids' && length(monthly_maps) > 0) {
    # Convert data.tables to SpatRasters using terra
    out.r <- lapply(monthly_maps, function(dt) {
      if (ncol(dt) > 2) {
        r <- terra::rast(dt, type = "xyz", crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")
        return(r)
      }
      return(NULL)
    })
    out.r <- Filter(Negate(is.null), out.r)
    
    if (length(out.r) > 1) {
      # Calculate consistent extent using terra
      all_extents <- lapply(out.r, terra::ext)
      combined_ext <- Reduce(function(e1, e2) {
        terra::ext(
          min(e1[1], e2[1]),
          max(e1[2], e2[2]),
          min(e1[3], e2[3]),
          max(e1[4], e2[4])
        )
      }, all_extents)
      
      # Extend all rasters to common extent
      out.b <- lapply(out.r, function(r) terra::extend(r, combined_ext))
      
      # Convert back to data.tables
      out.dt <- lapply(out.b, function(x) {
        df <- terra::as.data.frame(x, xy = TRUE, na.rm = FALSE)
        data.table::setDT(df)
        df[, `:=`(x = round(x), y = round(y))]
        return(df)
      })
      
      # Update monthly_maps with extended grid data
      for (nm in names(out.dt)) {
        monthly_maps[[nm]] <- out.dt[[nm]]
      }
    }
  }

  if (is.null(filename))
    filename <- paste0('hyads_unwgted_', link.to, '.RData')
  
  rda.filename <- file.path(rdata_dir, filename)
  
  # Save the list contents (not the list itself) for backward compatibility
  names.map <- names(monthly_maps)
  list2env(monthly_maps, envir = environment())
  save(list = names.map, file = rda.filename, envir = environment())

  message("Monthly RData file written to ", rda.filename)
  return(monthly_maps)
}
