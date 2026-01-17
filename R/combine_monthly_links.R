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
#'
#' @return Saves an .RData file to the rdata_dir defined by 
#'   `disperseR::create_dirs()` with filename `filename`.
#'
#' @export
#' @importFrom data.table data.table rbindlist dcast setDT
#' @importFrom fst read.fst
#' @importFrom terra rast ext extend as.data.frame

combine_monthly_links <- function(month_YYYYMMs,
                                   link.to = 'zips',
                                   filename = NULL) {

  names.map <- c()

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
      names.map <- append(names.map, name.map)
      assign(name.map, Merged_cast)
      rm("MergedDT", "Merged_cast")
    }
  }

  # Put all grid links on consistent extent using terra
  if (link.to == 'grids' && length(names.map) > 0) {
    out.d <- mget(names.map)
    
    # Convert data.tables to SpatRasters using terra
    out.r <- lapply(out.d, function(dt) {
      # Create raster from xyz data
      if (ncol(dt) > 2) {
        # Multiple columns - create multi-layer raster
        r <- terra::rast(dt, type = "xyz", crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")
        return(r)
      }
      return(NULL)
    })
    out.r <- Filter(Negate(is.null), out.r)
    
    if (length(out.r) > 1) {
      out.ids <- lapply(out.d, function(dt) names(dt))
      
      # Calculate consistent extent using terra
      all_extents <- lapply(out.r, terra::ext)
      combined_ext <- Reduce(function(e1, e2) {
        terra::ext(
          min(e1[1], e2[1]),  # xmin
          max(e1[2], e2[2]),  # xmax
          min(e1[3], e2[3]),  # ymin
          max(e1[4], e2[4])   # ymax
        )
      }, all_extents)
      
      # Extend all rasters to common extent
      out.b <- lapply(out.r, function(r) terra::extend(r, combined_ext))
      
      # Convert back to data.tables
      out.dt <- lapply(out.b, function(x) {
        df <- terra::as.data.frame(x, xy = TRUE, na.rm = FALSE)
        data.table::setDT(df)
        return(df)
      })
      
      # Round coordinates to nearest meter
      out.dt <- lapply(out.dt, function(dt) {
        dt[, `:=`(x = round(x), y = round(y))]
        return(dt)
      })
      
      # Extract from list and assign
      for (i in seq_along(names(out.dt))) {
        nm <- names(out.dt)[i]
        assign(nm, out.dt[[i]], envir = parent.frame())
      }
    }
  }

  if (is.null(filename))
    filename <- paste0('hyads_unwgted_', link.to, '.RData')
  
  rda.filename <- file.path(rdata_dir, filename)
  save(list = names.map, file = rda.filename)

  message("Monthly RData file written to ", rda.filename)
  return(mget(names.map))
}
