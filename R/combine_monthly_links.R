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
#' @param format Output format: `"wide"` (default, one column per ID — matches
#'   legacy behavior) or `"long"` (ZIP/grid/county, ID, N — much more
#'   memory-efficient for large runs). The `"long"` format skips the memory-heavy
#'   `dcast()` pivot and is recommended for national-scale analyses.
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
                                   format = c("wide", "long"),
                                   filename = NULL,
                                   ziplink_dir = NULL,
                                   rdata_dir = NULL) {

  format <- match.arg(format)

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
  skipped_months <- character(0)

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
      skipped_months <- c(skipped_months, ym)
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
          read_ziplinks_subfun,
          files.month
        )

        MergedDT <- data.table::rbindlist(data.h)
        if (format == "long") {
          Merged_cast <- MergedDT[, .(N = sum(N)), by = .(ZIP, ID)]
        } else {
          Merged_cast <- data.table::dcast(
            MergedDT,
            ZIP ~ ID,
            fun.aggregate = sum,
            value.var = "N"
          )
        }

      } else if (link.to == 'grids') {
        data.h <- lapply(
          seq_along(files.month),
          read_gridlinks_subfun,
          files.month
        )

        MergedDT <- data.table::rbindlist(data.h)
        if (format == "long") {
          Merged_cast <- MergedDT[, .(N = sum(N)), by = .(x, y, ID)]
        } else {
          Merged_cast <- data.table::dcast(
            MergedDT,
            x + y ~ ID,
            fun.aggregate = sum,
            value.var = "N"
          )
        }

      } else if (link.to == 'counties') {
        data.h <- lapply(
          seq_along(files.month),
          read_countylinks_subfun,
          files.month
        )

        MergedDT <- data.table::rbindlist(data.h)
        if (format == "long") {
          Merged_cast <- MergedDT[, .(N = sum(N)),
            by = .(statefp, countyfp, state_name, name, geoid, ID)]
        } else {
          Merged_cast <- data.table::dcast(
            MergedDT,
            statefp + countyfp + state_name + name + geoid ~ ID,
            fun.aggregate = sum,
            value.var = "N"
          )
        }
      }

      name.map <- paste0("MAP", month.m, ".", year.h)
      monthly_maps[[name.map]] <- Merged_cast
      rm("MergedDT", "Merged_cast")
    }
  }

  # Warn if any months were skipped

  if (length(skipped_months) > 0) {
    attr(monthly_maps, "missing_months") <- skipped_months
    warning(length(skipped_months), " of ", length(month_YYYYMMs),
            " requested months had no linked data files.", call. = FALSE)
  }

  # Align grid maps on a common (x, y) coordinate union (wide format only).
  # This avoids terra::rast() failures for sparse, irregular, or single-cell grids.
  # Long format doesn't need alignment — downstream aggregation handles it.
  if (link.to == 'grids' && format == "wide" && length(monthly_maps) > 0) {
    monthly_maps <- lapply(monthly_maps, function(dt) {
      dt <- data.table::as.data.table(dt)
      if (!all(c("x", "y") %in% names(dt))) {
        return(dt)
      }
      dt[, `:=`(
        x = as.numeric(round(x, 6)),
        y = as.numeric(round(y, 6))
      )]
      dt
    })

    xy_union <- data.table::rbindlist(
      lapply(monthly_maps, function(dt) {
        if (!all(c("x", "y") %in% names(dt))) {
          return(data.table::data.table(x = numeric(), y = numeric()))
        }
        dt[, .(x, y)]
      }),
      fill = TRUE
    )

    if (nrow(xy_union) > 0) {
      xy_union <- unique(xy_union)
      data.table::setorder(xy_union, x, y)

      monthly_maps <- lapply(monthly_maps, function(dt) {
        if (!all(c("x", "y") %in% names(dt))) {
          return(dt)
        }
        out <- merge(
          xy_union,
          dt,
          by = c("x", "y"),
          all.x = TRUE,
          sort = FALSE
        )
        data.table::setDT(out)
        data.table::setorder(out, x, y)
        out
      })
    }
  }

  if (is.null(filename))
    filename <- paste0('hyads_unwgted_', link.to, '.RData')
  
  rda.filename <- file.path(rdata_dir, filename)
  
  # Save as a single named list (cleaner than spreading into environment)
  save(monthly_maps, file = rda.filename)

  message("Monthly RData file written to ", rda.filename)
  return(monthly_maps)
}
