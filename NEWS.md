# disperseR 0.2.0

Major update migrating from retired spatial packages to modern alternatives.

## Breaking Changes

- Requires R >= 4.1.0
- Replaced `rgdal`, `rgeos`, `maptools`, `sp`, `raster` with `sf` and `terra`
- SplitR is now a required dependency for HYSPLIT runs (install from GitHub)

## New Features
 
- Cross-platform parallelization: `mclapply` on Unix/macOS, `parLapply` on Windows
- `path.expand()` support for `~` paths in all directory arguments
- Robust download validation with file existence and size checks
- HYSPLIT binary exit status checks with clear error messages
- New spatial utility functions in `spatial_utils.R`

## Bug Fixes

- Fixed `download_file()` warning handler that skipped post-download validation
- Fixed meteorology download validation after `get_met_reanalysis()`
- Fixed `start_day` handling in `hysplit_dispersion()` for Date/POSIXt inputs
- Fixed `run_dir` validation and auto-creation
- Fixed config file checks to use `run_dir` instead of working directory
- Added `is.na()` guard in `start_day` validation

## Documentation

- Fixed vignette title typo ("DipserseR" -> "DisperseR")
- Removed outdated package references (ggsn, USAboundaries, tidyverse)
- Updated SplitR install guidance to `rich-iannone/SplitR`
- Tightened prose throughout vignettes

## Internal

- Added testthat infrastructure with unit tests for core functions
- Removed unused files (bibliography.bib, RESEARCH_FINDINGS.md)
- Cleaned up .Rbuildignore

# disperseR 0.1.0

- Initial release
