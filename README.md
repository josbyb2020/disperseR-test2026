disperseR
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# disperseR 0.2.0

Run HYSPLIT many times in parallel and aggregate exposure to ZIP code
level. This fork modernizes the spatial stack (sf/terra) and keeps
compatibility with current R.

## Highlights

- Modern spatial stack (sf, terra) and cleaned dependencies
- Parallel execution with OS-aware strategy
- Built-in helpers for data download, linking, and exposure metrics

## Installation

Not on CRAN yet. Install from GitHub (CRAN instructions will be added
once released):

``` r
install.packages("remotes")
remotes::install_github("josbyb2020/disperseR-test2026")
```

If you plan to run HYSPLIT (dispersion or trajectories) or use GDAS1
meteorology, install SplitR if it is available on CRAN. If not, provide
your own HYSPLIT binaries via `binary_path` and `parhplot_path`.

Windows users who build vignettes will need Rtools.

## Quick start

``` r
library(disperseR)

dirs <- create_dirs(location = "~/disperseR_project")

get_data(
  data = "all",
  start.year = "2005",
  start.month = "01",
  end.year = "2005",
  end.month = "12"
)

check_spatial_packages()
```

## Project layout

- `main/input/zcta_500k`: ZCTA shapefile
- `main/input/hpbl`: PBL height data
- `main/input/meteo`: reanalysis meteorology files
- `main/output/hysplit`: dispersion outputs
- `main/output/ziplinks`: linked ZIP code outputs
- `main/output/rdata`: HyADS matrices
- `main/output/exp`: exposure tables
- `main/output/graph`: plots

## Data requirements

- ZCTA shapefile
- ZCTA to ZIP crosswalk (included)
- NOAA reanalysis meteorology files
- Planetary boundary layer height data
- Units data (included for 1995-2017)

Use `get_data()` to download most inputs automatically.

## Cross-platform notes

- `create_dirs()` defaults to Desktop when available and falls back to
  the home directory. Pass an explicit path on Windows or network
  drives.
- HYSPLIT binaries are required for dispersion runs. If SplitR is
  installed, disperseR will use its bundled binaries; otherwise provide
  `binary_path` and `parhplot_path` to `hysplit_dispersion()`.
- Windows uses socket clusters (`parLapply`). Set `mc.cores` to control
  parallelism.

## Documentation

- Main vignette: `vignette("Vignette_DisperseR")`
- Migration guide: `?migration_guide`
- Additional vignettes in `vignettes/` (render with
  `rmarkdown::render()`)

## System requirements

- R \>= 4.1.0
- sf \>= 1.0-0
- terra \>= 1.7-0
- Network access for meteorology downloads

## Citation

    Henneman, L., Choirat, C., & Garbulinska, M. (2019). disperseR:
    Run HYSPLIT many times in parallel, aggregate to zip code level.
    R package version 0.2.0. https://github.com/lhenneman/disperseR
