# disperseR 0.2.0

Run HYSPLIT many times in parallel and aggregate exposure to ZIP code level.
This version modernizes the spatial stack (sf/terra) and maintains compatibility
with current R (>= 4.1.0).

## Key Changes in 0.2.0

- **Modern spatial stack**: Replaced retired packages (rgdal, rgeos, maptools, sp, raster) with sf and terra
- **Cross-platform parallelization**: Uses mclapply on Unix/macOS, parLapply on Windows
- **Improved validation**: Functions validate inputs before attempting operations
- **Explicit directory parameters**: Functions accept directory paths explicitly or fall back to global variables

## Installation

Not on CRAN. Install from GitHub:

```r
install.packages("remotes")
remotes::install_github("josbyb2020/disperseR-test2026")
```

**Required for HYSPLIT runs:** SplitR provides the HYSPLIT binaries:

```r
remotes::install_github("rich-iannone/SplitR")
```

Without SplitR, `hysplit_dispersion()` and trajectory models will fail with a clear error message.

## Quick Start

```r
library(disperseR)

# Step 1: Create directory structure (required before other functions)
create_dirs(location = "~/disperseR_project")

# Step 2: Download required data
get_data(
  data = "all",
  start.year = "2005",
  start.month = "01",
  end.year = "2005",
  end.month = "12"
)

# Step 3: Verify spatial packages
check_spatial_packages()
```

## Workflow Overview

1. **`create_dirs()`** - Creates the project directory structure and sets global path variables
2. **`get_data()`** - Downloads ZCTA shapefiles, crosswalk, PBL height, and meteorology files
3. **`define_inputs()`** - Prepares unit/facility data for dispersion runs
4. **`run_disperser_parallel()`** - Runs HYSPLIT dispersion models in parallel
5. **`link_all_units()`** - Links dispersion outputs to spatial units (ZIP codes, counties, grids)
6. **`combine_monthly_links()`** - Combines linked files into monthly matrices
7. **`calculate_exposure()`** - Calculates population-weighted exposure metrics
8. **`plot_impact_*()`** - Creates spatial visualizations

## Project Directory Structure

After running `create_dirs()`:

```
main/
├── input/
│   ├── zcta_500k/     # ZCTA shapefile
│   ├── hpbl/          # Planetary boundary layer height data
│   └── meteo/         # Reanalysis meteorology files
├── output/
│   ├── hysplit/       # Raw HYSPLIT dispersion outputs
│   ├── ziplinks/      # Linked ZIP code outputs (.fst files)
│   ├── rdata/         # HyADS matrices (.RData files)
│   ├── exp/           # Exposure tables
│   └── graph/         # Plots and figures
└── process/           # Temporary processing files
```

## Function Parameters

Most functions accept explicit directory parameters OR fall back to global variables set by `create_dirs()`:

```r
# Option 1: Use global variables (set by create_dirs)
create_dirs(location = "~/disperseR_project")
combine_monthly_links(month_YYYYMMs = get_yearmon("2005", "01", "2005", "12"))

# Option 2: Pass directories explicitly
combine_monthly_links(
  month_YYYYMMs = get_yearmon("2005", "01", "2005", "12"),
  ziplink_dir = "~/my_project/ziplinks",
  rdata_dir = "~/my_project/rdata"
)
```

## Data Requirements

- **ZCTA shapefile**: Downloaded by `get_data(data = "zctashapefile")`
- **Crosswalk**: Included as `disperseR::crosswalk`
- **Meteorology files**: Downloaded by `get_data(data = "metfiles", ...)`
- **PBL height data**: Downloaded by `get_data(data = "pblheight")`
- **Unit data**: Included as `disperseR::PP.units.monthly1995_2017` (1995-2017)

## Cross-Platform Notes

- **Directory creation**: `create_dirs()` defaults to Desktop if available, otherwise home directory
- **HYSPLIT binaries**: Provided by SplitR for macOS, Linux, and Windows
- **Parallelization**: 
  - Unix/macOS: Uses `mclapply` (fork-based)
  - Windows: Uses `parLapply` with socket clusters
  - Set `mc.cores` parameter to control parallelism

## Documentation

- Main vignette: `vignette("Vignette_DisperseR")`
- Migration guide: `?migration_guide`
- Function help: `?function_name` (e.g., `?calculate_exposure`)

## System Requirements

- R >= 4.1.0
- sf >= 1.0-0
- terra >= 1.7-0
- Network access for data downloads
- SplitR for HYSPLIT functionality

## Citation

```
Henneman, L., Choirat, C., & Garbulinska, M. (2019). disperseR:
Run HYSPLIT many times in parallel, aggregate to ZIP code level.
R package version 0.2.0.
```

See `citation("disperseR")` for BibTeX entry.
