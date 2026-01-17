# disperseR 0.2.0

Run HYSPLIT many times in parallel, aggregate to ZIP code level.

## Important Update Notice

This is **version 0.2.0** of the disperseR package, updated to work with modern R spatial packages. The original disperseR package (v0.1.0) has not been updated since 2021 and **will no longer install** on modern R systems due to its dependencies on retired packages.

### What Changed?

The following packages were **retired from CRAN in October 2023** and have been replaced:

| Retired Package | Replacement |
|-----------------|-------------|
| `rgdal` | `sf`, `terra` |
| `rgeos` | `sf`, `terra` |
| `maptools` | `sf` |
| `sp` (deprecated) | `sf` |
| `raster` (minimal maintenance) | `terra` |

---

## Installation

### Prerequisites

Make sure you have the modern spatial packages installed:

```r
install.packages(c("sf", "terra"))
```

### Install disperseR 0.2.0

```r
# From GitHub
devtools::install_github("your-username/disperseR")

# Or from local source
install.packages("path/to/disperseR_0.2.0.tar.gz", repos = NULL, type = "source")
```

---

## Quick Start

```r
library(disperseR)

# Create project directories
create_dirs(location = "~/disperseR_project")

# Get required data
get_data(
  data = "all",
  start.year = "2005",
  start.month = "01",
  end.year = "2005",
  end.month = "12"
)

# Check your spatial package setup
check_spatial_packages()
```

---

## What is disperseR?

`disperseR` is an R package that:

1. Runs the **HYSPLIT** atmospheric dispersion model many times
2. Calculates the **HyADS** (HYSPLIT Average Dispersion) exposure metric
3. Aggregates results to **ZIP code level** for population exposure estimates
4. Tracks pollution plumes from sources (e.g., power plants) over time

It is based on the [hyspdisp package](https://github.com/lhenneman/hyspdisp) and [SplitR package](https://github.com/rich-iannone/SplitR).

---

## Key Functions

| Function | Description |
|----------|-------------|
| `create_dirs()` | Set up project directory structure |
| `get_data()` | Download/load required datasets |
| `get_met_reanalysis()` | Download meteorological data |
| `define_inputs()` | Configure emission scenarios |
| `run_disperser_parallel()` | Run HYSPLIT in parallel |
| `link_all_units()` | Link dispersion to ZIP codes |
| `calculate_exposure()` | Compute exposure metrics |
| `plot_impact_*()` | Visualize results |

---

## Data Requirements

disperseR requires several datasets:

1. **ZCTA Shapefile** - ZIP Code Tabulation Areas from US Census
2. **Crosswalk** - ZCTA to ZIP code mapping (included in package)
3. **Meteorological Files** - NOAA reanalysis data
4. **PBL Height** - Planetary Boundary Layer data
5. **Units Data** - Power plant emissions data (included for 1995-2017)

---

## Changes from v0.1.0

### Breaking Changes

- Return types for spatial functions are now `sf` objects instead of `sp` objects
- Raster objects are now `terra::SpatRaster` instead of `raster::RasterLayer`
- Removed dependencies on `rgdal`, `rgeos`, `maptools`, `sp`, `raster`, `ggsn`, `tidyverse`

### Migration for Existing Code

If your code uses sp objects directly:

```r
# Old code (won't work)
library(sp)
coords <- coordinates(zcta)

# New code (sf)
library(sf)
coords <- st_coordinates(zcta)
```

See `?migration_guide` for complete migration documentation.

### New Features

- `check_spatial_packages()` - Verify your installation
- `spatial_utils.R` - Helper functions for common spatial operations
- Better integration with tidyverse workflows
- Improved CRS handling using WKT2 format

---

## Documentation

- **Main Vignette**: `vignette("Vignette_DisperseR")`
- **Migration Guide**: `?migration_guide`
- **Original Documentation**: [GitHub Pages](https://htmlpreview.github.io/?https://github.com/lhenneman/disperseR/blob/master/vignettesHTML/Vignette_DisperseR.html)

---

## Docker

A Docker image with all dependencies is available:

```bash
docker pull audiracmichelle/disperser
docker run -d --name disperser -p 8787:8787 -e PASSWORD=rstudio audiracmichelle/disperser
```

Then access RStudio at http://localhost:8787

---

## System Requirements

- R >= 4.1.0
- sf >= 1.0-0
- terra >= 1.7-0
- HYSPLIT binary (for dispersion modeling)

---

## References

- HYSPLIT Model: https://www.ready.noaa.gov/HYSPLIT.php
- NCEP Reanalysis: https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.html
- sf package: https://r-spatial.github.io/sf/
- terra package: https://rspatial.github.io/terra/
- R-spatial evolution: https://r-spatial.org/r/2022/04/12/evolution.html

---

## Authors

- **Lucas Henneman** (maintainer)
- Christine Choirat (contributor)
- Maja Garbulinska (contributor)

## License

GPL-3

---

## Citation

If you use disperseR in your research, please cite:

```
Henneman, L., Choirat, C., & Garbulinska, M. (2019). disperseR: Run HYSPLIT
many times in parallel, aggregate to zip code level. R package version 0.2.0.
https://github.com/lhenneman/disperseR
```
