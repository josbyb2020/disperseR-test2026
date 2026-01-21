disperseR
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# disperseR 0.2.1

Run HYSPLIT many times in parallel and aggregate exposure to ZIP code
level.

## What is this?

disperseR implements the HyADS (HYSPLIT Average Dispersion) methodology
for estimating population exposure to air pollution from point sources
(like power plants). It runs the HYSPLIT atmospheric dispersion model,
links trajectories to geographic areas, and calculates exposure metrics.

## First-time setup (5-10 minutes)

### Step 1: Install R packages

Open R or RStudio and run:

``` r
# Install disperseR from GitHub
install.packages("remotes")
remotes::install_github("josbyb2020/disperseR-test2026")

# Install splitr (provides HYSPLIT binaries)
remotes::install_github("rich-iannone/splitr")
```

### Step 2: Platform-specific setup

**Check your platform first:**

- **Windows**: No extra steps needed.
- **macOS Intel** (pre-2020 Macs): No extra steps needed.
- **macOS Apple Silicon** (M1/M2/M3/M4 chips, 2020+): See below.
- **Linux**: Install GDAL/GEOS/PROJ (see below).

#### macOS Apple Silicon

The HYSPLIT binaries in splitr are compiled for Intel. Install Rosetta
to run them:

``` bash
softwareupdate --install-rosetta
```

#### Linux

Install system libraries for sf and terra:

``` bash
# Ubuntu/Debian
sudo apt install libgdal-dev libgeos-dev libproj-dev

# Fedora/RHEL
sudo dnf install gdal-devel geos-devel proj-devel
```

### Step 3: Create a project and download data

Back in R:

``` r
library(disperseR)

# Create project directories (do this at the start of EVERY R session)
dirs <- create_dirs(location = "~/disperseR_project")

# Download all required data for 2005
# This downloads ~500MB on first run, so it takes a few minutes
get_data(
  data = "all",
  start.year = "2005",
  start.month = "01",
  end.year = "2005",
  end.month = "12"
)

# Verify everything is ready
check_spatial_packages()
```

**Windows users:** Use a short path like `C:/disperseR_project` to avoid
Windows path length limits.

## Quick example

``` r
library(disperseR)
library(data.table)

# 1. Initialize (required every session)
dirs <- create_dirs("~/disperseR_project")

# 2. Load units data and select top emitter
units_dt <- data.table(disperseR::units)
test_unit <- units_dt[year == 2005][order(-SOx)][1]

# 3. Define what to run
input_refs <- define_inputs(
  units = test_unit,
  startday = "2005-01-15",
  endday = "2005-01-15",
  start.hours = c(0, 6, 12, 18),
  duration = 120
)

# 4. Run HYSPLIT dispersion
pbl <- get_data("pblheight")
run_disperser_parallel(
  input.refs = input_refs,
  pbl.height = pbl,
  species = "so2",
  mc.cores = 1
)

# 5. Check outputs
validate_pipeline()
```

## Common issues

### “splitr not found” or HYSPLIT errors

Install splitr: `remotes::install_github("rich-iannone/splitr")`

On Apple Silicon Macs, also install Rosetta (see Step 2 above).

### “object not found” after restarting R

Run `create_dirs()` at the start of every R session. disperseR caches
paths in memory, so they’re lost when R restarts.

### Windows: “could not find function” in parallel runs

disperseR must be *installed* (not just loaded via
`devtools::load_all`). Either install it properly or use `mc.cores = 1`.

### Download failures

If downloads fail (especially on Windows behind a proxy), try:

``` r
options(download.file.method = "wininet")
# Then retry get_data() or get_met_reanalysis()
```

### Windows: runs fail mid-batch or HYSPLIT “not found”

Windows Defender or corporate antivirus may flag rapid HYSPLIT execution
as suspicious (hundreds of .exe launches from temp directories).
Solutions:

1.  Add the HYSPLIT binary folder to your antivirus exclusion list
2.  Reduce `mc.cores` to slow down execution
3.  Run `cleanup_hysplit_zombies()` to kill orphaned processes from
    crashed runs

### CRS or projection errors

Install system libraries for sf/terra: - macOS:
`brew install gdal geos proj` - Ubuntu:
`sudo apt install libgdal-dev libgeos-dev libproj-dev`

### Linux: libgfortran.so.3 not found

The HYSPLIT binaries in splitr require an older Fortran runtime. On
newer Linux distros (Ubuntu 22.04+), install the compatibility library:

``` bash
sudo apt install libgfortran5
# If that doesn't work, you may need to compile HYSPLIT from source
```

Alternatively, download and compile HYSPLIT from [NOAA
ARL](https://www.ready.noaa.gov/HYSPLIT.php) and provide `binary_path`
and `parhplot_path` to bypass splitr.

## Project structure

After running `create_dirs()` and `get_data()`, your project looks like:

    disperseR_project/
      main/
        input/
          zcta_500k/     # ZCTA shapefile
          hpbl/          # Boundary layer height data
          meteo/         # Meteorology files (~120MB each)
        output/
          hysplit/       # Dispersion outputs (.fst files)
          ziplinks/      # Linked ZIP code data
          rdata/         # Combined monthly links
          exp/           # Exposure tables

## Learning more

- **Main tutorial**: `vignette("Vignette_DisperseR")`
- **Function help**: `?run_disperser_parallel`, `?link_all_units`,
  `?calculate_exposure`
- **Replication study**: See the `replication/` folder for a complete
  worked example

## Platform support

| Platform | Status | Notes |
|----|----|----|
| Windows 10/11 | Supported | Use short paths; parallel uses socket clusters |
| macOS Intel | Supported | Works out of the box |
| macOS Apple Silicon | Supported | Requires Rosetta (see setup) |
| Linux x86_64 | Supported | Install GDAL/GEOS/PROJ |
| Linux ARM | Partial | Need to supply your own HYSPLIT binaries |

## Citation

    Henneman, L., Choirat, C., & Garbulinska, M. (2019). disperseR:
    An R package for HYSPLIT dispersion modeling and ZIP code-level
    exposure assessment. R package version 0.2.1.
    https://github.com/josbyb2020/disperseR-test2026
