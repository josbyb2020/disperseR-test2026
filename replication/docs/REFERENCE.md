# disperseR Replication Reference Guide

Quick reference for the disperseR API, parameters, and troubleshooting.

---

## API Reference

### `define_inputs()`

Creates the emission event table for HYSPLIT runs.

| Parameter | Type | Description |
|-----------|------|-------------|
| `units` | data.table | Must have ID, Latitude, Longitude, Height, year columns |
| `startday` | character | First date to simulate (YYYY-MM-DD) |
| `endday` | character | Last date to simulate |
| `start.hours` | numeric vector | Hours to release parcels (0-23) |
| `duration` | numeric | Forward run duration in hours |

**Required columns in `units`:**
- `ID`: Unit identifier
- `Latitude`, `Longitude`: Source location
- `Height`: Stack height in meters
- `year`: Emissions year (used for filtering and output naming)

**Example:**
```r
input_refs <- define_inputs(
  units = my_units,
  startday = "2005-01-01",
  endday = "2005-01-31",
  start.hours = c(0, 6, 12, 18),
  duration = 120
)
```

---

### `run_disperser_parallel()`

Executes HYSPLIT for each input reference.

| Parameter | Type | Description |
|-----------|------|-------------|
| `input.refs` | data.table | Output from `define_inputs()` |
| `pbl.height` | SpatRaster | Planetary boundary layer data |
| `species` | character | `"so2"` (gas) or `"so4p"` (particulate) |
| `proc_dir` | character | Temporary processing directory |
| `hysp_dir` | character | Output directory for .fst files |
| `meteo_dir` | character | Directory with met files (RP*.gbl) |
| `npart` | numeric | Number of air parcels (default 100) |
| `mc.cores` | numeric | Parallel cores (max 2 for CRAN) |
| `overwrite` | logical | Rerun existing outputs? |
| `keep.hysplit.files` | logical | Retain raw HYSPLIT output? |

**Output:** .fst files with columns: `lon`, `lat`, `height`, `Pdate`, `hour`

---

### `link_all_units()`

Links HYSPLIT parcel outputs to geographic units.

| Parameter | Type | Description |
|-----------|------|-------------|
| `units.run` | data.table | Units with ID, uID columns |
| `link.to` | character | `"zips"`, `"counties"`, or `"grids"` |
| `year.mons` | character | Year-months to process (YYYYMM format) |
| `crosswalk.` | data.frame | ZCTA-County crosswalk (required for zips) |
| `pbl.height` | SpatRaster | PBL data for trimming |
| `pbl_trim` | logical | Exclude above-PBL parcels? |
| `hysp_dir` | character | Directory with .fst outputs |
| `ziplink_dir` | character | Output directory |
| `duration.run.hours` | numeric | Must match dispersion duration |
| `res.link` | numeric | Grid resolution in meters |

---

### `calculate_exposure()`

Computes HyADS exposure from linked data.

| Parameter | Type | Description |
|-----------|------|-------------|
| `year.E` | numeric | Emissions year |
| `year.D` | numeric | Dispersion year (usually same) |
| `link.to` | character | `"zips"`, `"counties"`, or `"grids"` |
| `pollutant` | character | Column in units.mo (e.g., `"SO2..tons."`) |
| `units.mo` | data.table | Monthly emissions (use `PP.units.monthly1995_2017`) |
| `rda_file` | character | Path to RData from `combine_monthly_links()` |
| `exp_dir` | character | Output directory |
| `source.agg` | character | `"total"`, `"facility"`, or `"unit"` |
| `time.agg` | character | `"year"` or `"month"` |
| `allow.partial` | logical | Continue with missing months? |
| `monthly_maps` | list | Optional: pre-loaded monthly link maps (alternative to rda_file) |

**Data source options (one required):**
- `rda_file`: Path to RData file from `combine_monthly_links()`
- `monthly_maps`: List of monthly link data.tables named like `MAP1.2005`, `MAP2.2005`, etc.

**Aggregation options:**
- `source.agg = "total"`: Single exposure column summing all sources
- `source.agg = "facility"`: One column per facility
- `source.agg = "unit"`: One column per unit

---

## Parameter Recommendations

| Parameter | Recommended | Why |
|-----------|-------------|-----|
| `npart` | >= 100 | Statistical stability |
| `res.link` | 12000 (12km) | Balance detail vs. file size |
| `duration` | 120-240 hours | Captures regional transport |
| `mc.cores` | 2 | CRAN compliance maximum |
| `pbl_trim` | TRUE | More physically realistic |

---

## PBL Trimming

When `pbl_trim = TRUE`, parcels above the monthly mean PBL height are excluded.

**Why it matters:**
- Surface exposure primarily comes from pollutants within the boundary layer
- Above-PBL parcels are less likely to affect ground-level concentrations
- Seasonal/geographic variations in PBL height affect exposure patterns

**Typical effects:**
- Reduces total exposure by 10-30%
- Stronger effect in winter (lower PBL) than summer
- Inland/nighttime parcels more affected than coastal/daytime

---

## Visualization Functions

### `plot_units_ranked()`
Bar chart showing units ranked by population-weighted exposure.

### `plot_impact_single()`
Choropleth map showing exposure from one specific unit.

### `plot_impact_weighted()`
Map showing cumulative exposure weighted by population.

### `plot_impact_unit()`
Faceted maps comparing exposure patterns from multiple units.

**Additional visualization ideas:**
- Time series: Monthly exposure variation
- Scatter plots: Emissions vs exposure, distance decay
- Sensitivity plots: PBL trim on/off comparison
- Distribution plots: Histograms, box plots by region
- Interactive maps: Use `leaflet` for web exploration

---

## Expected Results

### Dispersion Patterns
- Initial height: Stack height (~200-300m AGL)
- 5-day height: ~800-1000m AGL (atmospheric mixing)
- Transport distance: 500-1000+ km over 120 hours
- Direction: Determined by prevailing winds

### Exposure Distribution
- Highest within 100-200km downwind of sources
- Clear gradient from source location
- Decreases with distance (inverse relationship)

---

## Troubleshooting

### "splitr not found"
```r
remotes::install_github("rich-iannone/splitr")
```

### HYSPLIT exit code 132 (SIGILL) on Apple Silicon
Install Rosetta:
```bash
softwareupdate --install-rosetta
```

### "object not found" after R restart
Always call `create_dirs()` in each new session to repopulate the internal cache.

### Windows parallel errors
Ensure disperseR is installed (not just loaded via `devtools::load_all`) or use `mc.cores = 1`.

For PBL-trimmed runs, Windows socket clusters cannot serialize
`terra::SpatRaster` objects; use `mc.cores = 1`.

### CRS/projection errors with terra
```r
pbl <- terra::rast("hpbl.mon.mean.nc")
terra::crs(pbl) <- "+proj=lcc +x_0=5632642.22547 +y_0=4612545.65137 +lat_0=50 +lon_0=-107 +lat_1=50"
```

### Incomplete meteorology files
Re-download with explicit timeout:
```r
options(timeout = 600)
get_met_reanalysis(files = "RP200501.gbl", path_met_files = dirs$meteo_dir)
```

### Windows download failures
If downloads fail behind a proxy, try:
```r
options(download.file.method = "wininet")
```

---

## Data Sources

| Dataset | Source | Size |
|---------|--------|------|
| ZCTA Shapefile | US Census TIGER/Line | ~50MB |
| ZCTA-County Crosswalk | HUD | ~5MB |
| PBL Height | NARR | ~20MB |
| Meteorology | NCEP Reanalysis | ~100MB/month |
| Units | EPA CEMS (built-in) | Included |

---

## Output File Formats

| File Type | Contents | Location |
|-----------|----------|----------|
| `.fst` | Parcel trajectories | `hysp_dir/` |
| `.RData` | Combined monthly links | `rdata_dir/` |
| `.csv` | Exposure tables | `exp_dir/` |

---

## Limitations

1. **Meteorological resolution**: NCEP Reanalysis is ~2.5 degrees
2. **Temporal aggregation**: Daily/monthly may miss short-term peaks
3. **Source characterization**: Simplified stack parameters, no plume rise
4. **Population data**: ZCTA boundaries may not match residential patterns
5. **Chemistry**: No atmospheric transformation (secondary formation)
