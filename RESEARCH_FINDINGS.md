# Research Findings: Missing Functions and Code Issues [RESOLVED]

**Status:** All issues fixed in commit `8270a9b` (2026-01-18)

## Executive Summary

This document provides research findings on missing functions and code issues identified in disperseR v0.2.0. The package borrows code from SplitR but does not declare it as a dependency, leading to missing function calls.

**All issues below have been addressed.**

---

## 1. Missing Functions from SplitR Package

### 1.1 `create_grid()`

**Location:** `R/run_disperser_parallel_subfun.R:372` (called in `add_grid()`)

**Issue:** `add_grid()` calls `create_grid()` which is not defined/imported.

**Research Finding:**
- `create_grid()` is a function from the **SplitR** package (by rich-iannone)
- Purpose: Creates a regular grid of latitude/longitude points for ensemble trajectory/dispersion simulations
- Arguments: `lat`, `lon`, `range`, `division`
- Returns: List with `$lat` and `$lon` vectors

**Source:** SplitR package - https://rdrr.io/github/rich-iannone/SplitR/

**Recommendation:**
- **Option A (Preferred):** Add SplitR as a dependency and import `create_grid`
  ```r
  # In DESCRIPTION
  Imports:
      SplitR,
      ...
  
  # In NAMESPACE
  importFrom(SplitR, create_grid)
  ```
- **Option B:** Re-implement `create_grid()` locally (simple function, ~20 lines)

---

### 1.2 `hysplit_trajectory()`

**Location:** `R/run_model.R:7` (called for `traj_model` objects)

**Issue:** `run_model()` calls `hysplit_trajectory()` which is not defined/imported.

**Research Finding:**
- `hysplit_trajectory()` is the main trajectory function from **SplitR** package
- Purpose: Runs forward/backward HYSPLIT trajectory simulations
- Handles: met data download, control files, HYSPLIT execution, output parsing
- Returns: Data frame with trajectory points (lat, lon, height, pressure, etc.)

**Source:** SplitR package - https://rdrr.io/github/rich-iannone/SplitR/

**Recommendation:**
- **Option A (Preferred):** Add SplitR as dependency and import
  ```r
  importFrom(SplitR, hysplit_trajectory)
  ```
- **Option B:** Re-implement (complex, ~500+ lines, not recommended)

**Note:** The package description states it "uses code borrowed from the SplitR package in a slightly modified version", suggesting SplitR should be a dependency.

---

### 1.3 `get_met_gdas1()`

**Location:** `R/hysplit_dispersion.R:286, 317` (called when `met_type == "gdas1"`)

**Issue:** GDAS1 meteorological data downloads will fail.

**Research Finding:**
- `get_met_gdas1()` is from **SplitR** package
- Purpose: Downloads GDAS1 (Global Data Assimilation System, 1-degree) met files from NOAA
- Handles: Date range calculation, filename construction, FTP downloads
- Used when: `met_type = "gdas1"` in HYSPLIT runs

**Source:** SplitR package - https://rdrr.io/github/rich-iannone/SplitR/

**Recommendation:**
- **Option A (Preferred):** Add SplitR dependency and import
  ```r
  importFrom(SplitR, get_met_gdas1)
  ```
- **Option B:** Re-implement (moderate complexity, ~100 lines)

---

## 2. Code Logic Issues

### 2.1 `rankfacs_by_popwgt_location()` - Input Validation Bug

**Location:** `R/rankfacs_by_popwght_location.R:12-18`

**Issue:** Converts `data.linked` from NULL to data.table BEFORE validation check, causing false "both provided" error.

**Current Code:**
```r
crosswalk. <- data.table(crosswalk.)[, year := year]
data.linked <- data.table(data.linked)[, year := year]  # Line 13: converts NULL to empty DT

# Line 16-18: Validation check happens AFTER conversion
if ((is.null(link.files) & is.null(data.linked)) | 
    (!is.null(link.files) & !is.null(data.linked)))
  stop("Please provide EITHER link.files OR data.linked")
```

**Problem:** If only `link.files` is provided, `data.linked` is NULL but gets converted to empty data.table, so `is.null(data.linked)` is FALSE, triggering the error.

**Fix:**
```r
# Validate inputs FIRST
if ((is.null(link.files) & is.null(data.linked)) | 
    (!is.null(link.files) & !is.null(data.linked)))
  stop("Please provide EITHER link.files OR data.linked")

# THEN convert to data.table
crosswalk. <- data.table(crosswalk.)[, year := year]
if (!is.null(data.linked)) {
  data.linked <- data.table(data.linked)[, year := year]
}
```

---

### 2.2 `disperser_link_*()` - Self-Referential Defaults

**Location:** `R/link_all_units_subfun.R:270` (and similar in other `disperser_link_*` functions)

**Issue:** `duration.run.hours = duration.run.hours` creates self-referential default that will fail if called directly.

**Current Code:**
```r
disperser_link_grids <- function(month_YYYYMM = NULL,
                                  ...,
                                  duration.run.hours = duration.run.hours,  # PROBLEM
                                  ...)
```

**Fix:**
```r
disperser_link_grids <- function(month_YYYYMM = NULL,
                                  ...,
                                  duration.run.hours = 240,  # Default: 10 days
                                  ...)
```

**Note:** 240 hours = 10 days is the maximum time sulfur stays in atmosphere (mentioned in `link_all_units()` documentation).

---

### 2.3 `plot_units_ranked()` - Multi-Year Data Issue

**Location:** `R/plot_units_ranked.R:5, 8`

**Issue:** Merges multi-year `data.ranked` against single-year filtered `data.units` without filtering `data.ranked` by year.

**Current Code:**
```r
plot_units_ranked <- function(data.ranked, data.units, year, graph.dir) {
  data.units <- data.units[data.units$year == year, ]  # Filters by year
  data.ranked[, uID := as(uID, 'character')]
  unitRanks <- merge(data.ranked, data.units, by = 'uID')  # No year filter on data.ranked
```

**Problem:** If `data.ranked` contains multiple years, merge can duplicate/mix years.

**Fix:**
```r
plot_units_ranked <- function(data.ranked, data.units, year, graph.dir) {
  # Filter both by year
  data.units <- data.units[data.units$year == year, ]
  data.ranked <- data.ranked[data.ranked$year == year, ]  # ADD THIS
  data.units[, uID := as(uID, 'character')]
  data.ranked[, uID := as(uID, 'character')]
  unitRanks <- merge(data.ranked, data.units, by = 'uID')
```

---

## 3. Dead Code

### 3.1 `rankfacs_by_popwght_location()` - Unreachable Code

**Location:** `R/rankfacs_by_popwght_location.R:83-148`

**Issue:** Code after `return(uID.pw)` on line 83 references undefined objects (`data.units`, `unitRanks2005`, `graph.dir`).

**Analysis:** This appears to be old plotting code that was never removed. The function returns on line 83, so lines 84-148 are unreachable.

**Recommendation:** Remove lines 84-148 (dead code).

---

## 4. Recommendations Summary

### High Priority (Breaking Issues)

1. **Add SplitR as dependency** (recommended approach)
   - Add to DESCRIPTION: `Imports: SplitR`
   - Import functions in NAMESPACE:
     ```r
     importFrom(SplitR, create_grid)
     importFrom(SplitR, hysplit_trajectory)
     importFrom(SplitR, get_met_gdas1)
     ```
   - Update function calls to use `SplitR::` prefix or rely on imports

2. **Fix `rankfacs_by_popwght_location()` input validation** (move validation before conversion)

3. **Fix `disperser_link_*()` default parameters** (use `240` instead of self-reference)

4. **Fix `plot_units_ranked()` year filtering** (filter `data.ranked` by year)

### Medium Priority

5. **Remove dead code** in `rankfacs_by_popwght_location()` (lines 84-148)

### Low Priority

6. **Clean up check artifacts** (already handled in `.gitignore`)

---

## 5. Open Questions

### Q1: Should SplitR be a dependency?

**Answer:** YES - The package description explicitly states it "uses code borrowed from the SplitR package". The missing functions (`create_grid`, `hysplit_trajectory`, `get_met_gdas1`) are core SplitR functions. Without SplitR as a dependency, trajectory models and GDAS1 met downloads will fail.

**Evidence:**
- DESCRIPTION line 16: "The package uses code borrowed from the SplitR package"
- `hysplit_dispersion.R` already references SplitR binaries: `system.file("osx/hycs_std", package = "SplitR")`
- `run_model.R` calls `hysplit_trajectory()` which is SplitR's main function

### Q2: Are `disperser_link_*` functions public APIs?

**Answer:** YES - They are exported in NAMESPACE and documented. They should have proper defaults.

**Evidence:**
- All three are exported: `export(disperser_link_grids)`, `export(disperser_link_counties)`, `export(disperser_link_zips)`
- They are called internally by `link_all_units()` which passes `duration.run.hours` explicitly
- But users could call them directly, so defaults must work

### Q3: Should `plot_units_ranked` accept multi-year data?

**Answer:** NO - The function signature includes `year` parameter, suggesting single-year operation. Filter `data.ranked` by year to match `data.units` filtering.

---

## 6. Implementation Plan

### Phase 1: Critical Fixes (Breaking Issues)

1. Add SplitR to DESCRIPTION Imports
2. Add SplitR imports to NAMESPACE
3. Fix `rankfacs_by_popwght_location()` validation order
4. Fix `disperser_link_*()` default parameters
5. Fix `plot_units_ranked()` year filtering

### Phase 2: Code Cleanup

6. Remove dead code from `rankfacs_by_popwght_location()`

### Phase 3: Testing

7. Test trajectory model workflows
8. Test GDAS1 met downloads
9. Test all `disperser_link_*()` functions with defaults
10. Test `plot_units_ranked()` with multi-year data

---

## References

- SplitR package: https://github.com/rich-iannone/SplitR
- SplitR documentation: https://rdrr.io/github/rich-iannone/SplitR/
- Original disperseR: https://github.com/lhenneman/disperseR
- hyspdisp package: https://github.com/lhenneman/hyspdisp
