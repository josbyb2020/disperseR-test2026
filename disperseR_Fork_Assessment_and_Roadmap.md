# Improving the disperseR Package: Fork vs Original and Roadmap Forward

## Current Status (latest checks)
- CI green on macOS, Ubuntu, and Windows.
- `R CMD check --as-cran` on the tarball: 0 ERROR, 0 WARNING, 2 NOTE (network-only URL checks).
- Tests: 51 pass, 5 skip (platform or SplitR specific).
- Tarball size: ~9.8 MB; installed size ~9.8 MB (data ~9.2 MB).
- SplitR is optional; users can provide HYSPLIT binaries via `binary_path` and `parhplot_path`.

## Background and context
disperseR runs NOAA's HYSPLIT model many times and computes the HYSPLIT Average
Dispersion (HyADS) exposure metric, aggregating results to ZIP code level.
The original version (v0.1.0, circa 2019) was a restructured successor to
hyspdisp and incorporated code from SplitR. It enabled large batches of
dispersion runs but relied on deprecated spatial packages, had heavy
dependencies, weak error handling, and no automated tests. The repository
never shipped on CRAN, and users reported many installation and runtime
issues.

The fork (v0.2.0) modernizes the codebase, fixes bugs, and improves
reliability, making it close to CRAN-ready.

## Fork vs original: key improvements
- Modernized spatial stack: migrated from rgdal/rgeos/sp/raster to sf and terra.
- Reduced dependency footprint: removed unused or heavy imports.
- Cross-platform parallelism: Windows uses `parLapply`, Unix/macOS uses `mclapply`.
- Robust downloads and execution checks: validated files, clearer error messages.
- Optional HYSPLIT binaries: SplitR is optional, custom binaries are supported.
- Cleaner API: removed `eval(parse())`, global assignments, and `.GlobalEnv` state.
- Internal cache: directory paths and data are stored in a package cache.
- Safer defaults: `mc.cores` defaults to 2 (CRAN-safe), `create_dirs()` defaults
  to `tempdir()` (CRAN-safe).
- Tests and CI: testthat coverage and GitHub Actions across platforms.
- Documentation upgrades: updated vignettes, README, NEWS, and CITATION.

## North-star metrics
- Adoption: CRAN downloads, GitHub stars/forks, and citations (CITATION + DOI).
- Reliability: zero errors/warnings across platforms and deterministic outputs.
- Usability: new users complete a "happy path" in under 10 minutes.

## Roadmap (phased, research-driven)
### Phase 1: Scientific validation
- Reproduce one or two published workflows end-to-end.
- Add a validation vignette with expected outputs and tolerances.
- Document known limitations and scenarios where outputs are unreliable.

### Phase 2: Performance and robustness
- Profile memory/CPU for large runs and remove bottlenecks.
- Add caching via `tools::R_user_dir()` with explicit cache controls.
- Make downloads mockable (e.g., vcr or httptest2) for deterministic tests.

### Phase 3: Documentation and onboarding
- Write a 5-minute quickstart and troubleshooting guide.
- Clearly document SplitR vs custom HYSPLIT binary workflows.
- Publish a pkgdown site for structured navigation and search.

### Phase 4: Ecosystem readiness
- Submit to CRAN with a clean tarball check.
- Create a DOI (Zenodo) and update CITATION for academic use.
- Consider rOpenSci or JOSS submission for external review.

### Phase 5: Community and trust
- Add CONTRIBUTING and Code of Conduct.
- Maintain a predictable release cadence and crisp NEWS entries.
- Track reverse dependencies and provide migration guides for API changes.

## 4-week sprint (CRAN push)
**Week 1 - Reliability**
- Finalize remaining checks and expand tests for edge cases.
- Run full platform checks (CI + local tarball).

**Week 2 - Documentation**
- Polish README and vignettes.
- Add a troubleshooting section and common setup pitfalls.

**Week 3 - Pre-release testing**
- Run a clean tarball check with internet access for URL validation.
- Collect feedback from 1-2 external testers (fresh install + quickstart).

**Week 4 - Submission and outreach**
- Submit to CRAN with clear cran-comments.
- Tag release, publish DOI, and announce the release.

## Risks and caveats (current)
- SplitR is GitHub-only; CRAN users must provide their own HYSPLIT binaries or
  install SplitR separately.
- End-to-end HYSPLIT integration tests cannot run in CI without bundled binaries.
- Data size is below the 10 MB CRAN limit but above the 5 MB soft guideline;
  justification may be needed in submission comments.

## Sources
- Original repo: https://github.com/lhenneman/disperseR
- Fork repo: https://github.com/josbyb2020/disperseR-test2026
- SplitR docs: https://www.rdocumentation.org/packages/SplitR/versions/0.3
- NOAA HYSPLIT overview: https://www.ready.noaa.gov/HYSPLIT.php
