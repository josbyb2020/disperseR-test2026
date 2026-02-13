# Future Validation Plan (Networked + Linux)

This plan captures the remaining checks that require network access or a Linux
runtime. It is intended for a clean environment with outbound internet access
and a running Docker daemon.

## Networked CRAN checks
1. Install splitr (GitHub):
   - `Rscript -e "if (!requireNamespace('remotes', quietly=TRUE)) install.packages('remotes', repos='https://cloud.r-project.org'); remotes::install_github('rich-iannone/splitr', upgrade='never')"`
2. Re-run CRAN checks with suggests enabled:
   - `R CMD check --as-cran --no-manual disperseR_0.2.1.tar.gz`
3. Confirm URL validation passes (links in README and vignettes).

## Linux cross-platform validation
1. Start Docker Desktop (or Docker daemon).
2. Build the Linux check image:
   - `docker build -t disperser-linux-check:latest .`
3. Run the container and execute:
   - `R CMD check --as-cran --no-manual disperseR_0.2.1.tar.gz`

## Expected outcomes
- No CRAN URL NOTE when network access is available.
- No ERROR for missing splitr when suggests are installed.
- Consistent test pass status on Linux.
