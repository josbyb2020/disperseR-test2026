# disperseR Linux Testing - Downloads from GitHub
FROM rocker/r-ver:4.3.2

LABEL description="Cross-platform testing for disperseR v0.2.0 from GitHub fork"

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libnetcdf-dev \
    libgit2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    cmake \
    make \
    libtiff-dev \
    libjpeg-dev \
    wget \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install R dependencies
RUN R -e "install.packages(c('remotes', 'data.table', 'sf', 'terra', 'fst', 'ggplot2', 'ncdf4', 'lubridate'), repos='https://cloud.r-project.org/')"

# Install splitr from GitHub (provides HYSPLIT binaries)
RUN R -e "remotes::install_github('rich-iannone/splitr')"

# Install disperseR v0.2.0 from YOUR GitHub fork (NOT the deprecated upstream)
RUN R -e "remotes::install_github('josbyb2020/disperseR-test2026')"

# Simple test script
RUN echo 'cat("=== disperseR Linux Test ===\n"); \
    library(disperseR); \
    library(splitr); \
    cat("disperseR version:", as.character(packageVersion("disperseR")), "\n"); \
    cat("splitr version:", as.character(packageVersion("splitr")), "\n"); \
    cat("SUCCESS: All packages loaded\n")' > /test.R

CMD ["Rscript", "/test.R"]
