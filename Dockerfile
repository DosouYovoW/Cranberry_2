# =============================================================
# Dockerfile for your plumber API (fast, reliable binary installs)
# =============================================================

FROM rocker/r2u:4.3.3

# Optional: set a CRAN mirror for consistency
ENV CRAN_REPO=https://cloud.r-project.org
ENV PORT=8000

# Install essential system libraries
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev libcurl4-openssl-dev libssl-dev \
    libudunits2-dev libv8-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages (r2u provides fast precompiled binaries)
RUN Rscript -e "install.packages(c('plumber','jsonlite','tidyverse','tidymodels','bundle','xgboost','compositions','lubridate','recipes','readr','janitor'), repos='${CRAN_REPO}')"

# Copy all project files
WORKDIR /app
COPY . /app

# Expose port used by Render / Lovable
EXPOSE 8000

# Start plumber API
CMD ["R", "-e", "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '8000')))"]
