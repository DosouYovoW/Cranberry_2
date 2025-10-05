# =============================================================
# Dockerfile for your plumber API (hardened)
# =============================================================
FROM rocker/r-ver:4.3.3

# Set UTF-8 locale to avoid parseUTF8() issues
ENV LC_ALL=C.UTF-8 \
    LANG=C.UTF-8 \
    TZ=Etc/UTC

# System libs for tidyverse/xgboost/recipes/janitor/V8/udunits2, plus build tools & pandoc
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    libatlas-base-dev libgfortran5 libglpk-dev libudunits2-dev \
    libprotobuf-dev protobuf-compiler \
    libv8-dev libharfbuzz-dev libfribidi-dev libicu-dev \
    pandoc make g++ \
 && rm -rf /var/lib/apt/lists/*

# Install R packages (pin to CRAN mirror, avoid update.packages in Docker)
# install2.r comes from littler in the rocker image
ENV CRAN_REPO=https://cloud.r-project.org
RUN install2.r --error --repos ${CRAN_REPO} \
    plumber jsonlite tidyverse tidymodels bundle xgboost \
    compositions lubridate recipes readr janitor

# Copy project files
WORKDIR /app
COPY . /app

# Expose port (used by Render/Railway/etc.)
ENV PORT=8000
EXPOSE 8000

# Start plumber API
CMD ["R", "-e", "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=as.integer(Sys.getenv('PORT','8000')))"]
