# =============================================================
# Dockerfile for your plumber API
# =============================================================

FROM rocker/r-ver:4.3.3

# System libs needed by tidyverse, xgboost, keras, etc.
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    libatlas-base-dev libgfortran5 libglpk-dev libudunits2-dev \
    libprotobuf-dev protobuf-compiler \
    libv8-dev libharfbuzz-dev libfribidi-dev libicu-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r --error \
    plumber jsonlite tidyverse tidymodels bundle xgboost \
    compositions lubridate recipes readr janitor \
    && R -q -e "update.packages(ask=FALSE, repos='https://cloud.r-project.org')"

# Copy project files
WORKDIR /app
COPY . /app

# Expose port for Render/Railway/Lovable
ENV PORT=8000
EXPOSE 8000

# Start plumber API
CMD ["R", "-e", "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=as.integer(Sys.getenv('PORT','8000')))"]
