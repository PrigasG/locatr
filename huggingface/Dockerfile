# Hugging Face Space (Docker SDK) for the locatr demo app.
# rocker/geospatial ships R + tidyverse + sf/GDAL/GEOS/PROJ, so the heavy
# geospatial system libraries are already in place.
FROM rocker/geospatial:4.4.1

# A couple of system libraries the R packages below link against.
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# App + pipeline R dependencies. install2.r pulls Linux binaries from the
# Posit Public Package Manager that the rocker images are configured with.
RUN install2.r --error --skipinstalled \
      shiny \
      bslib \
      DT \
      leaflet \
      readr \
      readxl \
      tibble \
      writexl \
      arrow \
      tidygeocoder \
      tigris \
      remotes

# Cache-bust the locatr install layer whenever GitHub master changes.
ARG LOCATR_REF=ba05d1e
ENV LOCATR_REF=${LOCATR_REF}
ADD https://api.github.com/repos/PrigasG/locatr/commits/${LOCATR_REF} /tmp/locatr-ref.json

# Install locatr itself (ships the app under inst/locatr-app).
RUN R -e "remotes::install_github('PrigasG/locatr', ref = Sys.getenv('LOCATR_REF', 'ba05d1e'), dependencies = c('Depends', 'Imports'), upgrade = 'never')"

# Hugging Face Spaces route traffic to port 7860.
ENV PORT=7860
EXPOSE 7860

# Run the app bundled inside the installed package.
CMD ["R", "-e", "shiny::runApp(system.file('locatr-app', package = 'locatr'), host = '0.0.0.0', port = 7860)"]
