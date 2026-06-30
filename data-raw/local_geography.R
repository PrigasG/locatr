# data-raw/local_geography.R
# ---------------------------------------------------------------------------
# Builds an optional packaged `local_geography` sf object used by
# add_local_geography() when no boundary layer is supplied.
#
# This script is NOT run at install time. Run it once locally after downloading
# an authoritative locality or service-area boundary file, then commit the
# resulting data/local_geography.rda if you want packaged defaults.
#
# Example source: NJ Office of GIS (NJOGIS) "Municipal Boundaries of NJ".
#   https://njogis-newjersey.opendata.arcgis.com/
# ---------------------------------------------------------------------------

library(sf)
library(dplyr)

# 1. Point this at the downloaded boundary file ----------------------------
src_path <- "data-raw/source/local_boundaries.shp"  # <- edit me

local_raw <- sf::st_read(src_path, quiet = TRUE)

# 2. Keep a small, dashboard-friendly set of columns -----------------------
# Inspect names(local_raw) and adjust these to the real schema.
local_geography <- local_raw %>%
  transmute(
    MUN_NAME = as.character(.data$MUN),
    COUNTY   = as.character(.data$COUNTY),
    MUN_CODE = as.character(.data$MUN_CODE),
    geometry
  ) %>%
  # simplify geometry to keep the package light; choose a suitable local CRS
  sf::st_transform(3424) %>%
  sf::st_simplify(dTolerance = 20) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

# 3. Save to data/ ----------------------------------------------------------
usethis::use_data(local_geography, overwrite = TRUE, compress = "xz")
