# data-raw/local_geography.R
# ---------------------------------------------------------------------------
# Builds the packaged `local_geography` sf object that add_local_geography()
# uses by default. For production this is the authoritative NJ municipal
# boundary layer, so `location_locality` on a finished crosswalk is always
# traceable to a named boundary source (not the geocoder's guess, and not
# Census "county subdivision" names that merely look municipal).
#
# This script is NOT run at install time. Run it once locally after downloading
# the boundary file, then commit the resulting data/local_geography.rda.
#
# For other states (or a quick nationwide baseline), skip the manual download
# and use build_local_geography(state, geography = "county_subdivision"), which
# pulls authoritative Census TIGER/Line boundaries via the tigris package and
# returns the same location_county / location_locality schema. NJGIN is used
# here because it is the authoritative municipal layer for NJ specifically.
#
# Source: NJGIN / NJOGIS "Municipal Boundaries of NJ (Hosted, 3424)".
#   https://njogis-newjersey.opendata.arcgis.com/datasets/municipal-boundaries-of-nj-hosted-3424
#   https://www.nj.gov/njgin/edata/boundaries
# Download the shapefile or GeoJSON ("Download" -> full dataset) into
# data-raw/source/ and point `src_path` at it. The hosted layer is published in
# NJ State Plane (EPSG:3424); we reproject to WGS84 (EPSG:4326) for the join.
# ---------------------------------------------------------------------------

library(sf)
library(dplyr)

# 1. Point this at the downloaded boundary file ----------------------------
src_path <- "data-raw/source/Municipal_Boundaries_of_NJ.shp"  # <- edit me

munis_raw <- sf::st_read(src_path, quiet = TRUE)

# The hosted NJ layer carries municipality and county names in `MUN` and
# `COUNTY` (with `GNIS_NAME`, `MUN_CODE`, `MUN_TYPE` also present). Inspect with
# names(munis_raw) if your download differs, and adjust the two columns below.
stopifnot(all(c("MUN", "COUNTY") %in% names(munis_raw)))

# 2. Reduce to the production schema add_local_geography() expects ----------
# Naming the attributes location_county / location_locality means the default
# auto-detection picks them up with no extra arguments.
local_geography <- munis_raw %>%
  sf::st_make_valid() %>%
  dplyr::transmute(
    location_county   = as.character(.data$COUNTY),
    location_locality = as.character(.data$MUN)
  ) %>%
  # simplify in the projected CRS (feet) to keep the packaged data light, then
  # publish in WGS84 to match geocoded latitude/longitude.
  sf::st_transform(3424) %>%
  sf::st_simplify(dTolerance = 20) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid()

# 3. Save to data/ ----------------------------------------------------------
usethis::use_data(local_geography, overwrite = TRUE, compress = "xz")
