#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !! enquo quo_is_null
#' @importFrom stats setNames
## usethis namespace: end
NULL

# Quiet R CMD check notes about non-standard-evaluation column names that are
# referenced via .data$ or created mid-pipeline.
utils::globalVariables(c(
  "address_raw", "city_raw", "zip_raw",
  "full_address_raw", "address_clean", "city_clean", "state_clean", "zip_clean",
  "full_address_clean", "record_id", "record_name",
  "latitude", "longitude", "fb_latitude", "fb_longitude",
  "fb_in_bbox", "fb_status", "use_fb",
  "name_query", "nm_latitude", "nm_longitude", "nm_in_bbox", "nm_status",
  "nm_score", "nm_addr_type", "nm_scored", "nm_high_conf", "nm_low", "use_nm",
  "ref_latitude", "ref_longitude", "ref_in_bbox", "ref_status",
  "ref_county", "ref_locality", "use_ref", ".ref_key",
  ".retryable_for_geocoding",
  "bad_address_flag", "review_status", "match_status",
  "validation_status", "geocode_method", "geocode_pass",
  "location_county", "location_locality", "geography_match_status",
  ".locatr_row_id", ".match_count", "in_bbox", "tiger_line_id", ".",
  ".loc_address", ".loc_city", ".loc_zip",
  "match_score", "matched_address", "match_addr_type",
  "any_changed", ".dedupe_address", ".dedupe_latitude", ".dedupe_longitude",
  "match_confidence", "confidence_reason", "placed_at", "cache_status"
))

#' Region bounding box
#'
#' Returns an approximate latitude/longitude bounding box for a US state (or
#' `"DC"`), used as a fast sanity guard on geocoded coordinates. Presets are
#' deliberately a little generous so legitimate edge locations are not rejected;
#' they are coarse guard boxes, not precise boundaries. For a tighter or
#' non-state region, pass your own named vector, or derive one from an `sf`
#' layer with [bbox_from_sf()].
#'
#' @param region Two-letter US state abbreviation (or `"DC"`), case-insensitive.
#'   Defaults to `"NJ"`.
#'
#' @return A named numeric vector with elements `lat_min`, `lat_max`,
#'   `lon_min`, `lon_max`.
#' @export
#' @examples
#' region_bbox("NJ")
#' region_bbox("CA")
region_bbox <- function(region = "NJ") {
  region <- toupper(region)
  bb <- .STATE_BBOX[[region]]
  if (is.null(bb)) {
    stop("No bounding-box preset for `", region, "`. Use a two-letter US state ",
         "code (or \"DC\"), pass a custom `bbox`, or derive one with ",
         "`bbox_from_sf()`.", call. = FALSE)
  }
  bb
}

# Generous, coarse guard boxes (lat_min, lat_max, lon_min, lon_max) per state.
# NJ is kept at its original production values. Aleutian islands that cross the
# antimeridian are outside AK's box by design.
.STATE_BBOX <- list(
  AL = c(lat_min = 30.1, lat_max = 35.1, lon_min = -88.5, lon_max = -84.9),
  AK = c(lat_min = 51.0, lat_max = 71.6, lon_min = -170.0, lon_max = -129.9),
  AZ = c(lat_min = 31.3, lat_max = 37.1, lon_min = -114.9, lon_max = -109.0),
  AR = c(lat_min = 33.0, lat_max = 36.6, lon_min = -94.7, lon_max = -89.6),
  CA = c(lat_min = 32.5, lat_max = 42.1, lon_min = -124.5, lon_max = -114.1),
  CO = c(lat_min = 36.9, lat_max = 41.1, lon_min = -109.1, lon_max = -102.0),
  CT = c(lat_min = 40.9, lat_max = 42.1, lon_min = -73.8, lon_max = -71.7),
  DE = c(lat_min = 38.4, lat_max = 39.9, lon_min = -75.8, lon_max = -75.0),
  DC = c(lat_min = 38.79, lat_max = 39.00, lon_min = -77.13, lon_max = -76.90),
  FL = c(lat_min = 24.4, lat_max = 31.1, lon_min = -87.7, lon_max = -79.9),
  GA = c(lat_min = 30.3, lat_max = 35.1, lon_min = -85.7, lon_max = -80.8),
  HI = c(lat_min = 18.9, lat_max = 22.3, lon_min = -160.3, lon_max = -154.7),
  ID = c(lat_min = 41.9, lat_max = 49.1, lon_min = -117.3, lon_max = -110.9),
  IL = c(lat_min = 36.9, lat_max = 42.6, lon_min = -91.6, lon_max = -87.4),
  IN = c(lat_min = 37.7, lat_max = 41.8, lon_min = -88.1, lon_max = -84.7),
  IA = c(lat_min = 40.3, lat_max = 43.6, lon_min = -96.7, lon_max = -90.1),
  KS = c(lat_min = 36.9, lat_max = 40.1, lon_min = -102.1, lon_max = -94.5),
  KY = c(lat_min = 36.4, lat_max = 39.2, lon_min = -89.7, lon_max = -81.9),
  LA = c(lat_min = 28.9, lat_max = 33.1, lon_min = -94.1, lon_max = -88.8),
  ME = c(lat_min = 42.9, lat_max = 47.5, lon_min = -71.1, lon_max = -66.9),
  MD = c(lat_min = 37.8, lat_max = 39.8, lon_min = -79.5, lon_max = -75.0),
  MA = c(lat_min = 41.2, lat_max = 42.9, lon_min = -73.6, lon_max = -69.9),
  MI = c(lat_min = 41.6, lat_max = 48.3, lon_min = -90.5, lon_max = -82.3),
  MN = c(lat_min = 43.4, lat_max = 49.5, lon_min = -97.3, lon_max = -89.4),
  MS = c(lat_min = 30.1, lat_max = 35.1, lon_min = -91.7, lon_max = -88.0),
  MO = c(lat_min = 35.9, lat_max = 40.7, lon_min = -95.8, lon_max = -89.0),
  MT = c(lat_min = 44.3, lat_max = 49.1, lon_min = -116.1, lon_max = -103.9),
  NE = c(lat_min = 39.9, lat_max = 43.1, lon_min = -104.1, lon_max = -95.2),
  NV = c(lat_min = 34.9, lat_max = 42.1, lon_min = -120.1, lon_max = -113.9),
  NH = c(lat_min = 42.6, lat_max = 45.4, lon_min = -72.6, lon_max = -70.6),
  NJ = c(lat_min = 38.80, lat_max = 41.40, lon_min = -75.70, lon_max = -73.80),
  NM = c(lat_min = 31.2, lat_max = 37.1, lon_min = -109.1, lon_max = -102.9),
  NY = c(lat_min = 40.4, lat_max = 45.1, lon_min = -79.9, lon_max = -71.8),
  NC = c(lat_min = 33.7, lat_max = 36.7, lon_min = -84.4, lon_max = -75.4),
  ND = c(lat_min = 45.9, lat_max = 49.1, lon_min = -104.1, lon_max = -96.5),
  OH = c(lat_min = 38.3, lat_max = 42.4, lon_min = -84.9, lon_max = -80.5),
  OK = c(lat_min = 33.6, lat_max = 37.1, lon_min = -103.1, lon_max = -94.4),
  OR = c(lat_min = 41.9, lat_max = 46.4, lon_min = -124.6, lon_max = -116.4),
  PA = c(lat_min = 39.7, lat_max = 42.4, lon_min = -80.6, lon_max = -74.6),
  RI = c(lat_min = 41.1, lat_max = 42.1, lon_min = -71.9, lon_max = -71.1),
  SC = c(lat_min = 32.0, lat_max = 35.3, lon_min = -83.4, lon_max = -78.4),
  SD = c(lat_min = 42.4, lat_max = 46.0, lon_min = -104.1, lon_max = -96.4),
  TN = c(lat_min = 34.9, lat_max = 36.8, lon_min = -90.4, lon_max = -81.6),
  TX = c(lat_min = 25.8, lat_max = 36.6, lon_min = -106.7, lon_max = -93.4),
  UT = c(lat_min = 36.9, lat_max = 42.1, lon_min = -114.1, lon_max = -108.9),
  VT = c(lat_min = 42.7, lat_max = 45.1, lon_min = -73.5, lon_max = -71.4),
  VA = c(lat_min = 36.5, lat_max = 39.5, lon_min = -83.7, lon_max = -75.1),
  WA = c(lat_min = 45.5, lat_max = 49.1, lon_min = -124.9, lon_max = -116.9),
  WV = c(lat_min = 37.1, lat_max = 40.7, lon_min = -82.7, lon_max = -77.7),
  WI = c(lat_min = 42.4, lat_max = 47.4, lon_min = -92.9, lon_max = -86.8),
  WY = c(lat_min = 40.9, lat_max = 45.1, lon_min = -111.1, lon_max = -104.0)
)

#' Is a coordinate inside a bounding box?
#'
#' @param lat Numeric vector of latitudes.
#' @param lon Numeric vector of longitudes.
#' @param bbox A named bounding box as returned by [region_bbox()] or supplied
#'   by the caller.
#'
#' @return A logical vector the same length as `lat`/`lon`. `NA` coordinates
#'   return `FALSE`.
#' @export
#' @examples
#' in_bbox(40.2, -74.5, region_bbox("NJ"))
#' in_bbox(40.5, -104.9, region_bbox("NJ")) # a Colorado false-match
in_bbox <- function(lat, lon, bbox) {
  !is.na(lat) & !is.na(lon) &
    lat >= bbox[["lat_min"]] & lat <= bbox[["lat_max"]] &
    lon >= bbox[["lon_min"]] & lon <= bbox[["lon_max"]]
}

#' Build a geocoding bounding box from an sf layer
#'
#' Converts any point, line, or polygon `sf` layer to WGS84 and returns a named
#' latitude/longitude bounding box suitable for [geocode_records()],
#' [geocode_arcgis()], [geocode_by_name()], and [validate_geocodes()]. This is
#' the safest way to keep multi-state geocoding and local geography joins aligned:
#' build or load the geography layer first, then derive the bbox from it.
#'
#' @param geography_shapes An `sf` object.
#' @param buffer Numeric buffer in decimal degrees added to each side of the
#'   bounding box. Defaults to `0.05` to avoid rejecting edge locations.
#'
#' @return A named numeric vector with `lat_min`, `lat_max`, `lon_min`, and
#'   `lon_max`.
#' @export
#' @examples
#' if (interactive()) {
#' areas <- build_local_geography("PA")
#' bbox <- bbox_from_sf(areas)
#' }
bbox_from_sf <- function(geography_shapes, buffer = 0.05) {
  if (!inherits(geography_shapes, "sf") && !inherits(geography_shapes, "sfc")) {
    stop("`geography_shapes` must be an sf or sfc object.", call. = FALSE)
  }
  shapes <- geography_shapes
  if (!is.na(sf::st_crs(shapes))) {
    shapes <- sf::st_transform(shapes, 4326)
  }
  bb <- sf::st_bbox(shapes)
  c(
    lat_min = unname(bb[["ymin"]] - buffer),
    lat_max = unname(bb[["ymax"]] + buffer),
    lon_min = unname(bb[["xmin"]] - buffer),
    lon_max = unname(bb[["xmax"]] + buffer)
  )
}

# Rows that began with a usable address may be retried by looser geocoders after
# a prior tier failed validation. Rows flagged for manual review before any
# geocoder call, such as PO boxes or placeholders, stay out of external services.
# A `missing_zip` flag is informational, not blocking: an address + city row is
# still geocodable, so it remains retryable by the ArcGIS and name tiers.
.retryable_for_geocoding <- function(data) {
  status_retryable <- data$review_status %in%
    c("ready_for_geocoding", "needs_manual_review")

  if ("bad_address_flag" %in% names(data)) {
    blocking <- !is.na(data$bad_address_flag) &
      data$bad_address_flag != "missing_zip"
    status_retryable & !blocking
  } else {
    status_retryable
  }
}

# ArcGIS supports spatial filtering via `searchExtent`. Passing the region bbox
# into the request helps the service return plausible candidates first instead
# of making locatr reject a wrong-state top hit after the fact.
.region_geocoder_dots <- function(method, bbox, dots) {
  if (!identical(tolower(method), "arcgis")) {
    return(dots)
  }

  region_query <- list(
    searchExtent = paste(
      bbox[["lon_min"]], bbox[["lat_min"]],
      bbox[["lon_max"]], bbox[["lat_max"]],
      sep = ","
    ),
    countryCode = "USA"
  )
  dots$custom_query <- utils::modifyList(region_query, dots$custom_query %||% list())
  dots
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
