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
  ".locatr_row_id", ".match_count", "in_bbox", "tiger_line_id", "."
))

#' Region bounding box
#'
#' Returns an approximate latitude/longitude bounding box for a named region,
#' used as a fast sanity check on geocoded coordinates. Presets are deliberately
#' a little generous so legitimate edge locations are not rejected.
#'
#' @param region Region preset. Currently `"NJ"` is included for the package's
#'   first production workflow. For other regions, pass a custom named vector
#'   with `lat_min`, `lat_max`, `lon_min`, and `lon_max`.
#'
#' @return A named numeric vector with elements `lat_min`, `lat_max`,
#'   `lon_min`, `lon_max`.
#' @export
#' @examples
#' region_bbox("NJ")
region_bbox <- function(region = "NJ") {
  region <- toupper(region)
  if (region == "NJ") {
    return(c(
      lat_min = 38.80,
      lat_max = 41.40,
      lon_min = -75.70,
      lon_max = -73.80
    ))
  }

  stop("No bounding-box preset for `", region, "`. Pass a custom `bbox` ",
       "instead.", call. = FALSE)
}

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
#' \dontrun{
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
