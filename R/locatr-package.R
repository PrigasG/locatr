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
  "address_clean", "city_clean", "state_clean", "zip_clean",
  "full_address_clean", "record_id", "record_name",
  "latitude", "longitude", "fb_latitude", "fb_longitude",
  "name_query", "nm_latitude", "nm_longitude", "nm_in_bbox", "nm_status",
  "ref_latitude", "ref_longitude", "ref_in_bbox", "ref_status",
  "ref_county", "ref_locality", "use_ref", ".ref_key",
  ".retryable_for_geocoding",
  "bad_address_flag", "review_status", "match_status",
  "validation_status", "geocode_method", "geocode_pass",
  "location_county", "location_locality", "geography_match_status",
  "in_bbox", "tiger_line_id"
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

# Rows that began with a usable address may be retried by looser geocoders after
# a prior tier failed validation. Rows flagged for manual review before any
# geocoder call, such as PO boxes or placeholders, stay out of external services.
.retryable_for_geocoding <- function(data) {
  status_retryable <- data$review_status %in%
    c("ready_for_geocoding", "needs_manual_review")

  if ("bad_address_flag" %in% names(data)) {
    status_retryable & is.na(data$bad_address_flag)
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
