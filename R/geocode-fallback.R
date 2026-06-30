#' Fallback geocode pass via ArcGIS (Google-like fuzzy matching)
#'
#' For rows the Census pass could not place inside the configured region,
#' re-geocodes with
#' a composite geocoder (default ArcGIS: free, no API key, fuzzy matching close
#' to Google) using the single-line `full_address_clean`. ArcGIS requests are
#' constrained to the region bbox when possible, and results are still guarded
#' against the bounding box so out-of-region false matches are discarded before
#' coordinates are coalesced back into `latitude`/`longitude`.
#'
#' @param data A data frame from [geocode_census()] (or after
#'   [validate_geocodes()]).
#' @param method tidygeocoder method for the fallback (default `"arcgis"`).
#'   `"google"` also works if `GOOGLEGEOCODE_API_KEY` is set.
#' @param bbox Bounding box used to reject out-of-region matches; see
#'   [region_bbox()].
#' @param ... Passed through to [tidygeocoder::geocode()].
#'
#' @return `data` with fallback columns `fb_latitude`, `fb_longitude`,
#'   `fb_status`, and updated `latitude`, `longitude`, `geocode_method`,
#'   `geocode_pass`, `match_status` for rows the fallback filled.
#' @export
geocode_fallback <- function(data, method = "arcgis",
                             bbox = region_bbox("NJ"), ...) {
  stopifnot(all(c("record_id", "latitude", "longitude") %in% names(data)))
  dots <- .region_geocoder_dots(method, bbox, list(...))

  needs_fallback <- data %>%
    dplyr::mutate(.retryable_for_geocoding = .retryable_for_geocoding(.)) %>%
    dplyr::filter(
      .data$.retryable_for_geocoding,
      is.na(.data$latitude) | is.na(.data$longitude) |
        !in_bbox(.data$latitude, .data$longitude, bbox)
    ) %>%
    dplyr::select(-".retryable_for_geocoding")

  if (nrow(needs_fallback) == 0L) {
    return(
      data %>%
        dplyr::mutate(
          fb_latitude = NA_real_, fb_longitude = NA_real_,
          fb_status = NA_character_
        )
    )
  }

  fb_input <- needs_fallback %>%
    dplyr::select("record_id", "full_address_clean")
  fb_args <- c(
    list(fb_input, address = "full_address_clean", method = method,
         lat = "fb_latitude", long = "fb_longitude"),
    dots
  )

  fb <- do.call(tidygeocoder::geocode, fb_args) %>%
    dplyr::mutate(
      fb_in_bbox = in_bbox(.data$fb_latitude, .data$fb_longitude, bbox),
      fb_status = dplyr::case_when(
        is.na(.data$fb_latitude) | is.na(.data$fb_longitude) ~ "fallback_no_match",
        !.data$fb_in_bbox ~ "fallback_outside_region_rejected",
        TRUE ~ "fallback_matched"
      ),
      # null out coordinates that fell outside the region so they never map
      fb_latitude  = dplyr::if_else(.data$fb_in_bbox, .data$fb_latitude, NA_real_),
      fb_longitude = dplyr::if_else(.data$fb_in_bbox, .data$fb_longitude, NA_real_)
    ) %>%
    dplyr::select("record_id", "fb_latitude", "fb_longitude", "fb_status")

  data %>%
    dplyr::left_join(fb, by = "record_id") %>%
    dplyr::mutate(
      use_fb = !is.na(.data$fb_latitude) & !is.na(.data$fb_longitude) &
        (is.na(.data$latitude) | is.na(.data$longitude) |
           !in_bbox(.data$latitude, .data$longitude, bbox)),
      latitude       = dplyr::if_else(.data$use_fb, .data$fb_latitude, .data$latitude),
      longitude      = dplyr::if_else(.data$use_fb, .data$fb_longitude, .data$longitude),
      geocode_method = dplyr::if_else(.data$use_fb, method, .data$geocode_method),
      geocode_pass   = dplyr::if_else(.data$use_fb, "pass_2_fallback", .data$geocode_pass),
      match_status   = dplyr::if_else(.data$use_fb, "matched", .data$match_status)
    ) %>%
    dplyr::select(-"use_fb")
}
