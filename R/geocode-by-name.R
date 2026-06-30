#' Name-based geocode pass (the "paste it in a browser" tier)
#'
#' For rows still unplaced after the address-based passes, geocodes by record
#' *name* plus city and state rather than the street line. This can resolve
#' campus/landmark addresses (e.g. a unit inside a hospital) that street-range
#' interpolation cannot, because a composite geocoder recognises the named place.
#'
#' Because name matching is looser than address matching, filled rows are tagged
#' `geocode_pass == "pass_4_name_lookup"` so a reviewer can spot-check them. The
#' bounding box still rejects out-of-region hits, but cannot catch a wrong
#' same-state match, so treat these as lower-confidence by design.
#'
#' @param data A data frame carrying `record_id`, `record_name`,
#'   `city_clean`, `state_clean`, and the geocode audit columns.
#' @param method tidygeocoder method that accepts free-text queries
#'   (default `"arcgis"`; `"osm"` and `"google"` also work).
#' @param bbox Bounding box used to reject out-of-region matches; see
#'   [region_bbox()].
#' @param ... Passed through to [tidygeocoder::geocode()] (e.g.
#'   `full_results = TRUE` to inspect the ArcGIS match `score`).
#'
#' @return `data` with name-lookup columns `nm_latitude`, `nm_longitude`,
#'   `nm_status`, and updated `latitude`/`longitude`/`geocode_method`/
#'   `geocode_pass`/`match_status` for rows the name pass filled.
#' @export
geocode_by_name <- function(data, method = "arcgis",
                            bbox = region_bbox("NJ"), ...) {
  stopifnot(all(c("record_id", "record_name", "city_clean", "state_clean") %in%
                  names(data)))
  dots <- .region_geocoder_dots(method, bbox, list(...))

  needs <- data %>%
    dplyr::mutate(.retryable_for_geocoding = .retryable_for_geocoding(.)) %>%
    dplyr::filter(
      .data$.retryable_for_geocoding,
      is.na(.data$latitude) | is.na(.data$longitude) |
        !in_bbox(.data$latitude, .data$longitude, bbox),
      !is.na(.data$record_name), .data$record_name != ""
    ) %>%
    dplyr::select(-".retryable_for_geocoding") %>%
    dplyr::mutate(
      name_query = paste0(.data$record_name, ", ",
                          .data$city_clean, ", ", .data$state_clean)
    )

  if (nrow(needs) == 0L) {
    return(
      dplyr::mutate(data,
                    nm_latitude = NA_real_, nm_longitude = NA_real_,
                    nm_status = NA_character_)
    )
  }

  nm_input <- needs %>%
    dplyr::select("record_id", "name_query")
  nm_args <- c(
    list(nm_input, address = "name_query", method = method,
         lat = "nm_latitude", long = "nm_longitude"),
    dots
  )

  nm <- do.call(tidygeocoder::geocode, nm_args) %>%
    dplyr::mutate(
      nm_in_bbox = in_bbox(.data$nm_latitude, .data$nm_longitude, bbox),
      nm_status = dplyr::case_when(
        is.na(.data$nm_latitude) | is.na(.data$nm_longitude) ~ "name_no_match",
        !.data$nm_in_bbox ~ "name_outside_region_rejected",
        TRUE ~ "name_matched"
      ),
      nm_latitude  = dplyr::if_else(.data$nm_in_bbox, .data$nm_latitude,  NA_real_),
      nm_longitude = dplyr::if_else(.data$nm_in_bbox, .data$nm_longitude, NA_real_)
    ) %>%
    dplyr::select("record_id", "nm_latitude", "nm_longitude", "nm_status")

  data %>%
    dplyr::left_join(nm, by = "record_id") %>%
    dplyr::mutate(
      use_nm = !is.na(.data$nm_latitude) & !is.na(.data$nm_longitude) &
        (is.na(.data$latitude) | is.na(.data$longitude) |
           !in_bbox(.data$latitude, .data$longitude, bbox)),
      latitude       = dplyr::if_else(.data$use_nm, .data$nm_latitude, .data$latitude),
      longitude      = dplyr::if_else(.data$use_nm, .data$nm_longitude, .data$longitude),
      geocode_method = dplyr::if_else(.data$use_nm, paste0(method, "_byname"),
                                      .data$geocode_method),
      geocode_pass   = dplyr::if_else(.data$use_nm, "pass_4_name_lookup",
                                      .data$geocode_pass),
      match_status   = dplyr::if_else(.data$use_nm, "matched", .data$match_status)
    ) %>%
    dplyr::select(-"use_nm")
}
