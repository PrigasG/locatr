#' Primary geocode pass via the US Census batch geocoder
#'
#' Geocodes only the rows marked `ready_for_geocoding`, using the *structured*
#' Census engine (street / city / state / ZIP) rather than a single-line string,
#' which matches reliably more often. Rows not ready are returned untouched with
#' empty coordinate columns so the frame stays rectangular.
#'
#' Volatile Census full-result columns (`tiger_line_id`, `id`) are coerced to
#' character to avoid the `bind_rows()` integer/character type clash that the
#' Census service triggers intermittently between batches.
#'
#' @param data A data frame from [flag_bad_addresses()].
#' @param ... Passed through to [tidygeocoder::geocode()] (e.g. `full_results`).
#'
#' @return `data` with `latitude`, `longitude`, `geocode_method`,
#'   `geocode_pass`, `match_status`, plus Census full-result columns when
#'   `full_results = TRUE`.
#' @export
geocode_census <- function(data, ...) {
  stopifnot("review_status" %in% names(data))

  ready     <- dplyr::filter(data, .data$review_status == "ready_for_geocoding")
  not_ready <- dplyr::filter(data, .data$review_status != "ready_for_geocoding" |
                               is.na(.data$review_status))

  if (nrow(ready) == 0L) {
    # Nothing to geocode here. Guarantee the audit columns exist without
    # clobbering coordinates an earlier tier (e.g. reference backfill) may
    # already have placed.
    return(.ensure_geocode_cols(data))
  }

  geocoded <- ready %>%
    tidygeocoder::geocode(
      street     = address_clean,
      city       = city_clean,
      state      = state_clean,
      postalcode = zip_clean,
      method     = "census",
      lat        = latitude,
      long       = longitude,
      ...
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("tiger_line_id", "id")), as.character),
      geocode_method = "census",
      geocode_pass   = "pass_1_census_structured",
      match_status   = dplyr::if_else(
        !is.na(.data$latitude) & !is.na(.data$longitude),
        "matched", "no_match"
      )
    )

  dplyr::bind_rows(geocoded, not_ready)
}
