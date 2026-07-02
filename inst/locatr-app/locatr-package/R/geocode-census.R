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
#' @param cache Optional [locatr_cache()]. When supplied, rows whose structured
#'   query is already cached are filled from it instead of re-querying Census.
#' @param refresh If `TRUE`, ignore cached entries and re-query, overwriting
#'   them. Defaults to `FALSE`.
#'
#' @return `data` with `latitude`, `longitude`, `geocode_method`,
#'   `geocode_pass`, `match_status`, plus Census full-result columns when
#'   `full_results = TRUE` (full-result columns are not stored in the cache, so
#'   cache-filled rows omit them).
#' @export
geocode_census <- function(data, ..., cache = NULL, refresh = FALSE) {
  stopifnot("review_status" %in% names(data))
  .validate_cache_args(cache, refresh)

  ready     <- dplyr::filter(data, .data$review_status == "ready_for_geocoding")
  not_ready <- dplyr::filter(data, .data$review_status != "ready_for_geocoding" |
                               is.na(.data$review_status))

  if (nrow(ready) == 0L) {
    # Nothing to geocode here. Guarantee the audit columns exist without
    # clobbering coordinates an earlier tier (e.g. reference backfill) may
    # already have placed.
    return(.ensure_geocode_cols(data))
  }

  ready <- .census_fill_coords(ready, cache, refresh, ...)

  geocoded <- ready %>%
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

# Fill latitude/longitude on the ready rows via the Census structured geocoder,
# reusing cached coordinates when a cache is supplied. The `cache = NULL` branch
# is the original call, verbatim, so mocked tests and behaviour are unchanged.
.census_fill_coords <- function(ready, cache, refresh, ...) {
  dots <- list(...)
  live <- function(d) {
    do.call(
      tidygeocoder::geocode,
      c(list(
        d,
        street     = quote(address_clean),
        city       = quote(city_clean),
        state      = quote(state_clean),
        postalcode = quote(zip_clean),
        method     = "census",
        lat        = quote(latitude),
        long       = quote(longitude)
      ), dots)
    )
  }
  if (is.null(cache)) {
    return(live(ready))
  }

  coords <- .batch_geocode_cached(
    ready, .census_query_vec(ready), method = "census_structured",
    params = .census_params(dots),
    cache = cache, refresh = refresh,
    run = function(d) {
      g <- live(d)
      tibble::tibble(record_id = g$record_id, latitude = g$latitude,
                     longitude = g$longitude)
    }
  )
  ready$latitude <- coords$latitude
  ready$longitude <- coords$longitude
  ready
}
