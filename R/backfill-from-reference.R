#' Backfill verified coordinates from a trusted reference table (Tier 0)
#'
#' The authoritative first tier of the cascade. Joins coordinates from a curated
#' key -> coordinates table - an institutional-memory table of records whose
#' location was resolved and checked at some point - over the automated
#' geocoders, so a record that has already been verified never has to be
#' re-geocoded. This is what turns one analyst's manual review into a permanent
#' asset: feed last cycle's completed overrides (or any trusted coordinate list)
#' back in as `reference`, and those rows are placed instantly and exactly.
#'
#' Reference coordinates are still bbox-validated, so a stale or fat-fingered
#' entry cannot drop a point outside the region. Because the reference is
#' authoritative, a matched row is marked `review_status == "reference_backfilled"`
#' and carries valid coordinates, so [geocode_census()] and every later tier skip
#' it automatically - even if its raw address was previously flagged (e.g. a
#' PO box whose true coordinates were verified once).
#'
#' Run this before [geocode_census()] (the cascade does so when you pass
#' `reference =` to [geocode_records()]).
#'
#' @param data A data frame from [flag_bad_addresses()] (or any frame carrying
#'   the key column).
#' @param reference A data frame of verified records: the key column plus
#'   coordinate columns. May also carry county/locality columns. `NULL` or an
#'   empty frame makes this a no-op.
#' @param by Name of the key column shared by `data` and `reference`
#'   (default `"record_id"`).
#' @param lat_col,lon_col Coordinate column names in `reference`
#'   (default `"latitude"`/`"longitude"`).
#' @param county_col,locality_col Optional geography column names in `reference`
#'   to backfill into `location_county`/`location_locality`.
#' @param bbox Bounding box used to reject out-of-region reference coordinates;
#'   see [region_bbox()].
#'
#' @return `data` with reference audit columns `ref_latitude`, `ref_longitude`,
#'   `ref_status`, and, for rows the reference filled, updated
#'   `latitude`/`longitude`/`geocode_method`/`geocode_pass` (`"pass_0_reference"`)/
#'   `match_status`/`review_status`.
#' @export
#' @examples
#' records <- tibble::tibble(
#'   record_id = c("a", "b"),
#'   review_status = c("ready_for_geocoding", "needs_manual_review")
#' )
#' verified <- tibble::tibble(record_id = "b", latitude = 40.22, longitude = -74.76)
#' backfill_from_reference(records, verified)
backfill_from_reference <- function(data, reference, by = "record_id",
                                    lat_col = "latitude", lon_col = "longitude",
                                    county_col = NULL, locality_col = NULL,
                                    bbox = region_bbox("NJ")) {
  stopifnot(by %in% names(data))
  data <- .ensure_geocode_cols(data)

  if (is.null(reference) || nrow(reference) == 0L) {
    return(dplyr::mutate(
      data,
      ref_latitude = NA_real_, ref_longitude = NA_real_, ref_status = NA_character_
    ))
  }
  missing_cols <- setdiff(c(by, lat_col, lon_col), names(reference))
  if (length(missing_cols) > 0L) {
    stop("`reference` is missing required column(s): ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }

  ref <- reference %>%
    dplyr::transmute(
      .ref_key      = as.character(.data[[by]]),
      ref_latitude  = as.numeric(.data[[lat_col]]),
      ref_longitude = as.numeric(.data[[lon_col]]),
      ref_county    = if (!is.null(county_col)) as.character(.data[[county_col]]) else NA_character_,
      ref_locality  = if (!is.null(locality_col)) as.character(.data[[locality_col]]) else NA_character_
    ) %>%
    dplyr::mutate(
      ref_in_bbox = in_bbox(.data$ref_latitude, .data$ref_longitude, bbox),
      ref_status = dplyr::case_when(
        is.na(.data$ref_latitude) | is.na(.data$ref_longitude) ~ "reference_no_coords",
        !.data$ref_in_bbox ~ "reference_outside_region_rejected",
        TRUE ~ "reference_matched"
      ),
      # null out rejected coordinates so they can never be used downstream
      ref_latitude  = dplyr::if_else(.data$ref_in_bbox, .data$ref_latitude,  NA_real_),
      ref_longitude = dplyr::if_else(.data$ref_in_bbox, .data$ref_longitude, NA_real_)
    ) %>%
    # one verified coordinate per key wins (first); guards against a duplicated
    # reference table silently fanning out the join.
    dplyr::distinct(dplyr::across(dplyr::all_of(".ref_key")), .keep_all = TRUE)

  out <- data %>%
    dplyr::mutate(.ref_key = as.character(.data[[by]])) %>%
    dplyr::left_join(ref, by = ".ref_key") %>%
    dplyr::mutate(
      use_ref = !is.na(.data$ref_latitude) & !is.na(.data$ref_longitude),
      latitude       = dplyr::if_else(.data$use_ref, .data$ref_latitude,  .data$latitude),
      longitude      = dplyr::if_else(.data$use_ref, .data$ref_longitude, .data$longitude),
      geocode_method = dplyr::if_else(.data$use_ref, "reference",         .data$geocode_method),
      geocode_pass   = dplyr::if_else(.data$use_ref, "pass_0_reference",  .data$geocode_pass),
      match_status   = dplyr::if_else(.data$use_ref, "matched",           .data$match_status)
    )

  # A verified record is done: mark it terminal so census and the later tiers
  # skip it. Only touched when the frame actually carries a review status.
  if ("review_status" %in% names(out)) {
    out <- dplyr::mutate(
      out,
      review_status = dplyr::if_else(.data$use_ref, "reference_backfilled",
                                     .data$review_status)
    )
  }

  if (!is.null(county_col) || !is.null(locality_col)) {
    if (!"location_county" %in% names(out))   out$location_county   <- NA_character_
    if (!"location_locality" %in% names(out)) out$location_locality <- NA_character_
    out <- out %>%
      dplyr::mutate(
        location_county = dplyr::if_else(.data$use_ref & !is.na(.data$ref_county),
                                         .data$ref_county, .data$location_county),
        location_locality = dplyr::if_else(.data$use_ref & !is.na(.data$ref_locality),
                                           .data$ref_locality, .data$location_locality)
      )
  }

  out %>%
    dplyr::select(-dplyr::any_of(c("use_ref", ".ref_key", "ref_in_bbox",
                                   "ref_county", "ref_locality")))
}
