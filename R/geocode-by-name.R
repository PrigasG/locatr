#' Name-based geocode pass (the "paste it in a browser" tier)
#'
#' For rows still unplaced after the address-based passes, geocodes by record
#' *name* plus city and state rather than the street line. This can resolve
#' campus/landmark addresses (e.g. a unit inside a hospital) that street-range
#' interpolation cannot, because a composite geocoder recognises the named place.
#'
#' Because name lookups are looser than address matching, each hit is scored
#' using the geocoder's match `score` and address type (when available - ArcGIS
#' returns both via `full_results`, which this pass requests automatically). A
#' hit passes cleanly only when it resolves to a precise point address at or
#' above `min_score`; fuzzier hits (a POI, a locality centroid, or a low score)
#' still have their coordinates filled in for context but are marked
#' `match_status == "matched_low_confidence"` and routed to
#' `needs_manual_review` so a person can confirm them. When the geocoder returns
#' no score/type information (e.g. `method = "osm"`), the pass falls back to the
#' previous rule: any in-region match is accepted.
#'
#' Filled rows are tagged `geocode_pass == "pass_4_name_lookup"`. The bounding box
#' still rejects out-of-region hits, but cannot catch a wrong same-state match,
#' which is exactly what the score gate is for.
#'
#' @param data A data frame carrying `record_id`, `record_name`,
#'   `city_clean`, `state_clean`, and the geocode audit columns.
#' @param method tidygeocoder method that accepts free-text queries
#'   (default `"arcgis"`; `"osm"` and `"google"` also work).
#' @param bbox Bounding box used to reject out-of-region matches; see
#'   [region_bbox()].
#' @param min_score Minimum match score (0-100) for a name hit to pass without
#'   review. Hits below this stay reviewable. Default `90`.
#' @param accept_types Address types precise enough to pass without review
#'   (matched case-insensitively against the geocoder's `addr_type`). Default the
#'   point-address types `c("PointAddress", "Subaddress", "StreetAddress")`.
#' @param ... Passed through to [tidygeocoder::geocode()]. `full_results = TRUE`
#'   is requested automatically so scores are available; pass
#'   `full_results = FALSE` to opt out (which also disables score gating).
#'
#' @return `data` with name-lookup audit columns `nm_latitude`, `nm_longitude`,
#'   `nm_score`, `nm_addr_type`, `nm_status`, and updated
#'   `latitude`/`longitude`/`geocode_method`/`geocode_pass`/`match_status` for
#'   rows the name pass filled. Low-confidence fills also set
#'   `review_status == "needs_manual_review"`.
#' @export
geocode_by_name <- function(data, method = "arcgis",
                            bbox = region_bbox("NJ"),
                            min_score = 90,
                            accept_types = c("PointAddress", "Subaddress",
                                             "StreetAddress"),
                            ...) {
  stopifnot(all(c("record_id", "record_name", "city_clean", "state_clean") %in%
                  names(data)))
  dots <- .region_geocoder_dots(method, bbox, list(...))
  if (is.null(dots$full_results)) dots$full_results <- TRUE

  empty_cols <- function(d) {
    dplyr::mutate(d,
                  nm_latitude = NA_real_, nm_longitude = NA_real_,
                  nm_score = NA_real_, nm_addr_type = NA_character_,
                  nm_status = NA_character_)
  }

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
    return(empty_cols(data))
  }

  nm_input <- needs %>%
    dplyr::select("record_id", "name_query")
  nm_args <- c(
    list(nm_input, address = "name_query", method = method,
         lat = "nm_latitude", long = "nm_longitude"),
    dots
  )

  raw <- do.call(tidygeocoder::geocode, nm_args)
  score_col <- .pick_col(raw, "score")
  type_col  <- .pick_col(raw, "addr_type")

  nm <- raw %>%
    dplyr::mutate(
      nm_score = if (!is.null(score_col)) {
        suppressWarnings(as.numeric(.data[[score_col]]))
      } else NA_real_,
      nm_addr_type = if (!is.null(type_col)) as.character(.data[[type_col]]) else NA_character_,
      nm_in_bbox = in_bbox(.data$nm_latitude, .data$nm_longitude, bbox),
      # do we have any confidence signal to gate on?
      nm_scored = !is.na(.data$nm_score) | !is.na(.data$nm_addr_type),
      nm_high_conf =
        !is.na(.data$nm_score) & .data$nm_score >= min_score &
        !is.na(.data$nm_addr_type) &
        toupper(.data$nm_addr_type) %in% toupper(accept_types),
      nm_status = dplyr::case_when(
        is.na(.data$nm_latitude) | is.na(.data$nm_longitude) ~ "name_no_match",
        !.data$nm_in_bbox ~ "name_outside_region_rejected",
        !.data$nm_scored ~ "name_matched",                 # no score info: legacy accept
        .data$nm_high_conf ~ "name_matched_high_confidence",
        TRUE ~ "name_matched_low_confidence"
      ),
      nm_latitude  = dplyr::if_else(.data$nm_in_bbox, .data$nm_latitude,  NA_real_),
      nm_longitude = dplyr::if_else(.data$nm_in_bbox, .data$nm_longitude, NA_real_)
    ) %>%
    dplyr::select("record_id", "nm_latitude", "nm_longitude",
                  "nm_score", "nm_addr_type", "nm_status")

  out <- data %>%
    dplyr::left_join(nm, by = "record_id") %>%
    dplyr::mutate(
      use_nm = !is.na(.data$nm_latitude) & !is.na(.data$nm_longitude) &
        (is.na(.data$latitude) | is.na(.data$longitude) |
           !in_bbox(.data$latitude, .data$longitude, bbox)),
      nm_low = .data$use_nm &
        !is.na(.data$nm_status) & .data$nm_status == "name_matched_low_confidence",
      latitude       = dplyr::if_else(.data$use_nm, .data$nm_latitude, .data$latitude),
      longitude      = dplyr::if_else(.data$use_nm, .data$nm_longitude, .data$longitude),
      geocode_method = dplyr::if_else(.data$use_nm, paste0(method, "_byname"),
                                      .data$geocode_method),
      geocode_pass   = dplyr::if_else(.data$use_nm, "pass_4_name_lookup",
                                      .data$geocode_pass),
      match_status   = dplyr::case_when(
        .data$nm_low ~ "matched_low_confidence",
        .data$use_nm ~ "matched",
        TRUE ~ .data$match_status
      )
    )

  # A low-confidence name hit keeps its coordinates but must be reviewed.
  if ("review_status" %in% names(out)) {
    out <- dplyr::mutate(
      out,
      review_status = dplyr::if_else(.data$nm_low, "needs_manual_review",
                                     .data$review_status)
    )
  }

  out %>%
    dplyr::select(-dplyr::any_of(c("use_nm", "nm_low")))
}
