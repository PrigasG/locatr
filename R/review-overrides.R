#' Export only the records that still need a human
#'
#' Writes a tidy review CSV of rows whose `review_status` is
#' `"needs_manual_review"`, with blank `manual_*` columns for a reviewer to fill
#' in. Feed the completed file back through [apply_manual_overrides()].
#'
#' @param data A data frame carrying the audit columns.
#' @param path Output CSV path.
#'
#' @return Invisibly, the review tibble that was written.
#' @export
write_geocode_review <- function(data, path) {
  review <- data %>%
    dplyr::filter(.data$review_status == "needs_manual_review") %>%
    dplyr::transmute(
      record_id             = .data$record_id,
      record_name           = .data$record_name,
      full_address_clean    = .data$full_address_clean,
      latitude              = .data$latitude,
      longitude             = .data$longitude,
      location_county       = .pull_if(data, "location_county"),
      location_locality     = .pull_if(data, "location_locality"),
      match_status          = .data$match_status,
      validation_status     = .pull_if(data, "validation_status"),
      bad_address_flag      = .data$bad_address_flag,
      manual_latitude       = NA_real_,
      manual_longitude      = NA_real_,
      manual_county         = NA_character_,
      manual_locality       = NA_character_,
      manual_note           = NA_character_
    )

  readr::write_csv(review, path)
  invisible(review)
}

#' Apply completed manual overrides
#'
#' Joins a reviewer-completed override file (same layout
#' [write_geocode_review()] produced) and coalesces verified coordinates and
#' geography over the automated values. Overrides are themselves bbox-checked so
#' a typo can't drop a point in the ocean.
#'
#' @param data A data frame with `record_id` and the audit columns.
#' @param override_file Path to the completed override CSV.
#' @param bbox Bounding box for validating manual coordinates; see
#'   [region_bbox()].
#'
#' @return `data` with overrides applied and `manual_override_used` set.
#' @export
apply_manual_overrides <- function(data, override_file, bbox = region_bbox("NJ")) {
  if (!file.exists(override_file)) {
    warning("Override file not found: ", override_file, " - returning data unchanged.")
    return(dplyr::mutate(data, manual_override_used = FALSE))
  }

  overrides <- readr::read_csv(override_file, show_col_types = FALSE) %>%
    dplyr::mutate(
      record_id        = as.character(.data$record_id),
      manual_latitude  = as.numeric(.data$manual_latitude),
      manual_longitude = as.numeric(.data$manual_longitude),
      manual_ok        = in_bbox(.data$manual_latitude, .data$manual_longitude, bbox)
    ) %>%
    dplyr::filter(.data$manual_ok) %>%
    dplyr::select("record_id", "manual_latitude", "manual_longitude",
                  dplyr::any_of(c("manual_county", "manual_locality", "manual_note")))

  data %>%
    dplyr::left_join(overrides, by = "record_id") %>%
    dplyr::mutate(
      manual_override_used = !is.na(.data$manual_latitude) & !is.na(.data$manual_longitude),
      latitude  = dplyr::coalesce(.data$manual_latitude, .data$latitude),
      longitude = dplyr::coalesce(.data$manual_longitude, .data$longitude),
      geocode_method = dplyr::if_else(.data$manual_override_used, "manual", .data$geocode_method),
      geocode_pass   = dplyr::if_else(.data$manual_override_used, "pass_3_manual", .data$geocode_pass),
      match_status   = dplyr::if_else(.data$manual_override_used, "matched", .data$match_status),
      review_status  = dplyr::if_else(.data$manual_override_used,
                                      "manual_override_applied", .data$review_status)
    )
}

# Pull a column if present, else return NA of matching length.
.pull_if <- function(data, col) {
  if (col %in% names(data)) data[[col]] else NA
}

.pull_first <- function(data, cols) {
  for (col in cols) {
    if (col %in% names(data)) {
      return(data[[col]])
    }
  }
  NA
}
