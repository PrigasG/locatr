#' Export the dashboard-ready location crosswalk
#'
#' Selects the final, stable set of columns for Tableau (or any BI tool) and,
#' optionally, writes them to CSV. Audit columns are retained so a reviewer can
#' always see how each coordinate was produced.
#'
#' @param data A fully processed data frame.
#' @param path Optional output CSV path. When `NULL`, nothing is written.
#'
#' @return The crosswalk tibble (also written to `path` when supplied).
#' @export
export_location_crosswalk <- function(data, path = NULL) {
  crosswalk <- data %>%
    dplyr::transmute(
      record_id           = .data$record_id,
      record_name         = .data$record_name,
      address_clean         = .data$address_clean,
      city_clean            = .data$city_clean,
      state_clean           = .data$state_clean,
      zip_clean             = .data$zip_clean,
      full_address_clean    = .data$full_address_clean,
      latitude              = .data$latitude,
      longitude             = .data$longitude,
      location_county       = .pull_if(data, "location_county"),
      location_locality     = .pull_if(data, "location_locality"),
      geocode_method        = .data$geocode_method,
      geocode_pass          = .data$geocode_pass,
      match_status          = .data$match_status,
      validation_status     = .pull_if(data, "validation_status"),
      geography_match_status = .pull_if(data, "geography_match_status"),
      manual_override_used  = .pull_if(data, "manual_override_used"),
      review_status         = .data$review_status
    )

  if (!is.null(path)) {
    readr::write_csv(crosswalk, path)
  }
  crosswalk
}
