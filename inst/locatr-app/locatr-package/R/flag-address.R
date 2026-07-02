#' Flag addresses that should not be blindly geocoded
#'
#' Identifies PO boxes, placeholders, and missing fields so they go straight to
#' review instead of wasting geocoder calls (or producing confident-but-wrong
#' matches). Sets `bad_address_flag` and an initial `review_status`.
#'
#' A missing ZIP is recorded as `bad_address_flag == "missing_zip"` for audit,
#' but it does **not** block geocoding: as long as the address and city are
#' present, the row stays `ready_for_geocoding` (Census matches on
#' street/city/state and ArcGIS on the single-line address). Only genuinely
#' unusable rows - missing address or city, PO boxes, placeholders, test
#' records - are routed to `needs_manual_review`.
#'
#' @param data A data frame from [clean_addresses()].
#'
#' @return `data` with added columns `bad_address_flag` and `review_status`.
#'   Rows fit for geocoding get `review_status == "ready_for_geocoding"`.
#' @export
#' @examples
#' df <- tibble::tibble(
#'   record_id = c("a", "b"),
#'   address_clean = c("100 MAIN STREET", "PO BOX 42"),
#'   city_clean = c("TRENTON", "TRENTON"),
#'   zip_clean = c("08608", "08608"),
#'   record_name = c("Real Site", "Mailbox Co")
#' )
#' flag_bad_addresses(df)
flag_bad_addresses <- function(data) {
  data %>%
    dplyr::mutate(
      bad_address_flag = dplyr::case_when(
        is.na(.data$address_clean) | .data$address_clean == "" ~ "missing_address",
        is.na(.data$city_clean) | .data$city_clean == ""       ~ "missing_city",
        stringr::str_detect(.data$address_clean, "\\bP\\.?O\\.? BOX\\b") ~ "po_box",
        stringr::str_detect(.data$address_clean, "\\bTBD\\b|\\bUNKNOWN\\b|\\bTEST\\b") ~ "placeholder_address",
        stringr::str_detect(toupper(.data$record_name), "\\bTEST\\b") ~ "test_record",
        is.na(.data$zip_clean) | .data$zip_clean == ""         ~ "missing_zip",
        TRUE ~ NA_character_
      ),
      # missing_zip is informational only - a real address + city is still
      # geocodable, so those rows stay ready. Everything else with a flag goes
      # to manual review.
      review_status = dplyr::case_when(
        is.na(.data$bad_address_flag) ~ "ready_for_geocoding",
        .data$bad_address_flag == "missing_zip" ~ "ready_for_geocoding",
        TRUE ~ "needs_manual_review"
      )
    )
}
