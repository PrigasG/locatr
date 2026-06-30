#' Flag addresses that should not be blindly geocoded
#'
#' Identifies PO boxes, placeholders, and missing fields so they go straight to
#' review instead of wasting geocoder calls (or producing confident-but-wrong
#' matches). Sets `bad_address_flag` and an initial `review_status`.
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
        is.na(.data$zip_clean) | .data$zip_clean == ""         ~ "missing_zip",
        stringr::str_detect(.data$address_clean, "\\bP\\.?O\\.? BOX\\b") ~ "po_box",
        stringr::str_detect(.data$address_clean, "\\bTBD\\b|\\bUNKNOWN\\b|\\bTEST\\b") ~ "placeholder_address",
        stringr::str_detect(toupper(.data$record_name), "\\bTEST\\b") ~ "test_record",
        TRUE ~ NA_character_
      ),
      review_status = dplyr::if_else(
        is.na(.data$bad_address_flag),
        "ready_for_geocoding",
        "needs_manual_review"
      )
    )
}
