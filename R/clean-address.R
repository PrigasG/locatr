#' Clean and standardise address fields
#'
#' Normalises raw address, city and ZIP text into geocoder-friendly columns and
#' builds a single-line `full_address_clean`. Column mappings are supplied with
#' tidy-eval (bare column names). The original columns are preserved; cleaned
#' values are written to new `*_clean` columns, and a stable `record_id`
#' character key is created for downstream joins.
#'
#' @param data A data frame of records with addresses.
#' @param id   Bare column name holding a unique record identifier.
#' @param address,city,zip Bare column names for the raw address parts.
#' @param name Optional bare column name for the record name (kept as
#'   `record_name` for review/exports). Defaults to `NULL`.
#' @param state Two-letter state used for all rows. Defaults to `"NJ"` for the
#'   first production workflow; pass another state abbreviation as needed.
#'
#' @return `data` with added columns: `record_id`, `record_name`,
#'   `address_raw`, `city_raw`, `zip_raw`, `address_clean`, `city_clean`,
#'   `state_clean`, `zip_clean`, `full_address_clean`.
#' @export
#' @examples
#' df <- tibble::tibble(
#'   LocationID = "NJ306100", Name = "Hackensack-UMC Mountainside",
#'   Address = "ONE BAY AVE", City = "Montclair", Zip = "7042"
#' )
#' clean_addresses(df, id = LocationID, address = Address,
#'                        city = City, zip = Zip, name = Name)
clean_addresses <- function(data, id, address, city, zip,
                                   name = NULL, state = "NJ") {
  name_q <- rlang::enquo(name)

  out <- data %>%
    dplyr::mutate(
      record_id = as.character({{ id }}),
      address_raw = as.character({{ address }}),
      city_raw    = as.character({{ city }}),
      zip_raw     = as.character({{ zip }}),
      state_clean = state,

      address_clean = .data$address_raw %>%
        stringr::str_to_upper() %>%
        stringr::str_squish() %>%
        # spell out small numbers commonly written as words
        stringr::str_replace_all("\\bONE\\b", "1") %>%
        stringr::str_replace_all("\\bTWO\\b", "2") %>%
        stringr::str_replace_all("\\bTHREE\\b", "3") %>%
        stringr::str_replace_all("\\bFOUR\\b", "4") %>%
        stringr::str_replace_all("\\bFIVE\\b", "5") %>%
        # route / highway normalisation
        stringr::str_replace_all("\\bRTE?\\b", "ROUTE") %>%
        stringr::str_replace_all("\\bHWY\\b", "HIGHWAY") %>%
        stringr::str_replace_all("\\bROUTE\\s+([0-9]+)", "STATE ROUTE \\1") %>%
        stringr::str_replace_all("\\bHIGHWAY\\s+([0-9]+)", "STATE HIGHWAY \\1") %>%
        # common abbreviations
        stringr::str_replace_all("\\bMT\\b", "MOUNT") %>%
        stringr::str_replace_all("\\bAVE\\b", "AVENUE") %>%
        stringr::str_replace_all("\\bRD\\b", "ROAD") %>%
        stringr::str_replace_all("\\bBLVD\\b", "BOULEVARD") %>%
        stringr::str_replace_all("\\bST\\b", "STREET") %>%
        stringr::str_replace_all("\\bDR\\b", "DRIVE") %>%
        stringr::str_replace_all("\\bLN\\b", "LANE") %>%
        # strip secondary-unit designators that confuse the Census matcher
        stringr::str_replace_all(
          ",?\\s*(SUITE|STE|STES|UNIT|BLDG|BUILDING|FLOOR|FLR|ROOM|RM)\\s+[A-Z0-9\\-]+",
          ""
        ) %>%
        stringr::str_squish(),

      city_clean = .data$city_raw %>%
        stringr::str_to_upper() %>%
        stringr::str_squish(),

      zip_clean = stringr::str_pad(
        stringr::str_extract(.data$zip_raw, "\\d{5}"),
        width = 5, side = "left", pad = "0"
      ),

      full_address_clean = paste0(
        .data$address_clean, ", ",
        .data$city_clean, ", ",
        .data$state_clean, " ",
        .data$zip_clean
      )
    )

  if (!quo_is_null(name_q)) {
    out <- out %>% dplyr::mutate(record_name = as.character(!!name_q))
  } else {
    out$record_name <- NA_character_
  }

  out
}
