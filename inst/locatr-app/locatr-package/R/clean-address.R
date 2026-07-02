#' Clean and standardise address fields
#'
#' Normalises raw address, city and ZIP text into geocoder-friendly columns and
#' builds a single-line `full_address_clean`. Column mappings are supplied with
#' tidy-eval (bare column names). Original address pieces are preserved in
#' `*_raw` columns. If the input already contains a `full_address_clean` column
#' in any case style (for example `Full_Address_Clean`), locatr preserves that
#' user-supplied value as `full_address_raw` so there is only one canonical
#' `full_address_clean` column after cleaning.
#'
#' Only `address` and `city` are required. When `id` is omitted, a surrogate
#' `record_id` is generated from the row position. When `zip` is omitted (or
#' empty), `zip_clean` is `NA` and `full_address_clean` is built without a
#' trailing ZIP, so an address + city + state row is still geocodable. Supplying
#' a ZIP improves Census structured-match precision but is no longer mandatory.
#'
#' @param data A data frame of records with addresses.
#' @param address,city Bare column names for the raw address and city. Required.
#' @param id Optional bare column name holding a unique record identifier. When
#'   omitted, `record_id` is generated from the row number.
#' @param zip Optional bare column name for the raw ZIP/postal code. When
#'   omitted, `zip_clean` is `NA`.
#' @param name Optional bare column name for the record name (kept as
#'   `record_name` for review/exports). Defaults to `NULL`.
#' @param state Two-letter state used for all rows. Defaults to `"NJ"` for the
#'   first production workflow; pass another state abbreviation as needed.
#'
#' @return `data` with added columns: `record_id`, `record_name`,
#'   `address_raw`, `city_raw`, `zip_raw`, optional `full_address_raw`,
#'   `address_clean`, `city_clean`, `state_clean`, `zip_clean`,
#'   `full_address_clean`.
#' @export
#' @examples
#' df <- tibble::tibble(
#'   LocationID = "NJ306100", Name = "Hackensack-UMC Mountainside",
#'   Address = "ONE BAY AVE", City = "Montclair", Zip = "7042"
#' )
#' clean_addresses(df, id = LocationID, address = Address,
#'                        city = City, zip = Zip, name = Name)
#'
#' # address + city only (surrogate id, no ZIP)
#' clean_addresses(tibble::tibble(Address = "100 Main St", City = "Trenton"),
#'                 address = Address, city = City)
clean_addresses <- function(data, id = NULL, address, city, zip = NULL,
                                   name = NULL, state = "NJ") {
  id_q   <- rlang::enquo(id)
  zip_q  <- rlang::enquo(zip)
  name_q <- rlang::enquo(name)
  data <- .protect_existing_full_address_clean(data)

  record_id <- if (!quo_is_null(id_q)) {
    as.character(rlang::eval_tidy(id_q, data))
  } else {
    as.character(seq_len(nrow(data)))
  }
  zip_raw <- if (!quo_is_null(zip_q)) {
    as.character(rlang::eval_tidy(zip_q, data))
  } else {
    rep(NA_character_, nrow(data))
  }

  out <- data %>%
    dplyr::mutate(
      record_id   = !!record_id,
      address_raw = as.character({{ address }}),
      city_raw    = as.character({{ city }}),
      zip_raw     = !!zip_raw,
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

      zip_clean = .data$zip_raw %>%
        stringr::str_remove_all("\\D") %>%
        stringr::str_sub(1, 5) %>%
        stringr::str_pad(width = 5, side = "left", pad = "0") %>%
        dplyr::na_if("00000"),

      full_address_clean = .make_full_address(
        .data$address_clean, .data$city_clean,
        .data$state_clean, .data$zip_clean
      )
    )

  if (!quo_is_null(name_q)) {
    out <- out %>% dplyr::mutate(record_name = as.character(!!name_q))
  } else {
    out$record_name <- NA_character_
  }

  out
}

.protect_existing_full_address_clean <- function(data) {
  existing <- names(data)[toupper(names(data)) == "FULL_ADDRESS_CLEAN"]
  if (length(existing) == 0) {
    return(data)
  }

  keep <- existing[1]
  names(data)[names(data) == keep] <- .available_name(names(data), "full_address_raw")

  drop <- setdiff(existing, keep)
  if (length(drop) > 0) {
    data <- data[, !names(data) %in% drop, drop = FALSE]
  }
  data
}

.available_name <- function(existing, candidate) {
  if (!candidate %in% existing) {
    return(candidate)
  }

  i <- 1L
  repeat {
    next_name <- paste0(candidate, "_", i)
    if (!next_name %in% existing) {
      return(next_name)
    }
    i <- i + 1L
  }
}

# Build the single-line address, appending the ZIP only when present so a
# missing ZIP does not leave a trailing " NA" that confuses the geocoder.
.make_full_address <- function(address, city, state, zip) {
  base <- paste0(address, ", ", city, ", ", state)
  dplyr::if_else(
    is.na(zip) | zip == "",
    base,
    paste0(base, " ", zip)
  )
}
