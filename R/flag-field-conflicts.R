#' Flag cross-field conflicts in location data
#'
#' Catches a class of data-entry errors the geocoder itself will silently accept:
#' a ZIP that cannot belong to the stated state, and a stated county that
#' disagrees with the county the coordinate actually fell in. It adds audit
#' columns rather than changing any coordinate, so a reviewer can decide what to
#' do.
#'
#' The ZIP check is deliberately conservative. It compares the ZIP's leading
#' digit against the USPS regional assignment for the stated state, so it only
#' flags a ZIP that is definitively in the wrong region (for example a
#' `"8xxxx"` ZIP recorded in New Jersey). It never flags a same-region
#' near-miss, and it stays silent when the state is unknown or the ZIP is
#' missing, so it does not produce false positives.
#'
#' The county check compares a stated county column against a geocoded county
#' column (for example `location_county` from [add_county_muni()]), after
#' normalising case and stripping the trailing "County"/"Parish"/"Borough".
#'
#' @param data A data frame of cleaned/geocoded records.
#' @param zip Name of the ZIP column (default `"zip_clean"`). Set to `NULL` to
#'   skip the ZIP check.
#' @param state Name of the state column (default `"state_clean"`).
#' @param stated_county Optional name of a county column supplied in the input.
#'   The county check runs only when this is given.
#' @param geocoded_county Name of the geocoded county column to compare against
#'   (default `"location_county"`).
#'
#' @return `data` with three added columns: `zip_state_conflict` (logical, `NA`
#'   when indeterminate), `county_conflict` (logical, `NA` when either county is
#'   missing), and `field_conflict` (a `"; "`-joined summary such as
#'   `"zip_state"`, `"county"`, or `"zip_state; county"`; `NA` when clean).
#' @export
#' @examples
#' df <- data.frame(
#'   zip_clean = c("07030", "85001"),   # 07 is NJ; 85 is AZ
#'   state_clean = c("NJ", "NJ")
#' )
#' flag_field_conflicts(df)
flag_field_conflicts <- function(data, zip = "zip_clean",
                                 state = "state_clean",
                                 stated_county = NULL,
                                 geocoded_county = "location_county") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  n <- nrow(data)

  zip_state <- if (!is.null(zip) && zip %in% names(data) &&
                   !is.null(state) && state %in% names(data)) {
    .zip_state_conflict(data[[zip]], data[[state]])
  } else {
    rep(NA, n)
  }

  county <- if (!is.null(stated_county) && stated_county %in% names(data) &&
                !is.null(geocoded_county) && geocoded_county %in% names(data)) {
    .county_conflict(data[[stated_county]], data[[geocoded_county]])
  } else {
    rep(NA, n)
  }

  data$zip_state_conflict <- zip_state
  data$county_conflict <- county
  data$field_conflict <- .combine_conflicts(
    list(zip_state = zip_state, county = county)
  )
  data
}

# ---- internals --------------------------------------------------------------

# USPS leading-ZIP-digit -> states that have any ZIP starting with that digit.
# Inclusive on purpose (e.g. NY spans 0 and 1) so the state->digit inversion
# never rejects a legitimate ZIP.
.ZIP_FIRST_DIGIT_STATES <- list(
  "0" = c("CT", "MA", "ME", "NH", "NJ", "NY", "PR", "RI", "VT", "VI"),
  "1" = c("DE", "NY", "PA"),
  "2" = c("DC", "MD", "NC", "SC", "VA", "WV"),
  "3" = c("AL", "FL", "GA", "MS", "TN"),
  "4" = c("IN", "KY", "MI", "OH"),
  "5" = c("IA", "MN", "MT", "ND", "SD", "WI"),
  "6" = c("IL", "KS", "MO", "NE"),
  "7" = c("AR", "LA", "OK", "TX"),
  "8" = c("AZ", "CO", "ID", "NM", "NV", "UT", "WY"),
  "9" = c("AK", "AS", "CA", "GU", "HI", "MP", "OR", "WA")
)

# state -> character vector of valid leading ZIP digits.
.state_zip_digits <- function() {
  inv <- list()
  for (digit in names(.ZIP_FIRST_DIGIT_STATES)) {
    for (st in .ZIP_FIRST_DIGIT_STATES[[digit]]) {
      inv[[st]] <- c(inv[[st]], digit)
    }
  }
  inv
}

.zip_state_conflict <- function(zip, state) {
  zip <- as.character(zip)
  state <- toupper(as.character(state))
  digit <- substr(gsub("\\D", "", zip), 1, 1)
  inv <- .state_zip_digits()

  vapply(seq_along(zip), function(i) {
    d <- digit[i]
    st <- state[i]
    if (is.na(st) || !nzchar(st) || is.na(d) || !nzchar(d)) {
      return(NA)
    }
    allowed <- inv[[st]]
    if (is.null(allowed)) {
      return(NA)   # unknown/foreign state code: cannot judge, so do not flag
    }
    !(d %in% allowed)
  }, logical(1))
}

.county_conflict <- function(stated, geocoded) {
  norm <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- gsub("\\s+(COUNTY|PARISH|BOROUGH|CENSUS AREA|CITY)$", "", x)
    trimws(x)
  }
  s <- norm(stated)
  g <- norm(geocoded)
  ok <- !is.na(s) & nzchar(s) & !is.na(g) & nzchar(g)
  ifelse(ok, s != g, NA)
}

.combine_conflicts <- function(flags) {
  n <- length(flags[[1]])
  out <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    hits <- names(flags)[vapply(flags, function(f) isTRUE(f[i]), logical(1))]
    if (length(hits) > 0L) {
      out[i] <- paste(hits, collapse = "; ")
    }
  }
  out
}
