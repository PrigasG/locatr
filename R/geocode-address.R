#' Look up a single address and return ranked candidate points
#'
#' An interactive, one-shot companion to the batch pipeline: pass a literal
#' address (no data frame) and get back a tibble of candidate matches ranked by
#' geocoder confidence, highest first. Each candidate carries its coordinates,
#' match score, and ArcGIS address type, and - unless turned off - the county
#' and municipality the point falls in. Handy for spot-checking one address, or
#' for letting a reviewer eyeball the plausible locations the cascade would
#' choose from.
#'
#' The address text is normalised with [clean_addresses()] (so it benefits from
#' the same abbreviation/secondary-unit cleaning), then sent to the free ArcGIS
#' `findAddressCandidates` service, which returns several scored candidates for a
#' single query. Use `min_score` to keep only candidates at or above a confidence
#' threshold (for example `min_score = 90`), and `max_candidates` to cap how many
#' come back.
#'
#' @param address Single-line street address as a length-1 character string.
#' @param city Locality for the address (length-1 character).
#' @param state Two-letter state abbreviation. Defaults to `"NJ"`.
#' @param zip Optional ZIP/postal code. Improves match precision when supplied.
#' @param id Optional label echoed back in the `query_id` column.
#' @param min_score Minimum ArcGIS match score (0-100) a candidate must reach to
#'   be returned. Defaults to `0` (return all, still ranked).
#' @param max_candidates Maximum number of candidates to return. Defaults to `5`.
#' @param geography If `TRUE` (default), attach `County`/`Municipality` (and the
#'   other local-geography fields) to each candidate. Set `FALSE` for
#'   coordinates only.
#' @param geography_shapes Optional `sf` boundary layer to attach geography from
#'   (via [add_muni_from_shapes()]). When `NULL` and `geography = TRUE`, county
#'   subdivisions are built from Census TIGER/Line for `state` (needs `tigris`
#'   and network access).
#' @param bbox Optional region bounding box (see [region_bbox()]). When given,
#'   ArcGIS is asked to prefer that extent and an `in_bbox` flag is added.
#' @param quiet If `TRUE` (default), suppress routine messages from geography
#'   downloads/joins so the console shows only the returned candidate table.
#'
#' @return A tibble of candidates ordered by descending `match_score`, with
#'   `query_id`, `rank`, `match_score`, `match_addr_type`, `matched_address`,
#'   `latitude`, `longitude`, the cleaned `input_address`, an optional `in_bbox`
#'   flag, and (when `geography = TRUE`) `County`/`Municipality` and related
#'   fields. Zero rows if nothing matched at or above `min_score`.
#' @seealso [geocode_records()] for the batch cascade over a data frame.
#' @export
#' @examples
#' if (interactive()) {
#' # ranked candidates for one address, with county/municipality attached
#' geocode_address(address = "22 peachton", city = "Sicklerville", state = "NJ")
#'
#' # only high-confidence matches, coordinates only
#' geocode_address("1 Bay Ave", city = "Montclair", state = "NJ",
#'                 min_score = 90, geography = FALSE)
#' }
geocode_address <- function(address, city, state = "NJ", zip = NULL,
                            id = NULL, min_score = 0, max_candidates = 5L,
                            geography = TRUE, geography_shapes = NULL,
                            bbox = NULL, quiet = TRUE) {
  if (!is.character(address) || length(address) != 1L || is.na(address)) {
    stop("`address` must be a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.character(city) || length(city) != 1L || is.na(city)) {
    stop("`city` must be a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.null(zip) && (!is.character(zip) || length(zip) != 1L || is.na(zip))) {
    stop("`zip` must be `NULL` or a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.character(state) || length(state) != 1L || is.na(state)) {
    stop("`state` must be a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.null(id) && (!is.atomic(id) || length(id) < 1L || is.na(id[[1]]))) {
    stop("`id` must be `NULL` or a non-missing scalar value.", call. = FALSE)
  }
  if (!is.numeric(min_score) || length(min_score) != 1L ||
      is.na(min_score) || min_score < 0 || min_score > 100) {
    stop("`min_score` must be a single number from 0 to 100.", call. = FALSE)
  }
  if (!is.numeric(max_candidates) || length(max_candidates) != 1L ||
      is.na(max_candidates) || max_candidates < 1 ||
      max_candidates != as.integer(max_candidates)) {
    stop("`max_candidates` must be a positive whole number.", call. = FALSE)
  }
  if (!is.logical(geography) || length(geography) != 1L || is.na(geography)) {
    stop("`geography` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  max_candidates <- as.integer(max_candidates)

  query_id <- if (is.null(id)) NA_character_ else as.character(id)[1]

  cleaned <- clean_addresses(
    tibble::tibble(
      .loc_address = address,
      .loc_city    = city,
      .loc_zip     = if (is.null(zip)) NA_character_ else as.character(zip)
    ),
    address = .loc_address, city = .loc_city, zip = .loc_zip, state = state
  )
  single_line <- cleaned$full_address_clean[[1]]

  cands <- .arcgis_candidates(single_line, max_candidates = max_candidates,
                              bbox = bbox)
  cands <- cands %>%
    dplyr::filter(!is.na(.data$match_score), .data$match_score >= min_score) %>%
    dplyr::arrange(dplyr::desc(.data$match_score)) %>%
    dplyr::slice_head(n = max_candidates)

  empty_meta <- function(d) {
    d %>%
      dplyr::mutate(query_id = query_id, input_address = single_line,
                    rank = dplyr::row_number())
  }

  if (nrow(cands) == 0L) {
    return(empty_meta(cands))
  }

  cands <- empty_meta(cands)
  if (!is.null(bbox)) {
    cands$in_bbox <- in_bbox(cands$latitude, cands$longitude, bbox)
  }

  if (isTRUE(geography)) {
    attach_geography <- function() {
      if (!is.null(geography_shapes)) {
        add_muni_from_shapes(cands, muni_shapes = geography_shapes)
      } else {
        add_county_muni(cands, state = state)
      }
    }
    geo <- tryCatch(
      if (isTRUE(quiet)) {
        out <- NULL
        invisible(utils::capture.output(
          out <- suppressMessages(attach_geography()),
          type = "output"
        ))
        out
      } else {
        attach_geography()
      },
      error = function(e) {
        warning("Geography lookup skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (!is.null(geo)) cands <- geo
  }

  lead <- c("query_id", "rank", "match_score", "match_addr_type",
            "matched_address", "latitude", "longitude", "in_bbox",
            "input_address", "County", "Municipality")
  dplyr::relocate(cands, dplyr::any_of(lead))
}

# Query the free ArcGIS findAddressCandidates service for one single-line
# address and return a ranked tibble of candidates. httr/jsonlite are Suggests.
.arcgis_candidates <- function(single_line, max_candidates = 5L, bbox = NULL) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`geocode_address()` needs the 'httr' and 'jsonlite' packages. ",
         "Install them with install.packages(c(\"httr\", \"jsonlite\")).",
         call. = FALSE)
  }

  empty <- tibble::tibble(
    matched_address = character(), longitude = double(),
    latitude = double(), match_score = double(),
    match_addr_type = character()
  )

  query <- list(
    SingleLine   = single_line,
    outFields    = "Addr_type,Match_addr",
    maxLocations = as.integer(max_candidates),
    countryCode  = "USA",
    f            = "json"
  )
  if (!is.null(bbox)) {
    query$searchExtent <- paste(bbox[["lon_min"]], bbox[["lat_min"]],
                                bbox[["lon_max"]], bbox[["lat_max"]], sep = ",")
  }

  resp <- httr::GET(
    paste0("https://geocode.arcgis.com/arcgis/rest/services/World/",
           "GeocodeServer/findAddressCandidates"),
    query = query
  )
  httr::stop_for_status(resp)
  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = TRUE
  )

  cands <- parsed$candidates
  if (is.null(cands) || !is.data.frame(cands) || nrow(cands) == 0L) {
    return(empty)
  }

  addr_type <- if (!is.null(cands$attributes) &&
                   "Addr_type" %in% names(cands$attributes)) {
    as.character(cands$attributes$Addr_type)
  } else {
    rep(NA_character_, nrow(cands))
  }

  tibble::tibble(
    matched_address = as.character(cands$address),
    longitude       = as.numeric(cands$location$x),
    latitude        = as.numeric(cands$location$y),
    match_score     = as.numeric(cands$score),
    match_addr_type = addr_type
  ) %>%
    dplyr::arrange(dplyr::desc(.data$match_score))
}
