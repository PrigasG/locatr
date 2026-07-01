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
#' The address text is normalised with the same abbreviation/secondary-unit
#' cleaning used by [clean_addresses()], then sent to the free ArcGIS
#' `findAddressCandidates` service. `city`, `state`, and `zip` are optional for
#' this one-off helper: use them when you want to narrow the search, or pass only
#' `address` to inspect broad candidate matches. If `city` is supplied and
#' `state` is omitted, `state` defaults to `"NJ"` for compatibility with the
#' package's first workflow.
#'
#' @param address Single-line street address as a length-1 character string.
#' @param city Optional locality for the address (length-1 character).
#' @param state Optional two-letter state abbreviation. When `city` is supplied
#'   but `state` is omitted, defaults to `"NJ"` for compatibility.
#' @param zip Optional ZIP/postal code. Improves match precision when supplied.
#' @param id Optional label echoed back in the `query_id` column.
#' @param min_score Minimum ArcGIS match score (0-100) a candidate must reach to
#'   be returned. Defaults to `0` (return all, still ranked).
#' @param max_candidates Maximum number of candidates to return. Defaults to `5`.
#' @param geography If `TRUE` (default), attach `County`/`Municipality` (and the
#'   other local-geography fields). When `state` is not supplied, locatr tries
#'   to infer candidate states from ArcGIS matched addresses. Set `FALSE` for
#'   coordinates only.
#' @param geography_shapes Optional `sf` boundary layer to attach geography from
#'   (via [add_muni_from_shapes()]). When `NULL` and `geography = TRUE`, county
#'   subdivisions are built from Census TIGER/Line for `state` (needs `tigris`
#'   and network access).
#' @param bbox Optional region bounding box (see [region_bbox()]). When given,
#'   ArcGIS is asked to prefer that extent and an `in_bbox` flag is added.
#' @param quiet If `TRUE` (default), suppress routine messages from geography
#'   downloads/joins so the console shows only the returned candidate table.
#' @param show_progress If `TRUE`, print short progress messages while the
#'   lookup runs. Defaults to `interactive()`.
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
#' # ranked candidates for one address
#' geocode_address("1600 Pennsylvania Ave NW")
#'
#' # only high-confidence matches, coordinates only
#' geocode_address("1 City Hall Sq", city = "Boston", state = "MA",
#'                 min_score = 90, geography = FALSE)
#' }
geocode_address <- function(address, city = NULL, state = NULL, zip = NULL,
                            id = NULL, min_score = 0, max_candidates = 5L,
                            geography = TRUE, geography_shapes = NULL,
                            bbox = NULL, quiet = TRUE,
                            show_progress = interactive()) {
  if (!is.character(address) || length(address) != 1L || is.na(address)) {
    stop("`address` must be a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.null(city) && (!is.character(city) || length(city) != 1L ||
                         is.na(city))) {
    stop("`city` must be `NULL` or a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.null(zip) && (!is.character(zip) || length(zip) != 1L || is.na(zip))) {
    stop("`zip` must be `NULL` or a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.null(state) && (!is.character(state) || length(state) != 1L ||
                          is.na(state))) {
    stop("`state` must be `NULL` or a single, non-missing character string.",
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
  if (!is.logical(show_progress) || length(show_progress) != 1L ||
      is.na(show_progress)) {
    stop("`show_progress` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  max_candidates <- as.integer(max_candidates)

  query_id <- if (is.null(id)) NA_character_ else as.character(id)[1]
  effective_state <- state
  if (is.null(effective_state) && !is.null(city)) {
    effective_state <- "NJ"
  }
  single_line <- .single_address_query(address, city = city,
                                      state = effective_state, zip = zip)

  .geocode_address_progress(show_progress, "Looking up address candidates ...")
  cands <- .arcgis_candidates(single_line, max_candidates = max_candidates,
                              bbox = bbox)
  .geocode_address_progress(show_progress, "Scoring candidates ...")
  cands <- cands %>%
    dplyr::filter(!is.na(.data$match_score), .data$match_score >= min_score) %>%
    dplyr::arrange(dplyr::desc(.data$match_score)) %>%
    .dedupe_address_candidates() %>%
    dplyr::slice_head(n = max_candidates)
  .geocode_address_context_tip(
    cands,
    has_context = !is.null(city) || !is.null(state) || !is.null(zip) ||
      !is.null(bbox),
    show_progress = show_progress
  )

  empty_meta <- function(d) {
    d %>%
      dplyr::mutate(query_id = query_id, input_address = single_line,
                    rank = dplyr::row_number())
  }

  if (nrow(cands) == 0L) {
    .geocode_address_progress(show_progress, "No candidates met the threshold.")
    return(empty_meta(cands))
  }

  cands <- empty_meta(cands)
  if (!is.null(bbox)) {
    cands$in_bbox <- in_bbox(cands$latitude, cands$longitude, bbox)
  }

  if (isTRUE(geography)) {
    if (!is.null(geography_shapes) || !is.null(effective_state)) {
      .geocode_address_progress(show_progress, "Attaching local geography ...")
    } else {
      .geocode_address_progress(show_progress,
                                "Inferring candidate states for geography ...")
    }
    attach_geography <- function() {
      if (!is.null(geography_shapes)) {
        add_muni_from_shapes(cands, muni_shapes = geography_shapes)
      } else if (!is.null(effective_state)) {
        add_county_muni(cands, state = effective_state)
      } else {
        .attach_geography_by_inferred_state(cands)
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

  .geocode_address_progress(
    show_progress,
    paste0("Done. Returning ", nrow(cands), " candidate",
           if (nrow(cands) == 1L) "." else "s.")
  )
  lead <- c("query_id", "rank", "match_score", "match_addr_type",
            "matched_address", "latitude", "longitude", "in_bbox",
            "input_address", "County", "Municipality")
  dplyr::relocate(cands, dplyr::any_of(lead))
}

.attach_geography_by_inferred_state <- function(cands) {
  cands$candidate_state <- .infer_candidate_state(cands$matched_address)
  states <- unique(stats::na.omit(cands$candidate_state))
  if (length(states) == 0L) {
    return(.ensure_candidate_geography_cols(cands))
  }

  pieces <- lapply(states, function(st) {
    add_county_muni(cands[cands$candidate_state == st, , drop = FALSE],
                   state = st)
  })
  missing_state <- cands[is.na(cands$candidate_state), , drop = FALSE]
  if (nrow(missing_state) > 0L) {
    pieces <- c(pieces, list(.ensure_candidate_geography_cols(missing_state)))
  }
  dplyr::bind_rows(pieces) %>%
    dplyr::filter(.data$rank %in% cands$rank) %>%
    dplyr::arrange(.data$rank)
}

.ensure_candidate_geography_cols <- function(cands) {
  cols <- list(
    County = NA_character_,
    Municipality = NA_character_,
    location_county = NA_character_,
    location_locality = NA_character_,
    geography_match_status = NA_character_,
    muni_match_status = NA_character_,
    county_code = NA_character_,
    county_fips = NA_character_,
    municipality_code = NA_character_,
    municipality_geoid = NA_character_,
    municipality_name_standard = NA_character_,
    municipality_type = NA_character_,
    muni_join_key = NA_character_,
    `Muni Key` = NA_character_
  )
  for (col in names(cols)) {
    if (!col %in% names(cands)) cands[[col]] <- cols[[col]]
  }
  cands
}

.infer_candidate_state <- function(matched_address) {
  state_lookup <- c(
    stats::setNames(datasets::state.abb, toupper(datasets::state.abb)),
    stats::setNames(datasets::state.abb, toupper(datasets::state.name)),
    "DC" = "DC",
    "DISTRICT OF COLUMBIA" = "DC",
    "WASHINGTON DC" = "DC",
    "WASHINGTON D C" = "DC"
  )

  unname(vapply(matched_address, function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(NA_character_)
    }
    text <- toupper(x)
    tokens <- stringr::str_split(text, "\\s*,\\s*", simplify = FALSE)[[1]]
    tokens <- stringr::str_squish(tokens)
    hit <- state_lookup[tokens[tokens %in% names(state_lookup)]][1]
    if (!is.na(hit)) {
      return(unname(hit))
    }

    names_hit <- names(state_lookup)[
      vapply(names(state_lookup), function(state_name) {
        grepl(paste0("\\b", state_name, "\\b"), text)
      }, logical(1))
    ]
    if (length(names_hit) == 0L) NA_character_ else unname(state_lookup[[names_hit[1]]])
  }, character(1)))
}

.geocode_address_progress <- function(show_progress, text) {
  if (isTRUE(show_progress)) {
    message("[locatr] ", text)
  }
}

.geocode_address_context_tip <- function(cands, has_context, show_progress) {
  if (!isTRUE(show_progress) || isTRUE(has_context) || nrow(cands) == 0L) {
    return(invisible(NULL))
  }
  precise_types <- c("PointAddress", "Subaddress", "StreetAddress")
  types <- unique(stats::na.omit(cands$match_addr_type))
  if (length(types) > 0L && !any(types %in% precise_types)) {
    .geocode_address_progress(
      TRUE,
      "Tip: broad lookup returned locality/POI-style candidates; add city, state, or bbox to narrow the search."
    )
  }
  invisible(NULL)
}

.dedupe_address_candidates <- function(cands) {
  cands$.dedupe_address <- stringr::str_squish(stringr::str_to_upper(
    cands$matched_address
  ))
  cands$.dedupe_latitude <- round(cands$latitude, 5)
  cands$.dedupe_longitude <- round(cands$longitude, 5)
  cands <- dplyr::distinct(
    cands,
    .data$.dedupe_address,
    .data$.dedupe_latitude,
    .data$.dedupe_longitude,
    .data$match_addr_type,
    .keep_all = TRUE
  )
  cands$.dedupe_address <- NULL
  cands$.dedupe_latitude <- NULL
  cands$.dedupe_longitude <- NULL
  cands
}

.single_address_query <- function(address, city = NULL, state = NULL,
                                  zip = NULL) {
  address <- .clean_single_address_piece(address)
  pieces <- c(
    address,
    if (!is.null(city)) stringr::str_squish(stringr::str_to_upper(city)),
    if (!is.null(state)) stringr::str_squish(stringr::str_to_upper(state))
  )
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
  query <- paste(pieces, collapse = ", ")
  if (!is.null(zip)) {
    zip_clean <- zip %>%
      stringr::str_remove_all("\\D") %>%
      stringr::str_sub(1, 5) %>%
      stringr::str_pad(width = 5, side = "left", pad = "0") %>%
      dplyr::na_if("00000")
    if (!is.na(zip_clean) && nzchar(zip_clean)) {
      query <- paste(query, zip_clean)
    }
  }
  query
}

.clean_single_address_piece <- function(address) {
  address %>%
    stringr::str_to_upper() %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("\\bONE\\b", "1") %>%
    stringr::str_replace_all("\\bTWO\\b", "2") %>%
    stringr::str_replace_all("\\bTHREE\\b", "3") %>%
    stringr::str_replace_all("\\bFOUR\\b", "4") %>%
    stringr::str_replace_all("\\bFIVE\\b", "5") %>%
    stringr::str_replace_all("\\bRTE?\\b", "ROUTE") %>%
    stringr::str_replace_all("\\bHWY\\b", "HIGHWAY") %>%
    stringr::str_replace_all("\\bROUTE\\s+([0-9]+)", "STATE ROUTE \\1") %>%
    stringr::str_replace_all("\\bHIGHWAY\\s+([0-9]+)", "STATE HIGHWAY \\1") %>%
    stringr::str_replace_all("\\bMT\\b", "MOUNT") %>%
    stringr::str_replace_all("\\bAVE\\b", "AVENUE") %>%
    stringr::str_replace_all("\\bRD\\b", "ROAD") %>%
    stringr::str_replace_all("\\bBLVD\\b", "BOULEVARD") %>%
    stringr::str_replace_all("\\bST\\b", "STREET") %>%
    stringr::str_replace_all("\\bDR\\b", "DRIVE") %>%
    stringr::str_replace_all("\\bLN\\b", "LANE") %>%
    stringr::str_replace_all(
      ",?\\s*(SUITE|STE|STES|UNIT|BLDG|BUILDING|FLOOR|FLR|ROOM|RM)\\s+[A-Z0-9\\-]+",
      ""
    ) %>%
    stringr::str_squish()
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
