#' Explain how geocoded records were handled
#'
#' Turns the main audit columns into short, reviewer-friendly sentences. This is
#' useful when checking a few records by hand or when adding plain-English notes
#' to a review export.
#'
#' @param data A locatr output data frame.
#' @param row Optional row selector. Use `NULL` for all rows (default), a numeric
#'   row index, or a `record_id` value.
#'
#' @return A character vector of explanations.
#' @export
#' @examples
#' x <- tibble::tibble(
#'   record_id = "a",
#'   geocode_pass = "pass_4_name_lookup",
#'   match_status = "matched_low_confidence",
#'   validation_status = "coordinate_ok",
#'   review_status = "needs_manual_review",
#'   nm_score = 87,
#'   nm_addr_type = "POI"
#' )
#' explain_geocode_result(x)
explain_geocode_result <- function(data, row = NULL) {
  data <- .select_review_rows(data, row)
  if (nrow(data) == 0L) {
    return(character())
  }

  vapply(seq_len(nrow(data)), function(i) {
    rec <- data[i, , drop = FALSE]
    id <- .scalar_value(rec, "record_id")
    pass <- .scalar_value(rec, "geocode_pass")
    match <- .scalar_value(rec, "match_status")
    validation <- .scalar_value(rec, "validation_status")
    review <- .scalar_value(rec, "review_status")
    score <- .scalar_value(rec, c("nm_score", "name_match_score"))
    addr_type <- .scalar_value(rec, c("nm_addr_type", "name_match_type"))

    how <- .pass_label(pass)
    confidence <- if (!is.na(score) || !is.na(addr_type)) {
      paste0(" Name match score/type: ",
             if (is.na(score)) "not recorded" else format(score, trim = TRUE),
             " / ",
             if (is.na(addr_type)) "not recorded" else addr_type,
             ".")
    } else {
      ""
    }
    validation_text <- .validation_label(validation)
    review_text <- .review_label(review)
    match_text <- if (is.na(match)) {
      "Match status was not recorded."
    } else {
      paste0("Match status: ", match, ".")
    }
    prefix <- if (is.na(id)) "" else paste0("Record ", id, ": ")

    paste0(prefix, how, " ", match_text, confidence, " ",
           validation_text, " ", review_text)
  }, character(1))
}

#' Summarise geocoding quality
#'
#' Counts the main outcomes in a locatr result so you can quickly judge whether a
#' run is ready for review, export, or threshold tuning.
#'
#' @param data A locatr output data frame.
#'
#' @return A one-row tibble with counts and rates.
#' @export
#' @examples
#' x <- tibble::tibble(
#'   latitude = c(40, NA),
#'   longitude = c(-75, NA),
#'   match_status = c("matched", "unmatched"),
#'   review_status = c("auto_accepted", "needs_manual_review"),
#'   geocode_pass = c("pass_1_census_structured", NA_character_)
#' )
#' summarise_geocoding(x)
summarise_geocoding <- function(data) {
  n <- nrow(data)
  has_coords <- .has_coordinates(data)
  matched <- .col(data, "match_status") == "matched" | has_coords
  low_conf <- .col(data, "match_status") == "matched_low_confidence"
  review <- .col(data, "review_status")
  validation <- .col(data, "validation_status")
  pass <- .col(data, "geocode_pass")
  locality <- .first_existing_col(data, c("location_locality", "Municipality"))
  manual <- .logical_col(data, "manual_override_used", default = FALSE)

  tibble::tibble(
    n_records = n,
    matched = sum(matched, na.rm = TRUE),
    matched_pct = .pct(sum(matched, na.rm = TRUE), n),
    missing_coordinates = sum(!has_coords, na.rm = TRUE),
    auto_accepted = sum(review == "auto_accepted", na.rm = TRUE),
    needs_manual_review = sum(review == "needs_manual_review", na.rm = TRUE),
    rejected = sum(review == "rejected", na.rm = TRUE),
    manual_override_applied = sum(review == "manual_override_applied" |
                                    manual, na.rm = TRUE),
    outside_region = sum(validation == "outside_region", na.rm = TRUE),
    name_lookup = sum(pass == "pass_4_name_lookup", na.rm = TRUE),
    low_confidence_name = sum(low_conf & pass == "pass_4_name_lookup",
                              na.rm = TRUE),
    missing_geography = sum(has_coords & is.na(locality), na.rm = TRUE)
  )
}

#' Plot geocoded records for review
#'
#' Creates a small interactive leaflet map, colored by an audit column. The
#' helper is intentionally lightweight: it is for quick review, not for
#' producing a full dashboard.
#'
#' @param data A locatr output data frame with `latitude` and `longitude`.
#' @param color_by Column used to color points. Defaults to `review_status`.
#'
#' @return A `leaflet` map.
#' @export
#' @examples
#' if (interactive()) {
#'   plot_geocode_review_map(geocoded)
#' }
plot_geocode_review_map <- function(data,
                                    color_by = c("review_status",
                                                 "geocode_pass",
                                                 "match_status")) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("`plot_geocode_review_map()` needs the 'leaflet' package. ",
         "Install it with install.packages(\"leaflet\").", call. = FALSE)
  }
  color_by <- match.arg(color_by)
  if (!all(c("latitude", "longitude") %in% names(data))) {
    stop("`data` must include `latitude` and `longitude` columns.",
         call. = FALSE)
  }

  pts <- data[!is.na(data$latitude) & !is.na(data$longitude), , drop = FALSE]
  map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
  if (nrow(pts) == 0L) {
    return(map)
  }

  value <- if (color_by %in% names(pts)) {
    as.character(pts[[color_by]])
  } else {
    rep("not recorded", nrow(pts))
  }
  value[is.na(value) | !nzchar(value)] <- "not recorded"
  pal <- leaflet::colorFactor("Set2", domain = value, na.color = "#999999")
  label <- .popup_label(pts, color_by, value)

  map %>%
    leaflet::addCircleMarkers(
      data = pts,
      lng = ~longitude, lat = ~latitude,
      radius = 5, stroke = FALSE, fillOpacity = 0.8,
      color = pal(value), popup = label
    ) %>%
    leaflet::fitBounds(min(pts$longitude), min(pts$latitude),
                       max(pts$longitude), max(pts$latitude))
}

#' Suggest a Census geography level for a state
#'
#' Gives a practical starting point for [build_local_geography()] and
#' [add_county_muni()]. It is a recommendation, not a legal definition of local
#' government. For production municipal joins, an official state/local boundary
#' layer is still the strongest source.
#'
#' @param state Two-letter state abbreviation.
#'
#' @return A one-row tibble with the recommended Census geography and note.
#' @export
#' @examples
#' suggest_geography_level("NJ")
#' suggest_geography_level("CA")
suggest_geography_level <- function(state) {
  if (!is.character(state) || length(state) != 1L || is.na(state)) {
    stop("`state` must be a single, non-missing character string.",
         call. = FALSE)
  }
  state <- toupper(state)
  county_subdivision_states <- c(
    "CT", "ME", "MA", "NH", "RI", "VT",
    "NJ", "NY", "PA", "MI", "MN", "WI"
  )
  place_first_states <- c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "DE", "FL", "GA", "HI",
    "ID", "IL", "IN", "IA", "KS", "KY", "LA", "MD", "MS", "MO",
    "MT", "NE", "NV", "NM", "NC", "ND", "OH", "OK", "OR", "SC",
    "SD", "TN", "TX", "UT", "VA", "WA", "WV", "WY"
  )

  if (state %in% county_subdivision_states) {
    geography <- "county_subdivision"
    note <- paste(
      "County subdivisions are usually the best Census starting point for",
      "township/municipality-style local geography. Use an official boundary",
      "layer when legal municipal definitions matter."
    )
  } else if (state %in% place_first_states) {
    geography <- "place"
    note <- paste(
      "Census places are a useful starting point for incorporated places and",
      "CDPs, but they can miss unincorporated areas. Use county or an official",
      "local boundary layer when coverage matters."
    )
  } else if (identical(state, "DC")) {
    geography <- "county"
    note <- "DC is usually handled cleanly at the county-equivalent level."
  } else {
    geography <- "county"
    note <- paste(
      "Unknown or unsupported state code. County is the safest Census fallback;",
      "use an official boundary layer for local geography."
    )
  }

  tibble::tibble(
    state = state,
    recommended_geography = geography,
    function_call = paste0("build_local_geography(state = \"", state,
                           "\", geography = \"", geography, "\")"),
    note = note
  )
}

#' Compare two geocoding runs
#'
#' Finds records whose coordinates, review status, geocode pass, or geography
#' assignment changed between two runs. This is useful after changing thresholds,
#' adding a reference file, or swapping geography sources.
#'
#' @param old Previous locatr output.
#' @param new New locatr output.
#' @param by Key column used to match rows. Defaults to `record_id`.
#' @param coordinate_tolerance Numeric tolerance for latitude/longitude changes.
#' @param changed_only If `TRUE` (default), return only rows with at least one
#'   tracked change.
#'
#' @return A tibble with old/new values and change flags.
#' @export
#' @examples
#' old <- tibble::tibble(record_id = "a", latitude = 40, longitude = -75)
#' new <- tibble::tibble(record_id = "a", latitude = 41, longitude = -75)
#' compare_geocode_runs(old, new)
compare_geocode_runs <- function(old, new, by = "record_id",
                                 coordinate_tolerance = 1e-6,
                                 changed_only = TRUE) {
  if (!is.character(by) || length(by) != 1L || is.na(by) ||
      !nzchar(by)) {
    stop("`by` must be a single, non-empty column name.", call. = FALSE)
  }
  if (!by %in% names(old) || !by %in% names(new)) {
    stop("`by` must exist in both `old` and `new`.", call. = FALSE)
  }
  if (!is.numeric(coordinate_tolerance) ||
      length(coordinate_tolerance) != 1L ||
      is.na(coordinate_tolerance) || coordinate_tolerance < 0) {
    stop("`coordinate_tolerance` must be a non-negative number.",
         call. = FALSE)
  }

  fields <- c("latitude", "longitude", "review_status", "geocode_pass",
              "match_status", "location_county", "location_locality",
              "County", "Municipality", "muni_join_key",
              "municipality_geoid")
  old_slim <- .slim_for_compare(old, by, fields, "old")
  new_slim <- .slim_for_compare(new, by, fields, "new")

  out <- dplyr::full_join(old_slim, new_slim, by = by)
  out$coordinate_changed <- .coord_changed(out, coordinate_tolerance)
  out$review_status_changed <- .changed(out$review_status_old,
                                        out$review_status_new)
  out$geocode_pass_changed <- .changed(out$geocode_pass_old,
                                       out$geocode_pass_new)
  out$match_status_changed <- .changed(out$match_status_old,
                                       out$match_status_new)
  out$geography_changed <- .changed(
    .coalesce_chr(out$muni_join_key_old, out$municipality_geoid_old,
                  out$location_locality_old, out$Municipality_old),
    .coalesce_chr(out$muni_join_key_new, out$municipality_geoid_new,
                  out$location_locality_new, out$Municipality_new)
  )
  out$row_status <- dplyr::case_when(
    is.na(out[[paste0(".present_old")]]) ~ "added",
    is.na(out[[paste0(".present_new")]]) ~ "removed",
    TRUE ~ "kept"
  )
  out$any_changed <- out$row_status != "kept" |
    out$coordinate_changed |
    out$review_status_changed |
    out$geocode_pass_changed |
    out$match_status_changed |
    out$geography_changed
  out <- dplyr::select(out, -dplyr::any_of(c(".present_old",
                                             ".present_new")))
  if (isTRUE(changed_only)) {
    out <- dplyr::filter(out, .data$any_changed)
  }
  out
}

.select_review_rows <- function(data, row) {
  if (is.null(row)) {
    return(data)
  }
  if (is.numeric(row)) {
    return(data[row, , drop = FALSE])
  }
  if (!"record_id" %in% names(data)) {
    stop("Character `row` selectors require a `record_id` column.",
         call. = FALSE)
  }
  data[as.character(data$record_id) %in% as.character(row), , drop = FALSE]
}

.scalar_value <- function(data, cols) {
  col <- cols[cols %in% names(data)][1]
  if (is.na(col)) {
    return(NA)
  }
  value <- data[[col]][1]
  if (length(value) == 0L || is.na(value)) NA else value
}

.pass_label <- function(pass) {
  dplyr::case_when(
    is.na(pass) ~ "No geocode pass was recorded.",
    pass == "pass_0_reference" ~ "Placed from a trusted reference file.",
    pass == "pass_1_census_structured" ~ "Placed by the Census structured pass.",
    pass == "pass_2_fallback" ~ "Placed by the ArcGIS address fallback.",
    pass == "pass_3_manual" ~ "Placed by a manual override.",
    pass == "pass_4_name_lookup" ~ "Placed by the ArcGIS name lookup.",
    TRUE ~ paste0("Placed by ", pass, ".")
  )
}

.validation_label <- function(validation) {
  dplyr::case_when(
    is.na(validation) ~ "Coordinate validation was not recorded.",
    validation == "coordinate_ok" ~ "Coordinate validation passed.",
    validation == "outside_region" ~ "Coordinate was outside the expected region.",
    validation == "missing_coordinates" ~ "Coordinates were missing.",
    TRUE ~ paste0("Validation status: ", validation, ".")
  )
}

.review_label <- function(review) {
  dplyr::case_when(
    is.na(review) ~ "Review status was not recorded.",
    review == "auto_accepted" ~ "It was auto-accepted.",
    review == "needs_manual_review" ~ "It needs manual review.",
    review == "manual_override_applied" ~ "A manual override was applied.",
    review == "rejected" ~ "It was rejected.",
    TRUE ~ paste0("Review status: ", review, ".")
  )
}

.has_coordinates <- function(data) {
  if (!all(c("latitude", "longitude") %in% names(data))) {
    return(rep(FALSE, nrow(data)))
  }
  !is.na(data$latitude) & !is.na(data$longitude)
}

.col <- function(data, col, default = NA_character_) {
  if (col %in% names(data)) data[[col]] else rep(default, nrow(data))
}

.logical_col <- function(data, col, default = FALSE) {
  if (col %in% names(data)) {
    value <- data[[col]]
    value[is.na(value)] <- default
    as.logical(value)
  } else {
    rep(default, nrow(data))
  }
}

.first_existing_col <- function(data, cols) {
  col <- cols[cols %in% names(data)][1]
  if (is.na(col)) rep(NA_character_, nrow(data)) else data[[col]]
}

.pct <- function(num, den) {
  if (den == 0L) NA_real_ else round(100 * num / den, 1)
}

.popup_label <- function(data, color_by, value) {
  id <- .first_existing_col(data, c("record_id", "query_id"))
  name <- .first_existing_col(data, c("record_name", "matched_address"))
  paste0(
    ifelse(is.na(id), "", paste0("<strong>", id, "</strong><br/>")),
    ifelse(is.na(name), "", paste0(name, "<br/>")),
    color_by, ": ", value
  )
}

.slim_for_compare <- function(data, by, fields, suffix) {
  out <- data[, by, drop = FALSE]
  for (field in fields) {
    out[[field]] <- if (field %in% names(data)) {
      data[[field]]
    } else {
      NA
    }
  }
  names(out)[names(out) != by] <- paste0(names(out)[names(out) != by],
                                         "_", suffix)
  out[[paste0(".present_", suffix)]] <- TRUE
  out
}

.coord_changed <- function(data, tolerance) {
  lat_old <- data$latitude_old
  lat_new <- data$latitude_new
  lon_old <- data$longitude_old
  lon_new <- data$longitude_new
  lat_changed <- .num_changed(lat_old, lat_new, tolerance)
  lon_changed <- .num_changed(lon_old, lon_new, tolerance)
  lat_changed | lon_changed
}

.num_changed <- function(old, new, tolerance) {
  missing_changed <- xor(is.na(old), is.na(new))
  value_changed <- !is.na(old) & !is.na(new) & abs(old - new) > tolerance
  missing_changed | value_changed
}

.changed <- function(old, new) {
  old <- as.character(old)
  new <- as.character(new)
  xor(is.na(old), is.na(new)) | (!is.na(old) & !is.na(new) & old != new)
}

.coalesce_chr <- function(...) {
  values <- list(...)
  out <- rep(NA_character_, length(values[[1]]))
  for (value in values) {
    value <- as.character(value)
    take <- is.na(out) & !is.na(value) & nzchar(value)
    out[take] <- value[take]
  }
  out
}
