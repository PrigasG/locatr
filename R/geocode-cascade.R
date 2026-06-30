#' Run the full geocoding cascade
#'
#' Orchestrates the tiered strategy on an already-cleaned, already-flagged frame
#' (see [clean_addresses()] and [flag_bad_addresses()]): Census structured
#' match, then ArcGIS address fallback, then name lookup, validating against the
#' configured region after each tier so a later tier only retries what is still
#' unplaced.
#'
#' Each tier records how it placed a row in `geocode_pass`, so the final frame is
#' self-documenting: `pass_0_reference`, `pass_1_census_structured`,
#' `pass_2_fallback`, or `pass_4_name_lookup`. Anything still unmatched lands in
#' `needs_manual_review`.
#'
#' Supplying `reference` runs an authoritative Tier 0 first
#' ([backfill_from_reference()]): rows whose verified coordinates are already
#' known are placed exactly and skipped by every later tier. Feed prior cycles'
#' completed overrides back in here so manual review accrues over time.
#'
#' @param data A data frame from [flag_bad_addresses()].
#' @param tiers Which tiers to run, in order. Any subset of
#'   `c("census", "arcgis", "name")`.
#' @param reference Optional trusted key -> coordinates table for Tier 0; see
#'   [backfill_from_reference()]. `NULL` skips Tier 0. For non-default column
#'   names, call [backfill_from_reference()] yourself before [geocode_records()].
#' @param boundary Optional `sf` boundary for polygon-precise validation;
#'   passed to [validate_geocodes()]. `NULL` uses the bounding box.
#' @param bbox Bounding box for region guards; see [region_bbox()].
#' @param verbose Whether to print a per-tier match tally.
#'
#' @return `data` with coordinates and the full audit trail populated.
#' @export
geocode_records <- function(data,
                               tiers = c("census", "arcgis", "name"),
                               reference = NULL,
                               boundary = NULL,
                               bbox = region_bbox("NJ"),
                               verbose = TRUE) {
  stopifnot("review_status" %in% names(data))
  tiers <- match.arg(tiers, choices = c("census", "arcgis", "name"),
                     several.ok = TRUE)

  say <- function(...) if (isTRUE(verbose)) message(...)
  out <- data

  if (!is.null(reference)) {
    say("Tier 0 - reference backfill ...")
    out <- backfill_from_reference(out, reference = reference, bbox = bbox)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  }

  if ("census" %in% tiers) {
    say("Tier 1 - Census structured geocode ...")
    out <- geocode_census(out)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  } else {
    out <- .ensure_geocode_cols(out)
  }

  if ("arcgis" %in% tiers) {
    say("Tier 2 - ArcGIS address fallback ...")
    out <- geocode_fallback(out, method = "arcgis", bbox = bbox)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  }

  if ("name" %in% tiers) {
    say("Tier 3 - name lookup fallback ...")
    out <- geocode_by_name(out, method = "arcgis", bbox = bbox)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  }

  out
}

# Count rows whose coordinates fall inside the configured bbox.
.n_in_region <- function(data, bbox) {
  sum(in_bbox(data$latitude, data$longitude, bbox), na.rm = TRUE)
}

# Guarantee the coordinate/audit columns exist when the Census tier is skipped,
# so downstream tiers have something to coalesce into.
.ensure_geocode_cols <- function(data) {
  defaults <- list(
    latitude = NA_real_, longitude = NA_real_,
    geocode_method = NA_character_, geocode_pass = NA_character_,
    match_status = NA_character_
  )
  for (col in names(defaults)) {
    if (!col %in% names(data)) data[[col]] <- defaults[[col]]
  }
  data
}
