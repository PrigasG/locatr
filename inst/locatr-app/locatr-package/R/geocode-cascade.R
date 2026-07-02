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
#' `pass_2_fallback`, or `pass_4_name_lookup`. After the cascade, valid matched
#' rows are marked `review_status == "auto_accepted"`; anything still unmatched
#' lands in `needs_manual_review`, while invalid coordinates are `rejected`.
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
#' @param name_min_score Minimum ArcGIS score for a name lookup to pass without
#'   review. Passed to [geocode_by_name()].
#' @param name_accept_types ArcGIS address types precise enough for a name lookup
#'   to pass without review. Passed to [geocode_by_name()].
#' @param verbose Whether to print a per-tier match tally.
#' @param cache Optional [locatr_cache()] shared across the network tiers
#'   (Census, ArcGIS, name lookup), so repeated addresses are served from the
#'   cache and a re-run reproduces coordinates without re-querying.
#' @param refresh If `TRUE`, ignore cached entries and re-query every tier,
#'   overwriting the cache. Defaults to `FALSE`.
#'
#' @return `data` with coordinates and the full audit trail populated.
#' @export
geocode_records <- function(data,
                               tiers = c("census", "arcgis", "name"),
                               reference = NULL,
                               boundary = NULL,
                               bbox = region_bbox("NJ"),
                               name_min_score = 90,
                               name_accept_types = c("PointAddress", "Subaddress",
                                                     "StreetAddress"),
                               verbose = TRUE,
                               cache = NULL,
                               refresh = FALSE) {
  stopifnot("review_status" %in% names(data))
  .validate_cache_args(cache, refresh)
  tiers <- match.arg(tiers, choices = c("census", "arcgis", "name"),
                     several.ok = TRUE)

  say <- function(...) if (isTRUE(verbose)) message(...)
  out <- data
  run_started <- .cache_now()
  cache_before <- if (!is.null(cache)) {
    c(cache$hits, cache$misses, cache$writes)
  } else {
    rep(NA_integer_, 3L)
  }

  if (!is.null(reference)) {
    say("Tier 0 - reference backfill ...")
    out <- backfill_from_reference(out, reference = reference, bbox = bbox)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  }

  if ("census" %in% tiers) {
    say("Tier 1 - Census structured geocode ...")
    out <- geocode_census(out, cache = cache, refresh = refresh)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  } else {
    out <- .ensure_geocode_cols(out)
  }

  if ("arcgis" %in% tiers) {
    say("Tier 2 - ArcGIS address fallback ...")
    out <- geocode_arcgis(out, method = "arcgis", bbox = bbox,
                          cache = cache, refresh = refresh)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  }

  if ("name" %in% tiers) {
    say("Tier 3 - name lookup fallback ...")
    out <- geocode_by_name(out, method = "arcgis", bbox = bbox,
                           min_score = name_min_score,
                           accept_types = name_accept_types,
                           cache = cache, refresh = refresh)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  }

  out <- add_match_confidence(.finalize_review_status(out))
  out <- .stamp_placement(out, cache, run_started, bbox)
  attr(out, "locatr_run") <- .locatr_run_manifest(
    out, tiers, reference, boundary, bbox, cache, run_started, cache_before
  )
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

.finalize_review_status <- function(data) {
  if (!"review_status" %in% names(data)) {
    return(data)
  }

  validation <- if ("validation_status" %in% names(data)) {
    data$validation_status
  } else {
    rep(NA_character_, nrow(data))
  }
  matched <- if ("match_status" %in% names(data)) {
    data$match_status == "matched"
  } else {
    rep(FALSE, nrow(data))
  }

  data$review_status <- dplyr::case_when(
    data$review_status == "manual_override_applied" ~ data$review_status,
    validation == "outside_region" ~ "rejected",
    matched & (is.na(validation) | validation == "coordinate_ok") ~ "auto_accepted",
    data$review_status == "ready_for_geocoding" ~ "needs_manual_review",
    TRUE ~ data$review_status
  )
  data
}
