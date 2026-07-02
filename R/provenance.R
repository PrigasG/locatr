# Run provenance (roadmap #2, phase 3): a per-run manifest attached to
# geocode_records() output, plus per-row placed_at / cache_status. The stamper
# runs after the cascade and never touches the tiers; it reconstructs each row's
# cache key with the shared builders in cache.R, so classification cannot drift
# from what the tiers actually stored.

#' Read the provenance manifest from a geocoding run
#'
#' [geocode_records()] attaches a run manifest as an attribute of its output.
#' This returns it: a run id and UTC timestamp, the locatr / tidygeocoder /
#' cache-schema versions, the tiers run, whether a reference table was used, the
#' cache path, per-`review_status` counts, and cache activity
#' (`cache_hits` / `cache_misses` / `cache_writes`). Read it directly on the
#' `geocode_records()` result, since later data-frame operations may drop the
#' attribute.
#'
#' @param data Output of [geocode_records()].
#'
#' @return A `locatr_provenance` object (a named list) describing the run.
#' @seealso [geocode_records()], [locatr_cache()]
#' @export
geocode_provenance <- function(data) {
  manifest <- attr(data, "locatr_run", exact = TRUE)
  if (is.null(manifest)) {
    stop("No locatr run manifest found. It is attached by `geocode_records()` ",
         "and may be dropped by later data-frame operations.", call. = FALSE)
  }
  manifest
}

#' @rdname geocode_provenance
#' @param x A `locatr_provenance` object.
#' @param ... Ignored.
#' @export
print.locatr_provenance <- function(x, ...) {
  cat("<locatr run>", x$run_id, "\n")
  cat("  at:      ", x$run_at, "\n")
  cat("  versions: locatr", x$locatr_version, "| tidygeocoder",
      x$tidygeocoder_version, "| cache schema", x$cache_schema_version, "\n")
  cat("  tiers:   ", paste(x$tiers, collapse = ", "),
      "| reference used:", x$reference_used, "\n")
  cat("  records: ", x$n_records, "| cache:",
      if (is.na(x$cache_path)) "none" else x$cache_path, "\n")
  if (!is.na(x$cache_hits)) {
    cat("  cache:   ", x$cache_hits, "hit(s),", x$cache_misses, "miss(es),",
        x$cache_writes, "write(s)\n")
  }
  invisible(x)
}

# Build the run manifest. `cache_before` is the c(hits, misses, writes) snapshot
# taken before the cascade so the reported counts cover only this run.
.locatr_run_manifest <- function(out, tiers, reference, boundary, bbox, cache,
                                 run_started, cache_before) {
  after <- if (!is.null(cache)) {
    c(cache$hits, cache$misses, cache$writes)
  } else {
    rep(NA_integer_, 3L)
  }
  status <- if ("review_status" %in% names(out)) {
    as.list(table(out$review_status, useNA = "no"))
  } else {
    list()
  }
  manifest <- list(
    run_id = substr(rlang::hash(list(run_started, nrow(out))), 1, 12),
    run_at = run_started,
    locatr_version = as.character(utils::packageVersion("locatr")),
    tidygeocoder_version = tryCatch(
      as.character(utils::packageVersion("tidygeocoder")),
      error = function(e) NA_character_
    ),
    cache_schema_version = .CACHE_SCHEMA_VERSION,
    tiers = tiers,
    services = .locatr_services(tiers),
    bbox = bbox,
    boundary = if (is.null(boundary)) NA_character_ else class(boundary)[1],
    reference_used = !is.null(reference),
    cache_path = if (is.null(cache)) {
      NA_character_
    } else if (is.null(cache$path)) {
      "memory"
    } else {
      cache$path
    },
    n_records = nrow(out),
    status_counts = status,
    cache_hits = after[1] - cache_before[1],
    cache_misses = after[2] - cache_before[2],
    cache_writes = after[3] - cache_before[3]
  )
  class(manifest) <- "locatr_provenance"
  manifest
}

.locatr_services <- function(tiers) {
  services <- list()
  if ("census" %in% tiers) {
    services$census <- "tidygeocoder::geocode(method = 'census')"
  }
  if ("arcgis" %in% tiers) {
    services$arcgis <- "tidygeocoder::geocode(method = 'arcgis')"
  }
  if ("name" %in% tiers) {
    services$arcgis_byname <- "tidygeocoder::geocode(method = 'arcgis', full_results = TRUE)"
  }
  services
}

# Add per-row `placed_at` and `cache_status` to a finished frame. Reference and
# manual placements are provenance events, not new geocoding; a coordinate that
# a network tier placed is "cached" when the cache already held it before this
# run (cached_at < run_started), else "fresh".
.stamp_placement <- function(data, cache, run_started, bbox) {
  n <- nrow(data)
  pass <- if ("geocode_pass" %in% names(data)) {
    as.character(data$geocode_pass)
  } else {
    rep(NA_character_, n)
  }
  lat <- if ("latitude" %in% names(data)) data$latitude else rep(NA_real_, n)
  lon <- if ("longitude" %in% names(data)) data$longitude else rep(NA_real_, n)
  unplaced <- is.na(lat) | is.na(lon)
  starts <- function(pre) !is.na(pass) & startsWith(pass, pre)

  cache_status <- rep(NA_character_, n)
  placed_at <- rep(NA_character_, n)

  is0 <- starts("pass_0") & !unplaced
  is3 <- starts("pass_3") & !unplaced
  net <- !unplaced & !is0 & !is3

  cache_status[unplaced] <- "unplaced"
  cache_status[is0] <- "reference"
  placed_at[is0] <- .row_timestamp(
    data, c("reference_at", "ref_at", "reference_cached_at", "ref_cached_at",
            "reference_updated_at", "ref_updated_at", "cached_at"), is0
  )
  cache_status[is3] <- "manual"
  placed_at[is3] <- .row_timestamp(
    data, c("manual_at", "manual_applied_at", "override_at",
            "manual_updated_at"), is3
  )

  for (i in which(net)) {
    status_i <- "fresh"
    placed_i <- run_started
    if (!is.null(cache)) {
      spec <- .stamp_spec_for(pass[i], data[i, , drop = FALSE], bbox)
      if (!is.null(spec)) {
        rows <- .cache_peek(cache, .cache_key(spec$method, spec$query,
                                              spec$params))
        if (!is.null(rows)) {
          keep <- rows[rows$result_rank > 0L & rows$status != "no_match", ,
                       drop = FALSE]
          if (nrow(keep) >= 1L) {
            cached_at <- keep$cached_at[1]
            placed_i <- cached_at
            if (!is.na(cached_at) && cached_at < run_started) {
              status_i <- "cached"
            }
          }
        }
      }
    }
    cache_status[i] <- status_i
    placed_at[i] <- placed_i
  }

  data$placed_at <- placed_at
  data$cache_status <- cache_status
  data
}

.row_timestamp <- function(data, cols, idx) {
  out <- rep(NA_character_, nrow(data))
  for (col in cols) {
    if (!col %in% names(data)) {
      next
    }
    val <- as.character(data[[col]])
    take <- idx & is.na(out) & !is.na(val) & nzchar(val)
    out[take] <- val[take]
  }
  out[idx]
}

.stamp_spec_for <- function(pass, row, bbox) {
  if (startsWith(pass, "pass_1")) {
    list(method = "census_structured", query = .census_query_vec(row),
         params = .census_params())
  } else if (startsWith(pass, "pass_2")) {
    list(method = "arcgis_oneline", query = .arcgis_query_vec(row),
         params = .arcgis_params("arcgis", bbox))
  } else if (startsWith(pass, "pass_4")) {
    list(method = "arcgis_byname", query = .name_query_vec(row),
         params = .name_params("arcgis", bbox, list(full_results = TRUE)))
  } else {
    NULL
  }
}
