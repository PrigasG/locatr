# Response cache for reproducible, offline-replayable geocoding (roadmap #2,
# phase 1). The cache table is long - one row per candidate result - so
# geocode_address()'s ranked candidate set replays exactly, and a "no match" is
# stored as a single sentinel row so misses are replayable too.
#
# The `key` is an implementation detail (rlang::hash of method/query/params);
# the visible `method`/`endpoint`/`query`/`params` columns are the audit
# contract and can rebuild keys if a future rlang changes the hash.

# Bump when the on-disk column layout changes.
.CACHE_SCHEMA_VERSION <- "1"

.cache_empty_table <- function() {
  data.frame(
    key = character(), method = character(), endpoint = character(),
    query = character(), params = character(), params_hash = character(),
    result_rank = integer(), latitude = double(), longitude = double(),
    score = double(), addr_type = character(), status = character(),
    matched_address = character(), cached_at = character(),
    locatr_version = character(), cache_schema_version = character(),
    stringsAsFactors = FALSE
  )
}

#' Create a locatr response cache
#'
#' A cache of parsed geocoder results that makes runs reproducible: repeated
#' queries are served locally instead of re-hitting the service, and - because
#' the parsed coordinates are stored - a cached result can be replayed offline,
#' even without the `httr`/`jsonlite` packages that the live call needs. Pass the
#' returned object to [geocode_address()] via its `cache` argument.
#'
#' The cache table is *long*: one row per candidate result (so
#' [geocode_address()]'s ranked set round-trips exactly), plus a single
#' sentinel row (`result_rank = 0`, `status = "no_match"`) for a query that
#' matched nothing, so misses are replayable and never silently re-queried.
#'
#' The lookup `key` is an implementation detail (a hash). The visible
#' `method`, `endpoint`, `query`, and `params` columns are the audit contract -
#' keys can always be rebuilt from them if a future `rlang` changes its hash.
#'
#' @param path Optional file path for a persistent cache. `NULL` (default) keeps
#'   the cache in memory for the session only - no disk writes. When a path is
#'   given, the cache is loaded from it if present and flushed on every write.
#' @param format On-disk format when `path` is set: `"rds"` (default, no extra
#'   dependency) or `"parquet"` (needs `arrow`).
#' @param store_raw Reserved for storing raw service responses. `FALSE` by
#'   default; raw storage is only ever kept for services whose terms permit it.
#'
#' @return A `locatr_cache` object (an environment) to pass to geocoding
#'   functions.
#' @seealso [cache_info()], [cache_clear()], [geocode_address()]
#' @export
#' @examples
#' cache <- locatr_cache()
#' cache_info(cache)
locatr_cache <- function(path = NULL, format = c("rds", "parquet"),
                         store_raw = FALSE) {
  format <- match.arg(format)
  if (!is.null(path) && (!is.character(path) || length(path) != 1L ||
                         is.na(path))) {
    stop("`path` must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(store_raw) || length(store_raw) != 1L || is.na(store_raw)) {
    stop("`store_raw` must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(store_raw)) {
    warning("Raw response storage is not implemented yet; locatr will store ",
            "parsed cache fields only.", call. = FALSE)
  }
  if (identical(format, "parquet") &&
      !requireNamespace("arrow", quietly = TRUE)) {
    stop("A Parquet cache needs the 'arrow' package.", call. = FALSE)
  }

  cache <- new.env(parent = emptyenv())
  cache$path <- path
  cache$format <- format
  cache$store_raw <- store_raw
  cache$schema_version <- .CACHE_SCHEMA_VERSION
  cache$hits <- 0L
  cache$misses <- 0L
  cache$writes <- 0L
  cache$table <- if (!is.null(path) && file.exists(path)) {
    .cache_read(path, format)
  } else {
    .cache_empty_table()
  }
  class(cache) <- "locatr_cache"
  cache
}

#' Summarise a locatr cache
#'
#' @param cache A `locatr_cache` from [locatr_cache()].
#' @return A one-row tibble: `rows`, distinct `keys`, distinct `methods`,
#'   `oldest`/`newest` `cached_at`, whether it is `persistent`, its `path`, and
#'   `format`.
#' @seealso [locatr_cache()]
#' @export
cache_info <- function(cache) {
  .stop_if_not_cache(cache)
  tbl <- cache$table
  method_counts <- if (nrow(tbl) > 0L) {
    as.list(table(tbl$method, useNA = "no"))
  } else {
    list()
  }
  tibble::tibble(
    rows = nrow(tbl),
    keys = length(unique(tbl$key)),
    methods = length(unique(tbl$method)),
    method_counts = list(method_counts),
    oldest = if (nrow(tbl) > 0L) min(tbl$cached_at) else NA_character_,
    newest = if (nrow(tbl) > 0L) max(tbl$cached_at) else NA_character_,
    persistent = !is.null(cache$path),
    path = if (is.null(cache$path)) NA_character_ else cache$path,
    file_size = if (!is.null(cache$path) && file.exists(cache$path)) {
      file.info(cache$path)$size
    } else {
      NA_real_
    },
    format = cache$format
  )
}

#' Clear a locatr cache
#'
#' Empties the in-memory table. For a persistent cache this also deletes the
#' file, so it is guarded: a persistent cache requires `confirm = TRUE`.
#'
#' @param cache A `locatr_cache` from [locatr_cache()].
#' @param confirm Must be `TRUE` to clear a persistent (path-backed) cache and
#'   delete its file. Ignored for memory-only caches.
#' @return The cleared `cache`, invisibly.
#' @seealso [locatr_cache()]
#' @export
cache_clear <- function(cache, confirm = FALSE) {
  .stop_if_not_cache(cache)
  if (!is.null(cache$path) && !isTRUE(confirm)) {
    stop("Refusing to clear a persistent cache. Pass `confirm = TRUE` to also ",
         "delete ", cache$path, ".", call. = FALSE)
  }
  cache$table <- .cache_empty_table()
  if (!is.null(cache$path) && file.exists(cache$path)) {
    file.remove(cache$path)
  }
  invisible(cache)
}

#' @rdname locatr_cache
#' @param x A `locatr_cache` object.
#' @param ... Ignored.
#' @export
print.locatr_cache <- function(x, ...) {
  info <- cache_info(x)
  cat(sprintf(
    "<locatr_cache> %s | %d result row(s) across %d quer%s\n",
    if (is.null(x$path)) "memory" else paste0("disk: ", x$path),
    info$rows, info$keys, if (info$keys == 1L) "y" else "ies"
  ))
  invisible(x)
}

# ---- internals --------------------------------------------------------------

.stop_if_not_cache <- function(cache) {
  if (!inherits(cache, "locatr_cache")) {
    stop("`cache` must be a locatr_cache object from `locatr_cache()`.",
         call. = FALSE)
  }
  invisible(cache)
}

.validate_cache_args <- function(cache, refresh) {
  if (!is.null(cache)) {
    .stop_if_not_cache(cache)
  }
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    stop("`refresh` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  invisible(NULL)
}

.cache_read <- function(path, format) {
  tbl <- if (identical(format, "parquet")) {
    as.data.frame(arrow::read_parquet(path), stringsAsFactors = FALSE)
  } else {
    readRDS(path)
  }
  if (!is.data.frame(tbl)) .cache_empty_table() else tbl
}

.cache_flush <- function(cache) {
  if (is.null(cache$path)) {
    return(invisible(cache))
  }
  if (identical(cache$format, "parquet")) {
    arrow::write_parquet(cache$table, cache$path)
  } else {
    saveRDS(cache$table, cache$path)
  }
  invisible(cache)
}

# Canonical, dependency-free serialisation of the query params, sorted by name
# so the string (and its hash) are stable regardless of argument order.
.cache_params_string <- function(params) {
  if (length(params) == 0L) {
    return("")
  }
  if (is.null(names(params))) {
    names(params) <- as.character(seq_along(params))
  }
  params <- params[order(names(params))]
  flat <- vapply(params, .cache_param_value, character(1))
  paste(names(params), flat, sep = "=", collapse = "; ")
}

.cache_param_value <- function(v) {
  if (is.null(v) || length(v) == 0L) {
    return("")
  }
  if (is.list(v)) {
    if (is.null(names(v))) {
      names(v) <- as.character(seq_along(v))
    }
    v <- v[order(names(v))]
    inner <- vapply(v, .cache_param_value, character(1))
    return(paste(names(v), inner, sep = ":", collapse = ","))
  }
  x <- as.character(v)
  x[is.na(x)] <- "<NA>"
  paste(x, collapse = ",")
}

.cache_key <- function(method, query, params) {
  rlang::hash(list(method = method, query = query,
                   params = .cache_params_string(params)))
}

.cache_now <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Return cached rows for a key (ordered by rank), or NULL on miss. `max_age` is
# in days; when set, an entry older than that is treated as a miss. Updates the
# cache hit/miss counters used by the run manifest.
.cache_get <- function(cache, key, max_age = NULL) {
  rows <- cache$table[cache$table$key == key, , drop = FALSE]
  if (nrow(rows) == 0L) {
    cache$misses <- cache$misses + 1L
    return(NULL)
  }
  if (!is.null(max_age)) {
    newest <- max(as.POSIXct(rows$cached_at, tz = "UTC",
                             format = "%Y-%m-%dT%H:%M:%SZ"))
    if (as.numeric(difftime(Sys.time(), newest, units = "days")) > max_age) {
      cache$misses <- cache$misses + 1L
      return(NULL)
    }
  }
  cache$hits <- cache$hits + 1L
  rows[order(rows$result_rank), , drop = FALSE]
}

# Read cached rows without touching the hit/miss counters (used by the
# placement stamper, which must not distort the manifest counts).
.cache_peek <- function(cache, key) {
  rows <- cache$table[cache$table$key == key, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  rows[order(rows$result_rank), , drop = FALSE]
}

# Store a candidate set (or a no-match sentinel) under `key`, replacing any
# existing rows for that key so `refresh` overwrites cleanly.
.cache_put <- function(cache, key, method, endpoint, query, params,
                       candidates) {
  ps <- .cache_params_string(params)
  ph <- rlang::hash(ps)
  now <- .cache_now()
  ver <- as.character(utils::packageVersion("locatr"))

  row <- function(rank, lat, lon, score, atype, status, maddr) {
    data.frame(
      key = key, method = method, endpoint = endpoint, query = query,
      params = ps, params_hash = ph, result_rank = as.integer(rank),
      latitude = as.double(lat), longitude = as.double(lon),
      score = as.double(score), addr_type = as.character(atype),
      status = status, matched_address = as.character(maddr),
      cached_at = now, locatr_version = ver,
      cache_schema_version = cache$schema_version,
      stringsAsFactors = FALSE
    )
  }

  new_rows <- if (is.null(candidates) || nrow(candidates) == 0L) {
    row(0L, NA_real_, NA_real_, NA_real_, NA_character_, "no_match",
        NA_character_)
  } else {
    row(seq_len(nrow(candidates)), candidates$latitude, candidates$longitude,
        candidates$match_score, candidates$match_addr_type, "matched",
        candidates$matched_address)
  }

  cache$table <- rbind(
    cache$table[cache$table$key != key, , drop = FALSE], new_rows
  )
  cache$writes <- cache$writes + 1L
  .cache_flush(cache)
  invisible(cache)
}

# Shared cache-key builders for the batch tiers. Used by both the *_fill_coords
# helpers (to store/lookup) and the placement stamper (to classify fresh vs
# cached), so the two can never drift out of sync.
.census_query_vec <- function(df) {
  paste(df$address_clean, df$city_clean, df$state_clean, df$zip_clean,
        sep = "|")
}
.census_params <- function(dots = list()) {
  c(list(endpoint = "census", method = "census"), dots)
}

.arcgis_query_vec <- function(df) as.character(df$full_address_clean)
.arcgis_params <- function(method, bbox, dots = list()) {
  dots <- .region_geocoder_dots(method, bbox, dots)
  list(
    endpoint = "arcgis_findaddresscandidates_oneline",
    method = method,
    full_results = dots$full_results %||% NA,
    custom_query = dots$custom_query %||% list(),
    searchExtent = if (!is.null(dots$custom_query$searchExtent)) {
      dots$custom_query$searchExtent
    } else {
      NA_character_
    }
  )
}

.name_query_vec <- function(df) {
  paste0(df$record_name, ", ", df$city_clean, ", ", df$state_clean)
}
.name_params <- function(method, bbox, dots = list()) {
  dots <- .region_geocoder_dots(method, bbox, dots)
  list(
    endpoint = "arcgis_findaddresscandidates_byname",
    method = method,
    full_results = dots$full_results %||% NA,
    custom_query = dots$custom_query %||% list(),
    searchExtent = if (!is.null(dots$custom_query$searchExtent)) {
      dots$custom_query$searchExtent
    } else {
      NA_character_
    }
  )
}

# Rebuild the candidate tibble that .arcgis_candidates() would have returned
# from cached rows (dropping the no-match sentinel).
.cache_candidates_from_rows <- function(rows) {
  rows <- rows[rows$result_rank > 0L & rows$status != "no_match", ,
               drop = FALSE]
  tibble::tibble(
    matched_address = rows$matched_address,
    longitude = rows$longitude,
    latitude = rows$latitude,
    match_score = rows$score,
    match_addr_type = rows$addr_type
  )
}

# Cache-aware wrapper around .arcgis_candidates(). On a hit it returns the
# stored candidate set without any network call (or need for httr/jsonlite);
# on a miss it calls the live service and stores the result (or a sentinel).
.arcgis_candidates_cached <- function(single_line, max_candidates = 5L,
                                      bbox = NULL, cache = NULL,
                                      refresh = FALSE, max_age = NULL) {
  method <- "arcgis_findaddresscandidates"
  endpoint <- paste0(
    "https://geocode.arcgis.com/arcgis/rest/services/World/",
    "GeocodeServer/findAddressCandidates"
  )
  params <- list(
    endpoint = endpoint,
    countryCode = "USA",
    maxLocations = as.integer(max_candidates),
    outFields = "Addr_type,Match_addr",
    searchExtent = if (!is.null(bbox)) {
      paste(bbox[["lon_min"]], bbox[["lat_min"]],
            bbox[["lon_max"]], bbox[["lat_max"]], sep = ",")
    } else {
      NA_character_
    }
  )
  key <- .cache_key(method, single_line, params)

  if (!is.null(cache) && !isTRUE(refresh)) {
    hit <- .cache_get(cache, key, max_age = max_age)
    if (!is.null(hit)) {
      return(.cache_candidates_from_rows(hit))
    }
  }

  cands <- .arcgis_candidates(single_line, max_candidates = max_candidates,
                              bbox = bbox)
  if (!is.null(cache)) {
    .cache_put(cache, key, method, endpoint, single_line, params, cands)
  }
  cands
}

# Batch-tier cache orchestration. `input` is the subset of rows to geocode;
# `queries` is a per-row character vector used for the cache key; `run` runs the
# live tidygeocoder call on a subset and returns a tibble with `record_id`,
# `latitude`, `longitude`, and optionally `score` / `addr_type`. Returns those
# normalised columns for every row of `input`, served from cache where possible
# and stored (or sentinel-stored for a no-match) on a miss.
.batch_geocode_cached <- function(input, queries, method, params, cache,
                                  refresh, run) {
  n <- nrow(input)
  ids <- as.character(input$record_id)
  keys <- vapply(queries, function(query) .cache_key(method, query, params),
                 character(1))
  lat <- rep(NA_real_, n)
  lon <- rep(NA_real_, n)
  sc  <- rep(NA_real_, n)
  at  <- rep(NA_character_, n)
  hit <- rep(FALSE, n)

  if (!is.null(cache) && !isTRUE(refresh)) {
    for (key in unique(keys)) {
      idx <- which(keys == key)
      rows <- .cache_get(cache, key)
      if (!is.null(rows)) {
        hit[idx] <- TRUE
        keep <- rows[rows$result_rank > 0L & rows$status != "no_match", ,
                     drop = FALSE]
        if (nrow(keep) >= 1L) {
          lat[idx] <- keep$latitude[1]
          lon[idx] <- keep$longitude[1]
          sc[idx]  <- keep$score[1]
          at[idx]  <- keep$addr_type[1]
        }
      }
    }
  }

  miss <- !hit
  if (any(miss)) {
    miss_keys <- unique(keys[miss])
    reps <- match(miss_keys, keys)
    res <- run(input[reps, , drop = FALSE])
    res_ids <- as.character(res$record_id)

    for (j in seq_along(miss_keys)) {
      group <- which(keys == miss_keys[j])
      rep_row <- reps[j]
      res_idx <- match(ids[rep_row], res_ids)
      if (!is.na(res_idx)) {
        lat[group] <- res$latitude[res_idx]
        lon[group] <- res$longitude[res_idx]
        if ("score" %in% names(res)) sc[group] <- res$score[res_idx]
        if ("addr_type" %in% names(res)) {
          at[group] <- as.character(res$addr_type)[res_idx]
        }
      }
    }

    if (!is.null(cache)) {
      endpoint <- if (!is.null(params$endpoint)) params$endpoint else NA_character_
      for (j in seq_along(miss_keys)) {
        i <- reps[j]
        cand <- if (is.na(lat[i]) || is.na(lon[i])) {
          NULL
        } else {
          tibble::tibble(latitude = lat[i], longitude = lon[i],
                         match_score = sc[i], match_addr_type = at[i],
                         matched_address = NA_character_)
        }
        .cache_put(cache, miss_keys[j], method,
                   endpoint, queries[i], params, cand)
      }
    }
  }

  tibble::tibble(record_id = ids, latitude = lat, longitude = lon,
                 score = sc, addr_type = at)
}
