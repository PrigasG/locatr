ready_row <- function() {
  tibble::tibble(
    record_id = "a", record_name = "Site A",
    address_clean = "100 MAIN STREET", city_clean = "TRENTON",
    state_clean = "NJ", zip_clean = "08608",
    full_address_clean = "100 MAIN STREET, TRENTON, NJ 08608",
    review_status = "ready_for_geocoding", bad_address_flag = NA_character_
  )
}

test_that("geocode_records attaches a run manifest and stamps a fresh placement", {
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      dplyr::mutate(.tbl, latitude = 40.22, longitude = -74.76)
    },
    .package = "tidygeocoder"
  )
  out <- geocode_records(ready_row(), verbose = FALSE)
  prov <- geocode_provenance(out)

  expect_s3_class(prov, "locatr_provenance")
  expect_equal(prov$n_records, 1L)
  expect_equal(prov$tiers, c("census", "arcgis", "name"))
  expect_false(prov$reference_used)
  expect_true(is.na(prov$cache_hits))            # no cache used
  expect_equal(out$cache_status, "fresh")        # placed live this run
  expect_equal(out$placed_at, prov$run_at)
})

test_that("geocode_records reports cache misses and writes on a first run", {
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      dplyr::mutate(.tbl, latitude = 40.22, longitude = -74.76)
    },
    .package = "tidygeocoder"
  )
  cache <- locatr_cache()
  out <- geocode_records(ready_row(), verbose = FALSE, cache = cache)
  prov <- geocode_provenance(out)

  expect_equal(prov$cache_hits, 0L)
  expect_equal(prov$cache_misses, 1L)
  expect_equal(prov$cache_writes, 1L)
  expect_equal(prov$cache_path, "memory")
})

test_that(".stamp_placement marks a pre-existing cached coordinate as cached", {
  cache <- locatr_cache()
  query <- "100 MAIN STREET|TRENTON|NJ|08608"
  .cache_put(
    cache, .cache_key("census_structured", query, .census_params()),
    "census_structured", "census", query, .census_params(),
    tibble::tibble(latitude = 40.22, longitude = -74.76,
                   match_score = NA_real_, match_addr_type = NA_character_,
                   matched_address = NA_character_)
  )
  cache$table$cached_at <- "2000-01-01T00:00:00Z"   # backdate before the run

  df <- tibble::tibble(
    record_id = "a", address_clean = "100 MAIN STREET", city_clean = "TRENTON",
    state_clean = "NJ", zip_clean = "08608",
    latitude = 40.22, longitude = -74.76,
    geocode_pass = "pass_1_census_structured"
  )
  out <- .stamp_placement(df, cache, run_started = "2020-06-01T00:00:00Z",
                          bbox = region_bbox("NJ"))

  expect_equal(out$cache_status, "cached")
  expect_equal(out$placed_at, "2000-01-01T00:00:00Z")
})

test_that(".stamp_placement classifies reference, manual, and unplaced rows", {
  df <- tibble::tibble(
    record_id = c("r", "m", "u"),
    geocode_pass = c("pass_0_reference", "pass_3_manual", NA_character_),
    latitude = c(40.2, 40.3, NA_real_),
    longitude = c(-74.7, -74.8, NA_real_)
  )
  out <- .stamp_placement(df, cache = NULL, run_started = "2020-06-01T00:00:00Z",
                          bbox = region_bbox("NJ"))

  expect_equal(out$cache_status, c("reference", "manual", "unplaced"))
  expect_equal(out$placed_at,
               c(NA_character_, NA_character_, NA_character_))
})

test_that(".stamp_placement uses explicit reference and manual timestamps", {
  df <- tibble::tibble(
    record_id = c("r", "m"),
    geocode_pass = c("pass_0_reference", "pass_3_manual"),
    latitude = c(40.2, 40.3),
    longitude = c(-74.7, -74.8),
    reference_at = c("1999-01-01T00:00:00Z", NA_character_),
    manual_applied_at = c(NA_character_, "2020-02-02T00:00:00Z")
  )
  out <- .stamp_placement(df, cache = NULL, run_started = "2020-06-01T00:00:00Z",
                          bbox = region_bbox("NJ"))

  expect_equal(out$placed_at,
               c("1999-01-01T00:00:00Z", "2020-02-02T00:00:00Z"))
})

test_that("geocode_records manifest records services and boundary", {
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      dplyr::mutate(.tbl, latitude = 40.22, longitude = -74.76)
    },
    .package = "tidygeocoder"
  )
  out <- geocode_records(ready_row(), tiers = "census", verbose = FALSE)
  prov <- geocode_provenance(out)

  expect_true("census" %in% names(prov$services))
  expect_true("boundary" %in% names(prov))
})

test_that("geocode_provenance errors when no manifest is present", {
  expect_error(geocode_provenance(data.frame(x = 1)), "No locatr run manifest")
})
