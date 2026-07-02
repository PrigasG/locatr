test_that("geocode_census serves cached rows without re-querying", {
  calls <- 0L
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      calls <<- calls + 1L
      dplyr::mutate(.tbl, latitude = 40.22, longitude = -74.76)
    },
    .package = "tidygeocoder"
  )
  df <- tibble::tibble(
    record_id = "a", address_clean = "100 MAIN STREET", city_clean = "TRENTON",
    state_clean = "NJ", zip_clean = "08608",
    review_status = "ready_for_geocoding"
  )
  cache <- locatr_cache()

  o1 <- geocode_census(df, cache = cache)
  o2 <- geocode_census(df, cache = cache)

  expect_equal(calls, 1L)                       # second run hits the cache
  expect_equal(o1$latitude, 40.22)
  expect_equal(o2$latitude, 40.22)
  expect_equal(o2$match_status, "matched")
})

test_that("geocode_census deduplicates repeated queries within a run", {
  n_seen <- NULL
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      n_seen <<- nrow(.tbl)
      dplyr::mutate(.tbl, latitude = 40.22, longitude = -74.76)
    },
    .package = "tidygeocoder"
  )
  df <- tibble::tibble(
    record_id = c("a", "b"),
    address_clean = "100 MAIN STREET", city_clean = "TRENTON",
    state_clean = "NJ", zip_clean = "08608",
    review_status = "ready_for_geocoding"
  )
  cache <- locatr_cache()

  out <- geocode_census(df, cache = cache)

  expect_equal(n_seen, 1L)
  expect_equal(out$latitude, c(40.22, 40.22))
  expect_equal(cache_info(cache)$keys, 1L)
})

test_that("geocode_census refresh re-queries", {
  calls <- 0L
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      calls <<- calls + 1L
      dplyr::mutate(.tbl, latitude = 40.22, longitude = -74.76)
    },
    .package = "tidygeocoder"
  )
  df <- tibble::tibble(
    record_id = "a", address_clean = "100 MAIN STREET", city_clean = "TRENTON",
    state_clean = "NJ", zip_clean = "08608",
    review_status = "ready_for_geocoding"
  )
  cache <- locatr_cache()
  geocode_census(df, cache = cache)
  geocode_census(df, cache = cache, refresh = TRUE)
  expect_equal(calls, 2L)
})

test_that("geocode_arcgis serves cached rows without re-querying", {
  calls <- 0L
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      calls <<- calls + 1L
      dplyr::mutate(.tbl, fb_latitude = 40.30, fb_longitude = -74.60)
    },
    .package = "tidygeocoder"
  )
  df <- tibble::tibble(
    record_id = "a",
    full_address_clean = "100 MAIN STREET, TRENTON, NJ 08608",
    review_status = "ready_for_geocoding", bad_address_flag = NA_character_,
    latitude = NA_real_, longitude = NA_real_,
    geocode_method = NA_character_, geocode_pass = NA_character_,
    match_status = NA_character_
  )
  cache <- locatr_cache()

  o1 <- geocode_arcgis(df, cache = cache)
  o2 <- geocode_arcgis(df, cache = cache)

  expect_equal(calls, 1L)
  expect_equal(o1$latitude, 40.30)
  expect_equal(o2$latitude, 40.30)
  expect_equal(o2$geocode_pass, "pass_2_fallback")
})

test_that("geocode_by_name caches lookups including score and type", {
  calls <- 0L
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      calls <<- calls + 1L
      dplyr::mutate(.tbl, nm_latitude = 40.30, nm_longitude = -74.60,
                    score = 99, addr_type = "PointAddress")
    },
    .package = "tidygeocoder"
  )
  df <- tibble::tibble(
    record_id = "a", record_name = "Some Hospital",
    city_clean = "TRENTON", state_clean = "NJ",
    review_status = "ready_for_geocoding", bad_address_flag = NA_character_,
    latitude = NA_real_, longitude = NA_real_,
    geocode_method = NA_character_, geocode_pass = NA_character_,
    match_status = NA_character_
  )
  cache <- locatr_cache()

  o1 <- geocode_by_name(df, cache = cache)
  o2 <- geocode_by_name(df, cache = cache)

  expect_equal(calls, 1L)
  expect_equal(o1$nm_score, 99)
  expect_equal(o2$nm_score, 99)                 # score replayed from cache
  expect_equal(o2$nm_addr_type, "PointAddress")
  expect_equal(o2$nm_status, "name_matched_high_confidence")
  expect_equal(o2$match_status, "matched")
})

test_that("geocode_by_name cache key includes effective full_results arg", {
  calls <- 0L
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      calls <<- calls + 1L
      dots <- list(...)
      out <- dplyr::mutate(.tbl, nm_latitude = 40.30, nm_longitude = -74.60)
      if (!identical(dots$full_results, FALSE)) {
        out <- dplyr::mutate(out, score = 99, addr_type = "PointAddress")
      }
      out
    },
    .package = "tidygeocoder"
  )
  df <- tibble::tibble(
    record_id = "a", record_name = "Some Hospital",
    city_clean = "TRENTON", state_clean = "NJ",
    review_status = "ready_for_geocoding", bad_address_flag = NA_character_,
    latitude = NA_real_, longitude = NA_real_,
    geocode_method = NA_character_, geocode_pass = NA_character_,
    match_status = NA_character_
  )
  cache <- locatr_cache()

  low_info <- geocode_by_name(df, cache = cache, full_results = FALSE)
  high_info <- geocode_by_name(df, cache = cache)

  expect_equal(calls, 2L)
  expect_true(is.na(low_info$nm_score))
  expect_equal(high_info$nm_score, 99)
  expect_equal(cache_info(cache)$keys, 2L)
})

test_that("a batch no-match is cached as a sentinel and not re-queried", {
  calls <- 0L
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      calls <<- calls + 1L
      dplyr::mutate(.tbl, latitude = NA_real_, longitude = NA_real_)
    },
    .package = "tidygeocoder"
  )
  df <- tibble::tibble(
    record_id = "a", address_clean = "NOWHERE", city_clean = "TRENTON",
    state_clean = "NJ", zip_clean = "08608",
    review_status = "ready_for_geocoding"
  )
  cache <- locatr_cache()

  o1 <- geocode_census(df, cache = cache)
  o2 <- geocode_census(df, cache = cache)

  expect_equal(calls, 1L)                       # sentinel replayed, no re-query
  expect_true(is.na(o1$latitude))
  expect_equal(o2$match_status, "no_match")
})

test_that("batch cache args are validated", {
  df <- tibble::tibble(
    record_id = "a", address_clean = "100 MAIN STREET", city_clean = "TRENTON",
    state_clean = "NJ", zip_clean = "08608",
    review_status = "ready_for_geocoding"
  )

  expect_error(geocode_census(df, cache = "bad"), "locatr_cache")
  expect_error(geocode_census(df, refresh = NA), "TRUE.*FALSE")
})
