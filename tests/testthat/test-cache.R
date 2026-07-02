fake_candidates <- function(...) {
  tibble::tibble(
    matched_address = c("1 Bay Ave, Montclair", "1 Bay Ave, Newark"),
    longitude = c(-74.21, -74.17),
    latitude = c(40.82, 40.73),
    match_score = c(98, 91),
    match_addr_type = c("PointAddress", "StreetAddress")
  )
}

no_candidates <- function(...) {
  tibble::tibble(
    matched_address = character(), longitude = double(),
    latitude = double(), match_score = double(),
    match_addr_type = character()
  )
}

test_that("locatr_cache starts empty and validates args", {
  cache <- locatr_cache()
  expect_s3_class(cache, "locatr_cache")
  info <- cache_info(cache)
  expect_equal(info$rows, 0L)
  expect_false(info$persistent)

  expect_error(locatr_cache(path = 1), "single file path")
  expect_error(locatr_cache(store_raw = NA), "TRUE or FALSE")
})

test_that("candidate lookups are cached and replayed without re-calling", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L,
                                  bbox = NULL) {
      calls <<- calls + 1L
      fake_candidates()
    }
  )
  cache <- locatr_cache()
  a <- .arcgis_candidates_cached("1 BAY AVE", cache = cache)   # miss
  b <- .arcgis_candidates_cached("1 BAY AVE", cache = cache)   # hit

  expect_equal(calls, 1L)
  expect_equal(a, b)
  expect_equal(nrow(a), 2L)
  expect_equal(cache_info(cache)$keys, 1L)
})

test_that("refresh re-queries and overwrites the same key", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L,
                                  bbox = NULL) {
      calls <<- calls + 1L
      fake_candidates()
    }
  )
  cache <- locatr_cache()
  .arcgis_candidates_cached("1 BAY AVE", cache = cache)
  .arcgis_candidates_cached("1 BAY AVE", cache = cache, refresh = TRUE)

  expect_equal(calls, 2L)
  expect_equal(cache_info(cache)$keys, 1L)
})

test_that("a no-match is cached as a sentinel and replayed", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L,
                                  bbox = NULL) {
      calls <<- calls + 1L
      no_candidates()
    }
  )
  cache <- locatr_cache()
  a <- .arcgis_candidates_cached("NOWHERE AT ALL", cache = cache)
  b <- .arcgis_candidates_cached("NOWHERE AT ALL", cache = cache)

  expect_equal(calls, 1L)
  expect_equal(nrow(a), 0L)
  expect_equal(nrow(b), 0L)
  expect_equal(cache_info(cache)$rows, 1L)   # a single sentinel row
})

test_that("different query params do not collide", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L,
                                  bbox = NULL) {
      calls <<- calls + 1L
      fake_candidates()
    }
  )
  cache <- locatr_cache()
  .arcgis_candidates_cached("1 BAY AVE", cache = cache)
  .arcgis_candidates_cached("1 BAY AVE", cache = cache,
                            bbox = region_bbox("NJ"))

  expect_equal(calls, 2L)
  expect_equal(cache_info(cache)$keys, 2L)
})

test_that("a persistent cache round-trips to disk and replays offline", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L,
                                  bbox = NULL) fake_candidates()
  )
  cache <- locatr_cache(path = tmp)
  .arcgis_candidates_cached("1 BAY AVE", cache = cache)
  expect_true(file.exists(tmp))

  cache2 <- locatr_cache(path = tmp)          # reload from disk
  expect_gt(cache_info(cache2)$rows, 0L)

  # a hit from the reloaded cache must not touch the live service
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L,
                                  bbox = NULL) stop("should not be called")
  )
  out <- .arcgis_candidates_cached("1 BAY AVE", cache = cache2)
  expect_equal(nrow(out), 2L)
})

test_that("cache_clear protects a persistent cache", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L,
                                  bbox = NULL) fake_candidates()
  )
  cache <- locatr_cache(path = tmp)
  .arcgis_candidates_cached("1 BAY AVE", cache = cache)

  expect_error(cache_clear(cache), "confirm")
  expect_true(file.exists(tmp))

  cache_clear(cache, confirm = TRUE)
  expect_equal(cache_info(cache)$rows, 0L)
  expect_false(file.exists(tmp))
})

test_that("geocode_address caches candidate lookups end to end", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L,
                                  bbox = NULL) {
      calls <<- calls + 1L
      fake_candidates()
    }
  )
  cache <- locatr_cache()
  r1 <- geocode_address("1 Bay Ave", city = "Montclair", geography = FALSE,
                        cache = cache)
  r2 <- geocode_address("1 Bay Ave", city = "Montclair", geography = FALSE,
                        cache = cache)

  expect_equal(calls, 1L)
  expect_equal(r1$matched_address, r2$matched_address)
})

test_that("geocode_address validates cache and refresh", {
  expect_error(
    geocode_address("1 Bay Ave", city = "Montclair", cache = "nope"),
    "locatr_cache"
  )
  expect_error(
    geocode_address("1 Bay Ave", city = "Montclair", refresh = NA),
    "TRUE.*FALSE"
  )
})
