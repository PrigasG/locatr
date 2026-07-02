report_df <- function() {
  df <- tibble::tibble(
    record_id = c("a", "b", "c", "d"),
    geocode_pass = c("pass_1_census_structured", "pass_2_fallback",
                     "pass_4_name_lookup", NA_character_),
    review_status = c("auto_accepted", "auto_accepted",
                      "needs_manual_review", "rejected"),
    match_confidence = c(0.9, 0.72, 0.35, 0.02),
    cache_status = c("fresh", "cached", "fresh", "unplaced")
  )
  attr(df, "locatr_run") <- structure(
    list(
      run_id = "abc123def456", run_at = "2020-06-01T00:00:00Z",
      locatr_version = "0.1.0", tidygeocoder_version = "1.0.5",
      cache_schema_version = "1", tiers = c("census", "arcgis", "name"),
      services = list(), bbox = region_bbox("NJ"), boundary = NA_character_,
      reference_used = FALSE, cache_path = "memory", n_records = 4L,
      status_counts = list(), cache_hits = 1L, cache_misses = 2L,
      cache_writes = 2L
    ),
    class = "locatr_provenance"
  )
  df
}

test_that("geocode_report summarises a run", {
  rep <- geocode_report(report_df())

  expect_s3_class(rep, "locatr_report")
  expect_equal(rep$n_records, 4L)
  expect_equal(rep$review_status[["auto_accepted"]], 2L)
  expect_equal(rep$review_status[["rejected"]], 1L)
  expect_equal(rep$tiers[["census"]], 1L)
  expect_equal(rep$tiers[["name_lookup"]], 1L)
  expect_equal(rep$tiers[["unplaced"]], 1L)
  expect_equal(rep$confidence$n_below, 2L)

  expect_match(rep$methods, "auto-accepted")
  expect_match(rep$methods, "US Census")
  expect_match(rep$methods, "reproducibility cache")   # one cached row
  expect_match(rep$methods, "bounding box")            # bbox region guard
})

test_that("geocode_report writes a Markdown file and returns invisibly", {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)

  res <- geocode_report(report_df(), file = tmp)
  expect_s3_class(res, "locatr_report")
  expect_true(file.exists(tmp))

  txt <- paste(readLines(tmp), collapse = "\n")
  expect_match(txt, "# Geocoding report")
  expect_match(txt, "## Methods")
  expect_match(txt, "## Review status")
  expect_match(txt, "## Match confidence")
})

test_that("geocode_report works without a manifest and validates args", {
  df <- tibble::tibble(
    review_status = c("auto_accepted", "rejected"),
    geocode_pass = c("pass_1_census_structured", NA_character_),
    match_confidence = c(0.9, 0.1)
  )
  rep <- geocode_report(df)

  expect_s3_class(rep, "locatr_report")
  expect_null(rep$run)
  expect_match(rep$methods, "the configured region")

  expect_error(geocode_report(1), "data frame")
  expect_error(geocode_report(df, low_confidence_below = 2), "0 to 1")
  expect_error(geocode_report(df, file = 1), "single file path")
})
