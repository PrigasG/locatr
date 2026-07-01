test_that("summarise_geocoding reports core quality counts", {
  data <- tibble::tibble(
    record_id = c("a", "b", "c", "d"),
    latitude = c(40, 41, NA, 42),
    longitude = c(-75, -76, NA, -77),
    match_status = c("matched", "matched_low_confidence", "unmatched",
                     "matched"),
    review_status = c("auto_accepted", "needs_manual_review",
                      "needs_manual_review", "manual_override_applied"),
    validation_status = c("coordinate_ok", "coordinate_ok",
                          "missing_coordinates", "outside_region"),
    geocode_pass = c("pass_1_census_structured", "pass_4_name_lookup",
                     NA_character_, "pass_3_manual"),
    location_locality = c("A", NA, NA, "D"),
    manual_override_used = c(FALSE, FALSE, FALSE, TRUE)
  )

  out <- summarise_geocoding(data)

  expect_equal(out$n_records, 4)
  expect_equal(out$matched, 3)
  expect_equal(out$matched_pct, 75)
  expect_equal(out$missing_coordinates, 1)
  expect_equal(out$auto_accepted, 1)
  expect_equal(out$needs_manual_review, 2)
  expect_equal(out$manual_override_applied, 1)
  expect_equal(out$outside_region, 1)
  expect_equal(out$name_lookup, 1)
  expect_equal(out$low_confidence_name, 1)
  expect_equal(out$missing_geography, 1)
})

test_that("explain_geocode_result returns reviewer-friendly text", {
  data <- tibble::tibble(
    record_id = c("a", "b"),
    geocode_pass = c("pass_4_name_lookup", "pass_1_census_structured"),
    match_status = c("matched_low_confidence", "matched"),
    validation_status = c("coordinate_ok", "outside_region"),
    review_status = c("needs_manual_review", "rejected"),
    nm_score = c(87, NA),
    nm_addr_type = c("POI", NA)
  )

  all <- explain_geocode_result(data)
  one <- explain_geocode_result(data, row = "a")

  expect_length(all, 2)
  expect_match(one, "Record a")
  expect_match(one, "name lookup")
  expect_match(one, "87 / POI", fixed = TRUE)
  expect_match(one, "needs manual review")
  expect_match(explain_geocode_result(data, row = 2), "outside the expected region")
})

test_that("suggest_geography_level gives practical state recommendations", {
  nj <- suggest_geography_level("nj")
  ca <- suggest_geography_level("CA")
  dc <- suggest_geography_level("DC")

  expect_equal(nj$state, "NJ")
  expect_equal(nj$recommended_geography, "county_subdivision")
  expect_equal(ca$recommended_geography, "place")
  expect_equal(dc$recommended_geography, "county")
  expect_match(nj$function_call, "build_local_geography")
  expect_error(suggest_geography_level(NA_character_), "state")
})

test_that("compare_geocode_runs flags coordinate and audit changes", {
  old <- tibble::tibble(
    record_id = c("a", "b", "c"),
    latitude = c(40, 41, 42),
    longitude = c(-75, -76, -77),
    review_status = c("auto_accepted", "needs_manual_review",
                      "auto_accepted"),
    geocode_pass = c("pass_1_census_structured", "pass_4_name_lookup",
                     "pass_2_fallback"),
    location_locality = c("Alpha", "Beta", "Gamma")
  )
  new <- tibble::tibble(
    record_id = c("a", "b", "d"),
    latitude = c(40, 41.01, 43),
    longitude = c(-75, -76, -78),
    review_status = c("auto_accepted", "auto_accepted",
                      "needs_manual_review"),
    geocode_pass = c("pass_1_census_structured", "pass_4_name_lookup",
                     "pass_2_fallback"),
    location_locality = c("Alpha", "Beta City", "Delta")
  )

  changed <- compare_geocode_runs(old, new)
  all_rows <- compare_geocode_runs(old, new, changed_only = FALSE)

  expect_equal(sort(changed$record_id), c("b", "c", "d"))
  b <- changed[changed$record_id == "b", ]
  expect_true(b$coordinate_changed)
  expect_true(b$review_status_changed)
  expect_true(b$geography_changed)
  expect_equal(changed$row_status[changed$record_id == "c"], "removed")
  expect_equal(changed$row_status[changed$record_id == "d"], "added")
  expect_equal(nrow(all_rows), 4)
  expect_error(compare_geocode_runs(old, new, by = "missing"), "exist")
})

test_that("plot_geocode_review_map returns a leaflet map", {
  skip_if_not_installed("leaflet")
  data <- tibble::tibble(
    record_id = c("a", "b"),
    latitude = c(40, NA),
    longitude = c(-75, NA),
    review_status = c("auto_accepted", "needs_manual_review")
  )

  out <- plot_geocode_review_map(data)

  expect_s3_class(out, "leaflet")
  expect_error(plot_geocode_review_map(tibble::tibble(x = 1)), "latitude")
})
