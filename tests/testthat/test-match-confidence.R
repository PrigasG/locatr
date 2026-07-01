test_that("add_match_confidence scores pipeline rows by tier and status", {
  df <- data.frame(
    geocode_pass = c("pass_0_reference", "pass_1_census_structured",
                     "pass_2_fallback", "pass_4_name_lookup",
                     "pass_4_name_lookup", "pass_1_census_structured",
                     "pass_2_fallback"),
    match_status = c("matched", "matched", "matched", "matched",
                     "matched_low_confidence", "no_match", "matched"),
    validation_status = c("coordinate_ok", "coordinate_ok", "coordinate_ok",
                          "coordinate_ok", "coordinate_ok", "coordinate_ok",
                          "outside_region"),
    nm_status = c(NA, NA, NA, "name_matched_high_confidence",
                  "name_matched_low_confidence", NA, NA),
    latitude = c(40, 40, 40, 40, 40, NA, 40),
    longitude = c(-74, -74, -74, -74, -74, NA, -74),
    stringsAsFactors = FALSE
  )

  out <- add_match_confidence(df)

  expect_equal(out$match_confidence,
               c(0.97, 0.90, 0.72, 0.68, 0.35, 0.00, 0.02))
  expect_equal(out$confidence_reason[1], "reference-verified coordinate")
  expect_equal(out$confidence_reason[5], "low-confidence name match")
  expect_equal(out$confidence_reason[6], "no geocoder match")
  expect_equal(out$confidence_reason[7], "rejected: coordinate outside region")
})

test_that("add_match_confidence scores candidates by score and address type", {
  df <- tibble::tibble(
    match_score = c(92, 92, 90),
    match_addr_type = c("PointAddress", "Locality", "PointAddress"),
    in_bbox = c(NA, NA, FALSE)
  )

  out <- add_match_confidence(df)

  expect_equal(out$match_confidence, c(0.92, 0.736, 0.30))
  expect_match(out$confidence_reason[1], "^arcgis score 92, pointaddress$")
  expect_match(out$confidence_reason[3], "outside expected region")
})

test_that("add_match_confidence is monotone across tiers", {
  df <- data.frame(
    geocode_pass = c("pass_0_reference", "pass_1_census_structured",
                     "pass_2_fallback", "pass_4_name_lookup"),
    match_status = "matched",
    validation_status = "coordinate_ok",
    latitude = 40, longitude = -74,
    stringsAsFactors = FALSE
  )
  out <- add_match_confidence(df)
  expect_false(is.unsorted(rev(out$match_confidence)))
})

test_that("add_match_confidence validates input", {
  expect_error(add_match_confidence(list(a = 1)), "data frame")
})
