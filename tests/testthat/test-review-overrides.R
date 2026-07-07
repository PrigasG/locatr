test_that("build_review_overrides confirms, relocates, and rejects", {
  geocoded <- data.frame(
    record_id = c("a", "b", "c"),
    record_name = c("A", "B", "C"),
    full_address_clean = c("1 A St", "2 B St", "3 C St"),
    latitude = c(40.1, 40.2, 40.3),
    longitude = c(-74.1, -74.2, -74.3),
    stringsAsFactors = FALSE
  )
  decisions <- data.frame(
    record_id = c("a", "b", "c"),
    decision = c("accept", "relocate", "reject"),
    manual_latitude = c(NA, 40.25, NA),
    manual_longitude = c(NA, -74.25, NA),
    stringsAsFactors = FALSE
  )
  out <- build_review_overrides(geocoded, decisions)

  # accept -> manual = the current coordinate
  expect_equal(out$manual_latitude[out$record_id == "a"], 40.1)
  expect_equal(out$manual_longitude[out$record_id == "a"], -74.1)
  # relocate -> manual = the reviewer coordinate
  expect_equal(out$manual_latitude[out$record_id == "b"], 40.25)
  expect_equal(out$manual_longitude[out$record_id == "b"], -74.25)
  # reject -> manual NA
  expect_true(is.na(out$manual_latitude[out$record_id == "c"]))
  expect_equal(out$manual_note, c("accept", "relocate", "reject"))
})

test_that("build_review_overrides output feeds apply_manual_overrides", {
  geocoded <- data.frame(
    record_id = c("a", "b"),
    record_name = c("A", "B"),
    full_address_clean = c("1 A St", "2 B St"),
    latitude = c(NA_real_, NA_real_),
    longitude = c(NA_real_, NA_real_),
    geocode_method = NA_character_, geocode_pass = NA_character_,
    match_status = NA_character_, review_status = "needs_manual_review",
    stringsAsFactors = FALSE
  )
  decisions <- data.frame(
    record_id = c("a", "b"),
    decision = c("relocate", "reject"),
    manual_latitude = c(40.2, NA),
    manual_longitude = c(-74.7, NA),
    stringsAsFactors = FALSE
  )
  overrides <- build_review_overrides(geocoded, decisions)

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(overrides, tmp)

  applied <- apply_manual_overrides(geocoded, tmp, bbox = region_bbox("NJ"))

  expect_equal(applied$latitude[applied$record_id == "a"], 40.2)
  expect_true(applied$manual_override_used[applied$record_id == "a"])
  expect_false(applied$manual_override_used[applied$record_id == "b"])
  expect_equal(applied$review_status[applied$record_id == "a"],
               "manual_override_applied")
})

test_that("build_review_overrides validates its input", {
  geocoded <- data.frame(record_id = "a", latitude = 40, longitude = -74)
  expect_error(build_review_overrides(1, data.frame()), "data frame")
  expect_error(
    build_review_overrides(geocoded,
                           data.frame(record_id = "a", decision = "bogus")),
    "Unknown decision"
  )
  expect_error(
    build_review_overrides(geocoded, data.frame(record_id = "a")),
    "must have"
  )
})
