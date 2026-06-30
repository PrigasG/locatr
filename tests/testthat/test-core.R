test_that("clean_addresses normalises ONE and abbreviations", {
  df <- tibble::tibble(
    LocationID = "NJ306100", Name = "Test Site",
    Address = "ONE BAY AVE", City = "Montclair", Zip = "7042"
  )
  out <- clean_addresses(df, id = LocationID, address = Address,
                         city = City, zip = Zip, name = Name)

  expect_true(grepl("^1 BAY AVENUE", out$address_clean))
  expect_equal(out$zip_clean, "07042")        # zero-padded
  expect_equal(out$record_id, "NJ306100")
  expect_equal(out$state_clean, "NJ")
})

test_that("flag_bad_addresses routes PO boxes and placeholders to review", {
  df <- tibble::tibble(
    record_id = c("a", "b", "c"),
    record_name = c("Real", "Mailbox", "Real"),
    address_clean = c("100 MAIN STREET", "PO BOX 42", "TBD"),
    city_clean = "TRENTON",
    zip_clean = "08608"
  )
  out <- flag_bad_addresses(df)

  expect_equal(out$bad_address_flag, c(NA, "po_box", "placeholder_address"))
  expect_equal(out$review_status,
               c("ready_for_geocoding", "needs_manual_review", "needs_manual_review"))
})

test_that("in_bbox rejects out-of-region coordinates", {
  bbox <- region_bbox("NJ")
  expect_true(in_bbox(40.2, -74.5, bbox))    # central NJ
  expect_false(in_bbox(40.5, -104.9, bbox))  # Colorado false-match
  expect_false(in_bbox(42.8, -71.7, bbox))   # New Hampshire false-match
  expect_false(in_bbox(NA, NA, bbox))
})

test_that("validate_geocodes flags missing and out-of-region points", {
  df <- tibble::tibble(
    latitude = c(40.2, 40.5, NA),
    longitude = c(-74.5, -104.9, NA),
    match_status = "matched",
    review_status = "ready_for_geocoding"
  )
  out <- validate_geocodes(df, bbox = region_bbox("NJ"))

  expect_equal(out$validation_status,
               c("coordinate_ok", "outside_region", "missing_coordinates"))
  expect_equal(out$review_status[2:3], c("needs_manual_review", "needs_manual_review"))
})

test_that("geocode_by_name is a no-op when nothing needs it", {
  # all rows already placed inside NJ -> no network call, nm_* added as NA
  df <- tibble::tibble(
    record_id = "a",
    record_name = "Some Site",
    city_clean = "TRENTON",
    state_clean = "NJ",
    review_status = "ready_for_geocoding",
    latitude = 40.22,
    longitude = -74.76,
    geocode_method = "census",
    geocode_pass = "pass_1_census_structured",
    match_status = "matched"
  )
  out <- geocode_by_name(df)

  expect_true(all(c("nm_latitude", "nm_longitude", "nm_status") %in% names(out)))
  expect_true(is.na(out$nm_status))
  expect_equal(out$latitude, 40.22)        # untouched
  expect_equal(out$geocode_pass, "pass_1_census_structured")
})

test_that("retryable geocoder rows exclude pre-flagged manual review rows", {
  df <- tibble::tibble(
    review_status = c("ready_for_geocoding", "needs_manual_review", "needs_manual_review"),
    bad_address_flag = c(NA_character_, NA_character_, "po_box")
  )

  expect_equal(locatr:::.retryable_for_geocoding(df), c(TRUE, TRUE, FALSE))
})

test_that("address fallback is a no-op for pre-flagged manual review rows", {
  df <- tibble::tibble(
    record_id = "a",
    full_address_clean = "PO BOX 42, TRENTON, NJ 08608",
    review_status = "needs_manual_review",
    bad_address_flag = "po_box",
    latitude = NA_real_,
    longitude = NA_real_,
    geocode_method = NA_character_,
    geocode_pass = NA_character_,
    match_status = NA_character_
  )
  out <- geocode_fallback(df)

  expect_true(all(c("fb_latitude", "fb_longitude", "fb_status") %in% names(out)))
  expect_true(is.na(out$fb_status))
  expect_equal(out$review_status, "needs_manual_review")
})

# ---- Networked tiers (tidygeocoder::geocode mocked, no network) -------------

test_that("geocode_census fills coordinates and audit columns (mocked)", {
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      dplyr::mutate(.tbl, latitude = 40.22, longitude = -74.76)
    },
    .package = "tidygeocoder"
  )
  df <- tibble::tibble(
    record_id = "a", address_clean = "100 MAIN STREET", city_clean = "TRENTON",
    state_clean = "NJ", zip_clean = "08608", review_status = "ready_for_geocoding"
  )
  out <- geocode_census(df)

  expect_equal(out$latitude, 40.22)
  expect_equal(out$geocode_pass, "pass_1_census_structured")
  expect_equal(out$match_status, "matched")
})

test_that("geocode_fallback fills gap rows in region (mocked)", {
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
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
  out <- geocode_fallback(df)

  expect_equal(out$latitude, 40.30)
  expect_equal(out$geocode_pass, "pass_2_fallback")
  expect_equal(out$fb_status, "fallback_matched")
})

test_that("geocode_fallback rejects an out-of-region match (mocked)", {
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      dplyr::mutate(.tbl, fb_latitude = 40.50, fb_longitude = -104.90) # Colorado
    },
    .package = "tidygeocoder"
  )
  df <- tibble::tibble(
    record_id = "a",
    full_address_clean = "100 MAIN STREET, NOWHERE, NJ 08608",
    review_status = "ready_for_geocoding", bad_address_flag = NA_character_,
    latitude = NA_real_, longitude = NA_real_,
    geocode_method = NA_character_, geocode_pass = NA_character_,
    match_status = NA_character_
  )
  out <- geocode_fallback(df)

  expect_true(is.na(out$latitude))
  expect_equal(out$fb_status, "fallback_outside_region_rejected")
})

test_that("geocode_by_name fills unplaced rows in region (mocked)", {
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      dplyr::mutate(.tbl, nm_latitude = 40.30, nm_longitude = -74.60)
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
  out <- geocode_by_name(df)

  expect_equal(out$latitude, 40.30)
  expect_equal(out$geocode_pass, "pass_4_name_lookup")
  expect_equal(out$nm_status, "name_matched")
})

# ---- Tier 0 reference backfill ----------------------------------------------

test_that("backfill_from_reference places verified coords as Tier 0", {
  df <- tibble::tibble(
    record_id = c("a", "b"),
    record_name = c("Site A", "Site B"),
    review_status = c("ready_for_geocoding", "needs_manual_review"),
    bad_address_flag = c(NA_character_, "po_box")
  )
  ref <- tibble::tibble(record_id = "b", latitude = 40.22, longitude = -74.76)
  out <- backfill_from_reference(df, ref)

  # verified row is placed even though its address was flagged
  expect_equal(out$latitude[out$record_id == "b"], 40.22)
  expect_equal(out$geocode_pass[out$record_id == "b"], "pass_0_reference")
  expect_equal(out$match_status[out$record_id == "b"], "matched")
  expect_equal(out$review_status[out$record_id == "b"], "reference_backfilled")
  # unreferenced row is untouched and still awaits geocoding
  expect_true(is.na(out$latitude[out$record_id == "a"]))
  expect_equal(out$review_status[out$record_id == "a"], "ready_for_geocoding")
})

test_that("backfill_from_reference rejects out-of-region reference coords", {
  df <- tibble::tibble(record_id = "a", review_status = "ready_for_geocoding")
  ref <- tibble::tibble(record_id = "a", latitude = 40.50, longitude = -104.90)
  out <- backfill_from_reference(df, ref)

  expect_true(is.na(out$latitude))
  expect_equal(out$ref_status, "reference_outside_region_rejected")
  expect_equal(out$review_status, "ready_for_geocoding") # unchanged
})

test_that("backfill_from_reference is a no-op with NULL reference", {
  df <- tibble::tibble(record_id = "a", review_status = "ready_for_geocoding")
  out <- backfill_from_reference(df, reference = NULL)

  expect_true(all(c("ref_latitude", "ref_longitude", "ref_status") %in% names(out)))
  expect_true(is.na(out$ref_status))
})

test_that("census does not clobber Tier 0 coords when no rows are ready", {
  # every row already placed by reference -> review_status is terminal,
  # so geocode_census has nothing 'ready' and must preserve the coordinates.
  df <- tibble::tibble(
    record_id = "a", address_clean = "100 MAIN STREET", city_clean = "TRENTON",
    state_clean = "NJ", zip_clean = "08608",
    review_status = "reference_backfilled",
    latitude = 40.22, longitude = -74.76,
    geocode_method = "reference", geocode_pass = "pass_0_reference",
    match_status = "matched"
  )
  out <- geocode_census(df)

  expect_equal(out$latitude, 40.22)
  expect_equal(out$geocode_pass, "pass_0_reference")
})
