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

test_that("clean_addresses protects a user-supplied full address column", {
  df <- tibble::tibble(
    LocationID = "a",
    Full_Address_Clean = "ONE BAY AVE, Montclair, NJ 7042",
    Address = "ONE BAY AVE",
    City = "Montclair",
    Zip = "7042"
  )
  out <- clean_addresses(df, id = LocationID, address = Address,
                         city = City, zip = Zip)

  expect_false("Full_Address_Clean" %in% names(out))
  expect_equal(out$full_address_raw, "ONE BAY AVE, Montclair, NJ 7042")
  expect_equal(out$full_address_clean, "1 BAY AVENUE, MONTCLAIR, NJ 07042")
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
  expect_equal(out$nm_status, "not_run")
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
  out <- geocode_arcgis(df)

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

test_that("geocode_arcgis fills gap rows in region (mocked)", {
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
  out <- geocode_arcgis(df)

  expect_equal(out$latitude, 40.30)
  expect_equal(out$geocode_pass, "pass_2_fallback")
  expect_equal(out$fb_status, "fallback_matched")
})

test_that("geocode_arcgis rejects an out-of-region match (mocked)", {
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
  out <- geocode_arcgis(df)

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

test_that("geocode_by_name passes a high-confidence point address (mocked)", {
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
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
  out <- geocode_by_name(df)

  expect_equal(out$nm_status, "name_matched_high_confidence")
  expect_equal(out$match_status, "matched")
  expect_equal(out$latitude, 40.30)
  expect_equal(out$nm_score, 99)
  expect_equal(out$review_status, "ready_for_geocoding") # not sent to review
})

test_that("geocode_by_name routes a fuzzy POI/low-score hit to review (mocked)", {
  testthat::local_mocked_bindings(
    geocode = function(.tbl, ...) {
      dplyr::mutate(.tbl, nm_latitude = 40.30, nm_longitude = -74.60,
                    score = 80, addr_type = "POI")
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

  expect_equal(out$nm_status, "name_matched_low_confidence")
  expect_equal(out$match_status, "matched_low_confidence")
  expect_equal(out$latitude, 40.30)              # coords kept for the reviewer
  expect_equal(out$geocode_pass, "pass_4_name_lookup")
  expect_equal(out$review_status, "needs_manual_review")
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

test_that("geocode_records finalises output review statuses", {
  df <- tibble::tibble(
    record_id = c("a", "b", "c", "d"),
    review_status = c("ready_for_geocoding", "ready_for_geocoding",
                      "needs_manual_review", "manual_override_applied"),
    match_status = c("matched", "matched", "no_match", "matched"),
    validation_status = c("coordinate_ok", "outside_region",
                          "missing_coordinates", "coordinate_ok")
  )

  out <- locatr:::.finalize_review_status(df)

  expect_equal(out$review_status,
               c("auto_accepted", "rejected", "needs_manual_review",
                 "manual_override_applied"))
})

test_that("export_location_crosswalk keeps name match audit columns", {
  df <- tibble::tibble(
    record_id = "a",
    record_name = "Some Site",
    address_clean = "1 MAIN STREET",
    city_clean = "TRENTON",
    state_clean = "NJ",
    zip_clean = "08608",
    full_address_clean = "1 MAIN STREET, TRENTON, NJ 08608",
    latitude = 40.2,
    longitude = -74.7,
    location_county = "Mercer",
    location_locality = "Trenton",
    `Muni Key` = "34021-74000",
    muni_match_status = "muni_matched",
    geography_match_status = "geography_matched",
    geocode_method = "arcgis_byname",
    geocode_pass = "pass_4_name_lookup",
    match_status = "matched_low_confidence",
    nm_score = 80,
    nm_addr_type = "POI",
    nm_status = "name_matched_low_confidence",
    review_status = "needs_manual_review"
  )

  out <- export_location_crosswalk(df)

  expect_equal(out$name_match_score, 80)
  expect_equal(out$name_match_type, "POI")
  expect_equal(out$name_match_status, "name_matched_low_confidence")
  expect_equal(out$County, "Mercer")
  expect_equal(out$Municipality, "Trenton")
  expect_equal(out[["Muni Key"]], "34021-74000")
  expect_equal(out$muni_match_status, "muni_matched")
})

# ---- build_local_geography (tigris mocked) ----------------------------------

# Small helper: a rectangular polygon as an sfc in NAD83 (what tigris returns).
.test_poly <- function(xmin, ymin, xmax, ymax) {
  sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
      ncol = 2, byrow = TRUE
    ))),
    crs = 4269
  )
}

test_that("build_local_geography standardises county_subdivision schema (mocked)", {
  skip_if_not_installed("tigris")

  fake_counties <- sf::st_sf(
    STATEFP = "34", COUNTYFP = "021", NAME = "Mercer",
    geometry = .test_poly(-75, 40, -74, 41)
  )
  fake_subs <- sf::st_sf(
    STATEFP = "34", COUNTYFP = "021", NAME = "Trenton",
    geometry = .test_poly(-74.8, 40.1, -74.6, 40.3)
  )
  testthat::local_mocked_bindings(
    counties = function(...) fake_counties,
    county_subdivisions = function(...) fake_subs,
    .package = "tigris"
  )

  areas <- build_local_geography(state = "NJ", geography = "county_subdivision")

  expect_s3_class(areas, "sf")
  expect_true(all(c("location_county", "location_locality") %in% names(areas)))
  expect_equal(areas$location_county, "Mercer")
  expect_equal(areas$location_locality, "Trenton")
  expect_equal(sf::st_crs(areas)$epsg, 4326L)
})

test_that("build_local_geography 'county' sets locality to the county (mocked)", {
  skip_if_not_installed("tigris")

  fake_counties <- sf::st_sf(
    STATEFP = "34", COUNTYFP = "021", NAME = "Mercer",
    geometry = .test_poly(-75, 40, -74, 41)
  )
  testthat::local_mocked_bindings(
    counties = function(...) fake_counties,
    .package = "tigris"
  )

  areas <- build_local_geography(state = "NJ", geography = "county")

  expect_equal(areas$location_county, "Mercer")
  expect_equal(areas$location_locality, "Mercer")
})

test_that("bbox_from_sf returns a padded WGS84 bbox", {
  areas <- sf::st_sf(
    location_county = "Mercer",
    location_locality = "Trenton",
    geometry = .test_poly(-75, 40, -74, 41)
  )

  bbox <- bbox_from_sf(areas, buffer = 0.1)

  expect_equal(bbox[["lat_min"]], 39.9)
  expect_equal(bbox[["lat_max"]], 41.1)
  expect_equal(bbox[["lon_min"]], -75.1)
  expect_equal(bbox[["lon_max"]], -73.9)
})

test_that("add_local_geography flags ambiguous overlapping geography without duplicating rows", {
  shapes <- sf::st_sf(
    location_county = c("County A", "County B"),
    location_locality = c("Area A", "Area B"),
    geometry = c(.test_poly(-75, 40, -74, 41), .test_poly(-74.8, 40.2, -73.8, 41.2))
  )
  points <- tibble::tibble(
    record_id = "a",
    latitude = 40.5,
    longitude = -74.5
  )

  out <- add_local_geography(points, geography_shapes = shapes)

  expect_equal(nrow(out), 1)
  expect_equal(out$geography_match_status, "ambiguous_geography_match")
  expect_true(is.na(out$location_county))
  expect_true(is.na(out$location_locality))
})

test_that("add_muni_from_shapes adds Tableau municipality fields", {
  shapes <- sf::st_sf(
    COUNTY = "Mercer",
    MUN = "Trenton",
    MUNI_KEY = "34021-74000",
    geometry = .test_poly(-75, 40, -74, 41)
  )
  points <- tibble::tibble(
    record_id = "a",
    latitude = 40.5,
    longitude = -74.5
  )

  out <- add_muni_from_shapes(
    points,
    muni_shapes = shapes,
    county_col = "COUNTY",
    muni_col = "MUN",
    key_col = "MUNI_KEY"
  )

  expect_equal(out$County, "Mercer")
  expect_equal(out$Municipality, "Trenton")
  expect_equal(out[["Muni Key"]], "34021-74000")
  expect_equal(out$muni_match_status, "muni_matched")
})

test_that("build_local_geography uses tract GEOID as locality when available (mocked)", {
  skip_if_not_installed("tigris")

  fake_counties <- sf::st_sf(
    STATEFP = "34", COUNTYFP = "021", NAME = "Mercer",
    geometry = .test_poly(-75, 40, -74, 41)
  )
  fake_tracts <- sf::st_sf(
    STATEFP = "34", COUNTYFP = "021", NAME = "1.01", GEOID = "34021000101",
    geometry = .test_poly(-74.8, 40.1, -74.6, 40.3)
  )
  testthat::local_mocked_bindings(
    counties = function(...) fake_counties,
    tracts = function(...) fake_tracts,
    .package = "tigris"
  )

  areas <- build_local_geography(state = "NJ", geography = "tract")

  expect_equal(areas$location_county, "Mercer")
  expect_equal(areas$location_locality, "34021000101")
})

test_that("build_local_geography standardises place schema (mocked)", {
  skip_if_not_installed("tigris")

  fake_counties <- sf::st_sf(
    STATEFP = "34", COUNTYFP = "021", NAME = "Mercer",
    geometry = .test_poly(-75, 40, -74, 41)
  )
  fake_places <- sf::st_sf(
    STATEFP = "34", NAME = "Trenton",
    geometry = .test_poly(-74.9, 40.1, -74.5, 40.4)
  )
  testthat::local_mocked_bindings(
    counties = function(...) fake_counties,
    places = function(...) fake_places,
    .package = "tigris"
  )

  areas <- build_local_geography(state = "NJ", geography = "place")

  expect_equal(areas$location_county, "Mercer")
  expect_equal(areas$location_locality, "Trenton")
  expect_equal(sf::st_crs(areas)$epsg, 4326L)
})
