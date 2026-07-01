test_that("geocode_address validates its inputs before any network call", {
  expect_error(geocode_address(address = c("a", "b"), city = "Trenton"),
               "single, non-missing")
  expect_error(geocode_address(address = NA_character_),
               "single, non-missing")
  expect_error(geocode_address(address = "100 Main St", city = NA_character_),
               "NULL")
  expect_error(geocode_address(address = "100 Main St", state = NA_character_),
               "NULL")
  expect_error(geocode_address(address = "100 Main St", city = "Trenton",
                               min_score = 101),
               "0 to 100")
  expect_error(geocode_address(address = "100 Main St", city = "Trenton",
                               max_candidates = 0),
               "positive")
  expect_error(geocode_address(address = "100 Main St", city = "Trenton",
                               max_candidates = 1.5),
               "positive")
  expect_error(geocode_address(address = "100 Main St", city = "Trenton",
                               id = NA),
               "non-missing")
  expect_error(geocode_address(address = "100 Main St", city = "Trenton",
                               geography = NA),
               "TRUE")
  expect_error(geocode_address(address = "100 Main St", city = "Trenton",
                               quiet = NA),
               "TRUE")
  expect_error(geocode_address(address = "100 Main St", city = "Trenton",
                               show_progress = NA),
               "TRUE")
})

test_that("geocode_address can show friendly progress messages", {
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L, bbox = NULL) {
      tibble::tibble(
        matched_address = "1600 Pennsylvania Ave NW",
        longitude = -77.04,
        latitude = 38.90,
        match_score = 100,
        match_addr_type = "PointAddress"
      )
    }
  )

  messages <- character()
  out <- withCallingHandlers(
    geocode_address("1600 Pennsylvania Ave NW",
                    geography = FALSE,
                    show_progress = TRUE),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_match(paste(messages, collapse = " "), "Looking up address candidates")
  expect_match(paste(messages, collapse = " "), "Done. Returning 1 candidate")
  expect_s3_class(out, "tbl_df")
})

test_that("geocode_address errors clearly when httr/jsonlite are unavailable", {
  skip_if(requireNamespace("httr", quietly = TRUE) &&
            requireNamespace("jsonlite", quietly = TRUE),
          "httr/jsonlite are installed; cannot test the missing-deps path")
  expect_error(
    geocode_address(address = "100 Main St"),
    "httr"
  )
})

test_that("geocode_address accepts an address-only query", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L, bbox = NULL) {
      seen <<- single_line
      tibble::tibble(
        matched_address = "1600 Pennsylvania Ave, York, Pennsylvania, 17404",
        longitude = -76.73,
        latitude = 39.96,
        match_score = 100,
        match_addr_type = "PointAddress"
      )
    },
    add_county_muni = function(data, state, ...) {
      dplyr::mutate(data, County = state, Municipality = paste0(state, " muni"))
    }
  )

  res <- geocode_address("1600 Pennsylvania Ave NW", geography = TRUE)

  expect_equal(seen, "1600 PENNSYLVANIA AVENUE NW")
  expect_equal(res$input_address, "1600 PENNSYLVANIA AVENUE NW")
  expect_equal(res$candidate_state, "PA")
  expect_equal(res$County, "PA")
  expect_equal(res$Municipality, "PA muni")
})

test_that("geocode_address keeps geography columns when no state can be inferred", {
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L, bbox = NULL) {
      tibble::tibble(
        matched_address = "The Avenue",
        longitude = -79.90,
        latitude = 40.40,
        match_score = 83,
        match_addr_type = "POI"
      )
    }
  )

  res <- geocode_address("1600 Pennsylvania Ave NW", geography = TRUE)

  expect_true(all(c("candidate_state", "County", "Municipality",
                    "muni_match_status") %in% names(res)))
  expect_true(is.na(res$candidate_state))
  expect_true(is.na(res$County))
})

test_that("geocode_address returns ranked candidates", {
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L, bbox = NULL) {
      tibble::tibble(
        matched_address = c("Lower score", "Higher score"),
        longitude = c(-74.20, -74.21),
        latitude = c(40.81, 40.82),
        match_score = c(90, 98),
        match_addr_type = c("StreetAddress", "PointAddress")
      )
    }
  )

  res <- geocode_address(address = "1 Bay Ave", city = "Montclair",
                         geography = FALSE,
                         min_score = 95)

  expect_s3_class(res, "tbl_df")
  expect_true(all(c("query_id", "rank", "match_score", "matched_address",
                    "latitude", "longitude", "input_address") %in% names(res)))
  expect_equal(nrow(res), 1L)
  expect_equal(res$match_score, 98)
  expect_equal(res$rank, 1L)
  expect_equal(res$input_address, "1 BAY AVENUE, MONTCLAIR, NJ")
})

test_that("geocode_address de-duplicates repeated ArcGIS candidates", {
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L, bbox = NULL) {
      tibble::tibble(
        matched_address = c("Peachton, California", "Peachton, California",
                            "24 Peachton Ln"),
        longitude = c(-121.661361, -121.661360, -75.02),
        latitude = c(39.380444, 39.380440, 39.73),
        match_score = c(92, 92, 98),
        match_addr_type = c("Locality", "Locality", "PointAddress")
      )
    }
  )

  res <- geocode_address("24 Peachton", geography = FALSE)

  expect_equal(nrow(res), 2L)
  expect_equal(res$matched_address, c("24 Peachton Ln", "Peachton, California"))
  expect_equal(res$rank, c(1L, 2L))
})

test_that("geocode_address hints when a broad query returns only loose candidates", {
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L, bbox = NULL) {
      tibble::tibble(
        matched_address = "Peachton, California",
        longitude = -121.7,
        latitude = 39.4,
        match_score = 92,
        match_addr_type = "Locality"
      )
    }
  )

  messages <- character()
  out <- withCallingHandlers(
    geocode_address("24 Peachton", geography = FALSE, show_progress = TRUE),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_s3_class(out, "tbl_df")
  expect_match(paste(messages, collapse = " "), "add city, state, or bbox")
})

test_that("geocode_address suppresses routine geography messages by default", {
  testthat::local_mocked_bindings(
    .arcgis_candidates = function(single_line, max_candidates = 5L, bbox = NULL) {
      tibble::tibble(
        matched_address = "22 Peachton Ln",
        longitude = -75.02,
        latitude = 39.73,
        match_score = 98,
        match_addr_type = "PointAddress"
      )
    },
    add_county_muni = function(data, state = "NJ") {
      message("routine geography chatter")
      cat("routine progress chatter\n")
      dplyr::mutate(data, County = "Camden", Municipality = "Winslow")
    }
  )

  expect_output(
    expect_message(
      res <- geocode_address("1600 Pennsylvania Ave NW", city = "Washington",
                             state = "DC", geography = TRUE),
      NA
    ),
    NA
  )
  expect_equal(res$County, "Camden")

  expect_message(
    expect_output(
      geocode_address("1600 Pennsylvania Ave NW", city = "Washington",
                      state = "DC", geography = TRUE, quiet = FALSE),
      "routine progress chatter"
    ),
    "routine geography chatter"
  )
})
