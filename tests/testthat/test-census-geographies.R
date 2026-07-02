.geo_poly <- function(xmin, ymin, xmax, ymax) {
  sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
      ncol = 2, byrow = TRUE
    ))),
    crs = 4269
  )
}

test_that("add_census_geographies attaches tract and district GEOIDs", {
  skip_if_not_installed("tigris")

  fake_tracts <- sf::st_sf(
    GEOID = "34021001100", NAMELSAD = "Census Tract 11",
    geometry = .geo_poly(-75, 40, -74, 41)
  )
  fake_cd <- sf::st_sf(
    GEOID = "3412", NAMELSAD = "Congressional District 12",
    geometry = .geo_poly(-75, 40, -74, 41)
  )
  testthat::local_mocked_bindings(
    tracts = function(...) fake_tracts,
    congressional_districts = function(...) fake_cd,
    .package = "tigris"
  )

  data <- tibble::tibble(
    record_id = c("a", "b"),
    latitude = c(40.5, NA_real_),
    longitude = c(-74.5, NA_real_)
  )
  out <- add_census_geographies(
    data, state = "NJ",
    levels = c("tract", "congressional_district")
  )

  expect_equal(out$tract_geoid, c("34021001100", NA))
  expect_equal(out$tract_name, c("Census Tract 11", NA))
  expect_equal(out$congressional_district_geoid, c("3412", NA))
  expect_equal(out$congressional_district_name,
               c("Congressional District 12", NA))
})

test_that("add_census_geographies leaves out-of-polygon points NA", {
  skip_if_not_installed("tigris")

  fake_tracts <- sf::st_sf(
    GEOID = "34021001100", NAMELSAD = "Census Tract 11",
    geometry = .geo_poly(-75, 40, -74, 41)
  )
  testthat::local_mocked_bindings(
    tracts = function(...) fake_tracts,
    .package = "tigris"
  )

  data <- tibble::tibble(
    record_id = "far",
    latitude = 10, longitude = 10           # outside the polygon
  )
  out <- add_census_geographies(data, state = "NJ", levels = "tract")
  expect_true(is.na(out$tract_geoid))
})

test_that("add_census_geographies validates inputs", {
  data <- tibble::tibble(record_id = "a", latitude = 40.5, longitude = -74.5)

  expect_error(add_census_geographies(data.frame(x = 1), state = "NJ"),
               "latitude")
  expect_error(add_census_geographies(data, state = "NJ", levels = "bogus"),
               "Unsupported")
  expect_error(add_census_geographies(data, state = c("NJ", "PA")),
               "single state")
})
