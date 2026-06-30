make_key_shapes <- function() {
  geom <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1)))),
    crs = 4326
  )
  sf::st_sf(
    zip               = c("07042", "08608"),
    STATEFP           = c("34", "34"),
    County            = c("Essex", "Mercer"),
    COUNTY_CODE       = c("013", "021"),
    Municipality      = c("Montclair", "Trenton"),
    GEOID             = c("3401346260", "3402174000"),
    MUN_CODE          = c("46260", "74000"),
    geometry          = geom
  )
}

make_records <- function() {
  tibble::tibble(
    record_id = c("a", "b", "c"),
    zip       = c("07042", "08608", "99999"),
    latitude  = c(40.83, 40.22, NA),
    longitude = c(-74.21, -74.74, NA)
  )
}

test_that("add_muni_from_key joins geography by a shared key", {
  out <- add_muni_from_key(
    make_records(), make_key_shapes(),
    data_key = "zip", shp_key = "zip",
    county_col = "County", muni_col = "Municipality", key_col = "GEOID"
  )

  expect_equal(nrow(out), 3L)
  expect_equal(out$County, c("Essex", "Mercer", NA))
  expect_equal(out$Municipality, c("Montclair", "Trenton", NA))
  expect_equal(out$muni_match_status,
               c("muni_matched", "muni_matched", "no_muni_match"))
  expect_equal(out$geography_match_status,
               c("geography_matched", "geography_matched", "no_geography_match"))
  # muni_join_key comes from the GEOID key column
  expect_equal(out$muni_join_key, c("3401346260", "3402174000", NA))
  expect_equal(out[["Muni Key"]], c("3401346260", "3402174000", NA))
  # county_fips synthesised from STATEFP + auto-detected county code
  expect_equal(out$county_fips, c("34013", "34021", NA))
})

test_that("add_muni_from_key falls back to County::Municipality without a key", {
  # GEOID/MUN_CODE are auto-detected as identifiers; drop them so no official
  # key resolves and the readable County::Municipality fallback is used.
  shapes <- make_key_shapes()
  shapes$GEOID <- NULL
  shapes$MUN_CODE <- NULL
  out2 <- add_muni_from_key(
    make_records(), shapes,
    data_key = "zip", shp_key = "zip",
    county_col = "County", muni_col = "Municipality"
  )
  expect_equal(out2[["Muni Key"]],
               c("Essex::Montclair", "Mercer::Trenton", NA))
})

test_that("add_muni_from_key treats empty-string columns as unset", {
  out <- add_muni_from_key(
    make_records(), make_key_shapes(),
    data_key = "zip", shp_key = "zip",
    county_col = "", muni_col = "Municipality", key_col = ""
  )
  expect_true(all(is.na(out$County)))
  expect_equal(out$Municipality, c("Montclair", "Trenton", NA))
})

test_that("add_muni_from_key validates inputs", {
  recs <- make_records()
  shapes <- make_key_shapes()
  expect_error(add_muni_from_key(recs, data.frame(zip = "07042"),
                                 data_key = "zip", shp_key = "zip"),
               "sf object")
  expect_error(add_muni_from_key(recs, shapes, data_key = "missing",
                                 shp_key = "zip"),
               "not found in `data`")
  expect_error(add_muni_from_key(recs, shapes, data_key = "zip",
                                 shp_key = "missing"),
               "not found in `muni_shapes`")
})
