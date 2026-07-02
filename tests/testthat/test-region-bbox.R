test_that("region_bbox returns the NJ preset unchanged", {
  expect_equal(
    region_bbox("NJ"),
    c(lat_min = 38.80, lat_max = 41.40, lon_min = -75.70, lon_max = -73.80)
  )
})

test_that("region_bbox supports other states and is case-insensitive", {
  ca <- region_bbox("ca")
  expect_named(ca, c("lat_min", "lat_max", "lon_min", "lon_max"))
  expect_true(ca[["lat_min"]] < ca[["lat_max"]])
  expect_true(ca[["lon_min"]] < ca[["lon_max"]])
  # plausibility: California is western and mid-latitude
  expect_true(ca[["lon_max"]] < -110)
  expect_true(ca[["lat_min"]] > 30 && ca[["lat_max"]] < 43)

  expect_identical(region_bbox("DC"), region_bbox("dc"))
})

test_that("every preset is a well-formed guard box", {
  for (st in c("NJ", "CA", "TX", "NY", "AK", "HI", "FL", "WA", "ME")) {
    bb <- region_bbox(st)
    expect_named(bb, c("lat_min", "lat_max", "lon_min", "lon_max"))
    expect_true(bb[["lat_min"]] < bb[["lat_max"]])
    expect_true(bb[["lon_min"]] < bb[["lon_max"]])
  }
})

test_that("region_bbox errors on an unknown region", {
  expect_error(region_bbox("ZZ"), "No bounding-box preset")
})
