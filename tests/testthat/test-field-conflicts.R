test_that("flag_field_conflicts flags a ZIP in the wrong region", {
  df <- data.frame(
    zip_clean = c("07030", "85001", "10001", NA_character_),  # NJ, AZ, NY, NA
    state_clean = c("NJ", "NJ", "NJ", "NJ"),
    stringsAsFactors = FALSE
  )
  out <- flag_field_conflicts(df)

  expect_equal(out$zip_state_conflict, c(FALSE, TRUE, TRUE, NA))
  expect_equal(out$field_conflict, c(NA, "zip_state", "zip_state", NA))
})

test_that("flag_field_conflicts stays silent for an unknown state", {
  df <- data.frame(zip_clean = "07030", state_clean = "ZZ",
                   stringsAsFactors = FALSE)
  out <- flag_field_conflicts(df)

  expect_true(is.na(out$zip_state_conflict))
  expect_true(is.na(out$field_conflict))
})

test_that("flag_field_conflicts flags county mismatches", {
  df <- data.frame(
    zip_clean = c("08608", "08608"),
    state_clean = c("NJ", "NJ"),
    stated = c("Mercer County", "Camden"),
    location_county = c("Mercer", "Mercer"),
    stringsAsFactors = FALSE
  )
  out <- flag_field_conflicts(df, stated_county = "stated")

  expect_equal(out$county_conflict, c(FALSE, TRUE))
  expect_equal(out$field_conflict, c(NA, "county"))
})

test_that("flag_field_conflicts can report both conflicts at once", {
  df <- data.frame(
    zip_clean = "99801",             # AK region digit 9
    state_clean = "NJ",
    stated = "Camden",
    location_county = "Mercer",
    stringsAsFactors = FALSE
  )
  out <- flag_field_conflicts(df, stated_county = "stated")
  expect_equal(out$field_conflict, "zip_state; county")
})

test_that("flag_field_conflicts validates its input", {
  expect_error(flag_field_conflicts(1), "data frame")
})
