test_that("clean_addresses works with only address + city", {
  df <- tibble::tibble(Address = "100 Main St", City = "Trenton")
  out <- clean_addresses(df, address = Address, city = City, state = "NJ")

  expect_equal(out$record_id, "1")                 # surrogate id from row number
  expect_true(is.na(out$zip_clean))                # no zip -> NA
  expect_equal(out$state_clean, "NJ")
  expect_equal(out$address_clean, "100 MAIN STREET")
  # no trailing " NA" when ZIP is absent
  expect_equal(out$full_address_clean, "100 MAIN STREET, TRENTON, NJ")
})

test_that("clean_addresses generates surrogate ids only when id is omitted", {
  df <- tibble::tibble(Address = c("1 A St", "2 B St"), City = c("X", "Y"),
                       Zip = c("07042", "08608"))
  auto <- clean_addresses(df, address = Address, city = City, zip = Zip)
  expect_equal(auto$record_id, c("1", "2"))
  expect_equal(auto$zip_clean, c("07042", "08608"))

  withid <- clean_addresses(
    tibble::tibble(LID = c("a", "b"), Address = c("1 A St", "2 B St"),
                   City = c("X", "Y")),
    address = Address, city = City, id = LID
  )
  expect_equal(withid$record_id, c("a", "b"))
  expect_true(all(is.na(withid$zip_clean)))
})

test_that("clean_addresses keeps the original positional argument order", {
  df <- tibble::tibble(
    LocationID = "x1",
    Address = "ONE BAY AVE",
    City = "Montclair",
    Zip = "7042"
  )

  out <- clean_addresses(df, LocationID, Address, City, Zip)

  expect_equal(out$record_id, "x1")
  expect_equal(out$zip_clean, "07042")
  expect_equal(out$full_address_clean, "1 BAY AVENUE, MONTCLAIR, NJ 07042")
})

test_that("clean_addresses appends ZIP to full_address_clean when present", {
  df <- tibble::tibble(Address = "ONE BAY AVE", City = "Montclair", Zip = "7042")
  out <- clean_addresses(df, address = Address, city = City, zip = Zip)
  expect_equal(out$full_address_clean, "1 BAY AVENUE, MONTCLAIR, NJ 07042")
})

test_that("missing ZIP alone does not block geocoding", {
  cleaned <- clean_addresses(
    tibble::tibble(Address = "100 Main St", City = "Trenton"),
    address = Address, city = City
  )
  flagged <- flag_bad_addresses(cleaned)

  expect_equal(flagged$bad_address_flag, "missing_zip")   # recorded for audit
  expect_equal(flagged$review_status, "ready_for_geocoding")
  # and the looser tiers will still retry it
  expect_true(locatr:::.retryable_for_geocoding(flagged))
})

test_that("missing address or city still blocks, and bad addresses still flag", {
  df <- tibble::tibble(
    Address = c(NA_character_, "PO BOX 9", "100 Main St"),
    City    = c("Trenton", "Trenton", NA_character_)
  )
  flagged <- flag_bad_addresses(
    clean_addresses(df, address = Address, city = City)
  )
  expect_equal(flagged$bad_address_flag,
               c("missing_address", "po_box", "missing_city"))
  expect_true(all(flagged$review_status == "needs_manual_review"))
  expect_equal(locatr:::.retryable_for_geocoding(flagged), c(FALSE, FALSE, FALSE))
})
