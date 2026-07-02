test_that(".arcgis_candidates fails gracefully when the service errors", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")

  testthat::local_mocked_bindings(
    GET = function(...) stop("network down"),
    .package = "httr"
  )

  expect_warning(
    res <- .arcgis_candidates("100 MAIN ST"),
    "failed"
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0L)
})
