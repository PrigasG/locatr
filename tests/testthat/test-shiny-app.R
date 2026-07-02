test_that("bundled Shiny app has deployment metadata", {
  app_dir <- system.file("locatr-app", package = "locatr")
  if (!nzchar(app_dir)) {
    app_dir <- test_path("../../inst/locatr-app")
  }

  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "DESCRIPTION")))
  expect_true(file.exists(file.path(app_dir, "manifest.json")))
  expect_true(file.exists(file.path(app_dir, "locatr-package", "DESCRIPTION")))
  expect_true(file.exists(file.path(app_dir, "locatr-package", "NAMESPACE")))
  expect_gt(length(list.files(file.path(app_dir, "locatr-package", "R"),
                              pattern = "[.]R$")), 10)

  app_desc <- read.dcf(file.path(app_dir, "DESCRIPTION"))
  imports <- paste(app_desc[1, "Imports"], collapse = "\n")
  expect_false(grepl("locatr", imports, fixed = TRUE))
  expect_false("Remotes" %in% colnames(app_desc))
  expect_match(imports, "readxl", fixed = TRUE)
  expect_match(imports, "rlang", fixed = TRUE)
  expect_match(imports, "tigris", fixed = TRUE)

  manifest <- jsonlite::fromJSON(file.path(app_dir, "manifest.json"),
                                 simplifyVector = FALSE)
  expect_identical(manifest$metadata$appmode, "shiny")
  expect_null(manifest$packages$locatr)
})

test_that("bundled Shiny app parses", {
  app_dir <- system.file("locatr-app", package = "locatr")
  if (!nzchar(app_dir)) {
    app_dir <- test_path("../../inst/locatr-app")
  }

  expect_error(parse(file.path(app_dir, "app.R")), NA)
})
