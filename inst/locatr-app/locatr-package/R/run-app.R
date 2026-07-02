#' Launch the locatr demo Shiny app
#'
#' Runs the bundled web app (the same one published as a Hugging Face Space):
#' upload a CSV/Excel/Parquet file, geocode it with the locatr cascade and a
#' session cache, download the geocoded records directly, or optionally attach
#' local geography from Census TIGER/Line/an uploaded shapefile before
#' downloading a crosswalk. The app can add Census policy geographies, flag
#' ZIP/state and county conflicts, remove selected columns before export, and
#' download a provenance/reporting bundle for audit records. The app is for
#' demonstration and light interactive use; production pipelines should call the
#' package functions directly.
#'
#' The app depends on packages that are only listed under `Suggests`, so they
#' are not installed automatically. If any are missing, this function stops with
#' the install command you need.
#'
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `host`, `launch.browser`).
#'
#' @return Called for its side effect of starting the app; does not return.
#' @export
#' @examples
#' if (interactive()) {
#' run_locatr_app()
#' }
run_locatr_app <- function(...) {
  needed <- c("shiny", "bslib", "DT", "leaflet", "readxl", "writexl",
              "arrow", "readr", "tibble", "tigris")
  missing <- needed[!vapply(needed, requireNamespace, logical(1),
                            quietly = TRUE)]
  if (length(missing) > 0) {
    stop("The demo app needs these packages: ",
         paste(missing, collapse = ", "),
         ".\nInstall them with install.packages(c(",
         paste(sprintf('\"%s\"', missing), collapse = ", "), ")).",
         call. = FALSE)
  }

  app_dir <- system.file("locatr-app", package = "locatr")
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
    stop("Could not find the bundled app. Try reinstalling locatr.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
