#' Launch the locatr map-based review app
#'
#' A standalone Shiny app that takes records from upload through geocoding to
#' map-based review. Upload a file, map the ID and address columns, then either
#' geocode the addresses (clean, flag, cascade, and attach county/municipality)
#' or use existing coordinates. The flagged records appear on a map to accept,
#' reject, or relocate; the app builds a completed override table with
#' [build_review_overrides()] that you download as CSV and feed to
#' [apply_manual_overrides()]. Geocoding calls external services and needs
#' network access.
#'
#' The app depends on packages listed only under `Suggests`, so they are not
#' installed automatically. If any are missing, this function stops with the
#' install command you need.
#'
#' @param ... Passed to [shiny::runApp()] (e.g. `port`, `host`,
#'   `launch.browser`).
#'
#' @return Called for its side effect of starting the app; does not return.
#' @seealso [build_review_overrides()], [apply_manual_overrides()]
#' @export
#' @examples
#' \dontrun{
#' run_locatr_review_app()
#' }
run_locatr_review_app <- function(...) {
  needed <- c("shiny", "bslib", "DT", "leaflet", "readr", "readxl",
              "tibble", "writexl", "arrow", "tigris")
  missing <- needed[!vapply(needed, requireNamespace, logical(1),
                            quietly = TRUE)]
  if (length(missing) > 0) {
    stop("The review app needs these packages: ",
         paste(missing, collapse = ", "),
         ".\nInstall them with install.packages(c(",
         paste(sprintf('\"%s\"', missing), collapse = ", "), ")).",
         call. = FALSE)
  }

  app_dir <- system.file("locatr-review-app", package = "locatr")
  if (!nzchar(app_dir) || !file.exists(file.path(app_dir, "app.R"))) {
    stop("Could not find the bundled review app. Try reinstalling locatr.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
