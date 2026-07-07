#' Build a manual-override table from review decisions
#'
#' Turns a set of per-record review decisions (accept / reject / relocate) into
#' a completed override table with the same layout [write_geocode_review()]
#' produces, so it can be written to CSV and fed straight to
#' [apply_manual_overrides()]. This is the testable core behind the map-based
#' review app ([run_locatr_review_app()]); it holds no reactive or UI code, so
#' it can also be used headless.
#'
#' Decision semantics:
#' \itemize{
#'   \item `accept` - confirm the automated coordinate: `manual_latitude` /
#'     `manual_longitude` are set to the record's current coordinate.
#'   \item `relocate` - use the reviewer's coordinate: `manual_latitude` /
#'     `manual_longitude` come from the `decisions` table.
#'   \item `reject` - drop the coordinate: `manual_*` are left `NA` (so
#'     [apply_manual_overrides()] does not place it) and the note records the
#'     rejection.
#' }
#'
#' @param data A geocoded data frame with at least `record_id`, and ideally
#'   `latitude`/`longitude`/`record_name`/`full_address_clean` for context.
#' @param decisions A data frame with `record_id`, `decision` (one of
#'   `"accept"`, `"reject"`, `"relocate"`), and - for relocations -
#'   `manual_latitude` / `manual_longitude`.
#'
#' @return A tibble with `record_id`, `record_name`, `full_address_clean`,
#'   the current `latitude`/`longitude`, the reviewer's `manual_latitude` /
#'   `manual_longitude`, and `manual_note`. Compatible with
#'   [apply_manual_overrides()].
#' @seealso [run_locatr_review_app()], [apply_manual_overrides()],
#'   [write_geocode_review()]
#' @export
#' @examples
#' geocoded <- data.frame(
#'   record_id = c("a", "b", "c"),
#'   record_name = c("A", "B", "C"),
#'   full_address_clean = c("1 A St", "2 B St", "3 C St"),
#'   latitude = c(40.1, 40.2, NA),
#'   longitude = c(-74.1, -74.2, NA)
#' )
#' decisions <- data.frame(
#'   record_id = c("a", "b", "c"),
#'   decision = c("accept", "relocate", "reject"),
#'   manual_latitude = c(NA, 40.25, NA),
#'   manual_longitude = c(NA, -74.25, NA)
#' )
#' build_review_overrides(geocoded, decisions)
build_review_overrides <- function(data, decisions) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.data.frame(decisions)) {
    stop("`decisions` must be a data frame.", call. = FALSE)
  }
  if (!"record_id" %in% names(data)) {
    stop("`data` must have a `record_id` column.", call. = FALSE)
  }
  if (!all(c("record_id", "decision") %in% names(decisions))) {
    stop("`decisions` must have `record_id` and `decision` columns.",
         call. = FALSE)
  }
  valid <- c("accept", "reject", "relocate")
  bad <- setdiff(unique(as.character(decisions$decision)), valid)
  if (length(bad) > 0L) {
    stop("Unknown decision(s): ", paste(bad, collapse = ", "),
         ". Use accept, reject, or relocate.", call. = FALSE)
  }

  d <- tibble::as_tibble(decisions)
  d$record_id <- as.character(d$record_id)
  d$decision <- as.character(d$decision)
  if (!"manual_latitude" %in% names(d)) d$manual_latitude <- NA_real_
  if (!"manual_longitude" %in% names(d)) d$manual_longitude <- NA_real_
  d$manual_latitude <- suppressWarnings(as.numeric(d$manual_latitude))
  d$manual_longitude <- suppressWarnings(as.numeric(d$manual_longitude))
  d <- d[, c("record_id", "decision", "manual_latitude", "manual_longitude")]

  ctx <- tibble::tibble(
    record_id = as.character(data$record_id),
    record_name = .pull_if(data, "record_name"),
    full_address_clean = .pull_if(data, "full_address_clean"),
    latitude = .pull_if(data, "latitude"),
    longitude = .pull_if(data, "longitude")
  )
  ctx <- dplyr::distinct(ctx, .data$record_id, .keep_all = TRUE)

  out <- dplyr::left_join(d, ctx, by = "record_id")

  accept <- out$decision == "accept"
  reject <- out$decision == "reject"
  out$manual_latitude <- ifelse(accept, out$latitude, out$manual_latitude)
  out$manual_longitude <- ifelse(accept, out$longitude, out$manual_longitude)
  out$manual_latitude[reject] <- NA_real_
  out$manual_longitude[reject] <- NA_real_
  out$manual_note <- out$decision

  dplyr::select(
    out, "record_id", "record_name", "full_address_clean",
    "latitude", "longitude", "manual_latitude", "manual_longitude", "manual_note"
  )
}
