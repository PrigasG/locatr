#' Summarise a geocoding run into a provenance report
#'
#' Turns a finished [geocode_records()] frame into an audit report: counts by
#' review status, by placing tier, and by cache status; a `match_confidence`
#' summary; and an auto-generated plain-language *methods paragraph* suitable for
#' a report or a paper. When the run manifest is present (attached by
#' [geocode_records()]), the methods paragraph names the package versions, run
#' date, region guard, and cache activity; without it, the report is built from
#' the audit columns alone.
#'
#' @param data A finished data frame from [geocode_records()] (ideally still
#'   carrying its `locatr_run` manifest). At minimum, `review_status` and
#'   `geocode_pass` drive the summary; `match_confidence` and `cache_status` are
#'   used when present.
#' @param file Optional path. When given, a Markdown version of the report is
#'   written there and the report object is returned invisibly.
#' @param low_confidence_below Confidence threshold (0-1) used to count
#'   low-confidence rows in the summary. Defaults to `0.5`.
#'
#' @return A `locatr_report` object (a named list) with the counts, confidence
#'   summary, and `methods` paragraph. Printing it shows a formatted summary.
#' @seealso [geocode_records()], [geocode_provenance()], [add_match_confidence()]
#' @export
#' @examples
#' df <- data.frame(
#'   record_id = c("a", "b", "c"),
#'   geocode_pass = c("pass_1_census_structured", "pass_2_fallback",
#'                    "pass_4_name_lookup"),
#'   review_status = c("auto_accepted", "auto_accepted", "needs_manual_review"),
#'   match_confidence = c(0.9, 0.72, 0.35)
#' )
#' geocode_report(df)
geocode_report <- function(data, file = NULL, low_confidence_below = 0.5) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame from `geocode_records()`.", call. = FALSE)
  }
  if (!is.numeric(low_confidence_below) || length(low_confidence_below) != 1L ||
      is.na(low_confidence_below) || low_confidence_below < 0 ||
      low_confidence_below > 1) {
    stop("`low_confidence_below` must be a single number from 0 to 1.",
         call. = FALSE)
  }
  if (!is.null(file) && (!is.character(file) || length(file) != 1L ||
                         is.na(file))) {
    stop("`file` must be `NULL` or a single file path.", call. = FALSE)
  }

  manifest <- attr(data, "locatr_run", exact = TRUE)
  n <- nrow(data)
  review <- .report_counts(data, "review_status")
  tiers <- .report_tier_counts(data)
  cache <- .report_counts(data, "cache_status")
  confidence <- .report_confidence(data, low_confidence_below)

  report <- list(
    run = manifest,
    n_records = n,
    review_status = review,
    tiers = tiers,
    cache_status = cache,
    confidence = confidence,
    methods = .report_methods(manifest, n, review, tiers, cache, confidence,
                              low_confidence_below)
  )
  class(report) <- "locatr_report"

  if (!is.null(file)) {
    writeLines(.report_markdown(report), file)
    return(invisible(report))
  }
  report
}

#' @rdname geocode_report
#' @param x A `locatr_report` object.
#' @param ... Ignored.
#' @export
print.locatr_report <- function(x, ...) {
  cat("<locatr geocoding report>", x$n_records, "record(s)\n\n")
  cat("Methods:\n")
  cat(strwrap(x$methods, width = 76, prefix = "  "), sep = "\n")
  cat("\n\n")
  .report_print_counts("Review status", x$review_status)
  .report_print_counts("Placed by", x$tiers)
  if (length(x$cache_status) > 0L) {
    .report_print_counts("Cache status", x$cache_status)
  }
  cf <- x$confidence
  if (!is.null(cf) && !is.na(cf$median)) {
    cat(sprintf(
      "Match confidence: median %s, mean %s, %d below %s\n",
      format(cf$median), format(cf$mean), cf$n_below,
      format(cf$below_threshold)
    ))
  }
  invisible(x)
}

# ---- internals --------------------------------------------------------------

.report_counts <- function(data, col) {
  if (!col %in% names(data)) {
    return(integer(0))
  }
  tb <- table(as.character(data[[col]]), useNA = "no")
  stats::setNames(as.integer(tb), names(tb))
}

.report_tier_counts <- function(data) {
  if (!"geocode_pass" %in% names(data)) {
    return(integer(0))
  }
  pass <- as.character(data$geocode_pass)
  label <- rep("unplaced", length(pass))
  label[.starts_with(pass, "pass_0")] <- "reference"
  label[.starts_with(pass, "pass_1")] <- "census"
  label[.starts_with(pass, "pass_2")] <- "arcgis_address"
  label[.starts_with(pass, "pass_3")] <- "manual"
  label[.starts_with(pass, "pass_4")] <- "name_lookup"
  tb <- table(label)
  stats::setNames(as.integer(tb), names(tb))
}

# startsWith() that treats NA as FALSE.
.starts_with <- function(x, prefix) !is.na(x) & startsWith(x, prefix)

.report_confidence <- function(data, thresh) {
  if (!"match_confidence" %in% names(data)) {
    return(NULL)
  }
  mc <- suppressWarnings(as.numeric(data$match_confidence))
  ok <- mc[!is.na(mc)]
  list(
    n = length(mc),
    n_na = sum(is.na(mc)),
    min = if (length(ok) > 0L) min(ok) else NA_real_,
    median = if (length(ok) > 0L) stats::median(ok) else NA_real_,
    mean = if (length(ok) > 0L) round(mean(ok), 3) else NA_real_,
    below_threshold = thresh,
    n_below = sum(ok < thresh)
  )
}

.report_methods <- function(manifest, n, review, tiers, cache, confidence,
                            thresh) {
  pct <- function(x) if (n > 0L) paste0(round(100 * x / n), "%") else "0%"
  count <- function(v, nm) if (nm %in% names(v)) as.integer(v[[nm]]) else 0L

  engine <- if (!is.null(manifest)) {
    paste0("locatr ", manifest$locatr_version,
           " (on tidygeocoder ", manifest$tidygeocoder_version, ")")
  } else {
    "locatr"
  }
  when <- if (!is.null(manifest)) paste0(" on ", manifest$run_at) else ""

  parts <- c(
    sprintf(paste0("Addresses were cleaned and standardised, then geocoded ",
                   "with %s%s using a validation-guarded cascade of US Census ",
                   "structured matching, ArcGIS address fallback, and ArcGIS ",
                   "name lookup."), engine, when),
    sprintf(paste0("Each candidate coordinate was validated against %s; ",
                   "out-of-region matches were rejected rather than mapped."),
            .report_region_phrase(manifest)),
    sprintf(paste0("Of %d record(s), %s were auto-accepted, %s were flagged ",
                   "for manual review, and %s were rejected."),
            n, pct(count(review, "auto_accepted")),
            pct(count(review, "needs_manual_review")),
            pct(count(review, "rejected")))
  )

  tier_labels <- c(reference = "a verified reference table",
                   census = "US Census structured matching",
                   arcgis_address = "ArcGIS address matching",
                   name_lookup = "ArcGIS name lookup",
                   manual = "manual override")
  tier_bits <- character(0)
  for (lab in names(tier_labels)) {
    cnt <- count(tiers, lab)
    if (cnt > 0L) {
      tier_bits <- c(tier_bits, paste0(pct(cnt), " by ", tier_labels[[lab]]))
    }
  }
  if (length(tier_bits) > 0L) {
    parts <- c(parts, paste0("Coordinates were placed ",
                             .report_oxford(tier_bits), "."))
  }

  if (!is.null(confidence) && !is.na(confidence$median)) {
    parts <- c(parts, sprintf(
      paste0("Median match confidence (0-1) was %s, with %d record(s) below ",
             "%s flagged for closer review."),
      format(confidence$median), confidence$n_below, format(thresh)
    ))
  }

  if (length(cache) > 0L && "cached" %in% names(cache) &&
      as.integer(cache[["cached"]]) > 0L) {
    parts <- c(parts, sprintf(
      paste0("%s of coordinates were served from a reproducibility cache ",
             "rather than re-queried."),
      pct(as.integer(cache[["cached"]]))
    ))
  }

  paste(parts, collapse = " ")
}

.report_region_phrase <- function(manifest) {
  if (is.null(manifest)) {
    return("the configured region")
  }
  if (!is.null(manifest$boundary) && !is.na(manifest$boundary)) {
    return("the supplied boundary polygon")
  }
  bbox <- manifest$bbox
  if (!is.null(bbox) && all(c("lat_min", "lat_max", "lon_min", "lon_max") %in%
                            names(bbox))) {
    return(sprintf("a bounding box (lat %.2f to %.2f, lon %.2f to %.2f)",
                   bbox[["lat_min"]], bbox[["lat_max"]],
                   bbox[["lon_min"]], bbox[["lon_max"]]))
  }
  "the configured region"
}

.report_oxford <- function(x) {
  k <- length(x)
  if (k == 0L) {
    return("")
  }
  if (k == 1L) {
    return(x)
  }
  if (k == 2L) {
    return(paste(x, collapse = " and "))
  }
  paste0(paste(x[-k], collapse = ", "), ", and ", x[k])
}

.report_print_counts <- function(title, counts) {
  if (length(counts) == 0L) {
    return(invisible())
  }
  cat(title, ":\n", sep = "")
  for (nm in names(counts)) {
    cat(sprintf("  %-24s %d\n", nm, as.integer(counts[[nm]])))
  }
  cat("\n")
  invisible()
}

.report_markdown <- function(report) {
  m <- report$run
  lines <- c("# Geocoding report", "")
  if (!is.null(m)) {
    lines <- c(
      lines,
      sprintf("- Run: %s (%s)", m$run_id, m$run_at),
      sprintf("- Versions: locatr %s, tidygeocoder %s, cache schema %s",
              m$locatr_version, m$tidygeocoder_version, m$cache_schema_version),
      sprintf("- Tiers: %s", paste(m$tiers, collapse = ", ")),
      sprintf("- Records: %d", report$n_records),
      ""
    )
  } else {
    lines <- c(lines, sprintf("- Records: %d", report$n_records), "")
  }
  lines <- c(lines, "## Methods", "", report$methods, "")
  lines <- c(lines, .report_md_counts("Review status", report$review_status))
  lines <- c(lines, .report_md_counts("Placed by", report$tiers))
  if (length(report$cache_status) > 0L) {
    lines <- c(lines, .report_md_counts("Cache status", report$cache_status))
  }
  cf <- report$confidence
  if (!is.null(cf) && !is.na(cf$median)) {
    lines <- c(
      lines, "## Match confidence", "",
      sprintf("- Median: %s | Mean: %s | Min: %s",
              format(cf$median), format(cf$mean), format(cf$min)),
      sprintf("- Below %s: %d of %d",
              format(cf$below_threshold), cf$n_below, cf$n),
      ""
    )
  }
  lines
}

.report_md_counts <- function(title, counts) {
  if (length(counts) == 0L) {
    return(character(0))
  }
  c(sprintf("## %s", title), "",
    paste0("- ", names(counts), ": ", as.integer(counts)), "")
}
