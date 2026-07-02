#' Add a unified match-confidence score
#'
#' Collapses locatr's several trust signals into one calibrated
#' `match_confidence` on a 0-1 scale plus a short `confidence_reason` string, so
#' a reviewer can sort or threshold on a single column instead of reading
#' `match_status`, `validation_status`, `nm_status`, and `review_status`
#' together. Higher is more trustworthy.
#'
#' The right scoring model is chosen from the columns present:
#' \itemize{
#'   \item Pipeline output (from [geocode_records()] / the crosswalk): scored
#'     from the tier that placed the row (`geocode_pass`), the match and
#'     validation status, and the name-tier confidence. Rejected or unplaced
#'     rows score near zero; reference-verified and manual rows score highest.
#'   \item Candidate output (from [geocode_address()]): scored from the ArcGIS
#'     match score, discounted by how coarse the address type is, and capped
#'     when the point falls outside a supplied `bbox`.
#' }
#'
#' The score is a transparent, rule-based prior - deliberately explainable
#' rather than a black-box model - so every value can be traced to its
#' `confidence_reason`.
#'
#' @param data A data frame from the batch pipeline or from [geocode_address()].
#'
#' @return `data` with two added columns: `match_confidence` (0-1, rounded to
#'   three decimals) and `confidence_reason`.
#' @export
#' @examples
#' add_match_confidence(data.frame(
#'   geocode_pass = c("pass_1_census_structured", "pass_4_name_lookup"),
#'   match_status = c("matched", "matched_low_confidence"),
#'   validation_status = c("coordinate_ok", "coordinate_ok"),
#'   latitude = c(40.2, 40.3), longitude = c(-74.7, -74.8)
#' ))
add_match_confidence <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  n <- nrow(data)
  scored <- if ("geocode_pass" %in% names(data) ||
                "match_status" %in% names(data)) {
    .confidence_pipeline(data)
  } else if ("match_score" %in% names(data)) {
    .confidence_candidates(data)
  } else {
    list(confidence = rep(NA_real_, n), reason = rep(NA_character_, n))
  }
  data$match_confidence <- round(scored$confidence, 3)
  data$confidence_reason <- scored$reason
  data
}

# Confidence for batch-pipeline rows, keyed off how the coordinate was placed.
.confidence_pipeline <- function(data) {
  n <- nrow(data)
  pick <- function(col) {
    if (col %in% names(data)) as.character(data[[col]]) else rep(NA_character_, n)
  }
  pass <- pick("geocode_pass")
  ms   <- pick("match_status")
  vs   <- pick("validation_status")
  nmst <- pick("nm_status")
  lat  <- if ("latitude" %in% names(data)) data$latitude else rep(NA_real_, n)
  lon  <- if ("longitude" %in% names(data)) data$longitude else rep(NA_real_, n)

  no_coords <- is.na(lat) | is.na(lon)
  rejected  <- !is.na(vs) & vs == "outside_region"
  low_conf  <- !is.na(ms) & ms == "matched_low_confidence"
  no_match  <- !is.na(ms) & ms == "no_match"
  starts <- function(prefix) !is.na(pass) & startsWith(pass, prefix)
  name_hi <- starts("pass_4") & !is.na(nmst) &
    nmst == "name_matched_high_confidence"

  confidence <- dplyr::case_when(
    rejected             ~ 0.02,
    no_coords | no_match ~ 0.00,
    starts("pass_0")     ~ 0.97,
    starts("pass_3")     ~ 0.93,
    low_conf             ~ 0.35,
    starts("pass_1")     ~ 0.90,
    starts("pass_2")     ~ 0.72,
    name_hi              ~ 0.68,
    starts("pass_4")     ~ 0.55,
    TRUE                 ~ 0.50
  )
  reason <- dplyr::case_when(
    rejected             ~ "rejected: coordinate outside region",
    no_coords | no_match ~ "no geocoder match",
    starts("pass_0")     ~ "reference-verified coordinate",
    starts("pass_3")     ~ "manual override",
    low_conf             ~ "low-confidence name match",
    starts("pass_1")     ~ "census structured match",
    starts("pass_2")     ~ "arcgis address fallback",
    name_hi              ~ "high-confidence name match",
    starts("pass_4")     ~ "name-based match",
    TRUE                 ~ "geocoded (tier unspecified)"
  )
  list(confidence = confidence, reason = reason)
}

# Confidence for geocode_address() candidates, from the ArcGIS score discounted
# by address-type coarseness and capped when the point is out of region.
.confidence_candidates <- function(data) {
  n <- nrow(data)
  if (n == 0L) {
    return(list(confidence = numeric(), reason = character()))
  }
  score <- suppressWarnings(as.numeric(data$match_score))
  atype <- if ("match_addr_type" %in% names(data)) {
    toupper(as.character(data$match_addr_type))
  } else {
    rep(NA_character_, n)
  }
  precise     <- c("POINTADDRESS", "SUBADDRESS", "STREETADDRESS")
  coarse      <- c("LOCALITY", "POI", "STREETNAME", "STREETINT",
                   "POSTAL", "POSTALEXT", "DISTANCEMARKER")
  very_coarse <- c("REGION", "COUNTRY", "ZONE", "TERRITORY")

  type_factor <- dplyr::case_when(
    is.na(atype)           ~ 0.90,
    atype %in% precise     ~ 1.00,
    atype %in% very_coarse ~ 0.55,
    atype %in% coarse      ~ 0.80,
    TRUE                   ~ 0.85
  )
  confidence <- pmin(1, pmax(0, (score / 100) * type_factor))

  in_bbox <- if ("in_bbox" %in% names(data)) data$in_bbox else rep(NA, n)
  out_region <- !is.na(in_bbox) & !in_bbox
  confidence <- ifelse(out_region, pmin(confidence, 0.30), confidence)

  reason <- paste0(
    "arcgis score ",
    ifelse(is.na(score), "NA", as.character(round(score))),
    ifelse(is.na(atype), "", paste0(", ", tolower(atype)))
  )
  reason <- ifelse(out_region, paste0(reason, " (outside expected region)"),
                   reason)
  list(confidence = confidence, reason = reason)
}
