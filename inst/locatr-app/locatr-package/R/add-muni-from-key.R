#' Add county and municipality fields by joining on a shared key
#'
#' The non-spatial counterpart to [add_muni_from_shapes()]. Instead of a
#' point-in-polygon join, it merges geography attributes from a boundary layer
#' onto geocoded records by a shared key column (for example a ZIP, FIPS, GEOID,
#' or local region code that both tables carry). Use it when a spatial join is
#' not the right criterion - the records may lack coordinates, or the
#' authoritative geography is keyed by a code rather than a polygon footprint.
#'
#' Output columns match [add_muni_from_shapes()] exactly, so the two join paths
#' are interchangeable downstream: `County`, `Municipality`, `Muni Key`,
#' `muni_join_key`, the stable code/identifier fields, `location_county`,
#' `location_locality`, `geography_match_status`, and `muni_match_status`.
#' Stable code fields (`county_code`, `municipality_code`, `municipality_geoid`,
#' etc.) are auto-detected from common boundary schemas; `county_fips` is
#' synthesised from a state FIPS plus county code when not supplied directly,
#' and `Muni Key` falls back to `County::Municipality` when no official
#' identifier exists.
#'
#' @param data A data frame of geocoded records carrying `data_key`.
#' @param muni_shapes An `sf` polygon (or attribute) layer carrying `shp_key`
#'   and the geography attributes to attach.
#' @param data_key Name (string) of the join-key column in `data`.
#' @param shp_key Name (string) of the join-key column in `muni_shapes`.
#' @param county_col,muni_col Optional explicit county / locality column names
#'   in `muni_shapes`. Empty strings are treated as unset.
#' @param key_col Optional municipal key column in `muni_shapes`.
#'
#' @return `data` with `County`, `Municipality`, `Muni Key`, `muni_join_key`,
#'   `county_code`, `county_fips`, `municipality_code`, `municipality_geoid`,
#'   `municipality_name_standard`, `municipality_type`, `muni_match_status`, and
#'   the generic `location_county` / `location_locality` /
#'   `geography_match_status` audit fields.
#' @seealso [add_muni_from_shapes()] for the spatial (point-in-polygon) join.
#' @export
add_muni_from_key <- function(data, muni_shapes, data_key, shp_key,
                              county_col = NULL, muni_col = NULL,
                              key_col = NULL) {
  if (missing(muni_shapes) || is.null(muni_shapes)) {
    stop("`muni_shapes` must be an sf layer.", call. = FALSE)
  }
  if (!inherits(muni_shapes, "sf")) {
    stop("`muni_shapes` must be an sf object.", call. = FALSE)
  }
  if (missing(data_key) || missing(shp_key) ||
      is.null(data_key) || is.null(shp_key) ||
      !nzchar(data_key) || !nzchar(shp_key)) {
    stop("`data_key` and `shp_key` are both required for an attribute-key join.",
         call. = FALSE)
  }
  if (!data_key %in% names(data)) {
    stop("Join key '", data_key, "' was not found in `data`.", call. = FALSE)
  }
  if (!shp_key %in% names(muni_shapes)) {
    stop("Join key '", shp_key, "' was not found in `muni_shapes`.", call. = FALSE)
  }

  county_col <- .nz_or_null(county_col)
  muni_col   <- .nz_or_null(muni_col)
  key_col    <- .nz_or_null(key_col)

  statefp_col     <- .pick_col(muni_shapes,
    c("statefp", "STATEFP", "STATE_FIPS"))
  county_code_col <- .pick_col(muni_shapes,
    c("county_code", "COUNTY_CODE", "COUNTYCODE", "COUNTYFP", "CNTY_CODE"))
  county_fips_col <- .pick_col(muni_shapes,
    c("county_fips", "COUNTY_FIPS", "COUNTYFIPS", "CNTY_FIPS"))
  muni_code_col   <- .pick_col(muni_shapes,
    c("municipality_code", "MUNICIPALITY_CODE", "MUN_CODE", "MUNCODE",
      "MUNI_CODE", "MUNICODE", "COUSUBFP", "PLACEFP", "TRACTCE"))
  muni_geoid_col  <- .pick_col(muni_shapes,
    c("municipality_geoid", "MUNICIPALITY_GEOID", "MUNI_GEOID", "GEOID",
      "GNIS_ID"))
  muni_name_std_col <- .pick_col(muni_shapes,
    c("municipality_name_standard", "MUNICIPALITY_NAME_STANDARD", "NAMELSAD",
      "LSAD_NAME", "MUNICIPALITY_NAME", "MUN_NAME", "location_locality"))
  muni_type_col   <- .pick_col(muni_shapes,
    c("municipality_type", "MUNICIPALITY_TYPE", "LSAD", "TYPE", "CLASSFP",
      "MTFCC"))

  shp_tbl <- sf::st_drop_geometry(muni_shapes)
  n <- nrow(shp_tbl)
  col_or_na <- function(col) {
    if (!is.null(col) && col %in% names(shp_tbl)) {
      as.character(shp_tbl[[col]])
    } else {
      rep(NA_character_, n)
    }
  }

  attr_tbl <- data.frame(
    .join_key                  = as.character(shp_tbl[[shp_key]]),
    .statefp                   = col_or_na(statefp_col),
    location_county            = col_or_na(county_col),
    location_locality          = col_or_na(muni_col),
    muni_join_key              = col_or_na(key_col),
    county_code                = col_or_na(county_code_col),
    county_fips                = col_or_na(county_fips_col),
    municipality_code          = col_or_na(muni_code_col),
    municipality_geoid         = col_or_na(muni_geoid_col),
    municipality_name_standard = col_or_na(muni_name_std_col),
    municipality_type          = col_or_na(muni_type_col),
    stringsAsFactors           = FALSE,
    check.names                = FALSE
  )
  attr_tbl <- dplyr::distinct(attr_tbl, .data$.join_key, .keep_all = TRUE)

  data %>%
    dplyr::mutate(.join_key = as.character(.data[[data_key]])) %>%
    dplyr::left_join(attr_tbl, by = ".join_key") %>%
    dplyr::mutate(
      geography_match_status = dplyr::if_else(
        is.na(.data$location_locality) & is.na(.data$location_county),
        "no_geography_match", "geography_matched"
      ),
      County = .data$location_county,
      Municipality = .data$location_locality,
      muni_join_key = dplyr::coalesce(
        .data$muni_join_key, .data$municipality_geoid, .data$municipality_code
      ),
      county_fips = dplyr::coalesce(
        .data$county_fips,
        dplyr::if_else(
          !is.na(.data$.statefp) & !is.na(.data$county_code),
          paste0(.data$.statefp, .data$county_code),
          NA_character_
        )
      ),
      `Muni Key` = dplyr::if_else(
        .data$geography_match_status == "geography_matched",
        dplyr::coalesce(
          .data$muni_join_key,
          .make_muni_key(.data$location_county, .data$location_locality)
        ),
        NA_character_
      ),
      muni_match_status = dplyr::if_else(
        .data$geography_match_status == "geography_matched",
        "muni_matched", "no_muni_match"
      )
    ) %>%
    dplyr::select(-".join_key", -".statefp")
}

# "" / NULL / zero-length -> NULL, so callers can pass an unselected UI value
# without it being treated as a column literally named "".
.nz_or_null <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) NULL else x
}
