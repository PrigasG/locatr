#' Join records to local geography
#'
#' Spatially joins geocoded points to a local polygon layer and returns selected
#' geography attributes for dashboards. County and locality column names are
#' auto-detected from common boundary schemas, or can be set explicitly.
#'
#' If `geography_shapes` is `NULL`, the function looks for a packaged
#' `local_geography` dataset (for production this is the NJGIN/NJOGIS municipal
#' boundary layer built by `data-raw/local_geography.R`, whose attributes are
#' already named `location_county`/`location_locality`). Pass an `sf` polygon
#' layer to adapt this join to another state, county, or service area.
#'
#' For NJ production maps, `location_locality` is taken from an authoritative
#' municipal boundary polygon - not from the geocoder response or Census
#' reverse-geocoding, whose "county subdivision" names only look municipal - so
#' every locality is traceable to a named boundary source.
#'
#' @param data A validated data frame with `latitude`/`longitude`.
#' @param geography_shapes An `sf` polygon layer, or `NULL` to use packaged data.
#' @param county_col,locality_col Optional explicit column names in `geography_shapes`.
#'   When `NULL`, [add_local_geography()] guesses from common names, preferring
#'   `location_county`/`location_locality` when present.
#'
#' @return `data` with `location_county`, `location_locality`, and
#'   `geography_match_status`. Rows without usable coordinates are kept (audit-safe)
#'   with `NA` geography.
#' @export
add_local_geography <- function(data, geography_shapes = NULL,
                                county_col = NULL, locality_col = NULL) {
  if (is.null(geography_shapes)) {
    geography_shapes <- get0("local_geography",
                        envir = asNamespace("locatr"), inherits = FALSE)
    if (is.null(geography_shapes)) {
      stop("No `geography_shapes` supplied and packaged `local_geography` is not ",
           "available. Build a local boundary dataset, or pass an ",
           "sf object.", call. = FALSE)
    }
  }

  if (is.null(county_col)) {
    county_col <- .pick_col(geography_shapes,
      c("location_county", "COUNTY", "COUNTY_NAME", "COUNTY_NAM",
        "COUNTYNAME", "CNTYNAME"))
  }
  if (is.null(locality_col)) {
    locality_col <- .pick_col(geography_shapes,
      c("location_locality", "MUN_NAME", "MUN", "MUNICIPALITY",
        "MUNICIPALITY_NAME", "NAME", "GNIS_NAME"))
  }

  has_xy <- !is.na(data$latitude) & !is.na(data$longitude)
  no_point <- data[!has_xy, , drop = FALSE]
  no_point$location_county <- NA_character_
  no_point$location_locality <- NA_character_
  no_point$geography_match_status <- "no_point_available"

  if (!any(has_xy)) {
    return(dplyr::bind_rows(no_point))
  }

  point_data <- data[has_xy, , drop = FALSE]
  point_data$.locatr_row_id <- seq_len(nrow(point_data))

  pts <- sf::st_as_sf(
    point_data,
    coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
  ) %>%
    sf::st_transform(sf::st_crs(geography_shapes))

  joined <- sf::st_join(pts, geography_shapes, join = sf::st_intersects, left = TRUE) %>%
    sf::st_drop_geometry()

  joined$location_county <- if (!is.null(county_col)) {
    as.character(joined[[county_col]])
  } else NA_character_

  joined$location_locality <- if (!is.null(locality_col)) {
    as.character(joined[[locality_col]])
  } else NA_character_

  joined <- joined %>%
    dplyr::group_by(.data$.locatr_row_id) %>%
    dplyr::mutate(
      .match_count = sum(!is.na(.data$location_county) |
                           !is.na(.data$location_locality))
    ) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      geography_match_status = dplyr::case_when(
        .data$.match_count == 0L ~ "no_geography_match",
        .data$.match_count > 1L ~ "ambiguous_geography_match",
        TRUE ~ "geography_matched"
      ),
      location_county = dplyr::if_else(.data$.match_count > 1L,
                                       NA_character_, .data$location_county),
      location_locality = dplyr::if_else(.data$.match_count > 1L,
                                         NA_character_, .data$location_locality)
    ) %>%
    # keep only the original columns plus the three new ones, so the shapefile's
    # other attributes don't leak into the crosswalk
    dplyr::select(
      dplyr::any_of(names(data)),
      "location_county", "location_locality", "geography_match_status"
    )

  dplyr::bind_rows(joined, no_point)
}

# Return the first column in `df` whose name (case-insensitive) matches any of
# `candidates`, else NULL.
.pick_col <- function(df, candidates) {
  nm <- names(df)
  hit <- match(toupper(candidates), toupper(nm))
  hit <- hit[!is.na(hit)]
  if (length(hit) == 0) return(NULL)
  nm[hit[1]]
}
