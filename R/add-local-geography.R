#' Join records to local geography
#'
#' Spatially joins geocoded points to a local polygon layer and returns selected
#' geography attributes for dashboards. County and locality column names are
#' auto-detected from common boundary schemas, or can be set explicitly.
#'
#' If `geography_shapes` is `NULL`, the function looks for a packaged
#' `local_geography` dataset. Pass an `sf` polygon layer to adapt this join
#' to another state, county, or service area.
#'
#' @param data A validated data frame with `latitude`/`longitude`.
#' @param geography_shapes An `sf` polygon layer, or `NULL` to use packaged data.
#' @param county_col,locality_col Optional explicit column names in `geography_shapes`.
#'   When `NULL`, [add_local_geography()] guesses from common names.
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
      c("COUNTY", "COUNTY_NAME", "COUNTY_NAM", "COUNTYNAME", "CNTYNAME"))
  }
  if (is.null(locality_col)) {
    locality_col <- .pick_col(geography_shapes,
      c("MUN_NAME", "MUN", "MUNICIPALITY", "MUNICIPALITY_NAME", "NAME", "GNIS_NAME"))
  }

  has_xy <- !is.na(data$latitude) & !is.na(data$longitude)
  no_point <- data[!has_xy, , drop = FALSE]
  no_point$location_county <- NA_character_
  no_point$location_locality <- NA_character_
  no_point$geography_match_status <- "no_point_available"

  if (!any(has_xy)) {
    return(dplyr::bind_rows(no_point))
  }

  pts <- sf::st_as_sf(
    data[has_xy, , drop = FALSE],
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
    dplyr::mutate(
      geography_match_status = dplyr::if_else(
        is.na(.data$location_locality),
        "no_geography_match", "geography_matched"
      )
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
