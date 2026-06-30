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

#' Add county and municipality fields from boundary polygons
#'
#' A Tableau-oriented wrapper around [add_local_geography()] for workflows where
#' the final crosswalk should carry explicit county and municipality columns.
#' It spatially joins geocoded points to municipal/local boundary polygons and
#' adds `County`, `Municipality`, `Muni Key`, and `muni_match_status`.
#'
#' `Muni Key` is copied from `key_col` when supplied. Otherwise it is built from
#' `County` and `Municipality`, which is stable enough for review/export but not
#' a substitute for an official municipal identifier when one exists.
#'
#' @param data A validated data frame with `latitude`/`longitude`.
#' @param muni_shapes An `sf` polygon layer containing county and municipality
#'   attributes.
#' @param county_col,muni_col Optional explicit column names in `muni_shapes`.
#'   When `NULL`, common names are auto-detected.
#' @param key_col Optional municipal key column in `muni_shapes`.
#'
#' @return `data` with `County`, `Municipality`, `Muni Key`, and
#'   `muni_match_status`. The generic `location_county`,
#'   `location_locality`, and `geography_match_status` columns are retained.
#' @export
add_muni_from_shapes <- function(data, muni_shapes,
                                 county_col = NULL,
                                 muni_col = NULL,
                                 key_col = NULL) {
  if (missing(muni_shapes) || is.null(muni_shapes)) {
    stop("`muni_shapes` must be an sf polygon layer.", call. = FALSE)
  }
  if (!inherits(muni_shapes, "sf")) {
    stop("`muni_shapes` must be an sf object.", call. = FALSE)
  }

  county_col <- county_col %||% .pick_col(
    muni_shapes,
    c("County", "COUNTY", "COUNTY_NAME", "COUNTY_NAM", "COUNTYNAME",
      "CNTYNAME", "location_county")
  )
  muni_col <- muni_col %||% .pick_col(
    muni_shapes,
    c("Municipality", "MUNICIPALITY", "MUNICIPALITY_NAME", "MUN_NAME",
      "MUN", "NAME", "GNIS_NAME", "location_locality")
  )
  key_col <- key_col %||% .pick_col(
    muni_shapes,
    c("Muni Key", "MUNI_KEY", "MUNIKEY", "MUN_KEY", "MUN_CODE",
      "MUNICIPALITY_CODE", "GEOID", "GNIS_ID")
  )

  joined <- add_local_geography(
    data,
    geography_shapes = muni_shapes,
    county_col = county_col,
    locality_col = muni_col
  )

  joined$County <- joined$location_county
  joined$Municipality <- joined$location_locality
  joined$muni_match_status <- dplyr::case_when(
    joined$geography_match_status == "geography_matched" ~ "muni_matched",
    joined$geography_match_status == "ambiguous_geography_match" ~ "ambiguous_muni_match",
    joined$geography_match_status == "no_point_available" ~ "no_point_available",
    TRUE ~ "no_muni_match"
  )

  if (!is.null(key_col)) {
    key_values <- .join_shape_key(joined, muni_shapes, key_col)
    joined[["Muni Key"]] <- dplyr::if_else(
      joined$muni_match_status == "muni_matched",
      key_values,
      NA_character_
    )
  } else {
    joined[["Muni Key"]] <- dplyr::if_else(
      joined$muni_match_status == "muni_matched",
      .make_muni_key(joined$County, joined$Municipality),
      NA_character_
    )
  }

  joined
}

.join_shape_key <- function(data, shapes, key_col) {
  has_xy <- !is.na(data$latitude) & !is.na(data$longitude)
  result <- rep(NA_character_, nrow(data))
  if (!any(has_xy)) {
    return(result)
  }

  point_data <- data[has_xy, , drop = FALSE]
  point_data$.locatr_row_id <- seq_len(nrow(point_data))
  pts <- sf::st_as_sf(
    point_data,
    coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
  ) %>%
    sf::st_transform(sf::st_crs(shapes))

  keys <- shapes[, key_col, drop = FALSE]
  joined <- sf::st_join(pts, keys, join = sf::st_intersects, left = TRUE) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.data$.locatr_row_id) %>%
    dplyr::mutate(.match_count = sum(!is.na(.data[[key_col]]))) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()

  result[has_xy] <- dplyr::if_else(
    joined$.match_count == 1L,
    as.character(joined[[key_col]]),
    NA_character_
  )
  result
}

.make_muni_key <- function(county, municipality) {
  key <- paste(county, municipality, sep = "::")
  key[is.na(county) | is.na(municipality)] <- NA_character_
  key
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
