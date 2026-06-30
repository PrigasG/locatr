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
#' A crosswalk-oriented wrapper around [add_local_geography()] for workflows
#' where the final output should carry explicit county/municipality columns and
#' stable join identifiers. It spatially joins geocoded points to
#' municipal/local boundary polygons and adds `County`, `Municipality`,
#' `Muni Key`, `muni_join_key`, code fields, and `muni_match_status`.
#'
#' `muni_join_key` is copied from `key_col` when supplied, or auto-detected from
#' common stable identifier columns such as `GEOID` and `MUNI_KEY`. `Muni Key`
#' is retained as a readable key and falls back to `County::Municipality` when
#' no official identifier exists.
#'
#' @param data A validated data frame with `latitude`/`longitude`.
#' @param muni_shapes An `sf` polygon layer containing county and municipality
#'   attributes.
#' @param county_col,muni_col Optional explicit column names in `muni_shapes`.
#'   When `NULL`, common names are auto-detected.
#' @param key_col Optional municipal key column in `muni_shapes`.
#'
#' @return `data` with `County`, `Municipality`, `Muni Key`, `muni_join_key`,
#'   `county_code`, `county_fips`, `municipality_code`,
#'   `municipality_geoid`, `municipality_name_standard`, `municipality_type`,
#'   and `muni_match_status` when available. The generic `location_county`,
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
    c("muni_join_key", "Muni Key", "MUNI_KEY", "MUNIKEY", "MUN_KEY",
      "MUN_CODE", "MUNICIPALITY_CODE", "municipality_geoid", "GEOID",
      "GNIS_ID")
  )
  meta_cols <- list(
    county_code = .pick_col(
      muni_shapes,
      c("county_code", "COUNTY_CODE", "COUNTYCODE", "COUNTYFP", "CNTY_CODE")
    ),
    county_fips = .pick_col(
      muni_shapes,
      c("county_fips", "COUNTY_FIPS", "COUNTYFIPS", "CNTY_FIPS")
    ),
    municipality_code = .pick_col(
      muni_shapes,
      c("municipality_code", "MUNICIPALITY_CODE", "MUN_CODE", "MUNCODE",
        "MUN_KEY", "MUNI_CODE", "MUNICODE", "COUSUBFP", "PLACEFP",
        "TRACTCE")
    ),
    municipality_geoid = .pick_col(
      muni_shapes,
      c("municipality_geoid", "MUNICIPALITY_GEOID", "MUNI_GEOID",
        "GEOID", "GNIS_ID")
    ),
    municipality_name_standard = .pick_col(
      muni_shapes,
      c("municipality_name_standard", "MUNICIPALITY_NAME_STANDARD",
        "NAMELSAD", "LSAD_NAME", "MUNICIPALITY_NAME", "MUN_NAME",
        "location_locality")
    ),
    municipality_type = .pick_col(
      muni_shapes,
      c("municipality_type", "MUNICIPALITY_TYPE", "LSAD", "TYPE",
        "CLASSFP", "MTFCC")
    ),
    muni_join_key = key_col,
    .statefp = .pick_col(muni_shapes, c("STATEFP", "STATE_FIPS", "statefp"))
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

  meta_values <- .join_shape_attrs(joined, muni_shapes, meta_cols)
  for (col in names(meta_values)) {
    joined[[col]] <- dplyr::if_else(
      joined$muni_match_status == "muni_matched",
      meta_values[[col]],
      NA_character_
    )
  }
  for (col in names(meta_cols)) {
    if (!col %in% names(joined)) {
      joined[[col]] <- NA_character_
    }
  }
  joined$muni_join_key <- dplyr::coalesce(
    joined$muni_join_key,
    joined$municipality_geoid,
    joined$municipality_code
  )
  joined$county_fips <- dplyr::coalesce(
    joined$county_fips,
    dplyr::if_else(
      !is.na(joined$.statefp) & !is.na(joined$county_code),
      paste0(joined$.statefp, joined$county_code),
      NA_character_
    )
  )
  joined$.statefp <- NULL
  joined[["Muni Key"]] <- dplyr::if_else(
    joined$muni_match_status == "muni_matched",
    dplyr::coalesce(joined$muni_join_key,
                    .make_muni_key(joined$County, joined$Municipality)),
    NA_character_
  )

  joined
}

#' Add county and municipality fields from Census boundaries
#'
#' Convenience wrapper for the common workflow where users want county and
#' municipality/locality fields but do not already have a boundary file. It
#' builds a Census TIGER/Line geography layer with [build_local_geography()] and
#' then applies [add_muni_from_shapes()].
#'
#' For reporting where municipality has a state-specific legal definition, use
#' [add_muni_from_shapes()] with an official state/local GIS layer.
#'
#' @param data A validated data frame with `latitude`/`longitude`.
#' @param state Two-letter state abbreviation or FIPS code.
#' @param geography Which Census layer should become `Municipality`; passed to
#'   [build_local_geography()].
#' @param county Optional county filter for supported Census layers.
#' @param year Vintage year for the boundary files.
#' @param cb If `TRUE`, use smaller cartographic boundary files.
#' @param ... Passed through to [build_local_geography()].
#'
#' @return `data` with `County`, `Municipality`, `Muni Key`, stable code/join
#'   columns when Census provides them, and `muni_match_status`, plus the
#'   generic `location_*` geography audit fields.
#' @export
add_county_muni <- function(data,
                            state,
                            geography = c("county_subdivision", "place",
                                          "county", "tract"),
                            county = NULL,
                            year = NULL,
                            cb = TRUE,
                            ...) {
  geography <- match.arg(geography)
  shapes <- build_local_geography(
    state = state,
    geography = geography,
    county = county,
    year = year,
    cb = cb,
    ...
  )
  add_muni_from_shapes(data, muni_shapes = shapes, key_col = "muni_join_key")
}

.join_shape_attrs <- function(data, shapes, cols) {
  cols <- cols[!vapply(cols, is.null, logical(1))]
  result <- as.data.frame(
    stats::setNames(rep(list(rep(NA_character_, nrow(data))), length(cols)),
                    names(cols)),
    stringsAsFactors = FALSE
  )
  if (length(cols) == 0) {
    return(result)
  }

  has_xy <- !is.na(data$latitude) & !is.na(data$longitude)
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

  shape_cols <- unique(unlist(cols, use.names = FALSE))
  attrs <- shapes[, shape_cols, drop = FALSE]
  joined <- sf::st_join(pts, attrs, join = sf::st_intersects, left = TRUE) %>%
    sf::st_drop_geometry()

  row_ids <- split(seq_len(nrow(joined)), joined$.locatr_row_id)
  for (out_col in names(cols)) {
    shape_col <- cols[[out_col]]
    values <- vapply(seq_along(row_ids), function(i) {
      idx <- row_ids[[i]]
      unique_values <- unique(stats::na.omit(as.character(joined[[shape_col]][idx])))
      if (length(unique_values) == 1) unique_values else NA_character_
    }, character(1))
    result[[out_col]][has_xy] <- values
  }
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
