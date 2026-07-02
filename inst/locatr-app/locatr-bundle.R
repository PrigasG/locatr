# Generated app-local locatr function bundle for Posit Connect.
# Do not edit by hand; regenerate from R/*.R when app internals change.

# ---- add-census-geographies.R ----
#' Attach multiple Census geography levels to geocoded points
#'
#' Enriches geocoded records with one or more Census TIGER/Line geography levels
#' in a single call, by point-in-polygon assignment. Where [add_county_muni()]
#' answers "which county and municipality", this answers "which tract, block
#' group, ZCTA, congressional district, state legislative district, or school
#' district" - the analysis and policy geographies that dashboards and grant
#' reporting often need. Each requested level adds two columns:
#' `<level>_geoid` (the Census GEOID) and `<level>_name` (the layer's name field,
#' `NA` where the layer has none, e.g. ZCTAs).
#'
#' The geography step only needs `latitude`/`longitude`; it does not touch the
#' address columns. Downloads use \pkg{tigris} and therefore need network access.
#'
#' @param data A geocoded data frame with `latitude` and `longitude` columns.
#' @param state Two-letter state abbreviation or FIPS code (passed to
#'   \pkg{tigris}). ZCTAs are national, so `state` is ignored for the `"zcta"`
#'   level.
#' @param levels Character vector of geography levels to attach. Any of
#'   `"tract"`, `"block_group"`, `"zcta"`, `"county"`, `"place"`,
#'   `"county_subdivision"`, `"congressional_district"`,
#'   `"state_legislative_district_upper"`,
#'   `"state_legislative_district_lower"`, `"school_district"` (unified).
#'   Defaults to `"tract"`.
#' @param county Optional county filter (name or FIPS) for the levels that
#'   accept one (`"tract"`, `"block_group"`, `"county_subdivision"`).
#' @param year Vintage year for the boundary files. `NULL` uses the \pkg{tigris}
#'   default.
#' @param cb If `TRUE` (default), use the smaller cartographic boundary files.
#' @param ... Passed through to the underlying \pkg{tigris} download functions.
#'
#' @return `data` with, for each requested level, a `<level>_geoid` and
#'   `<level>_name` column. Rows without usable coordinates get `NA`.
#' @seealso [add_county_muni()] for county/municipality, [build_local_geography()]
#'   for a single reusable boundary layer.
#' @export
#' @examples
#' if (interactive()) {
#'   enriched <- add_census_geographies(
#'     geocoded, state = "NJ",
#'     levels = c("tract", "congressional_district", "school_district")
#'   )
#' }
add_census_geographies <- function(data, state, levels = "tract",
                                   county = NULL, year = NULL, cb = TRUE, ...) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!all(c("latitude", "longitude") %in% names(data))) {
    stop("`data` must have `latitude` and `longitude` columns; geocode first.",
         call. = FALSE)
  }
  if (missing(state) || !is.character(state) || length(state) != 1L ||
      is.na(state)) {
    stop("`state` must be a single state abbreviation or FIPS code.",
         call. = FALSE)
  }
  if (!is.character(levels) || length(levels) < 1L) {
    stop("`levels` must be a non-empty character vector.", call. = FALSE)
  }
  bad <- setdiff(levels, .CENSUS_GEOG_LEVELS)
  if (length(bad) > 0L) {
    stop("Unsupported geography level(s): ", paste(bad, collapse = ", "),
         ". Choose from: ", paste(.CENSUS_GEOG_LEVELS, collapse = ", "), ".",
         call. = FALSE)
  }
  if (!requireNamespace("tigris", quietly = TRUE)) {
    stop("`add_census_geographies()` needs the 'tigris' package. Install it ",
         "with install.packages('tigris').", call. = FALSE)
  }

  dots <- list(...)
  for (level in levels) {
    layer <- .tigris_layer(level, state, county, year, cb, dots)
    data <- .attach_one_geography(data, layer, prefix = level)
  }
  data
}

.CENSUS_GEOG_LEVELS <- c(
  "tract", "block_group", "zcta", "county", "place", "county_subdivision",
  "congressional_district", "state_legislative_district_upper",
  "state_legislative_district_lower", "school_district"
)

# Download one TIGER/Line layer for a level. Explicit `tigris::` calls so
# testthat::local_mocked_bindings(.package = "tigris") can intercept them.
.tigris_layer <- function(level, state, county, year, cb, dots) {
  base <- list(cb = cb, year = year)
  st <- list(state = state)
  cty <- if (!is.null(county)) list(county = county) else list()
  switch(
    level,
    tract = do.call(tigris::tracts, c(st, cty, base, dots)),
    block_group = do.call(tigris::block_groups, c(st, cty, base, dots)),
    county = do.call(tigris::counties, c(st, base, dots)),
    place = do.call(tigris::places, c(st, base, dots)),
    county_subdivision = do.call(tigris::county_subdivisions,
                                 c(st, cty, base, dots)),
    zcta = do.call(tigris::zctas, c(base, dots)),
    congressional_district = do.call(tigris::congressional_districts,
                                     c(st, base, dots)),
    state_legislative_district_upper = do.call(
      tigris::state_legislative_districts,
      c(st, list(house = "upper"), base, dots)
    ),
    state_legislative_district_lower = do.call(
      tigris::state_legislative_districts,
      c(st, list(house = "lower"), base, dots)
    ),
    school_district = do.call(tigris::school_districts, c(st, base, dots)),
    stop("Unsupported geography level: ", level, call. = FALSE)
  )
}

# Point-in-polygon assign one layer, adding `<prefix>_geoid` / `<prefix>_name`.
.attach_one_geography <- function(data, layer, prefix) {
  geoid_col <- .pick_col(layer, c("GEOID", "GEOID20", "GEOID10",
                                  "ZCTA5CE20", "ZCTA5CE10"))
  name_col <- .pick_col(layer, c("NAMELSAD", "NAME"))
  n <- nrow(data)
  geoid_out <- rep(NA_character_, n)
  name_out <- rep(NA_character_, n)

  has_xy <- !is.na(data$latitude) & !is.na(data$longitude)
  if (any(has_xy)) {
    point_data <- data[has_xy, , drop = FALSE]
    point_data$.locatr_row_id <- seq_len(nrow(point_data))
    keep_cols <- c(geoid_col, name_col)   # c() drops any NULLs

    layer <- sf::st_transform(sf::st_make_valid(layer), 4326)
    if (length(keep_cols) > 0L) {
      layer <- layer[, keep_cols, drop = FALSE]
    }

    pts <- sf::st_as_sf(point_data, coords = c("longitude", "latitude"),
                        crs = 4326, remove = FALSE)
    joined <- sf::st_join(pts, layer, join = sf::st_intersects, left = TRUE) %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(.data$.locatr_row_id) %>%
      dplyr::slice(1L) %>%
      dplyr::ungroup()

    if (!is.null(geoid_col)) {
      geoid_out[has_xy] <- as.character(joined[[geoid_col]])
    }
    if (!is.null(name_col)) {
      name_out[has_xy] <- as.character(joined[[name_col]])
    }
  }

  data[[paste0(prefix, "_geoid")]] <- geoid_out
  data[[paste0(prefix, "_name")]] <- name_out
  data
}

# ---- add-local-geography.R ----
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

# ---- add-muni-from-key.R ----
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

# ---- audit-helpers.R ----
#' Explain how geocoded records were handled
#'
#' Turns the main audit columns into short, reviewer-friendly sentences. This is
#' useful when checking a few records by hand or when adding plain-English notes
#' to a review export.
#'
#' @param data A locatr output data frame.
#' @param row Optional row selector. Use `NULL` for all rows (default), a numeric
#'   row index, or a `record_id` value.
#'
#' @return A character vector of explanations.
#' @export
#' @examples
#' x <- tibble::tibble(
#'   record_id = "a",
#'   geocode_pass = "pass_4_name_lookup",
#'   match_status = "matched_low_confidence",
#'   validation_status = "coordinate_ok",
#'   review_status = "needs_manual_review",
#'   nm_score = 87,
#'   nm_addr_type = "POI"
#' )
#' explain_geocode_result(x)
explain_geocode_result <- function(data, row = NULL) {
  data <- .select_review_rows(data, row)
  if (nrow(data) == 0L) {
    return(character())
  }

  vapply(seq_len(nrow(data)), function(i) {
    rec <- data[i, , drop = FALSE]
    id <- .scalar_value(rec, "record_id")
    pass <- .scalar_value(rec, "geocode_pass")
    match <- .scalar_value(rec, "match_status")
    validation <- .scalar_value(rec, "validation_status")
    review <- .scalar_value(rec, "review_status")
    score <- .scalar_value(rec, c("nm_score", "name_match_score"))
    addr_type <- .scalar_value(rec, c("nm_addr_type", "name_match_type"))

    how <- .pass_label(pass)
    confidence <- if (!is.na(score) || !is.na(addr_type)) {
      paste0(" Name match score/type: ",
             if (is.na(score)) "not recorded" else format(score, trim = TRUE),
             " / ",
             if (is.na(addr_type)) "not recorded" else addr_type,
             ".")
    } else {
      ""
    }
    validation_text <- .validation_label(validation)
    review_text <- .review_label(review)
    match_text <- if (is.na(match)) {
      "Match status was not recorded."
    } else {
      paste0("Match status: ", match, ".")
    }
    prefix <- if (is.na(id)) "" else paste0("Record ", id, ": ")

    paste0(prefix, how, " ", match_text, confidence, " ",
           validation_text, " ", review_text)
  }, character(1))
}

#' Summarise geocoding quality
#'
#' Counts the main outcomes in a locatr result so you can quickly judge whether a
#' run is ready for review, export, or threshold tuning.
#'
#' @param data A locatr output data frame.
#'
#' @return A one-row tibble with counts and rates.
#' @export
#' @examples
#' x <- tibble::tibble(
#'   latitude = c(40, NA),
#'   longitude = c(-75, NA),
#'   match_status = c("matched", "unmatched"),
#'   review_status = c("auto_accepted", "needs_manual_review"),
#'   geocode_pass = c("pass_1_census_structured", NA_character_)
#' )
#' summarise_geocoding(x)
summarise_geocoding <- function(data) {
  n <- nrow(data)
  has_coords <- .has_coordinates(data)
  matched <- .col(data, "match_status") == "matched" | has_coords
  low_conf <- .col(data, "match_status") == "matched_low_confidence"
  review <- .col(data, "review_status")
  validation <- .col(data, "validation_status")
  pass <- .col(data, "geocode_pass")
  locality <- .first_existing_col(data, c("location_locality", "Municipality"))
  manual <- .logical_col(data, "manual_override_used", default = FALSE)

  tibble::tibble(
    n_records = n,
    matched = sum(matched, na.rm = TRUE),
    matched_pct = .pct(sum(matched, na.rm = TRUE), n),
    missing_coordinates = sum(!has_coords, na.rm = TRUE),
    auto_accepted = sum(review == "auto_accepted", na.rm = TRUE),
    needs_manual_review = sum(review == "needs_manual_review", na.rm = TRUE),
    rejected = sum(review == "rejected", na.rm = TRUE),
    manual_override_applied = sum(review == "manual_override_applied" |
                                    manual, na.rm = TRUE),
    outside_region = sum(validation == "outside_region", na.rm = TRUE),
    name_lookup = sum(pass == "pass_4_name_lookup", na.rm = TRUE),
    low_confidence_name = sum(low_conf & pass == "pass_4_name_lookup",
                              na.rm = TRUE),
    missing_geography = sum(has_coords & is.na(locality), na.rm = TRUE)
  )
}

#' Plot geocoded records for review
#'
#' Creates a small interactive leaflet map, colored by an audit column. The
#' helper is intentionally lightweight: it is for quick review, not for
#' producing a full dashboard.
#'
#' @param data A locatr output data frame with `latitude` and `longitude`.
#' @param color_by Column used to color points. Defaults to `review_status`.
#'
#' @return A `leaflet` map.
#' @export
#' @examples
#' if (interactive()) {
#'   plot_geocode_review_map(geocoded)
#' }
plot_geocode_review_map <- function(data,
                                    color_by = c("review_status",
                                                 "geocode_pass",
                                                 "match_status")) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("`plot_geocode_review_map()` needs the 'leaflet' package. ",
         "Install it with install.packages(\"leaflet\").", call. = FALSE)
  }
  color_by <- match.arg(color_by)
  if (!all(c("latitude", "longitude") %in% names(data))) {
    stop("`data` must include `latitude` and `longitude` columns.",
         call. = FALSE)
  }

  pts <- data[!is.na(data$latitude) & !is.na(data$longitude), , drop = FALSE]
  map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
  if (nrow(pts) == 0L) {
    return(map)
  }

  value <- if (color_by %in% names(pts)) {
    as.character(pts[[color_by]])
  } else {
    rep("not recorded", nrow(pts))
  }
  value[is.na(value) | !nzchar(value)] <- "not recorded"
  pal <- leaflet::colorFactor("Set2", domain = value, na.color = "#999999")
  label <- .popup_label(pts, color_by, value)

  map %>%
    leaflet::addCircleMarkers(
      data = pts,
      lng = ~longitude, lat = ~latitude,
      radius = 5, stroke = FALSE, fillOpacity = 0.8,
      color = pal(value), popup = label
    ) %>%
    leaflet::fitBounds(min(pts$longitude), min(pts$latitude),
                       max(pts$longitude), max(pts$latitude))
}

#' Suggest a Census geography level for a state
#'
#' Gives a practical starting point for [build_local_geography()] and
#' [add_county_muni()]. It is a recommendation, not a legal definition of local
#' government. For production municipal joins, an official state/local boundary
#' layer is still the strongest source.
#'
#' @param state Two-letter state abbreviation.
#'
#' @return A one-row tibble with the recommended Census geography and note.
#' @export
#' @examples
#' suggest_geography_level("NJ")
#' suggest_geography_level("CA")
suggest_geography_level <- function(state) {
  if (!is.character(state) || length(state) != 1L || is.na(state)) {
    stop("`state` must be a single, non-missing character string.",
         call. = FALSE)
  }
  state <- toupper(state)
  county_subdivision_states <- c(
    "CT", "ME", "MA", "NH", "RI", "VT",
    "NJ", "NY", "PA", "MI", "MN", "WI"
  )
  place_first_states <- c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "DE", "FL", "GA", "HI",
    "ID", "IL", "IN", "IA", "KS", "KY", "LA", "MD", "MS", "MO",
    "MT", "NE", "NV", "NM", "NC", "ND", "OH", "OK", "OR", "SC",
    "SD", "TN", "TX", "UT", "VA", "WA", "WV", "WY"
  )

  if (state %in% county_subdivision_states) {
    geography <- "county_subdivision"
    note <- paste(
      "County subdivisions are usually the best Census starting point for",
      "township/municipality-style local geography. Use an official boundary",
      "layer when legal municipal definitions matter."
    )
  } else if (state %in% place_first_states) {
    geography <- "place"
    note <- paste(
      "Census places are a useful starting point for incorporated places and",
      "CDPs, but they can miss unincorporated areas. Use county or an official",
      "local boundary layer when coverage matters."
    )
  } else if (identical(state, "DC")) {
    geography <- "county"
    note <- "DC is usually handled cleanly at the county-equivalent level."
  } else {
    geography <- "county"
    note <- paste(
      "Unknown or unsupported state code. County is the safest Census fallback;",
      "use an official boundary layer for local geography."
    )
  }

  tibble::tibble(
    state = state,
    recommended_geography = geography,
    function_call = paste0("build_local_geography(state = \"", state,
                           "\", geography = \"", geography, "\")"),
    note = note
  )
}

#' Compare two geocoding runs
#'
#' Finds records whose coordinates, review status, geocode pass, or geography
#' assignment changed between two runs. This is useful after changing thresholds,
#' adding a reference file, or swapping geography sources.
#'
#' @param old Previous locatr output.
#' @param new New locatr output.
#' @param by Key column used to match rows. Defaults to `record_id`.
#' @param coordinate_tolerance Numeric tolerance for latitude/longitude changes.
#' @param changed_only If `TRUE` (default), return only rows with at least one
#'   tracked change.
#'
#' @return A tibble with old/new values and change flags.
#' @export
#' @examples
#' old <- tibble::tibble(record_id = "a", latitude = 40, longitude = -75)
#' new <- tibble::tibble(record_id = "a", latitude = 41, longitude = -75)
#' compare_geocode_runs(old, new)
compare_geocode_runs <- function(old, new, by = "record_id",
                                 coordinate_tolerance = 1e-6,
                                 changed_only = TRUE) {
  if (!is.character(by) || length(by) != 1L || is.na(by) ||
      !nzchar(by)) {
    stop("`by` must be a single, non-empty column name.", call. = FALSE)
  }
  if (!by %in% names(old) || !by %in% names(new)) {
    stop("`by` must exist in both `old` and `new`.", call. = FALSE)
  }
  if (!is.numeric(coordinate_tolerance) ||
      length(coordinate_tolerance) != 1L ||
      is.na(coordinate_tolerance) || coordinate_tolerance < 0) {
    stop("`coordinate_tolerance` must be a non-negative number.",
         call. = FALSE)
  }

  fields <- c("latitude", "longitude", "review_status", "geocode_pass",
              "match_status", "location_county", "location_locality",
              "County", "Municipality", "muni_join_key",
              "municipality_geoid")
  old_slim <- .slim_for_compare(old, by, fields, "old")
  new_slim <- .slim_for_compare(new, by, fields, "new")

  out <- dplyr::full_join(old_slim, new_slim, by = by)
  out$coordinate_changed <- .coord_changed(out, coordinate_tolerance)
  out$review_status_changed <- .changed(out$review_status_old,
                                        out$review_status_new)
  out$geocode_pass_changed <- .changed(out$geocode_pass_old,
                                       out$geocode_pass_new)
  out$match_status_changed <- .changed(out$match_status_old,
                                       out$match_status_new)
  out$geography_changed <- .changed(
    .coalesce_chr(out$muni_join_key_old, out$municipality_geoid_old,
                  out$location_locality_old, out$Municipality_old),
    .coalesce_chr(out$muni_join_key_new, out$municipality_geoid_new,
                  out$location_locality_new, out$Municipality_new)
  )
  out$row_status <- dplyr::case_when(
    is.na(out[[paste0(".present_old")]]) ~ "added",
    is.na(out[[paste0(".present_new")]]) ~ "removed",
    TRUE ~ "kept"
  )
  out$any_changed <- out$row_status != "kept" |
    out$coordinate_changed |
    out$review_status_changed |
    out$geocode_pass_changed |
    out$match_status_changed |
    out$geography_changed
  out <- dplyr::select(out, -dplyr::any_of(c(".present_old",
                                             ".present_new")))
  if (isTRUE(changed_only)) {
    out <- dplyr::filter(out, .data$any_changed)
  }
  out
}

.select_review_rows <- function(data, row) {
  if (is.null(row)) {
    return(data)
  }
  if (is.numeric(row)) {
    return(data[row, , drop = FALSE])
  }
  if (!"record_id" %in% names(data)) {
    stop("Character `row` selectors require a `record_id` column.",
         call. = FALSE)
  }
  data[as.character(data$record_id) %in% as.character(row), , drop = FALSE]
}

.scalar_value <- function(data, cols) {
  col <- cols[cols %in% names(data)][1]
  if (is.na(col)) {
    return(NA)
  }
  value <- data[[col]][1]
  if (length(value) == 0L || is.na(value)) NA else value
}

.pass_label <- function(pass) {
  dplyr::case_when(
    is.na(pass) ~ "No geocode pass was recorded.",
    pass == "pass_0_reference" ~ "Placed from a trusted reference file.",
    pass == "pass_1_census_structured" ~ "Placed by the Census structured pass.",
    pass == "pass_2_fallback" ~ "Placed by the ArcGIS address fallback.",
    pass == "pass_3_manual" ~ "Placed by a manual override.",
    pass == "pass_4_name_lookup" ~ "Placed by the ArcGIS name lookup.",
    TRUE ~ paste0("Placed by ", pass, ".")
  )
}

.validation_label <- function(validation) {
  dplyr::case_when(
    is.na(validation) ~ "Coordinate validation was not recorded.",
    validation == "coordinate_ok" ~ "Coordinate validation passed.",
    validation == "outside_region" ~ "Coordinate was outside the expected region.",
    validation == "missing_coordinates" ~ "Coordinates were missing.",
    TRUE ~ paste0("Validation status: ", validation, ".")
  )
}

.review_label <- function(review) {
  dplyr::case_when(
    is.na(review) ~ "Review status was not recorded.",
    review == "auto_accepted" ~ "It was auto-accepted.",
    review == "needs_manual_review" ~ "It needs manual review.",
    review == "manual_override_applied" ~ "A manual override was applied.",
    review == "rejected" ~ "It was rejected.",
    TRUE ~ paste0("Review status: ", review, ".")
  )
}

.has_coordinates <- function(data) {
  if (!all(c("latitude", "longitude") %in% names(data))) {
    return(rep(FALSE, nrow(data)))
  }
  !is.na(data$latitude) & !is.na(data$longitude)
}

.col <- function(data, col, default = NA_character_) {
  if (col %in% names(data)) data[[col]] else rep(default, nrow(data))
}

.logical_col <- function(data, col, default = FALSE) {
  if (col %in% names(data)) {
    value <- data[[col]]
    value[is.na(value)] <- default
    as.logical(value)
  } else {
    rep(default, nrow(data))
  }
}

.first_existing_col <- function(data, cols) {
  col <- cols[cols %in% names(data)][1]
  if (is.na(col)) rep(NA_character_, nrow(data)) else data[[col]]
}

.pct <- function(num, den) {
  if (den == 0L) NA_real_ else round(100 * num / den, 1)
}

.popup_label <- function(data, color_by, value) {
  id <- .first_existing_col(data, c("record_id", "query_id"))
  name <- .first_existing_col(data, c("record_name", "matched_address"))
  paste0(
    ifelse(is.na(id), "", paste0("<strong>", id, "</strong><br/>")),
    ifelse(is.na(name), "", paste0(name, "<br/>")),
    color_by, ": ", value
  )
}

.slim_for_compare <- function(data, by, fields, suffix) {
  out <- data[, by, drop = FALSE]
  for (field in fields) {
    out[[field]] <- if (field %in% names(data)) {
      data[[field]]
    } else {
      NA
    }
  }
  names(out)[names(out) != by] <- paste0(names(out)[names(out) != by],
                                         "_", suffix)
  out[[paste0(".present_", suffix)]] <- TRUE
  out
}

.coord_changed <- function(data, tolerance) {
  lat_old <- data$latitude_old
  lat_new <- data$latitude_new
  lon_old <- data$longitude_old
  lon_new <- data$longitude_new
  lat_changed <- .num_changed(lat_old, lat_new, tolerance)
  lon_changed <- .num_changed(lon_old, lon_new, tolerance)
  lat_changed | lon_changed
}

.num_changed <- function(old, new, tolerance) {
  missing_changed <- xor(is.na(old), is.na(new))
  value_changed <- !is.na(old) & !is.na(new) & abs(old - new) > tolerance
  missing_changed | value_changed
}

.changed <- function(old, new) {
  old <- as.character(old)
  new <- as.character(new)
  xor(is.na(old), is.na(new)) | (!is.na(old) & !is.na(new) & old != new)
}

.coalesce_chr <- function(...) {
  values <- list(...)
  out <- rep(NA_character_, length(values[[1]]))
  for (value in values) {
    value <- as.character(value)
    take <- is.na(out) & !is.na(value) & nzchar(value)
    out[take] <- value[take]
  }
  out
}

# ---- backfill-from-reference.R ----
#' Backfill verified coordinates from a trusted reference table (Tier 0)
#'
#' The authoritative first tier of the cascade. Joins coordinates from a curated
#' key -> coordinates table - an institutional-memory table of records whose
#' location was resolved and checked at some point - over the automated
#' geocoders, so a record that has already been verified never has to be
#' re-geocoded. This is what turns one analyst's manual review into a permanent
#' asset: feed last cycle's completed overrides (or any trusted coordinate list)
#' back in as `reference`, and those rows are placed instantly and exactly.
#'
#' Reference coordinates are still bbox-validated, so a stale or fat-fingered
#' entry cannot drop a point outside the region. Because the reference is
#' authoritative, a matched row is marked `review_status == "reference_backfilled"`
#' and carries valid coordinates, so [geocode_census()] and every later tier skip
#' it automatically - even if its raw address was previously flagged (e.g. a
#' PO box whose true coordinates were verified once).
#'
#' Run this before [geocode_census()] (the cascade does so when you pass
#' `reference =` to [geocode_records()]).
#'
#' @param data A data frame from [flag_bad_addresses()] (or any frame carrying
#'   the key column).
#' @param reference A data frame of verified records: the key column plus
#'   coordinate columns. May also carry county/locality columns. `NULL` or an
#'   empty frame makes this a no-op.
#' @param by Name of the key column shared by `data` and `reference`
#'   (default `"record_id"`).
#' @param lat_col,lon_col Coordinate column names in `reference`
#'   (default `"latitude"`/`"longitude"`).
#' @param county_col,locality_col Optional geography column names in `reference`
#'   to backfill into `location_county`/`location_locality`.
#' @param bbox Bounding box used to reject out-of-region reference coordinates;
#'   see [region_bbox()].
#'
#' @return `data` with reference audit columns `ref_latitude`, `ref_longitude`,
#'   `ref_status`, and, for rows the reference filled, updated
#'   `latitude`/`longitude`/`geocode_method`/`geocode_pass` (`"pass_0_reference"`)/
#'   `match_status`/`review_status`.
#' @export
#' @examples
#' records <- tibble::tibble(
#'   record_id = c("a", "b"),
#'   review_status = c("ready_for_geocoding", "needs_manual_review")
#' )
#' verified <- tibble::tibble(record_id = "b", latitude = 40.22, longitude = -74.76)
#' backfill_from_reference(records, verified)
backfill_from_reference <- function(data, reference, by = "record_id",
                                    lat_col = "latitude", lon_col = "longitude",
                                    county_col = NULL, locality_col = NULL,
                                    bbox = region_bbox("NJ")) {
  stopifnot(by %in% names(data))
  data <- .ensure_geocode_cols(data)

  if (is.null(reference) || nrow(reference) == 0L) {
    return(dplyr::mutate(
      data,
      ref_latitude = NA_real_, ref_longitude = NA_real_, ref_status = NA_character_
    ))
  }
  missing_cols <- setdiff(c(by, lat_col, lon_col), names(reference))
  if (length(missing_cols) > 0L) {
    stop("`reference` is missing required column(s): ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }

  ref <- reference %>%
    dplyr::transmute(
      .ref_key      = as.character(.data[[by]]),
      ref_latitude  = as.numeric(.data[[lat_col]]),
      ref_longitude = as.numeric(.data[[lon_col]]),
      ref_county    = if (!is.null(county_col)) as.character(.data[[county_col]]) else NA_character_,
      ref_locality  = if (!is.null(locality_col)) as.character(.data[[locality_col]]) else NA_character_
    ) %>%
    dplyr::mutate(
      ref_in_bbox = in_bbox(.data$ref_latitude, .data$ref_longitude, bbox),
      ref_status = dplyr::case_when(
        is.na(.data$ref_latitude) | is.na(.data$ref_longitude) ~ "reference_no_coords",
        !.data$ref_in_bbox ~ "reference_outside_region_rejected",
        TRUE ~ "reference_matched"
      ),
      # null out rejected coordinates so they can never be used downstream
      ref_latitude  = dplyr::if_else(.data$ref_in_bbox, .data$ref_latitude,  NA_real_),
      ref_longitude = dplyr::if_else(.data$ref_in_bbox, .data$ref_longitude, NA_real_)
    ) %>%
    # one verified coordinate per key wins (first); guards against a duplicated
    # reference table silently fanning out the join.
    dplyr::distinct(dplyr::across(dplyr::all_of(".ref_key")), .keep_all = TRUE)

  out <- data %>%
    dplyr::mutate(.ref_key = as.character(.data[[by]])) %>%
    dplyr::left_join(ref, by = ".ref_key") %>%
    dplyr::mutate(
      use_ref = !is.na(.data$ref_latitude) & !is.na(.data$ref_longitude),
      latitude       = dplyr::if_else(.data$use_ref, .data$ref_latitude,  .data$latitude),
      longitude      = dplyr::if_else(.data$use_ref, .data$ref_longitude, .data$longitude),
      geocode_method = dplyr::if_else(.data$use_ref, "reference",         .data$geocode_method),
      geocode_pass   = dplyr::if_else(.data$use_ref, "pass_0_reference",  .data$geocode_pass),
      match_status   = dplyr::if_else(.data$use_ref, "matched",           .data$match_status)
    )

  # A verified record is done: mark it terminal so census and the later tiers
  # skip it. Only touched when the frame actually carries a review status.
  if ("review_status" %in% names(out)) {
    out <- dplyr::mutate(
      out,
      review_status = dplyr::if_else(.data$use_ref, "reference_backfilled",
                                     .data$review_status)
    )
  }

  if (!is.null(county_col) || !is.null(locality_col)) {
    if (!"location_county" %in% names(out))   out$location_county   <- NA_character_
    if (!"location_locality" %in% names(out)) out$location_locality <- NA_character_
    out <- out %>%
      dplyr::mutate(
        location_county = dplyr::if_else(.data$use_ref & !is.na(.data$ref_county),
                                         .data$ref_county, .data$location_county),
        location_locality = dplyr::if_else(.data$use_ref & !is.na(.data$ref_locality),
                                           .data$ref_locality, .data$location_locality)
      )
  }

  out %>%
    dplyr::select(-dplyr::any_of(c("use_ref", ".ref_key", "ref_in_bbox",
                                   "ref_county", "ref_locality")))
}

# ---- build-local-geography.R ----
#' Build a local geography layer from Census TIGER/Line boundaries
#'
#' Downloads an authoritative Census boundary layer for a state with the
#' \pkg{tigris} package and standardises it into the two-column schema
#' [add_local_geography()] expects: `location_county` (always from counties) and
#' `location_locality` (from the requested `geography`). It also carries stable
#' Census identifiers such as `county_fips`, `municipality_geoid`, and
#' `muni_join_key` when those fields exist. This makes "locality" a configurable
#' concept, because what counts as a municipality is not consistent across
#' states.
#'
#' Choosing `geography`:
#' * `"county_subdivision"` (default) maps well to townships/municipalities in
#'   states like NJ, PA, NY and New England. Best general default for "locality".
#' * `"place"` maps to incorporated places and CDPs, but misses townships and
#'   many unincorporated areas. Places can also straddle counties, so each place
#'   is assigned the county it overlaps most.
#' * `"county"` sets locality to the county itself.
#' * `"tract"` uses the census tract identifier as the locality (analysis, not
#'   administrative naming).
#'
#' Census TIGER/Line is the best scalable baseline nationwide. For high-stakes,
#' state-specific reporting where "municipality" has legal meaning, prefer an
#' official state GIS layer and pass it straight to [add_local_geography()] -
#' that path works regardless, since the join accepts any polygon layer.
#'
#' @param state Two-letter state abbreviation or FIPS code (passed to
#'   \pkg{tigris}).
#' @param geography Which Census layer becomes `location_locality`. One of
#'   `"county_subdivision"`, `"place"`, `"county"`, `"tract"`.
#' @param county Optional county filter (name or FIPS) for
#'   `"county_subdivision"`/`"tract"`; passed to \pkg{tigris}.
#' @param year Vintage year for the boundary files. `NULL` uses the
#'   \pkg{tigris} default.
#' @param cb If `TRUE` (default), use the smaller cartographic boundary files;
#'   `FALSE` pulls the full-resolution TIGER/Line files.
#' @param ... Passed through to the underlying \pkg{tigris} download function.
#'
#' @return An `sf` polygon layer in WGS84 (EPSG:4326) with
#'   `location_county`, `location_locality`, stable join-code columns when
#'   available, and `geometry`, ready for
#'   `add_local_geography(geography_shapes = ...)`.
#' @export
#' @examples
#' if (interactive()) {
#' areas <- build_local_geography(state = "PA", geography = "county_subdivision")
#' final <- add_local_geography(geocoded, geography_shapes = areas)
#' }
build_local_geography <- function(state,
                                  geography = c("county_subdivision", "place",
                                                "county", "tract"),
                                  county = NULL,
                                  year = NULL,
                                  cb = TRUE,
                                  ...) {
  geography <- match.arg(geography)
  if (!requireNamespace("tigris", quietly = TRUE)) {
    stop("`build_local_geography()` needs the 'tigris' package. Install it with ",
         "install.packages('tigris'), or pass your own `sf` layer to ",
         "add_local_geography().", call. = FALSE)
  }

  counties <- tigris::counties(state = state, cb = cb, year = year, ...)
  if (!"NAME" %in% names(counties)) {
    stop("Unexpected counties schema from tigris (no `NAME` column).",
         call. = FALSE)
  }

  if (geography == "county") {
    areas <- counties %>%
      dplyr::transmute(
        location_county   = as.character(.data$NAME),
        location_locality = as.character(.data$NAME),
        county_code = as.character(.data$COUNTYFP),
        county_fips = paste0(.data$STATEFP, .data$COUNTYFP),
        municipality_code = as.character(.data$COUNTYFP),
        municipality_geoid = paste0(.data$STATEFP, .data$COUNTYFP),
        municipality_name_standard = as.character(.data$NAME),
        municipality_type = "county",
        muni_join_key = paste0(.data$STATEFP, .data$COUNTYFP)
      )
    return(.finalize_geography(areas))
  }

  layer <- switch(
    geography,
    county_subdivision = tigris::county_subdivisions(
      state = state, county = county, cb = cb, year = year, ...),
    place = tigris::places(state = state, cb = cb, year = year, ...),
    tract = tigris::tracts(
      state = state, county = county, cb = cb, year = year, ...)
  )
  if (!"NAME" %in% names(layer)) {
    stop("Unexpected ", geography, " schema from tigris (no `NAME` column).",
         call. = FALSE)
  }

  if (geography %in% c("county_subdivision", "tract")) {
    # County name comes from a clean attribute join on the FIPS keys both
    # layers share.
    county_names <- counties %>%
      sf::st_drop_geometry() %>%
      dplyr::transmute(
        STATEFP         = .data$STATEFP,
        COUNTYFP        = .data$COUNTYFP,
        location_county = as.character(.data$NAME),
        county_code     = as.character(.data$COUNTYFP),
        county_fips     = paste0(.data$STATEFP, .data$COUNTYFP)
      )
    locality_expr <- if (geography == "tract" && "GEOID" %in% names(layer)) {
      rlang::expr(as.character(.data$GEOID))
    } else {
      rlang::expr(as.character(.data$NAME))
    }
    muni_code_expr <- switch(
      geography,
      county_subdivision = if ("COUSUBFP" %in% names(layer)) {
        rlang::expr(as.character(.data$COUSUBFP))
      } else rlang::expr(NA_character_),
      tract = if ("TRACTCE" %in% names(layer)) {
        rlang::expr(as.character(.data$TRACTCE))
      } else rlang::expr(NA_character_)
    )
    muni_geoid_expr <- if ("GEOID" %in% names(layer)) {
      rlang::expr(as.character(.data$GEOID))
    } else rlang::expr(NA_character_)
    name_standard_expr <- if ("NAMELSAD" %in% names(layer)) {
      rlang::expr(as.character(.data$NAMELSAD))
    } else {
      rlang::expr(as.character(.data$NAME))
    }
    areas <- layer %>%
      dplyr::left_join(county_names, by = c("STATEFP", "COUNTYFP")) %>%
      dplyr::transmute(
        location_county   = .data$location_county,
        location_locality = !!locality_expr,
        county_code = .data$county_code,
        county_fips = .data$county_fips,
        municipality_code = !!muni_code_expr,
        municipality_geoid = !!muni_geoid_expr,
        municipality_name_standard = !!name_standard_expr,
        municipality_type = geography,
        muni_join_key = !!muni_geoid_expr
      )
  } else {
    # Places carry no county key and may straddle counties, so assign the
    # county each place overlaps most. Use an equal-area CRS and geometry-only
    # intersections so sf does not warn about duplicated polygon attributes.
    counties_for_join <- counties %>%
      dplyr::transmute(
        location_county = as.character(.data$NAME),
        county_code = as.character(.data$COUNTYFP),
        county_fips = paste0(.data$STATEFP, .data$COUNTYFP)
      ) %>%
      sf::st_make_valid() %>%
      sf::st_transform(5070)
    place_code_expr <- if ("PLACEFP" %in% names(layer)) {
      rlang::expr(as.character(.data$PLACEFP))
    } else rlang::expr(NA_character_)
    place_geoid_expr <- if ("GEOID" %in% names(layer)) {
      rlang::expr(as.character(.data$GEOID))
    } else rlang::expr(NA_character_)
    name_standard_expr <- if ("NAMELSAD" %in% names(layer)) {
      rlang::expr(as.character(.data$NAMELSAD))
    } else {
      rlang::expr(as.character(.data$NAME))
    }
    areas <- layer %>%
      dplyr::transmute(
        location_locality = as.character(.data$NAME),
        municipality_code = !!place_code_expr,
        municipality_geoid = !!place_geoid_expr,
        municipality_name_standard = !!name_standard_expr,
        municipality_type = "place",
        muni_join_key = !!place_geoid_expr
      ) %>%
      sf::st_make_valid() %>%
      sf::st_transform(5070)
    county_values <- .largest_overlap_county_values(areas, counties_for_join)
    areas$location_county <- county_values$location_county
    areas$county_code <- county_values$county_code
    areas$county_fips <- county_values$county_fips
  }

  .finalize_geography(areas)
}

# Make geometry valid and publish in WGS84 so the layer lines up with geocoded
# latitude/longitude in add_local_geography().
.finalize_geography <- function(areas) {
  areas <- sf::st_make_valid(areas)
  if (!is.na(sf::st_crs(areas))) {
    areas <- sf::st_transform(areas, 4326)
  }
  areas
}

.largest_overlap_county <- function(areas, counties) {
  .largest_overlap_county_values(areas, counties)$location_county
}

.largest_overlap_county_values <- function(areas, counties) {
  candidates <- sf::st_intersects(areas, counties)
  county_values <- sf::st_drop_geometry(counties)

  rows <- lapply(seq_along(candidates), function(i) {
    county_index <- candidates[[i]]
    if (length(county_index) == 0) {
      return(.empty_county_value())
    }

    overlaps <- sf::st_intersection(
      sf::st_geometry(areas[i, ]),
      sf::st_geometry(counties[county_index, ])
    )
    if (length(overlaps) == 0) {
      return(.empty_county_value())
    }

    overlap_area <- as.numeric(sf::st_area(overlaps))
    county_values[county_index[which.max(overlap_area)], , drop = FALSE]
  })
  dplyr::bind_rows(rows)
}

.empty_county_value <- function() {
  data.frame(
    location_county = NA_character_,
    county_code = NA_character_,
    county_fips = NA_character_,
    stringsAsFactors = FALSE
  )
}

# ---- cache.R ----
# Response cache for reproducible, offline-replayable geocoding (roadmap #2,
# phase 1). The cache table is long - one row per candidate result - so
# geocode_address()'s ranked candidate set replays exactly, and a "no match" is
# stored as a single sentinel row so misses are replayable too.
#
# The `key` is an implementation detail (rlang::hash of method/query/params);
# the visible `method`/`endpoint`/`query`/`params` columns are the audit
# contract and can rebuild keys if a future rlang changes the hash.

# Bump when the on-disk column layout changes.
.CACHE_SCHEMA_VERSION <- "1"

.cache_empty_table <- function() {
  data.frame(
    key = character(), method = character(), endpoint = character(),
    query = character(), params = character(), params_hash = character(),
    result_rank = integer(), latitude = double(), longitude = double(),
    score = double(), addr_type = character(), status = character(),
    matched_address = character(), cached_at = character(),
    locatr_version = character(), cache_schema_version = character(),
    stringsAsFactors = FALSE
  )
}

#' Create a locatr response cache
#'
#' A cache of parsed geocoder results that makes runs reproducible: repeated
#' queries are served locally instead of re-hitting the service, and - because
#' the parsed coordinates are stored - a cached result can be replayed offline,
#' even without the `httr`/`jsonlite` packages that the live call needs. Pass the
#' returned object to [geocode_address()] via its `cache` argument.
#'
#' The cache table is *long*: one row per candidate result (so
#' [geocode_address()]'s ranked set round-trips exactly), plus a single
#' sentinel row (`result_rank = 0`, `status = "no_match"`) for a query that
#' matched nothing, so misses are replayable and never silently re-queried.
#'
#' The lookup `key` is an implementation detail (a hash). The visible
#' `method`, `endpoint`, `query`, and `params` columns are the audit contract -
#' keys can always be rebuilt from them if a future `rlang` changes its hash.
#'
#' @param path Optional file path for a persistent cache. `NULL` (default) keeps
#'   the cache in memory for the session only - no disk writes. When a path is
#'   given, the cache is loaded from it if present and flushed on every write.
#' @param format On-disk format when `path` is set: `"rds"` (default, no extra
#'   dependency) or `"parquet"` (needs `arrow`).
#' @param store_raw Reserved for storing raw service responses. `FALSE` by
#'   default; raw storage is only ever kept for services whose terms permit it.
#'
#' @return A `locatr_cache` object (an environment) to pass to geocoding
#'   functions.
#' @seealso [cache_info()], [cache_clear()], [geocode_address()]
#' @export
#' @examples
#' cache <- locatr_cache()
#' cache_info(cache)
locatr_cache <- function(path = NULL, format = c("rds", "parquet"),
                         store_raw = FALSE) {
  format <- match.arg(format)
  if (!is.null(path) && (!is.character(path) || length(path) != 1L ||
                         is.na(path))) {
    stop("`path` must be NULL or a single file path.", call. = FALSE)
  }
  if (!is.logical(store_raw) || length(store_raw) != 1L || is.na(store_raw)) {
    stop("`store_raw` must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(store_raw)) {
    warning("Raw response storage is not implemented yet; locatr will store ",
            "parsed cache fields only.", call. = FALSE)
  }
  if (identical(format, "parquet") &&
      !requireNamespace("arrow", quietly = TRUE)) {
    stop("A Parquet cache needs the 'arrow' package.", call. = FALSE)
  }

  cache <- new.env(parent = emptyenv())
  cache$path <- path
  cache$format <- format
  cache$store_raw <- store_raw
  cache$schema_version <- .CACHE_SCHEMA_VERSION
  cache$hits <- 0L
  cache$misses <- 0L
  cache$writes <- 0L
  cache$table <- if (!is.null(path) && file.exists(path)) {
    .cache_read(path, format)
  } else {
    .cache_empty_table()
  }
  class(cache) <- "locatr_cache"
  cache
}

#' Summarise a locatr cache
#'
#' @param cache A `locatr_cache` from [locatr_cache()].
#' @return A one-row tibble: `rows`, distinct `keys`, distinct `methods`,
#'   `oldest`/`newest` `cached_at`, whether it is `persistent`, its `path`, and
#'   `format`.
#' @seealso [locatr_cache()]
#' @export
cache_info <- function(cache) {
  .stop_if_not_cache(cache)
  tbl <- cache$table
  method_counts <- if (nrow(tbl) > 0L) {
    as.list(table(tbl$method, useNA = "no"))
  } else {
    list()
  }
  tibble::tibble(
    rows = nrow(tbl),
    keys = length(unique(tbl$key)),
    methods = length(unique(tbl$method)),
    method_counts = list(method_counts),
    oldest = if (nrow(tbl) > 0L) min(tbl$cached_at) else NA_character_,
    newest = if (nrow(tbl) > 0L) max(tbl$cached_at) else NA_character_,
    persistent = !is.null(cache$path),
    path = if (is.null(cache$path)) NA_character_ else cache$path,
    file_size = if (!is.null(cache$path) && file.exists(cache$path)) {
      file.info(cache$path)$size
    } else {
      NA_real_
    },
    format = cache$format
  )
}

#' Clear a locatr cache
#'
#' Empties the in-memory table. For a persistent cache this also deletes the
#' file, so it is guarded: a persistent cache requires `confirm = TRUE`.
#'
#' @param cache A `locatr_cache` from [locatr_cache()].
#' @param confirm Must be `TRUE` to clear a persistent (path-backed) cache and
#'   delete its file. Ignored for memory-only caches.
#' @return The cleared `cache`, invisibly.
#' @seealso [locatr_cache()]
#' @export
cache_clear <- function(cache, confirm = FALSE) {
  .stop_if_not_cache(cache)
  if (!is.null(cache$path) && !isTRUE(confirm)) {
    stop("Refusing to clear a persistent cache. Pass `confirm = TRUE` to also ",
         "delete ", cache$path, ".", call. = FALSE)
  }
  cache$table <- .cache_empty_table()
  if (!is.null(cache$path) && file.exists(cache$path)) {
    file.remove(cache$path)
  }
  invisible(cache)
}

#' @rdname locatr_cache
#' @param x A `locatr_cache` object.
#' @param ... Ignored.
#' @export
print.locatr_cache <- function(x, ...) {
  info <- cache_info(x)
  cat(sprintf(
    "<locatr_cache> %s | %d result row(s) across %d quer%s\n",
    if (is.null(x$path)) "memory" else paste0("disk: ", x$path),
    info$rows, info$keys, if (info$keys == 1L) "y" else "ies"
  ))
  invisible(x)
}

# ---- internals --------------------------------------------------------------

.stop_if_not_cache <- function(cache) {
  if (!inherits(cache, "locatr_cache")) {
    stop("`cache` must be a locatr_cache object from `locatr_cache()`.",
         call. = FALSE)
  }
  invisible(cache)
}

.validate_cache_args <- function(cache, refresh) {
  if (!is.null(cache)) {
    .stop_if_not_cache(cache)
  }
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    stop("`refresh` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  invisible(NULL)
}

.cache_read <- function(path, format) {
  tbl <- if (identical(format, "parquet")) {
    as.data.frame(arrow::read_parquet(path), stringsAsFactors = FALSE)
  } else {
    readRDS(path)
  }
  if (!is.data.frame(tbl)) .cache_empty_table() else tbl
}

.cache_flush <- function(cache) {
  if (is.null(cache$path)) {
    return(invisible(cache))
  }
  if (identical(cache$format, "parquet")) {
    arrow::write_parquet(cache$table, cache$path)
  } else {
    saveRDS(cache$table, cache$path)
  }
  invisible(cache)
}

# Canonical, dependency-free serialisation of the query params, sorted by name
# so the string (and its hash) are stable regardless of argument order.
.cache_params_string <- function(params) {
  if (length(params) == 0L) {
    return("")
  }
  if (is.null(names(params))) {
    names(params) <- as.character(seq_along(params))
  }
  params <- params[order(names(params))]
  flat <- vapply(params, .cache_param_value, character(1))
  paste(names(params), flat, sep = "=", collapse = "; ")
}

.cache_param_value <- function(v) {
  if (is.null(v) || length(v) == 0L) {
    return("")
  }
  if (is.list(v)) {
    if (is.null(names(v))) {
      names(v) <- as.character(seq_along(v))
    }
    v <- v[order(names(v))]
    inner <- vapply(v, .cache_param_value, character(1))
    return(paste(names(v), inner, sep = ":", collapse = ","))
  }
  x <- as.character(v)
  x[is.na(x)] <- "<NA>"
  paste(x, collapse = ",")
}

.cache_key <- function(method, query, params) {
  rlang::hash(list(method = method, query = query,
                   params = .cache_params_string(params)))
}

.cache_now <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Return cached rows for a key (ordered by rank), or NULL on miss. `max_age` is
# in days; when set, an entry older than that is treated as a miss. Updates the
# cache hit/miss counters used by the run manifest.
.cache_get <- function(cache, key, max_age = NULL) {
  rows <- cache$table[cache$table$key == key, , drop = FALSE]
  if (nrow(rows) == 0L) {
    cache$misses <- cache$misses + 1L
    return(NULL)
  }
  if (!is.null(max_age)) {
    newest <- max(as.POSIXct(rows$cached_at, tz = "UTC",
                             format = "%Y-%m-%dT%H:%M:%SZ"))
    if (as.numeric(difftime(Sys.time(), newest, units = "days")) > max_age) {
      cache$misses <- cache$misses + 1L
      return(NULL)
    }
  }
  cache$hits <- cache$hits + 1L
  rows[order(rows$result_rank), , drop = FALSE]
}

# Read cached rows without touching the hit/miss counters (used by the
# placement stamper, which must not distort the manifest counts).
.cache_peek <- function(cache, key) {
  rows <- cache$table[cache$table$key == key, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  rows[order(rows$result_rank), , drop = FALSE]
}

# Store a candidate set (or a no-match sentinel) under `key`, replacing any
# existing rows for that key so `refresh` overwrites cleanly.
.cache_put <- function(cache, key, method, endpoint, query, params,
                       candidates) {
  ps <- .cache_params_string(params)
  ph <- rlang::hash(ps)
  now <- .cache_now()
  ver <- as.character(tryCatch(utils::packageVersion("locatr"), error = function(e) package_version("0.1.0")))

  row <- function(rank, lat, lon, score, atype, status, maddr) {
    data.frame(
      key = key, method = method, endpoint = endpoint, query = query,
      params = ps, params_hash = ph, result_rank = as.integer(rank),
      latitude = as.double(lat), longitude = as.double(lon),
      score = as.double(score), addr_type = as.character(atype),
      status = status, matched_address = as.character(maddr),
      cached_at = now, locatr_version = ver,
      cache_schema_version = cache$schema_version,
      stringsAsFactors = FALSE
    )
  }

  new_rows <- if (is.null(candidates) || nrow(candidates) == 0L) {
    row(0L, NA_real_, NA_real_, NA_real_, NA_character_, "no_match",
        NA_character_)
  } else {
    row(seq_len(nrow(candidates)), candidates$latitude, candidates$longitude,
        candidates$match_score, candidates$match_addr_type, "matched",
        candidates$matched_address)
  }

  cache$table <- rbind(
    cache$table[cache$table$key != key, , drop = FALSE], new_rows
  )
  cache$writes <- cache$writes + 1L
  .cache_flush(cache)
  invisible(cache)
}

# Shared cache-key builders for the batch tiers. Used by both the *_fill_coords
# helpers (to store/lookup) and the placement stamper (to classify fresh vs
# cached), so the two can never drift out of sync.
.census_query_vec <- function(df) {
  paste(df$address_clean, df$city_clean, df$state_clean, df$zip_clean,
        sep = "|")
}
.census_params <- function(dots = list()) {
  c(list(endpoint = "census", method = "census"), dots)
}

.arcgis_query_vec <- function(df) as.character(df$full_address_clean)
.arcgis_params <- function(method, bbox, dots = list()) {
  dots <- .region_geocoder_dots(method, bbox, dots)
  list(
    endpoint = "arcgis_findaddresscandidates_oneline",
    method = method,
    full_results = dots$full_results %||% NA,
    custom_query = dots$custom_query %||% list(),
    searchExtent = if (!is.null(dots$custom_query$searchExtent)) {
      dots$custom_query$searchExtent
    } else {
      NA_character_
    }
  )
}

.name_query_vec <- function(df) {
  paste0(df$record_name, ", ", df$city_clean, ", ", df$state_clean)
}
.name_params <- function(method, bbox, dots = list()) {
  dots <- .region_geocoder_dots(method, bbox, dots)
  list(
    endpoint = "arcgis_findaddresscandidates_byname",
    method = method,
    full_results = dots$full_results %||% NA,
    custom_query = dots$custom_query %||% list(),
    searchExtent = if (!is.null(dots$custom_query$searchExtent)) {
      dots$custom_query$searchExtent
    } else {
      NA_character_
    }
  )
}

# Rebuild the candidate tibble that .arcgis_candidates() would have returned
# from cached rows (dropping the no-match sentinel).
.cache_candidates_from_rows <- function(rows) {
  rows <- rows[rows$result_rank > 0L & rows$status != "no_match", ,
               drop = FALSE]
  tibble::tibble(
    matched_address = rows$matched_address,
    longitude = rows$longitude,
    latitude = rows$latitude,
    match_score = rows$score,
    match_addr_type = rows$addr_type
  )
}

# Cache-aware wrapper around .arcgis_candidates(). On a hit it returns the
# stored candidate set without any network call (or need for httr/jsonlite);
# on a miss it calls the live service and stores the result (or a sentinel).
.arcgis_candidates_cached <- function(single_line, max_candidates = 5L,
                                      bbox = NULL, cache = NULL,
                                      refresh = FALSE, max_age = NULL) {
  method <- "arcgis_findaddresscandidates"
  endpoint <- paste0(
    "https://geocode.arcgis.com/arcgis/rest/services/World/",
    "GeocodeServer/findAddressCandidates"
  )
  params <- list(
    endpoint = endpoint,
    countryCode = "USA",
    maxLocations = as.integer(max_candidates),
    outFields = "Addr_type,Match_addr",
    searchExtent = if (!is.null(bbox)) {
      paste(bbox[["lon_min"]], bbox[["lat_min"]],
            bbox[["lon_max"]], bbox[["lat_max"]], sep = ",")
    } else {
      NA_character_
    }
  )
  key <- .cache_key(method, single_line, params)

  if (!is.null(cache) && !isTRUE(refresh)) {
    hit <- .cache_get(cache, key, max_age = max_age)
    if (!is.null(hit)) {
      return(.cache_candidates_from_rows(hit))
    }
  }

  cands <- .arcgis_candidates(single_line, max_candidates = max_candidates,
                              bbox = bbox)
  if (!is.null(cache)) {
    .cache_put(cache, key, method, endpoint, single_line, params, cands)
  }
  cands
}

# Batch-tier cache orchestration. `input` is the subset of rows to geocode;
# `queries` is a per-row character vector used for the cache key; `run` runs the
# live tidygeocoder call on a subset and returns a tibble with `record_id`,
# `latitude`, `longitude`, and optionally `score` / `addr_type`. Returns those
# normalised columns for every row of `input`, served from cache where possible
# and stored (or sentinel-stored for a no-match) on a miss.
.batch_geocode_cached <- function(input, queries, method, params, cache,
                                  refresh, run) {
  n <- nrow(input)
  ids <- as.character(input$record_id)
  keys <- vapply(queries, function(query) .cache_key(method, query, params),
                 character(1))
  lat <- rep(NA_real_, n)
  lon <- rep(NA_real_, n)
  sc  <- rep(NA_real_, n)
  at  <- rep(NA_character_, n)
  hit <- rep(FALSE, n)

  if (!is.null(cache) && !isTRUE(refresh)) {
    for (key in unique(keys)) {
      idx <- which(keys == key)
      rows <- .cache_get(cache, key)
      if (!is.null(rows)) {
        hit[idx] <- TRUE
        keep <- rows[rows$result_rank > 0L & rows$status != "no_match", ,
                     drop = FALSE]
        if (nrow(keep) >= 1L) {
          lat[idx] <- keep$latitude[1]
          lon[idx] <- keep$longitude[1]
          sc[idx]  <- keep$score[1]
          at[idx]  <- keep$addr_type[1]
        }
      }
    }
  }

  miss <- !hit
  if (any(miss)) {
    miss_keys <- unique(keys[miss])
    reps <- match(miss_keys, keys)
    res <- run(input[reps, , drop = FALSE])
    res_ids <- as.character(res$record_id)

    for (j in seq_along(miss_keys)) {
      group <- which(keys == miss_keys[j])
      rep_row <- reps[j]
      res_idx <- match(ids[rep_row], res_ids)
      if (!is.na(res_idx)) {
        lat[group] <- res$latitude[res_idx]
        lon[group] <- res$longitude[res_idx]
        if ("score" %in% names(res)) sc[group] <- res$score[res_idx]
        if ("addr_type" %in% names(res)) {
          at[group] <- as.character(res$addr_type)[res_idx]
        }
      }
    }

    if (!is.null(cache)) {
      endpoint <- if (!is.null(params$endpoint)) params$endpoint else NA_character_
      for (j in seq_along(miss_keys)) {
        i <- reps[j]
        cand <- if (is.na(lat[i]) || is.na(lon[i])) {
          NULL
        } else {
          tibble::tibble(latitude = lat[i], longitude = lon[i],
                         match_score = sc[i], match_addr_type = at[i],
                         matched_address = NA_character_)
        }
        .cache_put(cache, miss_keys[j], method,
                   endpoint, queries[i], params, cand)
      }
    }
  }

  tibble::tibble(record_id = ids, latitude = lat, longitude = lon,
                 score = sc, addr_type = at)
}

# ---- clean-address.R ----
#' Clean and standardise address fields
#'
#' Normalises raw address, city and ZIP text into geocoder-friendly columns and
#' builds a single-line `full_address_clean`. Column mappings are supplied with
#' tidy-eval (bare column names). Original address pieces are preserved in
#' `*_raw` columns. If the input already contains a `full_address_clean` column
#' in any case style (for example `Full_Address_Clean`), locatr preserves that
#' user-supplied value as `full_address_raw` so there is only one canonical
#' `full_address_clean` column after cleaning.
#'
#' Only `address` and `city` are required. When `id` is omitted, a surrogate
#' `record_id` is generated from the row position. When `zip` is omitted (or
#' empty), `zip_clean` is `NA` and `full_address_clean` is built without a
#' trailing ZIP, so an address + city + state row is still geocodable. Supplying
#' a ZIP improves Census structured-match precision but is no longer mandatory.
#'
#' @param data A data frame of records with addresses.
#' @param address,city Bare column names for the raw address and city. Required.
#' @param id Optional bare column name holding a unique record identifier. When
#'   omitted, `record_id` is generated from the row number.
#' @param zip Optional bare column name for the raw ZIP/postal code. When
#'   omitted, `zip_clean` is `NA`.
#' @param name Optional bare column name for the record name (kept as
#'   `record_name` for review/exports). Defaults to `NULL`.
#' @param state Two-letter state used for all rows. Defaults to `"NJ"` for the
#'   first production workflow; pass another state abbreviation as needed.
#'
#' @return `data` with added columns: `record_id`, `record_name`,
#'   `address_raw`, `city_raw`, `zip_raw`, optional `full_address_raw`,
#'   `address_clean`, `city_clean`, `state_clean`, `zip_clean`,
#'   `full_address_clean`.
#' @export
#' @examples
#' df <- tibble::tibble(
#'   LocationID = "NJ306100", Name = "Hackensack-UMC Mountainside",
#'   Address = "ONE BAY AVE", City = "Montclair", Zip = "7042"
#' )
#' clean_addresses(df, id = LocationID, address = Address,
#'                        city = City, zip = Zip, name = Name)
#'
#' # address + city only (surrogate id, no ZIP)
#' clean_addresses(tibble::tibble(Address = "100 Main St", City = "Trenton"),
#'                 address = Address, city = City)
clean_addresses <- function(data, id = NULL, address, city, zip = NULL,
                                   name = NULL, state = "NJ") {
  id_q   <- rlang::enquo(id)
  zip_q  <- rlang::enquo(zip)
  name_q <- rlang::enquo(name)
  data <- .protect_existing_full_address_clean(data)

  record_id <- if (!rlang::quo_is_null(id_q)) {
    as.character(rlang::eval_tidy(id_q, data))
  } else {
    as.character(seq_len(nrow(data)))
  }
  zip_raw <- if (!rlang::quo_is_null(zip_q)) {
    as.character(rlang::eval_tidy(zip_q, data))
  } else {
    rep(NA_character_, nrow(data))
  }

  out <- data %>%
    dplyr::mutate(
      record_id   = !!record_id,
      address_raw = as.character({{ address }}),
      city_raw    = as.character({{ city }}),
      zip_raw     = !!zip_raw,
      state_clean = state,

      address_clean = .data$address_raw %>%
        stringr::str_to_upper() %>%
        stringr::str_squish() %>%
        # spell out small numbers commonly written as words
        stringr::str_replace_all("\\bONE\\b", "1") %>%
        stringr::str_replace_all("\\bTWO\\b", "2") %>%
        stringr::str_replace_all("\\bTHREE\\b", "3") %>%
        stringr::str_replace_all("\\bFOUR\\b", "4") %>%
        stringr::str_replace_all("\\bFIVE\\b", "5") %>%
        # route / highway normalisation
        stringr::str_replace_all("\\bRTE?\\b", "ROUTE") %>%
        stringr::str_replace_all("\\bHWY\\b", "HIGHWAY") %>%
        stringr::str_replace_all("\\bROUTE\\s+([0-9]+)", "STATE ROUTE \\1") %>%
        stringr::str_replace_all("\\bHIGHWAY\\s+([0-9]+)", "STATE HIGHWAY \\1") %>%
        # common abbreviations
        stringr::str_replace_all("\\bMT\\b", "MOUNT") %>%
        stringr::str_replace_all("\\bAVE\\b", "AVENUE") %>%
        stringr::str_replace_all("\\bRD\\b", "ROAD") %>%
        stringr::str_replace_all("\\bBLVD\\b", "BOULEVARD") %>%
        stringr::str_replace_all("\\bST\\b", "STREET") %>%
        stringr::str_replace_all("\\bDR\\b", "DRIVE") %>%
        stringr::str_replace_all("\\bLN\\b", "LANE") %>%
        # strip secondary-unit designators that confuse the Census matcher
        stringr::str_replace_all(
          ",?\\s*(SUITE|STE|STES|UNIT|BLDG|BUILDING|FLOOR|FLR|ROOM|RM)\\s+[A-Z0-9\\-]+",
          ""
        ) %>%
        stringr::str_squish(),

      city_clean = .data$city_raw %>%
        stringr::str_to_upper() %>%
        stringr::str_squish(),

      zip_clean = .data$zip_raw %>%
        stringr::str_remove_all("\\D") %>%
        stringr::str_sub(1, 5) %>%
        stringr::str_pad(width = 5, side = "left", pad = "0") %>%
        dplyr::na_if("00000"),

      full_address_clean = .make_full_address(
        .data$address_clean, .data$city_clean,
        .data$state_clean, .data$zip_clean
      )
    )

  if (!rlang::quo_is_null(name_q)) {
    out <- out %>% dplyr::mutate(record_name = as.character(!!name_q))
  } else {
    out$record_name <- NA_character_
  }

  out
}

.protect_existing_full_address_clean <- function(data) {
  existing <- names(data)[toupper(names(data)) == "FULL_ADDRESS_CLEAN"]
  if (length(existing) == 0) {
    return(data)
  }

  keep <- existing[1]
  names(data)[names(data) == keep] <- .available_name(names(data), "full_address_raw")

  drop <- setdiff(existing, keep)
  if (length(drop) > 0) {
    data <- data[, !names(data) %in% drop, drop = FALSE]
  }
  data
}

.available_name <- function(existing, candidate) {
  if (!candidate %in% existing) {
    return(candidate)
  }

  i <- 1L
  repeat {
    next_name <- paste0(candidate, "_", i)
    if (!next_name %in% existing) {
      return(next_name)
    }
    i <- i + 1L
  }
}

# Build the single-line address, appending the ZIP only when present so a
# missing ZIP does not leave a trailing " NA" that confuses the geocoder.
.make_full_address <- function(address, city, state, zip) {
  base <- paste0(address, ", ", city, ", ", state)
  dplyr::if_else(
    is.na(zip) | zip == "",
    base,
    paste0(base, " ", zip)
  )
}

# ---- export-location-crosswalk.R ----
#' Export the location crosswalk
#'
#' Selects the final, stable set of columns for dashboards, GIS joins, and
#' reusable reference tables, and optionally writes them to CSV. Audit columns
#' are retained so a reviewer can always see how each coordinate was produced,
#' including score/type/status fields from the name lookup tier when available.
#'
#' @param data A fully processed data frame.
#' @param path Optional output CSV path. When `NULL`, nothing is written.
#'
#' @return The crosswalk tibble (also written to `path` when supplied).
#' @export
export_location_crosswalk <- function(data, path = NULL) {
  if (!"match_confidence" %in% names(data)) {
    data <- add_match_confidence(data)
  }
  crosswalk <- data %>%
    dplyr::transmute(
      record_id           = .data$record_id,
      record_name         = .data$record_name,
      address_clean         = .data$address_clean,
      city_clean            = .data$city_clean,
      state_clean           = .data$state_clean,
      zip_clean             = .data$zip_clean,
      full_address_clean    = .data$full_address_clean,
      latitude              = .data$latitude,
      longitude             = .data$longitude,
      location_county       = .pull_if(data, "location_county"),
      location_locality     = .pull_if(data, "location_locality"),
      County                = .pull_first(data, c("County", "location_county")),
      Municipality          = .pull_first(data, c("Municipality", "location_locality")),
      `Muni Key`            = .pull_first(data, c("Muni Key", "muni_key")),
      muni_join_key         = .pull_if(data, "muni_join_key"),
      county_code           = .pull_if(data, "county_code"),
      county_fips           = .pull_if(data, "county_fips"),
      municipality_code     = .pull_if(data, "municipality_code"),
      municipality_geoid    = .pull_if(data, "municipality_geoid"),
      municipality_name_standard = .pull_if(data, "municipality_name_standard"),
      municipality_type     = .pull_if(data, "municipality_type"),
      muni_match_status     = .pull_first(data, c("muni_match_status",
                                                  "geography_match_status")),
      geocode_method        = .data$geocode_method,
      geocode_pass          = .data$geocode_pass,
      match_status          = .data$match_status,
      name_match_score      = .pull_if(data, "nm_score"),
      name_match_type       = .pull_if(data, "nm_addr_type"),
      name_match_status     = .pull_if(data, "nm_status"),
      validation_status     = .pull_if(data, "validation_status"),
      geography_match_status = .pull_if(data, "geography_match_status"),
      manual_override_used  = .pull_logical(data, "manual_override_used",
                                            default = FALSE),
      match_confidence      = .pull_if(data, "match_confidence"),
      confidence_reason     = .pull_if(data, "confidence_reason"),
      placed_at             = .pull_if(data, "placed_at"),
      cache_status          = .pull_if(data, "cache_status"),
      review_status         = .data$review_status
    )

  if (!is.null(path)) {
    readr::write_csv(crosswalk, path)
  }
  crosswalk
}

# ---- flag-address.R ----
#' Flag addresses that should not be blindly geocoded
#'
#' Identifies PO boxes, placeholders, and missing fields so they go straight to
#' review instead of wasting geocoder calls (or producing confident-but-wrong
#' matches). Sets `bad_address_flag` and an initial `review_status`.
#'
#' A missing ZIP is recorded as `bad_address_flag == "missing_zip"` for audit,
#' but it does **not** block geocoding: as long as the address and city are
#' present, the row stays `ready_for_geocoding` (Census matches on
#' street/city/state and ArcGIS on the single-line address). Only genuinely
#' unusable rows - missing address or city, PO boxes, placeholders, test
#' records - are routed to `needs_manual_review`.
#'
#' @param data A data frame from [clean_addresses()].
#'
#' @return `data` with added columns `bad_address_flag` and `review_status`.
#'   Rows fit for geocoding get `review_status == "ready_for_geocoding"`.
#' @export
#' @examples
#' df <- tibble::tibble(
#'   record_id = c("a", "b"),
#'   address_clean = c("100 MAIN STREET", "PO BOX 42"),
#'   city_clean = c("TRENTON", "TRENTON"),
#'   zip_clean = c("08608", "08608"),
#'   record_name = c("Real Site", "Mailbox Co")
#' )
#' flag_bad_addresses(df)
flag_bad_addresses <- function(data) {
  data %>%
    dplyr::mutate(
      bad_address_flag = dplyr::case_when(
        is.na(.data$address_clean) | .data$address_clean == "" ~ "missing_address",
        is.na(.data$city_clean) | .data$city_clean == ""       ~ "missing_city",
        stringr::str_detect(.data$address_clean, "\\bP\\.?O\\.? BOX\\b") ~ "po_box",
        stringr::str_detect(.data$address_clean, "\\bTBD\\b|\\bUNKNOWN\\b|\\bTEST\\b") ~ "placeholder_address",
        stringr::str_detect(toupper(.data$record_name), "\\bTEST\\b") ~ "test_record",
        is.na(.data$zip_clean) | .data$zip_clean == ""         ~ "missing_zip",
        TRUE ~ NA_character_
      ),
      # missing_zip is informational only - a real address + city is still
      # geocodable, so those rows stay ready. Everything else with a flag goes
      # to manual review.
      review_status = dplyr::case_when(
        is.na(.data$bad_address_flag) ~ "ready_for_geocoding",
        .data$bad_address_flag == "missing_zip" ~ "ready_for_geocoding",
        TRUE ~ "needs_manual_review"
      )
    )
}

# ---- flag-field-conflicts.R ----
#' Flag cross-field conflicts in location data
#'
#' Catches a class of data-entry errors the geocoder itself will silently accept:
#' a ZIP that cannot belong to the stated state, and a stated county that
#' disagrees with the county the coordinate actually fell in. It adds audit
#' columns rather than changing any coordinate, so a reviewer can decide what to
#' do.
#'
#' The ZIP check is deliberately conservative. It compares the ZIP's leading
#' digit against the USPS regional assignment for the stated state, so it only
#' flags a ZIP that is definitively in the wrong region (for example a
#' `"8xxxx"` ZIP recorded in New Jersey). It never flags a same-region
#' near-miss, and it stays silent when the state is unknown or the ZIP is
#' missing, so it does not produce false positives.
#'
#' The county check compares a stated county column against a geocoded county
#' column (for example `location_county` from [add_county_muni()]), after
#' normalising case and stripping the trailing "County"/"Parish"/"Borough".
#'
#' @param data A data frame of cleaned/geocoded records.
#' @param zip Name of the ZIP column (default `"zip_clean"`). Set to `NULL` to
#'   skip the ZIP check.
#' @param state Name of the state column (default `"state_clean"`).
#' @param stated_county Optional name of a county column supplied in the input.
#'   The county check runs only when this is given.
#' @param geocoded_county Name of the geocoded county column to compare against
#'   (default `"location_county"`).
#'
#' @return `data` with three added columns: `zip_state_conflict` (logical, `NA`
#'   when indeterminate), `county_conflict` (logical, `NA` when either county is
#'   missing), and `field_conflict` (a `"; "`-joined summary such as
#'   `"zip_state"`, `"county"`, or `"zip_state; county"`; `NA` when clean).
#' @export
#' @examples
#' df <- data.frame(
#'   zip_clean = c("07030", "85001"),   # 07 is NJ; 85 is AZ
#'   state_clean = c("NJ", "NJ")
#' )
#' flag_field_conflicts(df)
flag_field_conflicts <- function(data, zip = "zip_clean",
                                 state = "state_clean",
                                 stated_county = NULL,
                                 geocoded_county = "location_county") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  n <- nrow(data)

  zip_state <- if (!is.null(zip) && zip %in% names(data) &&
                   !is.null(state) && state %in% names(data)) {
    .zip_state_conflict(data[[zip]], data[[state]])
  } else {
    rep(NA, n)
  }

  county <- if (!is.null(stated_county) && stated_county %in% names(data) &&
                !is.null(geocoded_county) && geocoded_county %in% names(data)) {
    .county_conflict(data[[stated_county]], data[[geocoded_county]])
  } else {
    rep(NA, n)
  }

  data$zip_state_conflict <- zip_state
  data$county_conflict <- county
  data$field_conflict <- .combine_conflicts(
    list(zip_state = zip_state, county = county)
  )
  data
}

# ---- internals --------------------------------------------------------------

# USPS leading-ZIP-digit -> states that have any ZIP starting with that digit.
# Inclusive on purpose (e.g. NY spans 0 and 1) so the state->digit inversion
# never rejects a legitimate ZIP.
.ZIP_FIRST_DIGIT_STATES <- list(
  "0" = c("CT", "MA", "ME", "NH", "NJ", "NY", "PR", "RI", "VT", "VI"),
  "1" = c("DE", "NY", "PA"),
  "2" = c("DC", "MD", "NC", "SC", "VA", "WV"),
  "3" = c("AL", "FL", "GA", "MS", "TN"),
  "4" = c("IN", "KY", "MI", "OH"),
  "5" = c("IA", "MN", "MT", "ND", "SD", "WI"),
  "6" = c("IL", "KS", "MO", "NE"),
  "7" = c("AR", "LA", "OK", "TX"),
  "8" = c("AZ", "CO", "ID", "NM", "NV", "UT", "WY"),
  "9" = c("AK", "AS", "CA", "GU", "HI", "MP", "OR", "WA")
)

# state -> character vector of valid leading ZIP digits.
.state_zip_digits <- function() {
  inv <- list()
  for (digit in names(.ZIP_FIRST_DIGIT_STATES)) {
    for (st in .ZIP_FIRST_DIGIT_STATES[[digit]]) {
      inv[[st]] <- c(inv[[st]], digit)
    }
  }
  inv
}

.zip_state_conflict <- function(zip, state) {
  zip <- as.character(zip)
  state <- toupper(as.character(state))
  digit <- substr(gsub("\\D", "", zip), 1, 1)
  inv <- .state_zip_digits()

  vapply(seq_along(zip), function(i) {
    d <- digit[i]
    st <- state[i]
    if (is.na(st) || !nzchar(st) || is.na(d) || !nzchar(d)) {
      return(NA)
    }
    allowed <- inv[[st]]
    if (is.null(allowed)) {
      return(NA)   # unknown/foreign state code: cannot judge, so do not flag
    }
    !(d %in% allowed)
  }, logical(1))
}

.county_conflict <- function(stated, geocoded) {
  norm <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- gsub("\\s+(COUNTY|PARISH|BOROUGH|CENSUS AREA|CITY)$", "", x)
    trimws(x)
  }
  s <- norm(stated)
  g <- norm(geocoded)
  ok <- !is.na(s) & nzchar(s) & !is.na(g) & nzchar(g)
  ifelse(ok, s != g, NA)
}

.combine_conflicts <- function(flags) {
  n <- length(flags[[1]])
  out <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    hits <- names(flags)[vapply(flags, function(f) isTRUE(f[i]), logical(1))]
    if (length(hits) > 0L) {
      out[i] <- paste(hits, collapse = "; ")
    }
  }
  out
}

# ---- geocode-address.R ----
#' Look up a single address and return ranked candidate points
#'
#' An interactive, one-shot companion to the batch pipeline: pass a literal
#' address (no data frame) and get back a tibble of candidate matches ranked by
#' geocoder confidence, highest first. Each candidate carries its coordinates,
#' match score, and ArcGIS address type, and - unless turned off - the county
#' and municipality the point falls in. Handy for spot-checking one address, or
#' for letting a reviewer eyeball the plausible locations the cascade would
#' choose from.
#'
#' The address text is normalised with the same abbreviation/secondary-unit
#' cleaning used by [clean_addresses()], then sent to the free ArcGIS
#' `findAddressCandidates` service. `city`, `state`, and `zip` are optional for
#' this one-off helper: use them when you want to narrow the search, or pass only
#' `address` to inspect broad candidate matches. If `city` is supplied and
#' `state` is omitted, `state` defaults to `"NJ"` for compatibility with the
#' package's first workflow.
#'
#' @param address Single-line street address as a length-1 character string.
#' @param city Optional locality for the address (length-1 character).
#' @param state Optional two-letter state abbreviation. When `city` is supplied
#'   but `state` is omitted, defaults to `"NJ"` for compatibility.
#' @param zip Optional ZIP/postal code. Improves match precision when supplied.
#' @param id Optional label echoed back in the `query_id` column.
#' @param min_score Minimum ArcGIS match score (0-100) a returned candidate must
#'   reach to be kept. Defaults to `0`. This filters ArcGIS results; use `city`,
#'   `state`, `bbox`, or `zip` to change the search context.
#' @param max_candidates Maximum number of candidates to return. Defaults to `5`.
#' @param geography If `TRUE` (default), attach `County`/`Municipality` (and the
#'   other local-geography fields). When `state` is not supplied, locatr tries
#'   to infer candidate states from ArcGIS matched addresses. Set `FALSE` for
#'   coordinates only.
#' @param geography_shapes Optional `sf` boundary layer to attach geography from
#'   (via [add_muni_from_shapes()]). When `NULL` and `geography = TRUE`, county
#'   subdivisions are built from Census TIGER/Line for `state` (needs `tigris`
#'   and network access).
#' @param bbox Optional region bounding box (see [region_bbox()]). When given,
#'   ArcGIS is asked to prefer that extent and an `in_bbox` flag is added.
#' @param quiet If `TRUE` (default), suppress routine messages from geography
#'   downloads/joins so the console shows only the returned candidate table.
#' @param show_progress If `TRUE`, print short progress messages while the
#'   lookup runs. Defaults to `interactive()`.
#' @param cache Optional [locatr_cache()] object. When supplied, the ArcGIS
#'   candidate lookup for a given query is served from the cache on repeat calls
#'   (and replayable offline) instead of re-hitting the service.
#' @param refresh If `TRUE`, bypass any cached entry for this query and re-query
#'   the service, overwriting the cached result. Defaults to `FALSE`.
#'
#' @return A tibble of candidates ordered by descending `match_score`, with
#'   `query_id`, `rank`, `match_score`, `match_addr_type`, `matched_address`,
#'   `latitude`, `longitude`, the cleaned `input_address`, an optional `in_bbox`
#'   flag, and (when `geography = TRUE`) `County`/`Municipality` and related
#'   fields. Zero rows if nothing matched at or above `min_score`.
#' @seealso [geocode_records()] for the batch cascade over a data frame.
#' @export
#' @examples
#' if (interactive()) {
#' # ranked candidates for one address
#' geocode_address("1600 Pennsylvania Ave NW")
#'
#' # only high-confidence matches, coordinates only
#' geocode_address("1 City Hall Sq", city = "Boston", state = "MA",
#'                 min_score = 90, geography = FALSE)
#' }
geocode_address <- function(address, city = NULL, state = NULL, zip = NULL,
                            id = NULL, min_score = 0, max_candidates = 5L,
                            geography = TRUE, geography_shapes = NULL,
                            bbox = NULL, quiet = TRUE,
                            show_progress = interactive(),
                            cache = NULL, refresh = FALSE) {
  if (!is.character(address) || length(address) != 1L || is.na(address)) {
    stop("`address` must be a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.null(city) && (!is.character(city) || length(city) != 1L ||
                         is.na(city))) {
    stop("`city` must be `NULL` or a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.null(zip) && (!is.character(zip) || length(zip) != 1L || is.na(zip))) {
    stop("`zip` must be `NULL` or a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.null(state) && (!is.character(state) || length(state) != 1L ||
                          is.na(state))) {
    stop("`state` must be `NULL` or a single, non-missing character string.",
         call. = FALSE)
  }
  if (!is.null(id) && (!is.atomic(id) || length(id) < 1L || is.na(id[[1]]))) {
    stop("`id` must be `NULL` or a non-missing scalar value.", call. = FALSE)
  }
  if (!is.numeric(min_score) || length(min_score) != 1L ||
      is.na(min_score) || min_score < 0 || min_score > 100) {
    stop("`min_score` must be a single number from 0 to 100.", call. = FALSE)
  }
  if (!is.numeric(max_candidates) || length(max_candidates) != 1L ||
      is.na(max_candidates) || max_candidates < 1 ||
      max_candidates != as.integer(max_candidates)) {
    stop("`max_candidates` must be a positive whole number.", call. = FALSE)
  }
  if (!is.logical(geography) || length(geography) != 1L || is.na(geography)) {
    stop("`geography` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.logical(show_progress) || length(show_progress) != 1L ||
      is.na(show_progress)) {
    stop("`show_progress` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  .validate_cache_args(cache, refresh)
  max_candidates <- as.integer(max_candidates)

  query_id <- if (is.null(id)) NA_character_ else as.character(id)[1]
  effective_state <- state
  if (is.null(effective_state) && !is.null(city)) {
    effective_state <- "NJ"
  }
  single_line <- .single_address_query(address, city = city,
                                      state = effective_state, zip = zip)

  # Over-fetch from ArcGIS: a vague query can fill the top slots with
  # near-duplicate hits for one place, which dedupe then collapses. Requesting
  # more than we display keeps distinct localities (e.g. same street name in
  # another state) from being starved before the dedupe + cap.
  fetch_n <- min(50L, max(as.integer(max_candidates) * 5L, 25L))
  .geocode_address_progress(show_progress, "Looking up address candidates ...")
  cands <- .arcgis_candidates_cached(single_line, max_candidates = fetch_n,
                                     bbox = bbox, cache = cache,
                                     refresh = refresh)
  .geocode_address_progress(show_progress, "Scoring candidates ...")
  cands <- cands %>%
    dplyr::filter(!is.na(.data$match_score), .data$match_score >= min_score) %>%
    dplyr::arrange(dplyr::desc(.data$match_score)) %>%
    .dedupe_address_candidates() %>%
    dplyr::slice_head(n = max_candidates)
  .geocode_address_context_tip(
    cands,
    has_context = !is.null(city) || !is.null(state) || !is.null(zip) ||
      !is.null(bbox),
    show_progress = show_progress
  )

  empty_meta <- function(d) {
    d %>%
      dplyr::mutate(query_id = query_id, input_address = single_line,
                    rank = dplyr::row_number())
  }

  if (nrow(cands) == 0L) {
    .geocode_address_progress(show_progress, "No candidates met the threshold.")
    return(add_match_confidence(empty_meta(cands)))
  }

  cands <- empty_meta(cands)
  if (!is.null(bbox)) {
    cands$in_bbox <- in_bbox(cands$latitude, cands$longitude, bbox)
  }

  if (isTRUE(geography)) {
    if (!is.null(geography_shapes) || !is.null(effective_state)) {
      .geocode_address_progress(show_progress, "Attaching local geography ...")
    } else {
      .geocode_address_progress(show_progress,
                                "Inferring candidate states for geography ...")
    }
    attach_geography <- function() {
      if (!is.null(geography_shapes)) {
        add_muni_from_shapes(cands, muni_shapes = geography_shapes)
      } else if (!is.null(effective_state)) {
        add_county_muni(cands, state = effective_state)
      } else {
        .attach_geography_by_inferred_state(cands)
      }
    }
    geo <- tryCatch(
      if (isTRUE(quiet)) {
        out <- NULL
        invisible(utils::capture.output(
          out <- suppressMessages(attach_geography()),
          type = "output"
        ))
        out
      } else {
        attach_geography()
      },
      error = function(e) {
        warning("Geography lookup skipped: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (!is.null(geo)) cands <- geo
  }

  cands <- add_match_confidence(cands)

  .geocode_address_progress(
    show_progress,
    paste0("Done. Returning ", nrow(cands), " candidate",
           if (nrow(cands) == 1L) "." else "s.")
  )
  lead <- c("query_id", "rank", "match_confidence", "confidence_reason",
            "match_score", "match_addr_type", "matched_address",
            "latitude", "longitude", "in_bbox", "input_address",
            "County", "Municipality")
  dplyr::relocate(cands, dplyr::any_of(lead))
}

.attach_geography_by_inferred_state <- function(cands) {
  cands$candidate_state <- .infer_candidate_state(cands$matched_address)
  states <- unique(stats::na.omit(cands$candidate_state))
  if (length(states) == 0L) {
    return(.ensure_candidate_geography_cols(cands))
  }

  pieces <- lapply(states, function(st) {
    add_county_muni(cands[cands$candidate_state == st, , drop = FALSE],
                   state = st)
  })
  missing_state <- cands[is.na(cands$candidate_state), , drop = FALSE]
  if (nrow(missing_state) > 0L) {
    pieces <- c(pieces, list(.ensure_candidate_geography_cols(missing_state)))
  }
  dplyr::bind_rows(pieces) %>%
    dplyr::filter(.data$rank %in% cands$rank) %>%
    dplyr::arrange(.data$rank)
}

.ensure_candidate_geography_cols <- function(cands) {
  cols <- list(
    County = NA_character_,
    Municipality = NA_character_,
    location_county = NA_character_,
    location_locality = NA_character_,
    geography_match_status = NA_character_,
    muni_match_status = NA_character_,
    county_code = NA_character_,
    county_fips = NA_character_,
    municipality_code = NA_character_,
    municipality_geoid = NA_character_,
    municipality_name_standard = NA_character_,
    municipality_type = NA_character_,
    muni_join_key = NA_character_,
    `Muni Key` = NA_character_
  )
  for (col in names(cols)) {
    if (!col %in% names(cands)) cands[[col]] <- cols[[col]]
  }
  cands
}

.infer_candidate_state <- function(matched_address) {
  state_lookup <- c(
    stats::setNames(datasets::state.abb, toupper(datasets::state.abb)),
    stats::setNames(datasets::state.abb, toupper(datasets::state.name)),
    "DC" = "DC",
    "DISTRICT OF COLUMBIA" = "DC",
    "WASHINGTON DC" = "DC",
    "WASHINGTON D C" = "DC"
  )

  unname(vapply(matched_address, function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(NA_character_)
    }
    text <- toupper(x)
    tokens <- stringr::str_split(text, "\\s*,\\s*", simplify = FALSE)[[1]]
    tokens <- stringr::str_squish(tokens)
    hit <- state_lookup[tokens[tokens %in% names(state_lookup)]][1]
    if (!is.na(hit)) {
      return(unname(hit))
    }

    names_hit <- names(state_lookup)[
      vapply(names(state_lookup), function(state_name) {
        grepl(paste0("\\b", state_name, "\\b"), text)
      }, logical(1))
    ]
    if (length(names_hit) == 0L) NA_character_ else unname(state_lookup[[names_hit[1]]])
  }, character(1)))
}

.geocode_address_progress <- function(show_progress, text) {
  if (isTRUE(show_progress)) {
    message("[locatr] ", text)
  }
}

.geocode_address_context_tip <- function(cands, has_context, show_progress) {
  if (!isTRUE(show_progress) || isTRUE(has_context) || nrow(cands) == 0L) {
    return(invisible(NULL))
  }
  precise_types <- c("PointAddress", "Subaddress", "StreetAddress")
  types <- unique(stats::na.omit(cands$match_addr_type))
  if (length(types) > 0L && !any(types %in% precise_types)) {
    .geocode_address_progress(
      TRUE,
      "Tip: broad lookup returned locality/POI-style candidates. `min_score` only filters returned candidates; add city, state, zip, or bbox to change the search."
    )
  }
  invisible(NULL)
}

.dedupe_address_candidates <- function(cands) {
  cands$.dedupe_address <- stringr::str_squish(stringr::str_to_upper(
    cands$matched_address
  ))
  cands$.dedupe_latitude <- round(cands$latitude, 5)
  cands$.dedupe_longitude <- round(cands$longitude, 5)
  cands <- dplyr::distinct(
    cands,
    .data$.dedupe_address,
    .data$.dedupe_latitude,
    .data$.dedupe_longitude,
    .data$match_addr_type,
    .keep_all = TRUE
  )
  cands$.dedupe_address <- NULL
  cands$.dedupe_latitude <- NULL
  cands$.dedupe_longitude <- NULL
  cands
}

.single_address_query <- function(address, city = NULL, state = NULL,
                                  zip = NULL) {
  address <- .clean_single_address_piece(address)
  pieces <- c(
    address,
    if (!is.null(city)) stringr::str_squish(stringr::str_to_upper(city)),
    if (!is.null(state)) stringr::str_squish(stringr::str_to_upper(state))
  )
  pieces <- pieces[!is.na(pieces) & nzchar(pieces)]
  query <- paste(pieces, collapse = ", ")
  if (!is.null(zip)) {
    zip_clean <- zip %>%
      stringr::str_remove_all("\\D") %>%
      stringr::str_sub(1, 5) %>%
      stringr::str_pad(width = 5, side = "left", pad = "0") %>%
      dplyr::na_if("00000")
    if (!is.na(zip_clean) && nzchar(zip_clean)) {
      query <- paste(query, zip_clean)
    }
  }
  query
}

.clean_single_address_piece <- function(address) {
  address %>%
    stringr::str_to_upper() %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("\\bONE\\b", "1") %>%
    stringr::str_replace_all("\\bTWO\\b", "2") %>%
    stringr::str_replace_all("\\bTHREE\\b", "3") %>%
    stringr::str_replace_all("\\bFOUR\\b", "4") %>%
    stringr::str_replace_all("\\bFIVE\\b", "5") %>%
    stringr::str_replace_all("\\bRTE?\\b", "ROUTE") %>%
    stringr::str_replace_all("\\bHWY\\b", "HIGHWAY") %>%
    stringr::str_replace_all("\\bROUTE\\s+([0-9]+)", "STATE ROUTE \\1") %>%
    stringr::str_replace_all("\\bHIGHWAY\\s+([0-9]+)", "STATE HIGHWAY \\1") %>%
    stringr::str_replace_all("\\bMT\\b", "MOUNT") %>%
    stringr::str_replace_all("\\bAVE\\b", "AVENUE") %>%
    stringr::str_replace_all("\\bRD\\b", "ROAD") %>%
    stringr::str_replace_all("\\bBLVD\\b", "BOULEVARD") %>%
    stringr::str_replace_all("\\bST\\b", "STREET") %>%
    stringr::str_replace_all("\\bDR\\b", "DRIVE") %>%
    stringr::str_replace_all("\\bLN\\b", "LANE") %>%
    stringr::str_replace_all(
      ",?\\s*(SUITE|STE|STES|UNIT|BLDG|BUILDING|FLOOR|FLR|ROOM|RM)\\s+[A-Z0-9\\-]+",
      ""
    ) %>%
    stringr::str_squish()
}

# Query the free ArcGIS findAddressCandidates service for one single-line
# address and return a ranked tibble of candidates. httr/jsonlite are Suggests.
.arcgis_candidates <- function(single_line, max_candidates = 5L, bbox = NULL) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`geocode_address()` needs the 'httr' and 'jsonlite' packages. ",
         "Install them with install.packages(c(\"httr\", \"jsonlite\")).",
         call. = FALSE)
  }

  empty <- tibble::tibble(
    matched_address = character(), longitude = double(),
    latitude = double(), match_score = double(),
    match_addr_type = character()
  )

  query <- list(
    SingleLine   = single_line,
    outFields    = "Addr_type,Match_addr",
    maxLocations = as.integer(max_candidates),
    countryCode  = "USA",
    f            = "json"
  )
  if (!is.null(bbox)) {
    query$searchExtent <- paste(bbox[["lon_min"]], bbox[["lat_min"]],
                                bbox[["lon_max"]], bbox[["lat_max"]], sep = ",")
  }

  # Fail gracefully (informative warning, no candidates) if the service is
  # unreachable or errors, per CRAN policy for internet-using packages.
  parsed <- tryCatch(
    {
      resp <- httr::GET(
        paste0("https://geocode.arcgis.com/arcgis/rest/services/World/",
               "GeocodeServer/findAddressCandidates"),
        query = query
      )
      httr::stop_for_status(resp)
      jsonlite::fromJSON(
        httr::content(resp, as = "text", encoding = "UTF-8"),
        simplifyVector = TRUE
      )
    },
    error = function(e) {
      warning("ArcGIS geocoding request failed (", conditionMessage(e),
              "); returning no candidates.", call. = FALSE)
      NULL
    }
  )
  if (is.null(parsed)) {
    return(empty)
  }

  cands <- parsed$candidates
  if (is.null(cands) || !is.data.frame(cands) || nrow(cands) == 0L) {
    return(empty)
  }

  addr_type <- if (!is.null(cands$attributes) &&
                   "Addr_type" %in% names(cands$attributes)) {
    as.character(cands$attributes$Addr_type)
  } else {
    rep(NA_character_, nrow(cands))
  }

  tibble::tibble(
    matched_address = as.character(cands$address),
    longitude       = as.numeric(cands$location$x),
    latitude        = as.numeric(cands$location$y),
    match_score     = as.numeric(cands$score),
    match_addr_type = addr_type
  ) %>%
    dplyr::arrange(dplyr::desc(.data$match_score))
}

# ---- geocode-arcgis.R ----
#' ArcGIS address fallback pass (Google-like fuzzy matching)
#'
#' For rows the Census pass could not place inside the configured region,
#' re-geocodes with a composite geocoder (ArcGIS by default: free, no API key,
#' fuzzy matching close to Google) using the single-line `full_address_clean`.
#' ArcGIS requests are constrained to the region bbox when possible, and results
#' are still guarded against the bounding box so out-of-region false matches are
#' discarded before coordinates are coalesced back into `latitude`/`longitude`.
#'
#' Formerly `geocode_fallback()`; renamed because this tier is specifically the
#' ArcGIS (composite) address pass.
#'
#' @param data A data frame from [geocode_census()] (or after
#'   [validate_geocodes()]).
#' @param method tidygeocoder method for this pass (default `"arcgis"`).
#'   `"google"` also works if `GOOGLEGEOCODE_API_KEY` is set.
#' @param bbox Bounding box used to reject out-of-region matches; see
#'   [region_bbox()].
#' @param ... Passed through to [tidygeocoder::geocode()].
#' @param cache Optional [locatr_cache()]. When supplied, the ArcGIS lookup for
#'   a given `full_address_clean` (under the same region extent) is served from
#'   the cache instead of re-querying.
#' @param refresh If `TRUE`, ignore cached entries and re-query, overwriting
#'   them. Defaults to `FALSE`.
#'
#' @return `data` with fallback columns `fb_latitude`, `fb_longitude`,
#'   `fb_status`, and updated `latitude`, `longitude`, `geocode_method`,
#'   `geocode_pass`, `match_status` for rows this pass filled.
#' @export
geocode_arcgis <- function(data, method = "arcgis",
                           bbox = region_bbox("NJ"), ...,
                           cache = NULL, refresh = FALSE) {
  stopifnot(all(c("record_id", "latitude", "longitude") %in% names(data)))
  .validate_cache_args(cache, refresh)

  needs_fallback <- data %>%
    dplyr::mutate(.retryable_for_geocoding = .retryable_for_geocoding(.)) %>%
    dplyr::filter(
      .data$.retryable_for_geocoding,
      is.na(.data$latitude) | is.na(.data$longitude) |
        !in_bbox(.data$latitude, .data$longitude, bbox)
    ) %>%
    dplyr::select(-".retryable_for_geocoding")

  if (nrow(needs_fallback) == 0L) {
    return(
      data %>%
        dplyr::mutate(
          fb_latitude = NA_real_, fb_longitude = NA_real_,
          fb_status = NA_character_
        )
    )
  }

  fb <- .arcgis_fill_coords(needs_fallback, method, bbox, cache, refresh, ...) %>%
    dplyr::mutate(
      fb_in_bbox = in_bbox(.data$fb_latitude, .data$fb_longitude, bbox),
      fb_status = dplyr::case_when(
        is.na(.data$fb_latitude) | is.na(.data$fb_longitude) ~ "fallback_no_match",
        !.data$fb_in_bbox ~ "fallback_outside_region_rejected",
        TRUE ~ "fallback_matched"
      ),
      # null out coordinates that fell outside the region so they never map
      fb_latitude  = dplyr::if_else(.data$fb_in_bbox, .data$fb_latitude, NA_real_),
      fb_longitude = dplyr::if_else(.data$fb_in_bbox, .data$fb_longitude, NA_real_)
    ) %>%
    dplyr::select("record_id", "fb_latitude", "fb_longitude", "fb_status")

  data %>%
    dplyr::left_join(fb, by = "record_id") %>%
    dplyr::mutate(
      use_fb = !is.na(.data$fb_latitude) & !is.na(.data$fb_longitude) &
        (is.na(.data$latitude) | is.na(.data$longitude) |
           !in_bbox(.data$latitude, .data$longitude, bbox)),
      latitude       = dplyr::if_else(.data$use_fb, .data$fb_latitude, .data$latitude),
      longitude      = dplyr::if_else(.data$use_fb, .data$fb_longitude, .data$longitude),
      geocode_method = dplyr::if_else(.data$use_fb, method, .data$geocode_method),
      geocode_pass   = dplyr::if_else(.data$use_fb, "pass_2_fallback", .data$geocode_pass),
      match_status   = dplyr::if_else(.data$use_fb, "matched", .data$match_status)
    ) %>%
    dplyr::select(-"use_fb")
}

# Return raw ArcGIS fallback coordinates (record_id, fb_latitude, fb_longitude)
# for the rows needing a fallback, reusing a cache when supplied. The bbox
# rejection is applied by the caller, so the cache holds the raw geocoder
# coordinate keyed by address + region extent. `cache = NULL` is the original
# call, verbatim.
.arcgis_fill_coords <- function(needs_fallback, method, bbox, cache, refresh,
                                ...) {
  dots <- .region_geocoder_dots(method, bbox, list(...))
  fb_input <- needs_fallback %>%
    dplyr::select("record_id", "full_address_clean")
  live <- function(d) {
    fb_args <- c(
      list(d, address = "full_address_clean", method = method,
           lat = "fb_latitude", long = "fb_longitude"),
      dots
    )
    do.call(tidygeocoder::geocode, fb_args)
  }
  if (is.null(cache)) {
    g <- live(fb_input)
    return(tibble::tibble(record_id = g$record_id,
                          fb_latitude = g$fb_latitude,
                          fb_longitude = g$fb_longitude))
  }

  coords <- .batch_geocode_cached(
    fb_input, .arcgis_query_vec(fb_input), method = "arcgis_oneline",
    params = .arcgis_params(method, bbox, dots), cache = cache,
    refresh = refresh,
    run = function(d) {
      g <- live(d)
      tibble::tibble(record_id = g$record_id, latitude = g$fb_latitude,
                     longitude = g$fb_longitude)
    }
  )
  tibble::tibble(record_id = coords$record_id,
                 fb_latitude = coords$latitude,
                 fb_longitude = coords$longitude)
}

# ---- geocode-by-name.R ----
#' Name-based geocode pass (the "paste it in a browser" tier)
#'
#' For rows still unplaced after the address-based passes, geocodes by record
#' *name* plus city and state rather than the street line. This can resolve
#' campus/landmark addresses (e.g. a unit inside a hospital) that street-range
#' interpolation cannot, because a composite geocoder recognises the named place.
#'
#' Because name lookups are looser than address matching, each hit is scored
#' using the geocoder's match `score` and address type (when available - ArcGIS
#' returns both via `full_results`, which this pass requests automatically). A
#' hit passes cleanly only when it resolves to a precise point address at or
#' above `min_score`; fuzzier hits (a POI, a locality centroid, or a low score)
#' still have their coordinates filled in for context but are marked
#' `match_status == "matched_low_confidence"` and routed to
#' `needs_manual_review` so a person can confirm them. When the geocoder returns
#' no score/type information (e.g. `method = "osm"`), the pass falls back to the
#' previous rule: any in-region match is accepted.
#'
#' Filled rows are tagged `geocode_pass == "pass_4_name_lookup"`. The bounding box
#' still rejects out-of-region hits, but cannot catch a wrong same-state match,
#' which is exactly what the score gate is for.
#'
#' @param data A data frame carrying `record_id`, `record_name`,
#'   `city_clean`, `state_clean`, and the geocode audit columns.
#' @param method tidygeocoder method that accepts free-text queries
#'   (default `"arcgis"`; `"osm"` and `"google"` also work).
#' @param bbox Bounding box used to reject out-of-region matches; see
#'   [region_bbox()].
#' @param min_score Minimum match score (0-100) for a name hit to pass without
#'   review. Hits below this stay reviewable. Default `90`.
#' @param accept_types Address types precise enough to pass without review
#'   (matched case-insensitively against the geocoder's `addr_type`). Default the
#'   point-address types `c("PointAddress", "Subaddress", "StreetAddress")`.
#' @param ... Passed through to [tidygeocoder::geocode()]. `full_results = TRUE`
#'   is requested automatically so scores are available; pass
#'   `full_results = FALSE` to opt out (which also disables score gating).
#' @param cache Optional [locatr_cache()]. When supplied, the name lookup for a
#'   given query (under the same region extent) is served from the cache,
#'   including its cached score and address type, instead of re-querying.
#' @param refresh If `TRUE`, ignore cached entries and re-query, overwriting
#'   them. Defaults to `FALSE`.
#'
#' @return `data` with name-lookup audit columns `nm_latitude`, `nm_longitude`,
#'   `nm_score`, `nm_addr_type`, `nm_status`, and updated
#'   `latitude`/`longitude`/`geocode_method`/`geocode_pass`/`match_status` for
#'   rows the name pass filled. When there is nothing for the tier to geocode,
#'   `nm_status` is set to `"not_run"` for audit clarity. Low-confidence fills
#'   also set `review_status == "needs_manual_review"`.
#' @export
geocode_by_name <- function(data, method = "arcgis",
                            bbox = region_bbox("NJ"),
                            min_score = 90,
                            accept_types = c("PointAddress", "Subaddress",
                                             "StreetAddress"),
                            ...,
                            cache = NULL, refresh = FALSE) {
  stopifnot(all(c("record_id", "record_name", "city_clean", "state_clean") %in%
                  names(data)))
  .validate_cache_args(cache, refresh)
  dots <- .region_geocoder_dots(method, bbox, list(...))
  if (is.null(dots$full_results)) dots$full_results <- TRUE

  empty_cols <- function(d) {
    dplyr::mutate(d,
                  nm_latitude = NA_real_, nm_longitude = NA_real_,
                  nm_score = NA_real_, nm_addr_type = NA_character_,
                  nm_status = "not_run")
  }

  needs <- data %>%
    dplyr::mutate(.retryable_for_geocoding = .retryable_for_geocoding(.)) %>%
    dplyr::filter(
      .data$.retryable_for_geocoding,
      is.na(.data$latitude) | is.na(.data$longitude) |
        !in_bbox(.data$latitude, .data$longitude, bbox),
      !is.na(.data$record_name), .data$record_name != ""
    ) %>%
    dplyr::select(-".retryable_for_geocoding") %>%
    dplyr::mutate(
      name_query = paste0(.data$record_name, ", ",
                          .data$city_clean, ", ", .data$state_clean)
    )

  if (nrow(needs) == 0L) {
    return(empty_cols(data))
  }

  nm_raw <- .name_fill_coords(needs, method, bbox, cache, refresh, dots)

  nm <- nm_raw %>%
    dplyr::mutate(
      nm_in_bbox = in_bbox(.data$nm_latitude, .data$nm_longitude, bbox),
      # do we have any confidence signal to gate on?
      nm_scored = !is.na(.data$nm_score) | !is.na(.data$nm_addr_type),
      nm_high_conf =
        !is.na(.data$nm_score) & .data$nm_score >= min_score &
        !is.na(.data$nm_addr_type) &
        toupper(.data$nm_addr_type) %in% toupper(accept_types),
      nm_status = dplyr::case_when(
        is.na(.data$nm_latitude) | is.na(.data$nm_longitude) ~ "name_no_match",
        !.data$nm_in_bbox ~ "name_outside_region_rejected",
        !.data$nm_scored ~ "name_matched",                 # no score info: legacy accept
        .data$nm_high_conf ~ "name_matched_high_confidence",
        TRUE ~ "name_matched_low_confidence"
      ),
      nm_latitude  = dplyr::if_else(.data$nm_in_bbox, .data$nm_latitude,  NA_real_),
      nm_longitude = dplyr::if_else(.data$nm_in_bbox, .data$nm_longitude, NA_real_)
    ) %>%
    dplyr::select("record_id", "nm_latitude", "nm_longitude",
                  "nm_score", "nm_addr_type", "nm_status")

  out <- data %>%
    dplyr::left_join(nm, by = "record_id") %>%
    dplyr::mutate(
      use_nm = !is.na(.data$nm_latitude) & !is.na(.data$nm_longitude) &
        (is.na(.data$latitude) | is.na(.data$longitude) |
           !in_bbox(.data$latitude, .data$longitude, bbox)),
      nm_low = .data$use_nm &
        !is.na(.data$nm_status) & .data$nm_status == "name_matched_low_confidence",
      latitude       = dplyr::if_else(.data$use_nm, .data$nm_latitude, .data$latitude),
      longitude      = dplyr::if_else(.data$use_nm, .data$nm_longitude, .data$longitude),
      geocode_method = dplyr::if_else(.data$use_nm, paste0(method, "_byname"),
                                      .data$geocode_method),
      geocode_pass   = dplyr::if_else(.data$use_nm, "pass_4_name_lookup",
                                      .data$geocode_pass),
      match_status   = dplyr::case_when(
        .data$nm_low ~ "matched_low_confidence",
        .data$use_nm ~ "matched",
        TRUE ~ .data$match_status
      )
    )

  # A low-confidence name hit keeps its coordinates but must be reviewed.
  if ("review_status" %in% names(out)) {
    out <- dplyr::mutate(
      out,
      review_status = dplyr::if_else(.data$nm_low, "needs_manual_review",
                                     .data$review_status)
    )
  }

  out %>%
    dplyr::select(-dplyr::any_of(c("use_nm", "nm_low")))
}

# Return raw name-lookup results (record_id, nm_latitude, nm_longitude,
# nm_score, nm_addr_type) for the rows needing the name tier, reusing a cache
# when supplied. Score/type are extracted here so both the live and cached paths
# feed the same gating logic in geocode_by_name(). `cache = NULL` is the original
# call, verbatim.
.name_fill_coords <- function(needs, method, bbox, cache, refresh, dots) {
  nm_input <- needs %>%
    dplyr::select("record_id", "name_query")
  live <- function(d) {
    nm_args <- c(
      list(d, address = "name_query", method = method,
           lat = "nm_latitude", long = "nm_longitude"),
      dots
    )
    raw <- do.call(tidygeocoder::geocode, nm_args)
    score_col <- .pick_col(raw, "score")
    type_col  <- .pick_col(raw, "addr_type")
    tibble::tibble(
      record_id = raw$record_id,
      latitude  = raw$nm_latitude,
      longitude = raw$nm_longitude,
      score = if (!is.null(score_col)) {
        suppressWarnings(as.numeric(raw[[score_col]]))
      } else {
        NA_real_
      },
      addr_type = if (!is.null(type_col)) {
        as.character(raw[[type_col]])
      } else {
        NA_character_
      }
    )
  }
  if (is.null(cache)) {
    r <- live(nm_input)
    return(tibble::tibble(
      record_id = r$record_id, nm_latitude = r$latitude,
      nm_longitude = r$longitude, nm_score = r$score, nm_addr_type = r$addr_type
    ))
  }

  coords <- .batch_geocode_cached(
    nm_input, nm_input$name_query, method = "arcgis_byname",
    params = .name_params(method, bbox, dots), cache = cache,
    refresh = refresh,
    run = live
  )
  tibble::tibble(
    record_id = coords$record_id, nm_latitude = coords$latitude,
    nm_longitude = coords$longitude, nm_score = coords$score,
    nm_addr_type = coords$addr_type
  )
}

# ---- geocode-cascade.R ----
#' Run the full geocoding cascade
#'
#' Orchestrates the tiered strategy on an already-cleaned, already-flagged frame
#' (see [clean_addresses()] and [flag_bad_addresses()]): Census structured
#' match, then ArcGIS address fallback, then name lookup, validating against the
#' configured region after each tier so a later tier only retries what is still
#' unplaced.
#'
#' Each tier records how it placed a row in `geocode_pass`, so the final frame is
#' self-documenting: `pass_0_reference`, `pass_1_census_structured`,
#' `pass_2_fallback`, or `pass_4_name_lookup`. After the cascade, valid matched
#' rows are marked `review_status == "auto_accepted"`; anything still unmatched
#' lands in `needs_manual_review`, while invalid coordinates are `rejected`.
#'
#' Supplying `reference` runs an authoritative Tier 0 first
#' ([backfill_from_reference()]): rows whose verified coordinates are already
#' known are placed exactly and skipped by every later tier. Feed prior cycles'
#' completed overrides back in here so manual review accrues over time.
#'
#' @param data A data frame from [flag_bad_addresses()].
#' @param tiers Which tiers to run, in order. Any subset of
#'   `c("census", "arcgis", "name")`.
#' @param reference Optional trusted key -> coordinates table for Tier 0; see
#'   [backfill_from_reference()]. `NULL` skips Tier 0. For non-default column
#'   names, call [backfill_from_reference()] yourself before [geocode_records()].
#' @param boundary Optional `sf` boundary for polygon-precise validation;
#'   passed to [validate_geocodes()]. `NULL` uses the bounding box.
#' @param bbox Bounding box for region guards; see [region_bbox()].
#' @param name_min_score Minimum ArcGIS score for a name lookup to pass without
#'   review. Passed to [geocode_by_name()].
#' @param name_accept_types ArcGIS address types precise enough for a name lookup
#'   to pass without review. Passed to [geocode_by_name()].
#' @param verbose Whether to print a per-tier match tally.
#' @param cache Optional [locatr_cache()] shared across the network tiers
#'   (Census, ArcGIS, name lookup), so repeated addresses are served from the
#'   cache and a re-run reproduces coordinates without re-querying.
#' @param refresh If `TRUE`, ignore cached entries and re-query every tier,
#'   overwriting the cache. Defaults to `FALSE`.
#'
#' @return `data` with coordinates and the full audit trail populated.
#' @export
geocode_records <- function(data,
                               tiers = c("census", "arcgis", "name"),
                               reference = NULL,
                               boundary = NULL,
                               bbox = region_bbox("NJ"),
                               name_min_score = 90,
                               name_accept_types = c("PointAddress", "Subaddress",
                                                     "StreetAddress"),
                               verbose = TRUE,
                               cache = NULL,
                               refresh = FALSE) {
  stopifnot("review_status" %in% names(data))
  .validate_cache_args(cache, refresh)
  tiers <- match.arg(tiers, choices = c("census", "arcgis", "name"),
                     several.ok = TRUE)

  say <- function(...) if (isTRUE(verbose)) message(...)
  out <- data
  run_started <- .cache_now()
  cache_before <- if (!is.null(cache)) {
    c(cache$hits, cache$misses, cache$writes)
  } else {
    rep(NA_integer_, 3L)
  }

  if (!is.null(reference)) {
    say("Tier 0 - reference backfill ...")
    out <- backfill_from_reference(out, reference = reference, bbox = bbox)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  }

  if ("census" %in% tiers) {
    say("Tier 1 - Census structured geocode ...")
    out <- geocode_census(out, cache = cache, refresh = refresh)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  } else {
    out <- .ensure_geocode_cols(out)
  }

  if ("arcgis" %in% tiers) {
    say("Tier 2 - ArcGIS address fallback ...")
    out <- geocode_arcgis(out, method = "arcgis", bbox = bbox,
                          cache = cache, refresh = refresh)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  }

  if ("name" %in% tiers) {
    say("Tier 3 - name lookup fallback ...")
    out <- geocode_by_name(out, method = "arcgis", bbox = bbox,
                           min_score = name_min_score,
                           accept_types = name_accept_types,
                           cache = cache, refresh = refresh)
    out <- validate_geocodes(out, boundary = boundary, bbox = bbox)
    say("  placed in region so far: ", .n_in_region(out, bbox))
  }

  out <- add_match_confidence(.finalize_review_status(out))
  out <- .stamp_placement(out, cache, run_started, bbox)
  attr(out, "locatr_run") <- .locatr_run_manifest(
    out, tiers, reference, boundary, bbox, cache, run_started, cache_before
  )
  out
}

# Count rows whose coordinates fall inside the configured bbox.
.n_in_region <- function(data, bbox) {
  sum(in_bbox(data$latitude, data$longitude, bbox), na.rm = TRUE)
}

# Guarantee the coordinate/audit columns exist when the Census tier is skipped,
# so downstream tiers have something to coalesce into.
.ensure_geocode_cols <- function(data) {
  defaults <- list(
    latitude = NA_real_, longitude = NA_real_,
    geocode_method = NA_character_, geocode_pass = NA_character_,
    match_status = NA_character_
  )
  for (col in names(defaults)) {
    if (!col %in% names(data)) data[[col]] <- defaults[[col]]
  }
  data
}

.finalize_review_status <- function(data) {
  if (!"review_status" %in% names(data)) {
    return(data)
  }

  validation <- if ("validation_status" %in% names(data)) {
    data$validation_status
  } else {
    rep(NA_character_, nrow(data))
  }
  matched <- if ("match_status" %in% names(data)) {
    data$match_status == "matched"
  } else {
    rep(FALSE, nrow(data))
  }

  data$review_status <- dplyr::case_when(
    data$review_status == "manual_override_applied" ~ data$review_status,
    validation == "outside_region" ~ "rejected",
    matched & (is.na(validation) | validation == "coordinate_ok") ~ "auto_accepted",
    data$review_status == "ready_for_geocoding" ~ "needs_manual_review",
    TRUE ~ data$review_status
  )
  data
}

# ---- geocode-census.R ----
#' Primary geocode pass via the US Census batch geocoder
#'
#' Geocodes only the rows marked `ready_for_geocoding`, using the *structured*
#' Census engine (street / city / state / ZIP) rather than a single-line string,
#' which matches reliably more often. Rows not ready are returned untouched with
#' empty coordinate columns so the frame stays rectangular.
#'
#' Volatile Census full-result columns (`tiger_line_id`, `id`) are coerced to
#' character to avoid the `bind_rows()` integer/character type clash that the
#' Census service triggers intermittently between batches.
#'
#' @param data A data frame from [flag_bad_addresses()].
#' @param ... Passed through to [tidygeocoder::geocode()] (e.g. `full_results`).
#' @param cache Optional [locatr_cache()]. When supplied, rows whose structured
#'   query is already cached are filled from it instead of re-querying Census.
#' @param refresh If `TRUE`, ignore cached entries and re-query, overwriting
#'   them. Defaults to `FALSE`.
#'
#' @return `data` with `latitude`, `longitude`, `geocode_method`,
#'   `geocode_pass`, `match_status`, plus Census full-result columns when
#'   `full_results = TRUE` (full-result columns are not stored in the cache, so
#'   cache-filled rows omit them).
#' @export
geocode_census <- function(data, ..., cache = NULL, refresh = FALSE) {
  stopifnot("review_status" %in% names(data))
  .validate_cache_args(cache, refresh)

  ready     <- dplyr::filter(data, .data$review_status == "ready_for_geocoding")
  not_ready <- dplyr::filter(data, .data$review_status != "ready_for_geocoding" |
                               is.na(.data$review_status))

  if (nrow(ready) == 0L) {
    # Nothing to geocode here. Guarantee the audit columns exist without
    # clobbering coordinates an earlier tier (e.g. reference backfill) may
    # already have placed.
    return(.ensure_geocode_cols(data))
  }

  ready <- .census_fill_coords(ready, cache, refresh, ...)

  geocoded <- ready %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("tiger_line_id", "id")), as.character),
      geocode_method = "census",
      geocode_pass   = "pass_1_census_structured",
      match_status   = dplyr::if_else(
        !is.na(.data$latitude) & !is.na(.data$longitude),
        "matched", "no_match"
      )
    )

  dplyr::bind_rows(geocoded, not_ready)
}

# Fill latitude/longitude on the ready rows via the Census structured geocoder,
# reusing cached coordinates when a cache is supplied. The `cache = NULL` branch
# is the original call, verbatim, so mocked tests and behaviour are unchanged.
.census_fill_coords <- function(ready, cache, refresh, ...) {
  dots <- list(...)
  live <- function(d) {
    do.call(
      tidygeocoder::geocode,
      c(list(
        d,
        street     = quote(address_clean),
        city       = quote(city_clean),
        state      = quote(state_clean),
        postalcode = quote(zip_clean),
        method     = "census",
        lat        = quote(latitude),
        long       = quote(longitude)
      ), dots)
    )
  }
  if (is.null(cache)) {
    return(live(ready))
  }

  coords <- .batch_geocode_cached(
    ready, .census_query_vec(ready), method = "census_structured",
    params = .census_params(dots),
    cache = cache, refresh = refresh,
    run = function(d) {
      g <- live(d)
      tibble::tibble(record_id = g$record_id, latitude = g$latitude,
                     longitude = g$longitude)
    }
  )
  ready$latitude <- coords$latitude
  ready$longitude <- coords$longitude
  ready
}

# ---- geocode-report.R ----
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

# ---- io-helpers.R ----
# Internal IO helpers shared by the bundled Shiny app (inst/locatr-app/app.R).
# Kept in the package rather than the app so the app stays a thin presentation
# layer and these readers are covered by package tests. Not exported: they are
# called as locatr:::.read_location_table() / locatr:::.read_geography_layer().

# Read a tabular upload by file extension. `name` carries the original file name
# (and therefore the extension) because Shiny stores uploads under a temp path.
.read_location_table <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  switch(
    ext,
    csv     = readr::read_csv(path, show_col_types = FALSE),
    tsv     = readr::read_tsv(path, show_col_types = FALSE),
    txt     = readr::read_csv(path, show_col_types = FALSE),
    xlsx    = .read_excel(path),
    xls     = .read_excel(path),
    parquet = .read_parquet(path),
    stop("Unsupported data file type: .", ext,
         " (use csv, tsv, txt, xlsx, xls, or parquet).", call. = FALSE)
  )
}

# Read polygons from a zipped shapefile, a set of .shp sidecar files uploaded
# together, or a single .geojson/.gpkg. `upload` is the Shiny fileInput data
# frame (columns `name`, `datapath`).
.read_geography_layer <- function(upload) {
  exts <- tolower(tools::file_ext(upload$name))
  work <- file.path(
    tempdir(),
    paste0("locatr_shp_", as.integer(stats::runif(1, 1, 1e9)))
  )
  dir.create(work, showWarnings = FALSE, recursive = TRUE)

  if (any(exts == "zip")) {
    utils::unzip(upload$datapath[exts == "zip"][1], exdir = work)
  } else {
    # copy each uploaded file back to its real name so the .shp finds its
    # .dbf/.shx/.prj sidecars
    file.copy(upload$datapath, file.path(work, upload$name))
  }

  candidates <- list.files(
    work, pattern = "\\.(shp|gpkg|geojson|json)$",
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  )
  if (length(candidates) == 0) {
    stop("No .shp, .gpkg or .geojson found. For a shapefile, upload the .zip ",
         "or select the .shp together with its .dbf/.shx/.prj sidecars.",
         call. = FALSE)
  }
  sf::st_read(candidates[1], quiet = TRUE)
}

# readxl / arrow are Suggests (app-only deps); guard so the core package does
# not hard-require them.
.read_excel <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Reading Excel files needs the 'readxl' package.", call. = FALSE)
  }
  readxl::read_excel(path)
}

.read_parquet <- function(path) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Reading Parquet files needs the 'arrow' package.", call. = FALSE)
  }
  out <- arrow::read_parquet(path)
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out) else out
}

# ---- locatr-package.R ----
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom rlang .data := !! enquo quo_is_null
#' @importFrom stats setNames
## usethis namespace: end
NULL

# Quiet R CMD check notes about non-standard-evaluation column names that are
# referenced via .data$ or created mid-pipeline.
utils::globalVariables(c(
  "address_raw", "city_raw", "zip_raw",
  "full_address_raw", "address_clean", "city_clean", "state_clean", "zip_clean",
  "full_address_clean", "record_id", "record_name",
  "latitude", "longitude", "fb_latitude", "fb_longitude",
  "fb_in_bbox", "fb_status", "use_fb",
  "name_query", "nm_latitude", "nm_longitude", "nm_in_bbox", "nm_status",
  "nm_score", "nm_addr_type", "nm_scored", "nm_high_conf", "nm_low", "use_nm",
  "ref_latitude", "ref_longitude", "ref_in_bbox", "ref_status",
  "ref_county", "ref_locality", "use_ref", ".ref_key",
  ".retryable_for_geocoding",
  "bad_address_flag", "review_status", "match_status",
  "validation_status", "geocode_method", "geocode_pass",
  "location_county", "location_locality", "geography_match_status",
  ".locatr_row_id", ".match_count", "in_bbox", "tiger_line_id", ".",
  ".loc_address", ".loc_city", ".loc_zip",
  "match_score", "matched_address", "match_addr_type",
  "any_changed", ".dedupe_address", ".dedupe_latitude", ".dedupe_longitude",
  "match_confidence", "confidence_reason", "placed_at", "cache_status"
))

#' Region bounding box
#'
#' Returns an approximate latitude/longitude bounding box for a US state (or
#' `"DC"`), used as a fast sanity guard on geocoded coordinates. Presets are
#' deliberately a little generous so legitimate edge locations are not rejected;
#' they are coarse guard boxes, not precise boundaries. For a tighter or
#' non-state region, pass your own named vector, or derive one from an `sf`
#' layer with [bbox_from_sf()].
#'
#' @param region Two-letter US state abbreviation (or `"DC"`), case-insensitive.
#'   Defaults to `"NJ"`.
#'
#' @return A named numeric vector with elements `lat_min`, `lat_max`,
#'   `lon_min`, `lon_max`.
#' @export
#' @examples
#' region_bbox("NJ")
#' region_bbox("CA")
region_bbox <- function(region = "NJ") {
  region <- toupper(region)
  bb <- .STATE_BBOX[[region]]
  if (is.null(bb)) {
    stop("No bounding-box preset for `", region, "`. Use a two-letter US state ",
         "code (or \"DC\"), pass a custom `bbox`, or derive one with ",
         "`bbox_from_sf()`.", call. = FALSE)
  }
  bb
}

# Generous, coarse guard boxes (lat_min, lat_max, lon_min, lon_max) per state.
# NJ is kept at its original production values. Aleutian islands that cross the
# antimeridian are outside AK's box by design.
.STATE_BBOX <- list(
  AL = c(lat_min = 30.1, lat_max = 35.1, lon_min = -88.5, lon_max = -84.9),
  AK = c(lat_min = 51.0, lat_max = 71.6, lon_min = -170.0, lon_max = -129.9),
  AZ = c(lat_min = 31.3, lat_max = 37.1, lon_min = -114.9, lon_max = -109.0),
  AR = c(lat_min = 33.0, lat_max = 36.6, lon_min = -94.7, lon_max = -89.6),
  CA = c(lat_min = 32.5, lat_max = 42.1, lon_min = -124.5, lon_max = -114.1),
  CO = c(lat_min = 36.9, lat_max = 41.1, lon_min = -109.1, lon_max = -102.0),
  CT = c(lat_min = 40.9, lat_max = 42.1, lon_min = -73.8, lon_max = -71.7),
  DE = c(lat_min = 38.4, lat_max = 39.9, lon_min = -75.8, lon_max = -75.0),
  DC = c(lat_min = 38.79, lat_max = 39.00, lon_min = -77.13, lon_max = -76.90),
  FL = c(lat_min = 24.4, lat_max = 31.1, lon_min = -87.7, lon_max = -79.9),
  GA = c(lat_min = 30.3, lat_max = 35.1, lon_min = -85.7, lon_max = -80.8),
  HI = c(lat_min = 18.9, lat_max = 22.3, lon_min = -160.3, lon_max = -154.7),
  ID = c(lat_min = 41.9, lat_max = 49.1, lon_min = -117.3, lon_max = -110.9),
  IL = c(lat_min = 36.9, lat_max = 42.6, lon_min = -91.6, lon_max = -87.4),
  IN = c(lat_min = 37.7, lat_max = 41.8, lon_min = -88.1, lon_max = -84.7),
  IA = c(lat_min = 40.3, lat_max = 43.6, lon_min = -96.7, lon_max = -90.1),
  KS = c(lat_min = 36.9, lat_max = 40.1, lon_min = -102.1, lon_max = -94.5),
  KY = c(lat_min = 36.4, lat_max = 39.2, lon_min = -89.7, lon_max = -81.9),
  LA = c(lat_min = 28.9, lat_max = 33.1, lon_min = -94.1, lon_max = -88.8),
  ME = c(lat_min = 42.9, lat_max = 47.5, lon_min = -71.1, lon_max = -66.9),
  MD = c(lat_min = 37.8, lat_max = 39.8, lon_min = -79.5, lon_max = -75.0),
  MA = c(lat_min = 41.2, lat_max = 42.9, lon_min = -73.6, lon_max = -69.9),
  MI = c(lat_min = 41.6, lat_max = 48.3, lon_min = -90.5, lon_max = -82.3),
  MN = c(lat_min = 43.4, lat_max = 49.5, lon_min = -97.3, lon_max = -89.4),
  MS = c(lat_min = 30.1, lat_max = 35.1, lon_min = -91.7, lon_max = -88.0),
  MO = c(lat_min = 35.9, lat_max = 40.7, lon_min = -95.8, lon_max = -89.0),
  MT = c(lat_min = 44.3, lat_max = 49.1, lon_min = -116.1, lon_max = -103.9),
  NE = c(lat_min = 39.9, lat_max = 43.1, lon_min = -104.1, lon_max = -95.2),
  NV = c(lat_min = 34.9, lat_max = 42.1, lon_min = -120.1, lon_max = -113.9),
  NH = c(lat_min = 42.6, lat_max = 45.4, lon_min = -72.6, lon_max = -70.6),
  NJ = c(lat_min = 38.80, lat_max = 41.40, lon_min = -75.70, lon_max = -73.80),
  NM = c(lat_min = 31.2, lat_max = 37.1, lon_min = -109.1, lon_max = -102.9),
  NY = c(lat_min = 40.4, lat_max = 45.1, lon_min = -79.9, lon_max = -71.8),
  NC = c(lat_min = 33.7, lat_max = 36.7, lon_min = -84.4, lon_max = -75.4),
  ND = c(lat_min = 45.9, lat_max = 49.1, lon_min = -104.1, lon_max = -96.5),
  OH = c(lat_min = 38.3, lat_max = 42.4, lon_min = -84.9, lon_max = -80.5),
  OK = c(lat_min = 33.6, lat_max = 37.1, lon_min = -103.1, lon_max = -94.4),
  OR = c(lat_min = 41.9, lat_max = 46.4, lon_min = -124.6, lon_max = -116.4),
  PA = c(lat_min = 39.7, lat_max = 42.4, lon_min = -80.6, lon_max = -74.6),
  RI = c(lat_min = 41.1, lat_max = 42.1, lon_min = -71.9, lon_max = -71.1),
  SC = c(lat_min = 32.0, lat_max = 35.3, lon_min = -83.4, lon_max = -78.4),
  SD = c(lat_min = 42.4, lat_max = 46.0, lon_min = -104.1, lon_max = -96.4),
  TN = c(lat_min = 34.9, lat_max = 36.8, lon_min = -90.4, lon_max = -81.6),
  TX = c(lat_min = 25.8, lat_max = 36.6, lon_min = -106.7, lon_max = -93.4),
  UT = c(lat_min = 36.9, lat_max = 42.1, lon_min = -114.1, lon_max = -108.9),
  VT = c(lat_min = 42.7, lat_max = 45.1, lon_min = -73.5, lon_max = -71.4),
  VA = c(lat_min = 36.5, lat_max = 39.5, lon_min = -83.7, lon_max = -75.1),
  WA = c(lat_min = 45.5, lat_max = 49.1, lon_min = -124.9, lon_max = -116.9),
  WV = c(lat_min = 37.1, lat_max = 40.7, lon_min = -82.7, lon_max = -77.7),
  WI = c(lat_min = 42.4, lat_max = 47.4, lon_min = -92.9, lon_max = -86.8),
  WY = c(lat_min = 40.9, lat_max = 45.1, lon_min = -111.1, lon_max = -104.0)
)

#' Is a coordinate inside a bounding box?
#'
#' @param lat Numeric vector of latitudes.
#' @param lon Numeric vector of longitudes.
#' @param bbox A named bounding box as returned by [region_bbox()] or supplied
#'   by the caller.
#'
#' @return A logical vector the same length as `lat`/`lon`. `NA` coordinates
#'   return `FALSE`.
#' @export
#' @examples
#' in_bbox(40.2, -74.5, region_bbox("NJ"))
#' in_bbox(40.5, -104.9, region_bbox("NJ")) # a Colorado false-match
in_bbox <- function(lat, lon, bbox) {
  !is.na(lat) & !is.na(lon) &
    lat >= bbox[["lat_min"]] & lat <= bbox[["lat_max"]] &
    lon >= bbox[["lon_min"]] & lon <= bbox[["lon_max"]]
}

#' Build a geocoding bounding box from an sf layer
#'
#' Converts any point, line, or polygon `sf` layer to WGS84 and returns a named
#' latitude/longitude bounding box suitable for [geocode_records()],
#' [geocode_arcgis()], [geocode_by_name()], and [validate_geocodes()]. This is
#' the safest way to keep multi-state geocoding and local geography joins aligned:
#' build or load the geography layer first, then derive the bbox from it.
#'
#' @param geography_shapes An `sf` object.
#' @param buffer Numeric buffer in decimal degrees added to each side of the
#'   bounding box. Defaults to `0.05` to avoid rejecting edge locations.
#'
#' @return A named numeric vector with `lat_min`, `lat_max`, `lon_min`, and
#'   `lon_max`.
#' @export
#' @examples
#' if (interactive()) {
#' areas <- build_local_geography("PA")
#' bbox <- bbox_from_sf(areas)
#' }
bbox_from_sf <- function(geography_shapes, buffer = 0.05) {
  if (!inherits(geography_shapes, "sf") && !inherits(geography_shapes, "sfc")) {
    stop("`geography_shapes` must be an sf or sfc object.", call. = FALSE)
  }
  shapes <- geography_shapes
  if (!is.na(sf::st_crs(shapes))) {
    shapes <- sf::st_transform(shapes, 4326)
  }
  bb <- sf::st_bbox(shapes)
  c(
    lat_min = unname(bb[["ymin"]] - buffer),
    lat_max = unname(bb[["ymax"]] + buffer),
    lon_min = unname(bb[["xmin"]] - buffer),
    lon_max = unname(bb[["xmax"]] + buffer)
  )
}

# Rows that began with a usable address may be retried by looser geocoders after
# a prior tier failed validation. Rows flagged for manual review before any
# geocoder call, such as PO boxes or placeholders, stay out of external services.
# A `missing_zip` flag is informational, not blocking: an address + city row is
# still geocodable, so it remains retryable by the ArcGIS and name tiers.
.retryable_for_geocoding <- function(data) {
  status_retryable <- data$review_status %in%
    c("ready_for_geocoding", "needs_manual_review")

  if ("bad_address_flag" %in% names(data)) {
    blocking <- !is.na(data$bad_address_flag) &
      data$bad_address_flag != "missing_zip"
    status_retryable & !blocking
  } else {
    status_retryable
  }
}

# ArcGIS supports spatial filtering via `searchExtent`. Passing the region bbox
# into the request helps the service return plausible candidates first instead
# of making locatr reject a wrong-state top hit after the fact.
.region_geocoder_dots <- function(method, bbox, dots) {
  if (!identical(tolower(method), "arcgis")) {
    return(dots)
  }

  region_query <- list(
    searchExtent = paste(
      bbox[["lon_min"]], bbox[["lat_min"]],
      bbox[["lon_max"]], bbox[["lat_max"]],
      sep = ","
    ),
    countryCode = "USA"
  )
  dots$custom_query <- utils::modifyList(region_query, dots$custom_query %||% list())
  dots
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ---- match-confidence.R ----
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

# ---- provenance.R ----
# Run provenance (roadmap #2, phase 3): a per-run manifest attached to
# geocode_records() output, plus per-row placed_at / cache_status. The stamper
# runs after the cascade and never touches the tiers; it reconstructs each row's
# cache key with the shared builders in cache.R, so classification cannot drift
# from what the tiers actually stored.

#' Read the provenance manifest from a geocoding run
#'
#' [geocode_records()] attaches a run manifest as an attribute of its output.
#' This returns it: a run id and UTC timestamp, the locatr / tidygeocoder /
#' cache-schema versions, the tiers run, whether a reference table was used, the
#' cache path, per-`review_status` counts, and cache activity
#' (`cache_hits` / `cache_misses` / `cache_writes`). Read it directly on the
#' `geocode_records()` result, since later data-frame operations may drop the
#' attribute.
#'
#' @param data Output of [geocode_records()].
#'
#' @return A `locatr_provenance` object (a named list) describing the run.
#' @seealso [geocode_records()], [locatr_cache()]
#' @export
geocode_provenance <- function(data) {
  manifest <- attr(data, "locatr_run", exact = TRUE)
  if (is.null(manifest)) {
    stop("No locatr run manifest found. It is attached by `geocode_records()` ",
         "and may be dropped by later data-frame operations.", call. = FALSE)
  }
  manifest
}

#' @rdname geocode_provenance
#' @param x A `locatr_provenance` object.
#' @param ... Ignored.
#' @export
print.locatr_provenance <- function(x, ...) {
  cat("<locatr run>", x$run_id, "\n")
  cat("  at:      ", x$run_at, "\n")
  cat("  versions: locatr", x$locatr_version, "| tidygeocoder",
      x$tidygeocoder_version, "| cache schema", x$cache_schema_version, "\n")
  cat("  tiers:   ", paste(x$tiers, collapse = ", "),
      "| reference used:", x$reference_used, "\n")
  cat("  records: ", x$n_records, "| cache:",
      if (is.na(x$cache_path)) "none" else x$cache_path, "\n")
  if (!is.na(x$cache_hits)) {
    cat("  cache:   ", x$cache_hits, "hit(s),", x$cache_misses, "miss(es),",
        x$cache_writes, "write(s)\n")
  }
  invisible(x)
}

# Build the run manifest. `cache_before` is the c(hits, misses, writes) snapshot
# taken before the cascade so the reported counts cover only this run.
.locatr_run_manifest <- function(out, tiers, reference, boundary, bbox, cache,
                                 run_started, cache_before) {
  after <- if (!is.null(cache)) {
    c(cache$hits, cache$misses, cache$writes)
  } else {
    rep(NA_integer_, 3L)
  }
  status <- if ("review_status" %in% names(out)) {
    as.list(table(out$review_status, useNA = "no"))
  } else {
    list()
  }
  manifest <- list(
    run_id = substr(rlang::hash(list(run_started, nrow(out))), 1, 12),
    run_at = run_started,
    locatr_version = as.character(tryCatch(utils::packageVersion("locatr"), error = function(e) package_version("0.1.0"))),
    tidygeocoder_version = tryCatch(
      as.character(utils::packageVersion("tidygeocoder")),
      error = function(e) NA_character_
    ),
    cache_schema_version = .CACHE_SCHEMA_VERSION,
    tiers = tiers,
    services = .locatr_services(tiers),
    bbox = bbox,
    boundary = if (is.null(boundary)) NA_character_ else class(boundary)[1],
    reference_used = !is.null(reference),
    cache_path = if (is.null(cache)) {
      NA_character_
    } else if (is.null(cache$path)) {
      "memory"
    } else {
      cache$path
    },
    n_records = nrow(out),
    status_counts = status,
    cache_hits = after[1] - cache_before[1],
    cache_misses = after[2] - cache_before[2],
    cache_writes = after[3] - cache_before[3]
  )
  class(manifest) <- "locatr_provenance"
  manifest
}

.locatr_services <- function(tiers) {
  services <- list()
  if ("census" %in% tiers) {
    services$census <- "tidygeocoder::geocode(method = 'census')"
  }
  if ("arcgis" %in% tiers) {
    services$arcgis <- "tidygeocoder::geocode(method = 'arcgis')"
  }
  if ("name" %in% tiers) {
    services$arcgis_byname <- "tidygeocoder::geocode(method = 'arcgis', full_results = TRUE)"
  }
  services
}

# Add per-row `placed_at` and `cache_status` to a finished frame. Reference and
# manual placements are provenance events, not new geocoding; a coordinate that
# a network tier placed is "cached" when the cache already held it before this
# run (cached_at < run_started), else "fresh".
.stamp_placement <- function(data, cache, run_started, bbox) {
  n <- nrow(data)
  pass <- if ("geocode_pass" %in% names(data)) {
    as.character(data$geocode_pass)
  } else {
    rep(NA_character_, n)
  }
  lat <- if ("latitude" %in% names(data)) data$latitude else rep(NA_real_, n)
  lon <- if ("longitude" %in% names(data)) data$longitude else rep(NA_real_, n)
  unplaced <- is.na(lat) | is.na(lon)
  starts <- function(pre) !is.na(pass) & startsWith(pass, pre)

  cache_status <- rep(NA_character_, n)
  placed_at <- rep(NA_character_, n)

  is0 <- starts("pass_0") & !unplaced
  is3 <- starts("pass_3") & !unplaced
  net <- !unplaced & !is0 & !is3

  cache_status[unplaced] <- "unplaced"
  cache_status[is0] <- "reference"
  placed_at[is0] <- .row_timestamp(
    data, c("reference_at", "ref_at", "reference_cached_at", "ref_cached_at",
            "reference_updated_at", "ref_updated_at", "cached_at"), is0
  )
  cache_status[is3] <- "manual"
  placed_at[is3] <- .row_timestamp(
    data, c("manual_at", "manual_applied_at", "override_at",
            "manual_updated_at"), is3
  )

  for (i in which(net)) {
    status_i <- "fresh"
    placed_i <- run_started
    if (!is.null(cache)) {
      spec <- .stamp_spec_for(pass[i], data[i, , drop = FALSE], bbox)
      if (!is.null(spec)) {
        rows <- .cache_peek(cache, .cache_key(spec$method, spec$query,
                                              spec$params))
        if (!is.null(rows)) {
          keep <- rows[rows$result_rank > 0L & rows$status != "no_match", ,
                       drop = FALSE]
          if (nrow(keep) >= 1L) {
            cached_at <- keep$cached_at[1]
            placed_i <- cached_at
            if (!is.na(cached_at) && cached_at < run_started) {
              status_i <- "cached"
            }
          }
        }
      }
    }
    cache_status[i] <- status_i
    placed_at[i] <- placed_i
  }

  data$placed_at <- placed_at
  data$cache_status <- cache_status
  data
}

.row_timestamp <- function(data, cols, idx) {
  out <- rep(NA_character_, nrow(data))
  for (col in cols) {
    if (!col %in% names(data)) {
      next
    }
    val <- as.character(data[[col]])
    take <- idx & is.na(out) & !is.na(val) & nzchar(val)
    out[take] <- val[take]
  }
  out[idx]
}

.stamp_spec_for <- function(pass, row, bbox) {
  if (startsWith(pass, "pass_1")) {
    list(method = "census_structured", query = .census_query_vec(row),
         params = .census_params())
  } else if (startsWith(pass, "pass_2")) {
    list(method = "arcgis_oneline", query = .arcgis_query_vec(row),
         params = .arcgis_params("arcgis", bbox))
  } else if (startsWith(pass, "pass_4")) {
    list(method = "arcgis_byname", query = .name_query_vec(row),
         params = .name_params("arcgis", bbox, list(full_results = TRUE)))
  } else {
    NULL
  }
}

# ---- review-overrides.R ----
#' Export only the records that still need a human
#'
#' Writes a tidy review CSV of rows whose `review_status` is
#' `"needs_manual_review"`, with blank `manual_*` columns for a reviewer to fill
#' in. Feed the completed file back through [apply_manual_overrides()].
#'
#' @param data A data frame carrying the audit columns.
#' @param path Output CSV path.
#'
#' @return Invisibly, the review tibble that was written.
#' @export
write_geocode_review <- function(data, path) {
  review <- data %>%
    dplyr::filter(.data$review_status == "needs_manual_review") %>%
    dplyr::transmute(
      record_id             = .data$record_id,
      record_name           = .data$record_name,
      full_address_clean    = .data$full_address_clean,
      latitude              = .data$latitude,
      longitude             = .data$longitude,
      location_county       = .pull_if(data, "location_county"),
      location_locality     = .pull_if(data, "location_locality"),
      match_status          = .data$match_status,
      validation_status     = .pull_if(data, "validation_status"),
      bad_address_flag      = .data$bad_address_flag,
      manual_latitude       = NA_real_,
      manual_longitude      = NA_real_,
      manual_county         = NA_character_,
      manual_locality       = NA_character_,
      manual_note           = NA_character_
    )

  readr::write_csv(review, path)
  invisible(review)
}

#' Apply completed manual overrides
#'
#' Joins a reviewer-completed override file (same layout
#' [write_geocode_review()] produced) and coalesces verified coordinates and
#' geography over the automated values. Overrides are themselves bbox-checked so
#' a typo can't drop a point in the ocean.
#'
#' @param data A data frame with `record_id` and the audit columns.
#' @param override_file Path to the completed override CSV.
#' @param bbox Bounding box for validating manual coordinates; see
#'   [region_bbox()].
#'
#' @return `data` with overrides applied and `manual_override_used` set.
#' @export
apply_manual_overrides <- function(data, override_file, bbox = region_bbox("NJ")) {
  if (!file.exists(override_file)) {
    warning("Override file not found: ", override_file, " - returning data unchanged.")
    return(dplyr::mutate(data, manual_override_used = FALSE))
  }

  overrides <- readr::read_csv(override_file, show_col_types = FALSE) %>%
    dplyr::mutate(
      record_id        = as.character(.data$record_id),
      manual_latitude  = as.numeric(.data$manual_latitude),
      manual_longitude = as.numeric(.data$manual_longitude),
      manual_ok        = in_bbox(.data$manual_latitude, .data$manual_longitude, bbox)
    ) %>%
    dplyr::filter(.data$manual_ok) %>%
    dplyr::select("record_id", "manual_latitude", "manual_longitude",
                  dplyr::any_of(c("manual_county", "manual_locality", "manual_note")))

  data %>%
    dplyr::left_join(overrides, by = "record_id") %>%
    dplyr::mutate(
      manual_override_used = !is.na(.data$manual_latitude) & !is.na(.data$manual_longitude),
      latitude  = dplyr::coalesce(.data$manual_latitude, .data$latitude),
      longitude = dplyr::coalesce(.data$manual_longitude, .data$longitude),
      geocode_method = dplyr::if_else(.data$manual_override_used, "manual", .data$geocode_method),
      geocode_pass   = dplyr::if_else(.data$manual_override_used, "pass_3_manual", .data$geocode_pass),
      match_status   = dplyr::if_else(.data$manual_override_used, "matched", .data$match_status),
      review_status  = dplyr::if_else(.data$manual_override_used,
                                      "manual_override_applied", .data$review_status)
    )
}

# Pull a column if present, else return NA of matching length.
.pull_if <- function(data, col) {
  if (col %in% names(data)) data[[col]] else NA
}

.pull_first <- function(data, cols) {
  for (col in cols) {
    if (col %in% names(data)) {
      return(data[[col]])
    }
  }
  NA
}

.pull_logical <- function(data, col, default = NA) {
  if (col %in% names(data)) {
    out <- data[[col]]
    out[is.na(out)] <- default
    out
  } else {
    rep(default, nrow(data))
  }
}

# ---- validate-geocodes.R ----
#' Validate geocoded coordinates against a region
#'
#' Rejects suspicious coordinates before they reach a dashboard. By default this
#' is a fast bounding-box check; supply `boundary` (an `sf` polygon of the
#' service area) for a precise point-in-polygon test.
#'
#' @param data A geocoded data frame with `latitude`/`longitude`.
#' @param boundary Optional `sf` polygon. When given, validation uses
#'   point-in-polygon instead of the bounding box.
#' @param bbox Bounding box for the fast path; see [region_bbox()].
#'
#' @return `data` with `validation_status` and an updated `review_status`
#'   (anything failing validation or unmatched becomes `needs_manual_review`).
#' @export
validate_geocodes <- function(data, boundary = NULL, bbox = region_bbox("NJ")) {
  if (!is.null(boundary)) {
    in_region <- .point_in_polygon(data, boundary)
  } else {
    in_region <- in_bbox(data$latitude, data$longitude, bbox)
  }

  data %>%
    dplyr::mutate(
      validation_status = dplyr::case_when(
        is.na(.data$latitude) | is.na(.data$longitude) ~ "missing_coordinates",
        in_region ~ "coordinate_ok",
        TRUE ~ "outside_region"
      ),
      review_status = dplyr::case_when(
        .data$validation_status != "coordinate_ok" ~ "needs_manual_review",
        !is.na(.data$match_status) & .data$match_status == "no_match" ~ "needs_manual_review",
        TRUE ~ .data$review_status
      )
    )
}

# Point-in-polygon helper. Returns a logical vector aligned to `data` rows.
.point_in_polygon <- function(data, boundary) {
  has_xy <- !is.na(data$latitude) & !is.na(data$longitude)
  result <- rep(FALSE, nrow(data))
  if (!any(has_xy)) return(result)

  pts <- sf::st_as_sf(
    data[has_xy, , drop = FALSE],
    coords = c("longitude", "latitude"),
    crs = 4326, remove = FALSE
  )
  boundary <- sf::st_transform(boundary, 4326)
  hits <- lengths(sf::st_intersects(pts, boundary)) > 0
  result[has_xy] <- hits
  result
}

