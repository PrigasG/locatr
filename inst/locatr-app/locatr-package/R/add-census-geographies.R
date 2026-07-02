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
