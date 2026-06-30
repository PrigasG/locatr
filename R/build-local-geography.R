#' Build a local geography layer from Census TIGER/Line boundaries
#'
#' Downloads an authoritative Census boundary layer for a state with the
#' \pkg{tigris} package and standardises it into the two-column schema
#' [add_local_geography()] expects: `location_county` (always from counties) and
#' `location_locality` (from the requested `geography`). This makes "locality" a
#' configurable concept, because what counts as a municipality is not consistent
#' across states.
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
#' @return An `sf` polygon layer in WGS84 (EPSG:4326) with `location_county`,
#'   `location_locality`, and `geometry`, ready for
#'   `add_local_geography(geography_shapes = ...)`.
#' @export
#' @examples
#' \dontrun{
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
        location_locality = as.character(.data$NAME)
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
        location_county = as.character(.data$NAME)
      )
    locality_expr <- if (geography == "tract" && "GEOID" %in% names(layer)) {
      rlang::expr(as.character(.data$GEOID))
    } else {
      rlang::expr(as.character(.data$NAME))
    }
    areas <- layer %>%
      dplyr::left_join(county_names, by = c("STATEFP", "COUNTYFP")) %>%
      dplyr::transmute(
        location_county   = .data$location_county,
        location_locality = !!locality_expr
      )
  } else {
    # Places carry no county key and may straddle counties, so assign the
    # county each place overlaps most. Use an equal-area CRS and geometry-only
    # intersections so sf does not warn about duplicated polygon attributes.
    counties_for_join <- counties %>%
      dplyr::transmute(location_county = as.character(.data$NAME)) %>%
      sf::st_make_valid() %>%
      sf::st_transform(5070)
    areas <- layer %>%
      dplyr::transmute(location_locality = as.character(.data$NAME)) %>%
      sf::st_make_valid() %>%
      sf::st_transform(5070)
    areas$location_county <- .largest_overlap_county(areas, counties_for_join)
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
  candidates <- sf::st_intersects(areas, counties)
  county_names <- counties$location_county

  vapply(seq_along(candidates), function(i) {
    county_index <- candidates[[i]]
    if (length(county_index) == 0) {
      return(NA_character_)
    }

    overlaps <- sf::st_intersection(
      sf::st_geometry(areas[i, ]),
      sf::st_geometry(counties[county_index, ])
    )
    if (length(overlaps) == 0) {
      return(NA_character_)
    }

    overlap_area <- as.numeric(sf::st_area(overlaps))
    county_names[county_index[which.max(overlap_area)]]
  }, character(1))
}
