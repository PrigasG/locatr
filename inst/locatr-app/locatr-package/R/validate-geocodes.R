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
