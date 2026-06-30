# Validate geocoded coordinates against a region

Rejects suspicious coordinates before they reach a dashboard. By default
this is a fast bounding-box check; supply `boundary` (an `sf` polygon of
the service area) for a precise point-in-polygon test.

## Usage

``` r
validate_geocodes(data, boundary = NULL, bbox = region_bbox("NJ"))
```

## Arguments

- data:

  A geocoded data frame with `latitude`/`longitude`.

- boundary:

  Optional `sf` polygon. When given, validation uses point-in-polygon
  instead of the bounding box.

- bbox:

  Bounding box for the fast path; see
  [`region_bbox()`](https://prigasg.github.io/locatr/reference/region_bbox.md).

## Value

`data` with `validation_status` and an updated `review_status` (anything
failing validation or unmatched becomes `needs_manual_review`).
