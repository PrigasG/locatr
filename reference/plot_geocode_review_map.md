# Plot geocoded records for review

Creates a small interactive leaflet map, colored by an audit column. The
helper is intentionally lightweight: it is for quick review, not for
producing a full dashboard.

## Usage

``` r
plot_geocode_review_map(
  data,
  color_by = c("review_status", "geocode_pass", "match_status")
)
```

## Arguments

- data:

  A locatr output data frame with `latitude` and `longitude`.

- color_by:

  Column used to color points. Defaults to `review_status`.

## Value

A `leaflet` map.

## Examples

``` r
if (interactive()) {
  plot_geocode_review_map(geocoded)
}
```
