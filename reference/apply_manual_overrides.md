# Apply completed manual overrides

Joins a reviewer-completed override file (same layout
[`write_geocode_review()`](https://prigasg.github.io/locatr/reference/write_geocode_review.md)
produced) and coalesces verified coordinates and geography over the
automated values. Overrides are themselves bbox-checked so a typo can't
drop a point in the ocean.

## Usage

``` r
apply_manual_overrides(data, override_file, bbox = region_bbox("NJ"))
```

## Arguments

- data:

  A data frame with `record_id` and the audit columns.

- override_file:

  Path to the completed override CSV.

- bbox:

  Bounding box for validating manual coordinates; see
  [`region_bbox()`](https://prigasg.github.io/locatr/reference/region_bbox.md).

## Value

`data` with overrides applied and `manual_override_used` set.
