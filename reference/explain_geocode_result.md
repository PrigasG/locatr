# Explain how geocoded records were handled

Turns the main audit columns into short, reviewer-friendly sentences.
This is useful when checking a few records by hand or when adding
plain-English notes to a review export.

## Usage

``` r
explain_geocode_result(data, row = NULL)
```

## Arguments

- data:

  A locatr output data frame.

- row:

  Optional row selector. Use `NULL` for all rows (default), a numeric
  row index, or a `record_id` value.

## Value

A character vector of explanations.

## Examples

``` r
x <- tibble::tibble(
  record_id = "a",
  geocode_pass = "pass_4_name_lookup",
  match_status = "matched_low_confidence",
  validation_status = "coordinate_ok",
  review_status = "needs_manual_review",
  nm_score = 87,
  nm_addr_type = "POI"
)
explain_geocode_result(x)
#> [1] "Record a: Placed by the ArcGIS name lookup. Match status: matched_low_confidence. Name match score/type: 87 / POI. Coordinate validation passed. It needs manual review."
```
