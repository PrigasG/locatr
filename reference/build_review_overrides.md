# Build a manual-override table from review decisions

Turns a set of per-record review decisions (accept / reject / relocate)
into a completed override table with the same layout
[`write_geocode_review()`](https://prigasg.github.io/locatr/reference/write_geocode_review.md)
produces, so it can be written to CSV and fed straight to
[`apply_manual_overrides()`](https://prigasg.github.io/locatr/reference/apply_manual_overrides.md).
This is the testable core behind the map-based review app
([`run_locatr_review_app()`](https://prigasg.github.io/locatr/reference/run_locatr_review_app.md));
it holds no reactive or UI code, so it can also be used headless.

## Usage

``` r
build_review_overrides(data, decisions)
```

## Arguments

- data:

  A geocoded data frame with at least `record_id`, and ideally
  `latitude`/`longitude`/`record_name`/`full_address_clean` for context.

- decisions:

  A data frame with `record_id`, `decision` (one of `"accept"`,
  `"reject"`, `"relocate"`), and - for relocations - `manual_latitude` /
  `manual_longitude`.

## Value

A tibble with `record_id`, `record_name`, `full_address_clean`, the
current `latitude`/`longitude`, the reviewer's `manual_latitude` /
`manual_longitude`, and `manual_note`. Compatible with
[`apply_manual_overrides()`](https://prigasg.github.io/locatr/reference/apply_manual_overrides.md).

## Details

Decision semantics:

- `accept` - confirm the automated coordinate: `manual_latitude` /
  `manual_longitude` are set to the record's current coordinate.

- `relocate` - use the reviewer's coordinate: `manual_latitude` /
  `manual_longitude` come from the `decisions` table.

- `reject` - drop the coordinate: `manual_*` are left `NA` (so
  [`apply_manual_overrides()`](https://prigasg.github.io/locatr/reference/apply_manual_overrides.md)
  does not place it) and the note records the rejection.

## See also

[`run_locatr_review_app()`](https://prigasg.github.io/locatr/reference/run_locatr_review_app.md),
[`apply_manual_overrides()`](https://prigasg.github.io/locatr/reference/apply_manual_overrides.md),
[`write_geocode_review()`](https://prigasg.github.io/locatr/reference/write_geocode_review.md)

## Examples

``` r
geocoded <- data.frame(
  record_id = c("a", "b", "c"),
  record_name = c("A", "B", "C"),
  full_address_clean = c("1 A St", "2 B St", "3 C St"),
  latitude = c(40.1, 40.2, NA),
  longitude = c(-74.1, -74.2, NA)
)
decisions <- data.frame(
  record_id = c("a", "b", "c"),
  decision = c("accept", "relocate", "reject"),
  manual_latitude = c(NA, 40.25, NA),
  manual_longitude = c(NA, -74.25, NA)
)
build_review_overrides(geocoded, decisions)
#> # A tibble: 3 × 8
#>   record_id record_name full_address_clean latitude longitude manual_latitude
#>   <chr>     <chr>       <chr>                 <dbl>     <dbl>           <dbl>
#> 1 a         A           1 A St                 40.1     -74.1            40.1
#> 2 b         B           2 B St                 40.2     -74.2            40.2
#> 3 c         C           3 C St                 NA        NA              NA  
#> # ℹ 2 more variables: manual_longitude <dbl>, manual_note <chr>
```
