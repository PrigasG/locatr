# Launch the locatr map-based review app

A standalone Shiny app that takes records from upload through geocoding
to map-based review. Upload a file, map the ID and address columns, then
either geocode the addresses (clean, flag, cascade, and attach
county/municipality) or use existing coordinates. The flagged records
appear on a map to accept, reject, or relocate; the app builds a
completed override table with
[`build_review_overrides()`](https://prigasg.github.io/locatr/reference/build_review_overrides.md)
that you download as CSV and feed to
[`apply_manual_overrides()`](https://prigasg.github.io/locatr/reference/apply_manual_overrides.md).
Geocoding calls external services and needs network access.

## Usage

``` r
run_locatr_review_app(...)
```

## Arguments

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.
  `port`, `host`, `launch.browser`).

## Value

Called for its side effect of starting the app; does not return.

## Details

The app depends on packages listed only under `Suggests`, so they are
not installed automatically. If any are missing, this function stops
with the install command you need.

## See also

[`build_review_overrides()`](https://prigasg.github.io/locatr/reference/build_review_overrides.md),
[`apply_manual_overrides()`](https://prigasg.github.io/locatr/reference/apply_manual_overrides.md)

## Examples

``` r
if (FALSE) { # \dontrun{
run_locatr_review_app()
} # }
```
