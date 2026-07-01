# Compare two geocoding runs

Finds records whose coordinates, review status, geocode pass, or
geography assignment changed between two runs. This is useful after
changing thresholds, adding a reference file, or swapping geography
sources.

## Usage

``` r
compare_geocode_runs(
  old,
  new,
  by = "record_id",
  coordinate_tolerance = 1e-06,
  changed_only = TRUE
)
```

## Arguments

- old:

  Previous locatr output.

- new:

  New locatr output.

- by:

  Key column used to match rows. Defaults to `record_id`.

- coordinate_tolerance:

  Numeric tolerance for latitude/longitude changes.

- changed_only:

  If `TRUE` (default), return only rows with at least one tracked
  change.

## Value

A tibble with old/new values and change flags.

## Examples

``` r
old <- tibble::tibble(record_id = "a", latitude = 40, longitude = -75)
new <- tibble::tibble(record_id = "a", latitude = 41, longitude = -75)
compare_geocode_runs(old, new)
#> # A tibble: 1 × 30
#>   record_id latitude_old longitude_old review_status_old geocode_pass_old
#>   <chr>            <dbl>         <dbl> <lgl>             <lgl>           
#> 1 a                   40           -75 NA                NA              
#> # ℹ 25 more variables: match_status_old <lgl>, location_county_old <lgl>,
#> #   location_locality_old <lgl>, County_old <lgl>, Municipality_old <lgl>,
#> #   muni_join_key_old <lgl>, municipality_geoid_old <lgl>, latitude_new <dbl>,
#> #   longitude_new <dbl>, review_status_new <lgl>, geocode_pass_new <lgl>,
#> #   match_status_new <lgl>, location_county_new <lgl>,
#> #   location_locality_new <lgl>, County_new <lgl>, Municipality_new <lgl>,
#> #   muni_join_key_new <lgl>, municipality_geoid_new <lgl>, …
```
