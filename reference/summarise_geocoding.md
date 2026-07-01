# Summarise geocoding quality

Counts the main outcomes in a locatr result so you can quickly judge
whether a run is ready for review, export, or threshold tuning.

## Usage

``` r
summarise_geocoding(data)
```

## Arguments

- data:

  A locatr output data frame.

## Value

A one-row tibble with counts and rates.

## Examples

``` r
x <- tibble::tibble(
  latitude = c(40, NA),
  longitude = c(-75, NA),
  match_status = c("matched", "unmatched"),
  review_status = c("auto_accepted", "needs_manual_review"),
  geocode_pass = c("pass_1_census_structured", NA_character_)
)
summarise_geocoding(x)
#> # A tibble: 1 × 12
#>   n_records matched matched_pct missing_coordinates auto_accepted
#>       <int>   <int>       <dbl>               <int>         <int>
#> 1         2       1          50                   1             1
#> # ℹ 7 more variables: needs_manual_review <int>, rejected <int>,
#> #   manual_override_applied <int>, outside_region <int>, name_lookup <int>,
#> #   low_confidence_name <int>, missing_geography <int>
```
