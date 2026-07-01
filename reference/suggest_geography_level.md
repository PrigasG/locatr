# Suggest a Census geography level for a state

Gives a practical starting point for
[`build_local_geography()`](https://prigasg.github.io/locatr/reference/build_local_geography.md)
and
[`add_county_muni()`](https://prigasg.github.io/locatr/reference/add_county_muni.md).
It is a recommendation, not a legal definition of local government. For
production municipal joins, an official state/local boundary layer is
still the strongest source.

## Usage

``` r
suggest_geography_level(state)
```

## Arguments

- state:

  Two-letter state abbreviation.

## Value

A one-row tibble with the recommended Census geography and note.

## Examples

``` r
suggest_geography_level("NJ")
#> # A tibble: 1 × 4
#>   state recommended_geography function_call                                note 
#>   <chr> <chr>                 <chr>                                        <chr>
#> 1 NJ    county_subdivision    "build_local_geography(state = \"NJ\", geog… Coun…
suggest_geography_level("CA")
#> # A tibble: 1 × 4
#>   state recommended_geography function_call                                note 
#>   <chr> <chr>                 <chr>                                        <chr>
#> 1 CA    place                 "build_local_geography(state = \"CA\", geog… Cens…
```
