# Region bounding box

Returns an approximate latitude/longitude bounding box for a named
region, used as a fast sanity check on geocoded coordinates. Presets are
deliberately a little generous so legitimate edge locations are not
rejected.

## Usage

``` r
region_bbox(region = "NJ")
```

## Arguments

- region:

  Region preset. Currently `"NJ"` is included for the package's first
  production workflow. For other regions, pass a custom named vector
  with `lat_min`, `lat_max`, `lon_min`, and `lon_max`.

## Value

A named numeric vector with elements `lat_min`, `lat_max`, `lon_min`,
`lon_max`.

## Examples

``` r
region_bbox("NJ")
#> lat_min lat_max lon_min lon_max 
#>    38.8    41.4   -75.7   -73.8 
```
