# Is a coordinate inside a bounding box?

Is a coordinate inside a bounding box?

## Usage

``` r
in_bbox(lat, lon, bbox)
```

## Arguments

- lat:

  Numeric vector of latitudes.

- lon:

  Numeric vector of longitudes.

- bbox:

  A named bounding box as returned by
  [`region_bbox()`](https://prigasg.github.io/locatr/reference/region_bbox.md)
  or supplied by the caller.

## Value

A logical vector the same length as `lat`/`lon`. `NA` coordinates return
`FALSE`.

## Examples

``` r
in_bbox(40.2, -74.5, region_bbox("NJ"))
#> [1] TRUE
in_bbox(40.5, -104.9, region_bbox("NJ")) # a Colorado false-match
#> [1] FALSE
```
