# Region bounding box

Returns an approximate latitude/longitude bounding box for a US state
(or `"DC"`), used as a fast sanity guard on geocoded coordinates.
Presets are deliberately a little generous so legitimate edge locations
are not rejected; they are coarse guard boxes, not precise boundaries.
For a tighter or non-state region, pass your own named vector, or derive
one from an `sf` layer with
[`bbox_from_sf()`](https://prigasg.github.io/locatr/reference/bbox_from_sf.md).

## Usage

``` r
region_bbox(region = "NJ")
```

## Arguments

- region:

  Two-letter US state abbreviation (or `"DC"`), case-insensitive.
  Defaults to `"NJ"`.

## Value

A named numeric vector with elements `lat_min`, `lat_max`, `lon_min`,
`lon_max`.

## Examples

``` r
region_bbox("NJ")
#> lat_min lat_max lon_min lon_max 
#>    38.8    41.4   -75.7   -73.8 
region_bbox("CA")
#> lat_min lat_max lon_min lon_max 
#>    32.5    42.1  -124.5  -114.1 
```
