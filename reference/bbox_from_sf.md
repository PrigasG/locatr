# Build a geocoding bounding box from an sf layer

Converts any point, line, or polygon `sf` layer to WGS84 and returns a
named latitude/longitude bounding box suitable for
[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md),
[`geocode_arcgis()`](https://prigasg.github.io/locatr/reference/geocode_arcgis.md),
[`geocode_by_name()`](https://prigasg.github.io/locatr/reference/geocode_by_name.md),
and
[`validate_geocodes()`](https://prigasg.github.io/locatr/reference/validate_geocodes.md).
This is the safest way to keep multi-state geocoding and local geography
joins aligned: build or load the geography layer first, then derive the
bbox from it.

## Usage

``` r
bbox_from_sf(geography_shapes, buffer = 0.05)
```

## Arguments

- geography_shapes:

  An `sf` object.

- buffer:

  Numeric buffer in decimal degrees added to each side of the bounding
  box. Defaults to `0.05` to avoid rejecting edge locations.

## Value

A named numeric vector with `lat_min`, `lat_max`, `lon_min`, and
`lon_max`.

## Examples

``` r
if (FALSE) { # \dontrun{
areas <- build_local_geography("PA")
bbox <- bbox_from_sf(areas)
} # }
```
