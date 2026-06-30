# Add county and municipality fields from Census boundaries

Convenience wrapper for the common workflow where users want county and
municipality/locality fields but do not already have a boundary file. It
builds a Census TIGER/Line geography layer with
[`build_local_geography()`](https://prigasg.github.io/locatr/reference/build_local_geography.md)
and then applies
[`add_muni_from_shapes()`](https://prigasg.github.io/locatr/reference/add_muni_from_shapes.md).

## Usage

``` r
add_county_muni(
  data,
  state,
  geography = c("county_subdivision", "place", "county", "tract"),
  county = NULL,
  year = NULL,
  cb = TRUE,
  ...
)
```

## Arguments

- data:

  A validated data frame with `latitude`/`longitude`.

- state:

  Two-letter state abbreviation or FIPS code.

- geography:

  Which Census layer should become `Municipality`; passed to
  [`build_local_geography()`](https://prigasg.github.io/locatr/reference/build_local_geography.md).

- county:

  Optional county filter for supported Census layers.

- year:

  Vintage year for the boundary files.

- cb:

  If `TRUE`, use smaller cartographic boundary files.

- ...:

  Passed through to
  [`build_local_geography()`](https://prigasg.github.io/locatr/reference/build_local_geography.md).

## Value

`data` with `County`, `Municipality`, `Muni Key`, stable code/join
columns when Census provides them, and `muni_match_status`, plus the
generic `location_*` geography audit fields.

## Details

For reporting where municipality has a state-specific legal definition,
use
[`add_muni_from_shapes()`](https://prigasg.github.io/locatr/reference/add_muni_from_shapes.md)
with an official state/local GIS layer.
