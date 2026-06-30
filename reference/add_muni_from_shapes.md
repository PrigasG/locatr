# Add county and municipality fields from boundary polygons

A crosswalk-oriented wrapper around
[`add_local_geography()`](https://prigasg.github.io/locatr/reference/add_local_geography.md)
for workflows where the final output should carry explicit
county/municipality columns and stable join identifiers. It spatially
joins geocoded points to municipal/local boundary polygons and adds
`County`, `Municipality`, `Muni Key`, `muni_join_key`, code fields, and
`muni_match_status`.

## Usage

``` r
add_muni_from_shapes(
  data,
  muni_shapes,
  county_col = NULL,
  muni_col = NULL,
  key_col = NULL
)
```

## Arguments

- data:

  A validated data frame with `latitude`/`longitude`.

- muni_shapes:

  An `sf` polygon layer containing county and municipality attributes.

- county_col, muni_col:

  Optional explicit column names in `muni_shapes`. When `NULL`, common
  names are auto-detected.

- key_col:

  Optional municipal key column in `muni_shapes`.

## Value

`data` with `County`, `Municipality`, `Muni Key`, `muni_join_key`,
`county_code`, `county_fips`, `municipality_code`, `municipality_geoid`,
`municipality_name_standard`, `municipality_type`, and
`muni_match_status` when available. The generic `location_county`,
`location_locality`, and `geography_match_status` columns are retained.

## Details

`muni_join_key` is copied from `key_col` when supplied, or auto-detected
from common stable identifier columns such as `GEOID` and `MUNI_KEY`.
`Muni Key` is retained as a readable key and falls back to
`County::Municipality` when no official identifier exists.
