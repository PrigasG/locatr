# Add county and municipality fields by joining on a shared key

The non-spatial counterpart to
[`add_muni_from_shapes()`](https://prigasg.github.io/locatr/reference/add_muni_from_shapes.md).
Instead of a point-in-polygon join, it merges geography attributes from
a boundary layer onto geocoded records by a shared key column (for
example a ZIP, FIPS, GEOID, or local region code that both tables
carry). Use it when a spatial join is not the right criterion - the
records may lack coordinates, or the authoritative geography is keyed by
a code rather than a polygon footprint.

## Usage

``` r
add_muni_from_key(
  data,
  muni_shapes,
  data_key,
  shp_key,
  county_col = NULL,
  muni_col = NULL,
  key_col = NULL
)
```

## Arguments

- data:

  A data frame of geocoded records carrying `data_key`.

- muni_shapes:

  An `sf` polygon (or attribute) layer carrying `shp_key` and the
  geography attributes to attach.

- data_key:

  Name (string) of the join-key column in `data`.

- shp_key:

  Name (string) of the join-key column in `muni_shapes`.

- county_col, muni_col:

  Optional explicit county / locality column names in `muni_shapes`.
  Empty strings are treated as unset.

- key_col:

  Optional municipal key column in `muni_shapes`.

## Value

`data` with `County`, `Municipality`, `Muni Key`, `muni_join_key`,
`county_code`, `county_fips`, `municipality_code`, `municipality_geoid`,
`municipality_name_standard`, `municipality_type`, `muni_match_status`,
and the generic `location_county` / `location_locality` /
`geography_match_status` audit fields.

## Details

Output columns match
[`add_muni_from_shapes()`](https://prigasg.github.io/locatr/reference/add_muni_from_shapes.md)
exactly, so the two join paths are interchangeable downstream: `County`,
`Municipality`, `Muni Key`, `muni_join_key`, the stable code/identifier
fields, `location_county`, `location_locality`,
`geography_match_status`, and `muni_match_status`. Stable code fields
(`county_code`, `municipality_code`, `municipality_geoid`, etc.) are
auto-detected from common boundary schemas; `county_fips` is synthesised
from a state FIPS plus county code when not supplied directly, and
`Muni Key` falls back to `County::Municipality` when no official
identifier exists.

## See also

[`add_muni_from_shapes()`](https://prigasg.github.io/locatr/reference/add_muni_from_shapes.md)
for the spatial (point-in-polygon) join.
