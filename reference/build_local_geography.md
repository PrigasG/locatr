# Build a local geography layer from Census TIGER/Line boundaries

Downloads an authoritative Census boundary layer for a state with the
tigris package and standardises it into the two-column schema
[`add_local_geography()`](https://prigasg.github.io/locatr/reference/add_local_geography.md)
expects: `location_county` (always from counties) and
`location_locality` (from the requested `geography`). It also carries
stable Census identifiers such as `county_fips`, `municipality_geoid`,
and `muni_join_key` when those fields exist. This makes "locality" a
configurable concept, because what counts as a municipality is not
consistent across states.

## Usage

``` r
build_local_geography(
  state,
  geography = c("county_subdivision", "place", "county", "tract"),
  county = NULL,
  year = NULL,
  cb = TRUE,
  ...
)
```

## Arguments

- state:

  Two-letter state abbreviation or FIPS code (passed to tigris).

- geography:

  Which Census layer becomes `location_locality`. One of
  `"county_subdivision"`, `"place"`, `"county"`, `"tract"`.

- county:

  Optional county filter (name or FIPS) for
  `"county_subdivision"`/`"tract"`; passed to tigris.

- year:

  Vintage year for the boundary files. `NULL` uses the tigris default.

- cb:

  If `TRUE` (default), use the smaller cartographic boundary files;
  `FALSE` pulls the full-resolution TIGER/Line files.

- ...:

  Passed through to the underlying tigris download function.

## Value

An `sf` polygon layer in WGS84 (EPSG:4326) with `location_county`,
`location_locality`, stable join-code columns when available, and
`geometry`, ready for `add_local_geography(geography_shapes = ...)`.

## Details

Choosing `geography`:

- `"county_subdivision"` (default) maps well to townships/municipalities
  in states like NJ, PA, NY and New England. Best general default for
  "locality".

- `"place"` maps to incorporated places and CDPs, but misses townships
  and many unincorporated areas. Places can also straddle counties, so
  each place is assigned the county it overlaps most.

- `"county"` sets locality to the county itself.

- `"tract"` uses the census tract identifier as the locality (analysis,
  not administrative naming).

Census TIGER/Line is the best scalable baseline nationwide. For
high-stakes, state-specific reporting where "municipality" has legal
meaning, prefer an official state GIS layer and pass it straight to
[`add_local_geography()`](https://prigasg.github.io/locatr/reference/add_local_geography.md) -
that path works regardless, since the join accepts any polygon layer.

## Examples

``` r
if (interactive()) {
areas <- build_local_geography(state = "PA", geography = "county_subdivision")
final <- add_local_geography(geocoded, geography_shapes = areas)
}
```
