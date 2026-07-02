# Attach multiple Census geography levels to geocoded points

Enriches geocoded records with one or more Census TIGER/Line geography
levels in a single call, by point-in-polygon assignment. Where
[`add_county_muni()`](https://prigasg.github.io/locatr/reference/add_county_muni.md)
answers "which county and municipality", this answers "which tract,
block group, ZCTA, congressional district, state legislative district,
or school district" - the analysis and policy geographies that
dashboards and grant reporting often need. Each requested level adds two
columns: `<level>_geoid` (the Census GEOID) and `<level>_name` (the
layer's name field, `NA` where the layer has none, e.g. ZCTAs).

## Usage

``` r
add_census_geographies(
  data,
  state,
  levels = "tract",
  county = NULL,
  year = NULL,
  cb = TRUE,
  ...
)
```

## Arguments

- data:

  A geocoded data frame with `latitude` and `longitude` columns.

- state:

  Two-letter state abbreviation or FIPS code (passed to tigris). ZCTAs
  are national, so `state` is ignored for the `"zcta"` level.

- levels:

  Character vector of geography levels to attach. Any of `"tract"`,
  `"block_group"`, `"zcta"`, `"county"`, `"place"`,
  `"county_subdivision"`, `"congressional_district"`,
  `"state_legislative_district_upper"`,
  `"state_legislative_district_lower"`, `"school_district"` (unified).
  Defaults to `"tract"`.

- county:

  Optional county filter (name or FIPS) for the levels that accept one
  (`"tract"`, `"block_group"`, `"county_subdivision"`).

- year:

  Vintage year for the boundary files. `NULL` uses the tigris default.

- cb:

  If `TRUE` (default), use the smaller cartographic boundary files.

- ...:

  Passed through to the underlying tigris download functions.

## Value

`data` with, for each requested level, a `<level>_geoid` and
`<level>_name` column. Rows without usable coordinates get `NA`.

## Details

The geography step only needs `latitude`/`longitude`; it does not touch
the address columns. Downloads use tigris and therefore need network
access.

## See also

[`add_county_muni()`](https://prigasg.github.io/locatr/reference/add_county_muni.md)
for county/municipality,
[`build_local_geography()`](https://prigasg.github.io/locatr/reference/build_local_geography.md)
for a single reusable boundary layer.

## Examples

``` r
if (interactive()) {
  enriched <- add_census_geographies(
    geocoded, state = "NJ",
    levels = c("tract", "congressional_district", "school_district")
  )
}
```
