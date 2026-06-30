# Join records to local geography

Spatially joins geocoded points to a local polygon layer and returns
selected geography attributes for dashboards. County and locality column
names are auto-detected from common boundary schemas, or can be set
explicitly.

## Usage

``` r
add_local_geography(
  data,
  geography_shapes = NULL,
  county_col = NULL,
  locality_col = NULL
)
```

## Arguments

- data:

  A validated data frame with `latitude`/`longitude`.

- geography_shapes:

  An `sf` polygon layer, or `NULL` to use packaged data.

- county_col, locality_col:

  Optional explicit column names in `geography_shapes`. When `NULL`,
  `add_local_geography()` guesses from common names, preferring
  `location_county`/`location_locality` when present.

## Value

`data` with `location_county`, `location_locality`, and
`geography_match_status`. Rows without usable coordinates are kept
(audit-safe) with `NA` geography.

## Details

If `geography_shapes` is `NULL`, the function looks for a packaged
`local_geography` dataset (for production this is the NJGIN/NJOGIS
municipal boundary layer built by `data-raw/local_geography.R`, whose
attributes are already named `location_county`/`location_locality`).
Pass an `sf` polygon layer to adapt this join to another state, county,
or service area.

For NJ production maps, `location_locality` is taken from an
authoritative municipal boundary polygon - not from the geocoder
response or Census reverse-geocoding, whose "county subdivision" names
only look municipal - so every locality is traceable to a named boundary
source.
