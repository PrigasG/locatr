# Backfill verified coordinates from a trusted reference table (Tier 0)

The authoritative first tier of the cascade. Joins coordinates from a
curated key -\> coordinates table - an institutional-memory table of
records whose location was resolved and checked at some point - over the
automated geocoders, so a record that has already been verified never
has to be re-geocoded. This is what turns one analyst's manual review
into a permanent asset: feed last cycle's completed overrides (or any
trusted coordinate list) back in as `reference`, and those rows are
placed instantly and exactly.

## Usage

``` r
backfill_from_reference(
  data,
  reference,
  by = "record_id",
  lat_col = "latitude",
  lon_col = "longitude",
  county_col = NULL,
  locality_col = NULL,
  bbox = region_bbox("NJ")
)
```

## Arguments

- data:

  A data frame from
  [`flag_bad_addresses()`](https://prigasg.github.io/locatr/reference/flag_bad_addresses.md)
  (or any frame carrying the key column).

- reference:

  A data frame of verified records: the key column plus coordinate
  columns. May also carry county/locality columns. `NULL` or an empty
  frame makes this a no-op.

- by:

  Name of the key column shared by `data` and `reference` (default
  `"record_id"`).

- lat_col, lon_col:

  Coordinate column names in `reference` (default
  `"latitude"`/`"longitude"`).

- county_col, locality_col:

  Optional geography column names in `reference` to backfill into
  `location_county`/`location_locality`.

- bbox:

  Bounding box used to reject out-of-region reference coordinates; see
  [`region_bbox()`](https://prigasg.github.io/locatr/reference/region_bbox.md).

## Value

`data` with reference audit columns `ref_latitude`, `ref_longitude`,
`ref_status`, and, for rows the reference filled, updated
`latitude`/`longitude`/`geocode_method`/`geocode_pass`
(`"pass_0_reference"`)/ `match_status`/`review_status`.

## Details

Reference coordinates are still bbox-validated, so a stale or
fat-fingered entry cannot drop a point outside the region. Because the
reference is authoritative, a matched row is marked
`review_status == "reference_backfilled"` and carries valid coordinates,
so
[`geocode_census()`](https://prigasg.github.io/locatr/reference/geocode_census.md)
and every later tier skip it automatically - even if its raw address was
previously flagged (e.g. a PO box whose true coordinates were verified
once).

Run this before
[`geocode_census()`](https://prigasg.github.io/locatr/reference/geocode_census.md)
(the cascade does so when you pass `reference =` to
[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)).

## Examples

``` r
records <- tibble::tibble(
  record_id = c("a", "b"),
  review_status = c("ready_for_geocoding", "needs_manual_review")
)
verified <- tibble::tibble(record_id = "b", latitude = 40.22, longitude = -74.76)
backfill_from_reference(records, verified)
#> # A tibble: 2 × 10
#>   record_id review_status        latitude longitude geocode_method geocode_pass 
#>   <chr>     <chr>                   <dbl>     <dbl> <chr>          <chr>        
#> 1 a         ready_for_geocoding      NA        NA   NA             NA           
#> 2 b         reference_backfilled     40.2     -74.8 reference      pass_0_refer…
#> # ℹ 4 more variables: match_status <chr>, ref_latitude <dbl>,
#> #   ref_longitude <dbl>, ref_status <chr>
```
