# Run the full geocoding cascade

Orchestrates the tiered strategy on an already-cleaned, already-flagged
frame (see
[`clean_addresses()`](https://prigasg.github.io/locatr/reference/clean_addresses.md)
and
[`flag_bad_addresses()`](https://prigasg.github.io/locatr/reference/flag_bad_addresses.md)):
Census structured match, then ArcGIS address fallback, then name lookup,
validating against the configured region after each tier so a later tier
only retries what is still unplaced.

## Usage

``` r
geocode_records(
  data,
  tiers = c("census", "arcgis", "name"),
  reference = NULL,
  boundary = NULL,
  bbox = region_bbox("NJ"),
  name_min_score = 90,
  name_accept_types = c("PointAddress", "Subaddress", "StreetAddress"),
  verbose = TRUE,
  cache = NULL,
  refresh = FALSE
)
```

## Arguments

- data:

  A data frame from
  [`flag_bad_addresses()`](https://prigasg.github.io/locatr/reference/flag_bad_addresses.md).

- tiers:

  Which tiers to run, in order. Any subset of
  `c("census", "arcgis", "name")`.

- reference:

  Optional trusted key -\> coordinates table for Tier 0; see
  [`backfill_from_reference()`](https://prigasg.github.io/locatr/reference/backfill_from_reference.md).
  `NULL` skips Tier 0. For non-default column names, call
  [`backfill_from_reference()`](https://prigasg.github.io/locatr/reference/backfill_from_reference.md)
  yourself before `geocode_records()`.

- boundary:

  Optional `sf` boundary for polygon-precise validation; passed to
  [`validate_geocodes()`](https://prigasg.github.io/locatr/reference/validate_geocodes.md).
  `NULL` uses the bounding box.

- bbox:

  Bounding box for region guards; see
  [`region_bbox()`](https://prigasg.github.io/locatr/reference/region_bbox.md).

- name_min_score:

  Minimum ArcGIS score for a name lookup to pass without review. Passed
  to
  [`geocode_by_name()`](https://prigasg.github.io/locatr/reference/geocode_by_name.md).

- name_accept_types:

  ArcGIS address types precise enough for a name lookup to pass without
  review. Passed to
  [`geocode_by_name()`](https://prigasg.github.io/locatr/reference/geocode_by_name.md).

- verbose:

  Whether to print a per-tier match tally.

- cache:

  Optional
  [`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md)
  shared across the network tiers (Census, ArcGIS, name lookup), so
  repeated addresses are served from the cache and a re-run reproduces
  coordinates without re-querying.

- refresh:

  If `TRUE`, ignore cached entries and re-query every tier, overwriting
  the cache. Defaults to `FALSE`.

## Value

`data` with coordinates and the full audit trail populated.

## Details

Each tier records how it placed a row in `geocode_pass`, so the final
frame is self-documenting: `pass_0_reference`,
`pass_1_census_structured`, `pass_2_fallback`, or `pass_4_name_lookup`.
After the cascade, valid matched rows are marked
`review_status == "auto_accepted"`; anything still unmatched lands in
`needs_manual_review`, while invalid coordinates are `rejected`.

Supplying `reference` runs an authoritative Tier 0 first
([`backfill_from_reference()`](https://prigasg.github.io/locatr/reference/backfill_from_reference.md)):
rows whose verified coordinates are already known are placed exactly and
skipped by every later tier. Feed prior cycles' completed overrides back
in here so manual review accrues over time.
