# Name-based geocode pass (the "paste it in a browser" tier)

For rows still unplaced after the address-based passes, geocodes by
record *name* plus city and state rather than the street line. This can
resolve campus/landmark addresses (e.g. a unit inside a hospital) that
street-range interpolation cannot, because a composite geocoder
recognises the named place.

## Usage

``` r
geocode_by_name(
  data,
  method = "arcgis",
  bbox = region_bbox("NJ"),
  min_score = 90,
  accept_types = c("PointAddress", "Subaddress", "StreetAddress"),
  ...
)
```

## Arguments

- data:

  A data frame carrying `record_id`, `record_name`, `city_clean`,
  `state_clean`, and the geocode audit columns.

- method:

  tidygeocoder method that accepts free-text queries (default
  `"arcgis"`; `"osm"` and `"google"` also work).

- bbox:

  Bounding box used to reject out-of-region matches; see
  [`region_bbox()`](https://prigasg.github.io/locatr/reference/region_bbox.md).

- min_score:

  Minimum match score (0-100) for a name hit to pass without review.
  Hits below this stay reviewable. Default `90`.

- accept_types:

  Address types precise enough to pass without review (matched
  case-insensitively against the geocoder's `addr_type`). Default the
  point-address types
  `c("PointAddress", "Subaddress", "StreetAddress")`.

- ...:

  Passed through to
  [`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html).
  `full_results = TRUE` is requested automatically so scores are
  available; pass `full_results = FALSE` to opt out (which also disables
  score gating).

## Value

`data` with name-lookup audit columns `nm_latitude`, `nm_longitude`,
`nm_score`, `nm_addr_type`, `nm_status`, and updated
`latitude`/`longitude`/`geocode_method`/`geocode_pass`/`match_status`
for rows the name pass filled. When there is nothing for the tier to
geocode, `nm_status` is set to `"not_run"` for audit clarity.
Low-confidence fills also set `review_status == "needs_manual_review"`.

## Details

Because name lookups are looser than address matching, each hit is
scored using the geocoder's match `score` and address type (when
available - ArcGIS returns both via `full_results`, which this pass
requests automatically). A hit passes cleanly only when it resolves to a
precise point address at or above `min_score`; fuzzier hits (a POI, a
locality centroid, or a low score) still have their coordinates filled
in for context but are marked `match_status == "matched_low_confidence"`
and routed to `needs_manual_review` so a person can confirm them. When
the geocoder returns no score/type information (e.g. `method = "osm"`),
the pass falls back to the previous rule: any in-region match is
accepted.

Filled rows are tagged `geocode_pass == "pass_4_name_lookup"`. The
bounding box still rejects out-of-region hits, but cannot catch a wrong
same-state match, which is exactly what the score gate is for.
