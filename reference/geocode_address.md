# Look up a single address and return ranked candidate points

An interactive, one-shot companion to the batch pipeline: pass a literal
address (no data frame) and get back a tibble of candidate matches
ranked by geocoder confidence, highest first. Each candidate carries its
coordinates, match score, and ArcGIS address type, and - unless turned
off - the county and municipality the point falls in. Handy for
spot-checking one address, or for letting a reviewer eyeball the
plausible locations the cascade would choose from.

## Usage

``` r
geocode_address(
  address,
  city = NULL,
  state = NULL,
  zip = NULL,
  id = NULL,
  min_score = 0,
  max_candidates = 5L,
  geography = TRUE,
  geography_shapes = NULL,
  bbox = NULL,
  quiet = TRUE,
  show_progress = interactive(),
  cache = NULL,
  refresh = FALSE
)
```

## Arguments

- address:

  Single-line street address as a length-1 character string.

- city:

  Optional locality for the address (length-1 character).

- state:

  Optional two-letter state abbreviation. When `city` is supplied but
  `state` is omitted, defaults to `"NJ"` for compatibility.

- zip:

  Optional ZIP/postal code. Improves match precision when supplied.

- id:

  Optional label echoed back in the `query_id` column.

- min_score:

  Minimum ArcGIS match score (0-100) a returned candidate must reach to
  be kept. Defaults to `0`. This filters ArcGIS results; use `city`,
  `state`, `bbox`, or `zip` to change the search context.

- max_candidates:

  Maximum number of candidates to return. Defaults to `5`.

- geography:

  If `TRUE` (default), attach `County`/`Municipality` (and the other
  local-geography fields). When `state` is not supplied, locatr tries to
  infer candidate states from ArcGIS matched addresses. Set `FALSE` for
  coordinates only.

- geography_shapes:

  Optional `sf` boundary layer to attach geography from (via
  [`add_muni_from_shapes()`](https://prigasg.github.io/locatr/reference/add_muni_from_shapes.md)).
  When `NULL` and `geography = TRUE`, county subdivisions are built from
  Census TIGER/Line for `state` (needs `tigris` and network access).

- bbox:

  Optional region bounding box (see
  [`region_bbox()`](https://prigasg.github.io/locatr/reference/region_bbox.md)).
  When given, ArcGIS is asked to prefer that extent and an `in_bbox`
  flag is added.

- quiet:

  If `TRUE` (default), suppress routine messages from geography
  downloads/joins so the console shows only the returned candidate
  table.

- show_progress:

  If `TRUE`, print short progress messages while the lookup runs.
  Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- cache:

  Optional
  [`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md)
  object. When supplied, the ArcGIS candidate lookup for a given query
  is served from the cache on repeat calls (and replayable offline)
  instead of re-hitting the service.

- refresh:

  If `TRUE`, bypass any cached entry for this query and re-query the
  service, overwriting the cached result. Defaults to `FALSE`.

## Value

A tibble of candidates ordered by descending `match_score`, with
`query_id`, `rank`, `match_score`, `match_addr_type`, `matched_address`,
`latitude`, `longitude`, the cleaned `input_address`, an optional
`in_bbox` flag, and (when `geography = TRUE`) `County`/`Municipality`
and related fields. Zero rows if nothing matched at or above
`min_score`.

## Details

The address text is normalised with the same abbreviation/secondary-unit
cleaning used by
[`clean_addresses()`](https://prigasg.github.io/locatr/reference/clean_addresses.md),
then sent to the free ArcGIS `findAddressCandidates` service. `city`,
`state`, and `zip` are optional for this one-off helper: use them when
you want to narrow the search, or pass only `address` to inspect broad
candidate matches. If `city` is supplied and `state` is omitted, `state`
defaults to `"NJ"` for compatibility with the package's first workflow.

## See also

[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
for the batch cascade over a data frame.

## Examples

``` r
if (interactive()) {
# ranked candidates for one address
geocode_address("1600 Pennsylvania Ave NW")

# only high-confidence matches, coordinates only
geocode_address("1 City Hall Sq", city = "Boston", state = "MA",
                min_score = 90, geography = FALSE)
}
```
