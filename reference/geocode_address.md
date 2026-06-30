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
  city,
  state = "NJ",
  zip = NULL,
  id = NULL,
  min_score = 0,
  max_candidates = 5L,
  geography = TRUE,
  geography_shapes = NULL,
  bbox = NULL,
  quiet = TRUE
)
```

## Arguments

- address:

  Single-line street address as a length-1 character string.

- city:

  Locality for the address (length-1 character).

- state:

  Two-letter state abbreviation. Defaults to `"NJ"`.

- zip:

  Optional ZIP/postal code. Improves match precision when supplied.

- id:

  Optional label echoed back in the `query_id` column.

- min_score:

  Minimum ArcGIS match score (0-100) a candidate must reach to be
  returned. Defaults to `0` (return all, still ranked).

- max_candidates:

  Maximum number of candidates to return. Defaults to `5`.

- geography:

  If `TRUE` (default), attach `County`/`Municipality` (and the other
  local-geography fields) to each candidate. Set `FALSE` for coordinates
  only.

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

## Value

A tibble of candidates ordered by descending `match_score`, with
`query_id`, `rank`, `match_score`, `match_addr_type`, `matched_address`,
`latitude`, `longitude`, the cleaned `input_address`, an optional
`in_bbox` flag, and (when `geography = TRUE`) `County`/`Municipality`
and related fields. Zero rows if nothing matched at or above
`min_score`.

## Details

The address text is normalised with
[`clean_addresses()`](https://prigasg.github.io/locatr/reference/clean_addresses.md)
(so it benefits from the same abbreviation/secondary-unit cleaning),
then sent to the free ArcGIS `findAddressCandidates` service, which
returns several scored candidates for a single query. Use `min_score` to
keep only candidates at or above a confidence threshold (for example
`min_score = 90`), and `max_candidates` to cap how many come back.

## See also

[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
for the batch cascade over a data frame.

## Examples

``` r
if (interactive()) {
# ranked candidates for one address, with county/municipality attached
geocode_address("1600 Pennsylvania Ave NW", city = "Washington", state = "DC")

# only high-confidence matches, coordinates only
geocode_address("1 City Hall Sq", city = "Boston", state = "MA",
                min_score = 90, geography = FALSE)
}
```
