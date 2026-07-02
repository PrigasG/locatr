# locatr

<img src="man/figures/logo.png" align="right" width="160" alt="locatr logo" />

[![R-CMD-check](https://github.com/PrigasG/locatr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PrigasG/locatr/actions/workflows/R-CMD-check.yaml)
[![Hugging Face Space](https://img.shields.io/badge/Hugging%20Face-Space-yellow.svg)](https://huggingface.co/spaces/Prigas89/locatr_reviewer)

An audit-ready R toolkit for cleaning, geocoding, validating, reviewing, and
exporting messy address and location data. It sits **on top of** `tidygeocoder`:
`tidygeocoder` fetches coordinates; `locatr` decides whether those coordinates
are trustworthy and produces a dashboard- and GIS-ready crosswalk.

## How locatr relates to tidygeocoder

`locatr` is built on top of
[`tidygeocoder`](https://jessecambon.github.io/tidygeocoder/), not as a
replacement. tidygeocoder is the geocoding engine: hand it an address, pick a
service, and it returns coordinates and service results. `locatr` uses that
engine for its main geocoding passes and adds the workflow layer around it:
deciding whether a coordinate can be trusted, recording how it was produced, and
tying it to local geography for review and reuse.

If you just need coordinates from already-clean addresses via one service, use
`tidygeocoder::geo()` / `geocode()` directly. `locatr` earns its place when the
data is messy, the matches need to be defensible, and you need local geography
enrichment.

| Concern | tidygeocoder | locatr |
|---|---|---|
| Address -> coordinates | core purpose | delegates main geocoding passes to tidygeocoder |
| Breadth of services / reverse geocoding | broad support | focused workflow |
| Normalise messy address text | assumes prepared input | `clean_addresses()` |
| Flag PO boxes / placeholders before calls | outside scope | `flag_bad_addresses()` |
| Reject out-of-region false matches | outside scope | `validate_geocodes()`, `region_bbox()` |
| Multi-service cascade | `geocode_combine()` cascades not-found records | `geocode_records()` adds region validation, tier stamps, and name-match score/type gates |
| Audit trail / review status | service results, but no standard review trail | method, pass, match, validation, review, and override fields |
| Local geography crosswalk | Census geographies possible for Census results | point-in-polygon/key joins to Census or user boundary layers |
| Human review + reusable crosswalk | outside scope | review export, manual overrides, final crosswalk |
| Single-address candidate lookup | `geo(limit = ..., full_results = TRUE)` can return candidates | `geocode_address()` adds cleaning, score filtering, context hints, and geography |
| No-code app | outside scope | Shiny app / Hugging Face Space |

In short: **tidygeocoder turns an address into geocoder results; `locatr`
decides whether those results can be trusted, records why, and ties them to
local geography for review and reuse.**

## Why It Exists

Geocoders are imperfect. Strict services miss real addresses; fuzzy services can
confidently place a bad match far outside the intended service area. `locatr`
wraps that reality with three guards:

1. **Flagging** - PO boxes, placeholders, and missing address/city fields never
   hit the geocoder. ZIP is helpful but optional; missing ZIP is audited without
   blocking address + city + state geocoding.
2. **Fallback passes** - ArcGIS and name-based lookup can pick up what a stricter
   geocoder misses.
3. **Region validation** - anything that lands outside the configured bounding
   box or boundary polygon is rejected, not mapped.

Every function records an audit trail (`geocode_method`, `geocode_pass`,
`match_status`, `validation_status`, `review_status`, `manual_override_used`) so a
reviewer can see how each coordinate was produced. In finished geocoding output,
`review_status` uses outcome-oriented values such as `auto_accepted`,
`needs_manual_review`, `manual_override_applied`, and `rejected`.

Those signals are also distilled into a single `match_confidence` (0-1) with a
plain-language `confidence_reason`, via `add_match_confidence()`. It is attached
automatically by `geocode_records()`, carried through the crosswalk, and
included in `geocode_address()` candidates, so you can sort or threshold on one
trust column instead of reading four.

## Install

```r
# from a local clone
devtools::document()
devtools::install()
```

## Workflow

```r
library(locatr)

cleaned <- records %>%
  clean_addresses(id = `Facility ID`, address = Address,
                  city = City, zip = Zip, name = `Facility Name`,
                  state = "NJ") %>%
  flag_bad_addresses()

geocoded <- geocode_records(cleaned, bbox = region_bbox("NJ"))

with_geography <- add_county_muni(geocoded, state = "NJ")

write_geocode_review(with_geography, "manual_review.csv")

final <- with_geography %>%
  apply_manual_overrides("manual_review_completed.csv", bbox = region_bbox("NJ")) %>%
  export_location_crosswalk("location_crosswalk.csv")
```

At minimum, `clean_addresses()` needs an address and city. If no ID is supplied,
`locatr` creates row-number IDs; if no ZIP is supplied, `zip_clean` stays `NA`
and the single-line address omits the trailing ZIP:

```r
cleaned <- records %>%
  clean_addresses(address = Address, city = City, state = "NJ") %>%
  flag_bad_addresses()
```

Rows with missing ZIP remain geocodable. Rows missing address/city, PO boxes,
placeholder addresses, and test records still go to manual review.

To catch data-entry errors the geocoder would silently accept, run
`flag_field_conflicts()`: it flags a ZIP that cannot belong to the stated state
(a conservative USPS-region check that never false-flags) and, given a stated
county column, a county that disagrees with the geocoded `location_county`. It
adds `zip_state_conflict`, `county_conflict`, and a combined `field_conflict`
column without changing any coordinate.

```r
checked <- flag_field_conflicts(with_geography, stated_county = "County")
```

## Quick single-address lookup

To check one address interactively - no data frame required - use
`geocode_address()`. It cleans the text, asks ArcGIS for candidate matches
ranked by confidence (highest first), and attaches county/municipality when
`state` is supplied, a candidate state can be inferred, or you provide a
geography layer:

```r
geocode_address("1600 Pennsylvania Ave NW")

# ambiguous street-only searches often need a little location context
geocode_address("24 Peachton", state = "NJ")

# keep only high-confidence matches, coordinates only
geocode_address("1 City Hall Sq", city = "Boston", state = "MA",
                min_score = 90, geography = FALSE)
```

Set `min_score` to a confidence threshold (0-100), `max_candidates` to cap how
many come back, and `geography = FALSE` to skip the county/municipality join.
`min_score` filters candidates ArcGIS already returned; use `city`, `state`,
`zip`, or `bbox` when you need to change where ArcGIS searches.
In interactive sessions, `geocode_address()` prints short progress messages; set
`show_progress = FALSE` to silence them.

## Audit helpers

After a run, use the small audit helpers to understand and compare results:

```r
summarise_geocoding(geocoded)
explain_geocode_result(geocoded, row = 1)
suggest_geography_level("PA")
compare_geocode_runs(previous_geocoded, geocoded)

if (interactive()) {
  plot_geocode_review_map(geocoded)
}
```

## No-code web app

For users who would rather not write R, the same pipeline is available as a
Shiny app: upload a CSV/Excel/Parquet file, geocode it, and download the
geocoded records immediately as CSV, Excel, or Parquet. If you also need
county/locality fields, attach geography from Census TIGER/Line or an uploaded
shapefile first, then download the geography crosswalk. The download step lets
you remove columns before exporting. In the app, Unique ID and ZIP are optional:
leave them as `(auto)` / `(none)` when your file only has address and city.

```r
install.packages(c("shiny", "bslib", "DT", "leaflet",
                   "readxl", "writexl", "arrow"))
run_locatr_app()
```

The app is also published as a Hugging Face Space (Docker):
<https://huggingface.co/spaces/Prigas89/locatr_reviewer>. The deployment
scaffolding lives in [`huggingface/`](huggingface/).

## The Geocoding Cascade

`geocode_records()` runs progressively fuzzier internet services, retrying only
rows that began with geocodable addresses and are still unplaced after each
validation pass:

| Tier | Function | Service | What it catches |
|------|----------|---------|-----------------|
| 0 | `backfill_from_reference()` | trusted table (no network) | records already verified in a prior cycle |
| 1 | `geocode_census()` | US Census (structured) | clean, in-range street addresses |
| 2 | `geocode_arcgis()` | ArcGIS (address line) | typos, range gaps, fuzzy streets |
| 3 | `geocode_by_name()` | ArcGIS (name + city) | named places, campuses, landmarks |
| manual | `apply_manual_overrides()` | human | the rest |

Tier 0 is optional and authoritative: pass
`geocode_records(cleaned, reference = verified)` or call
`backfill_from_reference()` directly to place records whose coordinates were
checked before. These rows are bbox-validated, stamped `pass_0_reference`, and
skipped by every later tier.

The input readiness value `ready_for_geocoding` is internal to the cascade; after
`geocode_records()` completes, valid matched rows are marked `auto_accepted`.

Name lookup is score-gated when the geocoder returns score/type fields. Precise
point-address hits above `min_score` pass cleanly; fuzzier POI, locality, or
low-score hits keep coordinates for reviewer context but remain in
`needs_manual_review`. The Shiny app exposes the name score threshold and the
address types that are allowed to pass without review.

## Reproducible Runs

Geocoding hits external services, so a run is only reproducible if you can replay
it without re-querying. Pass a `locatr_cache()` to `geocode_records()` (it also
works on the single tiers and on `geocode_address()`): repeated addresses are
served from the cache, and a later run reproduces the same coordinates offline
instead of re-hitting Census or ArcGIS.

```r
cache <- locatr_cache("nj_geocode_cache.rds")  # persistent; omit the path for memory-only

geocoded <- geocode_records(cleaned, bbox = region_bbox("NJ"), cache = cache)

# a second run reuses the cache - no network for addresses already placed
geocoded_again <- geocode_records(cleaned, bbox = region_bbox("NJ"), cache = cache)

cache_info(cache)   # rows, distinct keys, per-method counts, file size, oldest/newest
```

The cache stores one row per candidate result (plus a no-match sentinel, so
misses are replayed too), keyed by the exact query and request parameters. Pass
`refresh = TRUE` to force a re-query and overwrite. Nothing is written to disk
unless you give `locatr_cache()` a path.

Every `geocode_records()` result also carries a run manifest and two per-row
provenance columns:

```r
geocode_provenance(geocoded)  # run id, timestamp, versions, tiers, cache activity, status counts

geocoded[, c("record_id", "placed_at", "cache_status")]
```

`cache_status` is one of `fresh` (geocoded this run), `cached` (the cache already
held the coordinate before this run), `reference`, `manual`, or `unplaced`.
`placed_at` records when a coordinate actually entered the output - the cached
timestamp for cached rows, or an explicit reference/override timestamp when your
data carries one, rather than the current run time. Both columns flow through
`export_location_crosswalk()`. The manifest is attached as an attribute
(`attr(geocoded, "locatr_run")`), so read it with `geocode_provenance()` directly
off the `geocode_records()` result, before further data-frame operations that
might drop attributes.

For a shareable summary, `geocode_report()` rolls the manifest and audit columns
into counts by review status, placing tier, and cache status, a
`match_confidence` summary, and an auto-generated plain-language methods
paragraph for a report or paper. Print it, or write Markdown with `file =`:

```r
geocode_report(geocoded)                      # printed summary + methods paragraph
geocode_report(geocoded, file = "run.md")     # Markdown report
```

## Regions And Geography

`region_bbox()` ships coarse guard boxes for every US state and `DC`
(case-insensitive), so validation works out of the box beyond NJ:

```r
geocoded <- geocode_records(cleaned, bbox = region_bbox("CA"))
```

These presets are deliberately generous sanity boxes, not precise boundaries.
For a tighter or non-state region, pass your own bbox, or derive one from an
`sf` layer with `bbox_from_sf()`:

```r
custom_bbox <- c(lat_min = 38.0, lat_max = 39.0, lon_min = -77.5, lon_max = -76.0)
geocoded <- geocode_records(cleaned, bbox = custom_bbox)
```

For stricter validation, pass an `sf` boundary polygon with `boundary =`.

## Local Geography (NJ Municipalities)

In production, `location_locality` and `location_county` come from a spatial
join against an authoritative NJ municipal boundary layer, not from the geocoder
response or Census reverse-geocoding. The workflow is: load boundaries, derive a
bbox, geocode, validate, convert to `sf` points, spatially join to the polygons,
and write `location_county`, `location_locality`, `geography_match_status`.

```r
munis <- sf::st_read("path/to/Municipal_Boundaries_of_NJ.shp") %>%
  sf::st_make_valid() %>%
  dplyr::transmute(location_county = COUNTY, location_locality = MUN)

bbox <- bbox_from_sf(munis)
geocoded <- geocode_records(cleaned, bbox = bbox)

with_geography <- add_local_geography(
  geocoded,
  geography_shapes = munis,
  county_col   = "location_county",
  locality_col = "location_locality"
)
```

Or build the packaged default once with `data-raw/local_geography.R`, sourced
from [NJGIN/NJOGIS Municipal Boundaries of NJ](https://njogis-newjersey.opendata.arcgis.com/datasets/municipal-boundaries-of-nj-hosted-3424).

## Any State, With Census TIGER/Line

`add_county_muni()` is the shortest path when Census TIGER/Line boundaries are
good enough for your workflow:

```r
final <- add_county_muni(geocoded, state = "PA", geography = "county_subdivision")
```

Under the hood, `build_local_geography()` pulls Census boundaries through
`tigris` and standardises them to the `location_county` / `location_locality`
schema, plus stable join fields when the source provides them:
`county_code`, `county_fips`, `municipality_code`, `municipality_geoid`,
`municipality_name_standard`, `municipality_type`, and `muni_join_key`.

```r
areas <- build_local_geography(state = "PA", geography = "county_subdivision")
bbox <- bbox_from_sf(areas)

geocoded <- geocode_records(cleaned, bbox = bbox)

final <- add_local_geography(
  geocoded,
  geography_shapes = areas,
  county_col   = "location_county",
  locality_col = "location_locality"
)
```

Pick `geography` for the state: `county` is easy everywhere;
`county_subdivision` maps well to townships/municipalities in NJ, PA, NY, and
New England; `place` covers incorporated places/CDPs but misses townships and
many unincorporated areas; `tract` uses tract GEOIDs. For high-stakes,
state-specific reporting where "municipality" has legal meaning, use an
official state GIS layer and pass it directly. `Muni Key` is kept as a readable
fallback, but production joins should prefer `muni_join_key`,
`municipality_geoid`, or another official code from your boundary source.

Use `add_muni_from_shapes()` for point-in-polygon joins against your own
boundary layer, or `add_muni_from_key()` when your records and geography share a
code column such as ZIP, FIPS, or GEOID.

For analysis and policy geographies beyond county/municipality - census tract,
block group, ZCTA, congressional and state legislative districts, or unified
school districts - `add_census_geographies()` attaches any combination in one
call, each as a `<level>_geoid` / `<level>_name` pair:

```r
enriched <- add_census_geographies(
  geocoded, state = "NJ",
  levels = c("tract", "congressional_district", "school_district")
)
```
