# Location geocoding workflow

`locatr` turns a messy address table into an audit-ready crosswalk. The
chunks below are not evaluated because they need live geocoding
services, but they show the intended end-to-end run. The package assumes
you already have a data frame; it does not connect to databases or
source systems.

`locatr` does not replace `tidygeocoder`; it builds on it. tidygeocoder
is the geocoding engine for the main batch passes. `locatr` adds the
workflow layer: address cleaning, bad-address flagging, region
validation, tier-by-tier audit columns, local geography joins, review
exports, manual overrides, and final crosswalk output. If your addresses
are already clean and you only need one service’s coordinates, call
[`tidygeocoder::geo()`](https://jessecambon.github.io/tidygeocoder/reference/geo.html)
or
[`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)
directly. Use `locatr` when the coordinates need to be defensible and
reusable.

## 1. Pull and clean

``` r

library(locatr)

cleaned <- records %>%
  clean_addresses(
    id = `Location ID`, address = Address,
    city = City, zip = Zip, name = `Location Name`
  ) %>%
  flag_bad_addresses()
```

[`clean_addresses()`](https://prigasg.github.io/locatr/reference/clean_addresses.md)
adds `*_clean` columns and a `full_address_clean` string;
[`flag_bad_addresses()`](https://prigasg.github.io/locatr/reference/flag_bad_addresses.md)
sends PO boxes and placeholders straight to review. Only address and
city are required. If your file has no ID, locatr generates row-number
IDs; if it has no ZIP, `zip_clean` stays `NA` and the single-line
address omits the ZIP instead of ending in `NA`:

``` r

cleaned_minimal <- records %>%
  clean_addresses(address = Address, city = City, state = "NJ") %>%
  flag_bad_addresses()
```

Missing ZIP is recorded as `bad_address_flag == "missing_zip"` for
audit, but it does not block geocoding when address + city + state are
present.

For one-off review, use
[`geocode_address()`](https://prigasg.github.io/locatr/reference/geocode_address.md)
to see ranked ArcGIS candidates for a single address:

``` r

if (interactive()) {
  geocode_address("1600 Pennsylvania Ave NW", city = "Washington", state = "DC")
}
```

## 2. Geocode with a guarded cascade

``` r

geocoded <- geocode_records(cleaned)   # Census -> ArcGIS -> name lookup
```

[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
runs each tier in turn and validates against the configured region after
each pass, so a later, fuzzier service only retries what is still
unplaced. The `geocode_pass` column records which tier placed each row.

## 3. Geography join

``` r

with_geography <- add_county_muni(geocoded, state = "NJ")
```

The geography step is independent of the input address columns once
coordinates exist.
[`add_county_muni()`](https://prigasg.github.io/locatr/reference/add_county_muni.md)
builds Census TIGER/Line geography and attaches county/locality fields.
Pass an `sf` boundary layer to adapt the geography join,
e.g. `add_muni_from_shapes(geocoded, muni_shapes = my_local_shapes)`, or
use
[`add_muni_from_key()`](https://prigasg.github.io/locatr/reference/add_muni_from_key.md)
when your records and geography share a code column.

## 4. Review, override, export

``` r

write_geocode_review(with_geography, "manual_review.csv")

final <- with_geography %>%
  apply_manual_overrides("manual_review_completed.csv") %>%
  export_location_crosswalk("location_crosswalk.csv")
```

`final` is ready for Tableau, GIS joins, or a reusable reference table,
and every row carries the audit columns that explain how its coordinate
was produced.

## 5. Reproducible runs and provenance

Because the cascade calls external services, reuse a
[`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md)
to make a run reproducible and cheap to repeat:

``` r

cache <- locatr_cache("geocode_cache.rds")  # omit the path for a memory-only cache

geocoded <- geocode_records(cleaned, cache = cache)

# a repeat run replays cached coordinates instead of re-querying
geocoded_again <- geocode_records(cleaned, cache = cache)

cache_info(cache)
```

The cache is keyed by the exact query and request parameters and stores
one row per candidate result (with a no-match sentinel so misses replay
too). Pass `refresh = TRUE` to re-query and overwrite. Nothing is
written to disk unless you give
[`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md)
a path.

Every
[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
result also carries a run manifest and two per-row provenance columns:

``` r

geocode_provenance(geocoded)

geocoded[, c("record_id", "placed_at", "cache_status")]
```

`cache_status` is `fresh`, `cached`, `reference`, `manual`, or
`unplaced`, and `placed_at` is when the coordinate actually entered the
output (the cached timestamp for cached rows, not the current run). Both
columns are carried into
[`export_location_crosswalk()`](https://prigasg.github.io/locatr/reference/export_location_crosswalk.md).
The manifest is attached as an attribute, so read it with
[`geocode_provenance()`](https://prigasg.github.io/locatr/reference/geocode_provenance.md)
right after the run, before any later data-frame operation that might
drop attributes.
