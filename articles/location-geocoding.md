# Location geocoding workflow

`locatr` turns a messy address table into an audit-ready crosswalk. The
chunks below are not evaluated because they need live geocoding
services, but they show the intended end-to-end run. The package assumes
you already have a data frame; it does not connect to databases or
source systems.

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
sends PO boxes and placeholders straight to review.

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

with_geography <- add_local_geography(geocoded)
```

Pass an `sf` boundary layer to adapt the geography join, e.g.
`add_local_geography(geocoded, geography_shapes = my_local_shapes)`.

## 4. Review, override, export

``` r

write_geocode_review(with_geography, "manual_review.csv")

final <- with_geography %>%
  apply_manual_overrides("manual_review_completed.csv") %>%
  export_location_crosswalk("location_crosswalk.csv")
```

`final` is ready to point Tableau at, and every row carries the audit
columns that explain how its coordinate was produced.
