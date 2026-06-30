# locatr

An audit-ready R toolkit for cleaning, geocoding, validating, reviewing, and
exporting messy address and location data. It sits **on top of** `tidygeocoder`:
`tidygeocoder` fetches coordinates; `locatr` decides whether those coordinates
are trustworthy and produces a dashboard-ready crosswalk.

Version 0.1 is deliberately narrow: the first production workflow uses New
Jersey staffing records for a Tableau map. `locatr` does not connect to SQL,
databases, or source systems. It starts from a data frame you already have and
helps generate the location fields: latitude, longitude, validation status, and
local geography such as county or municipality/locality.

## Why it exists

Geocoders are imperfect. Strict services miss real addresses; fuzzy services can
confidently place a bad match far outside the intended service area. `locatr`
wraps that reality with three guards:

1. **Flagging** - PO boxes, placeholders, and missing fields never hit the
   geocoder.
2. **Fallback passes** - ArcGIS and name-based lookup can pick up what a stricter
   geocoder misses.
3. **Region validation** - anything that lands outside the configured bounding
   box or boundary polygon is rejected, not mapped.

Every function records an audit trail (`geocode_method`, `geocode_pass`,
`match_status`, `validation_status`, `review_status`, `manual_override_used`) so a
reviewer can see how each coordinate was produced.

## Install

```r
# from a local clone
devtools::document()   # regenerate man/ and NAMESPACE from roxygen
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

# One call runs Census -> ArcGIS address -> name lookup, validating after each:
geocoded <- geocode_records(cleaned, bbox = region_bbox("NJ"))

with_geography <- add_local_geography(geocoded, geography_shapes = my_local_shapes)

write_geocode_review(with_geography, "manual_review.csv")
# ... a human fills in manual_* columns ...

final <- with_geography %>%
  apply_manual_overrides("manual_review_completed.csv", bbox = region_bbox("NJ")) %>%
  export_location_crosswalk("location_crosswalk.csv")
```

### The Geocoding Cascade

`geocode_records()` runs progressively fuzzier internet services, retrying
only rows that began with geocodable addresses and are still unplaced after each
validation pass:

| Tier | Function | Service | What it catches |
|------|----------|---------|-----------------|
| 0 | `backfill_from_reference()` | trusted table (no network) | records already verified in a prior cycle |
| 1 | `geocode_census()` | US Census (structured) | clean, in-range street addresses |
| 2 | `geocode_fallback()` | ArcGIS (address line) | typos, range gaps, fuzzy streets |
| 3 | `geocode_by_name()` | ArcGIS (name + city) | named places, campuses, landmarks |
| manual | `apply_manual_overrides()` | human | the rest |

Run a subset with `geocode_records(cleaned, tiers = c("census", "name"))`.
Each filled row is stamped in `geocode_pass`, so name-lookup matches (the
loosest tier) can be spot-checked in review.

Tier 0 is optional and authoritative: pass
`geocode_records(cleaned, reference = verified)` or call
`backfill_from_reference()` directly to place records whose coordinates were
checked before, for example last cycle's completed `manual_*` overrides, without
re-hitting any geocoder. These rows are bbox-validated, stamped
`pass_0_reference`, and skipped by every later tier, so manual review accrues
into reusable institutional memory.

## Adapting Regions

Use `region_bbox("NJ")` for the built-in New Jersey preset, or pass your own
bounding box:

```r
custom_bbox <- c(lat_min = 38.0, lat_max = 39.0, lon_min = -77.5, lon_max = -76.0)
geocoded <- geocode_records(cleaned, bbox = custom_bbox)
```

For stricter validation, pass an `sf` boundary polygon with `boundary =`.

## Status / TODO

- [ ] Add more region presets once real workflows need them
- [ ] Add a packaged local geography dataset, if useful
- [ ] Add name-match score thresholds for fuzzy providers that expose scores
