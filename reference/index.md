# Package index

## Clean and flag

- [`clean_addresses()`](https://prigasg.github.io/locatr/reference/clean_addresses.md)
  : Clean and standardise address fields
- [`flag_bad_addresses()`](https://prigasg.github.io/locatr/reference/flag_bad_addresses.md)
  : Flag addresses that should not be blindly geocoded

## Geocode and validate

- [`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
  : Run the full geocoding cascade
- [`geocode_census()`](https://prigasg.github.io/locatr/reference/geocode_census.md)
  : Primary geocode pass via the US Census batch geocoder
- [`geocode_arcgis()`](https://prigasg.github.io/locatr/reference/geocode_arcgis.md)
  : ArcGIS address fallback pass (Google-like fuzzy matching)
- [`geocode_by_name()`](https://prigasg.github.io/locatr/reference/geocode_by_name.md)
  : Name-based geocode pass (the "paste it in a browser" tier)
- [`validate_geocodes()`](https://prigasg.github.io/locatr/reference/validate_geocodes.md)
  : Validate geocoded coordinates against a region
- [`region_bbox()`](https://prigasg.github.io/locatr/reference/region_bbox.md)
  : Region bounding box
- [`bbox_from_sf()`](https://prigasg.github.io/locatr/reference/bbox_from_sf.md)
  : Build a geocoding bounding box from an sf layer
- [`in_bbox()`](https://prigasg.github.io/locatr/reference/in_bbox.md) :
  Is a coordinate inside a bounding box?

## Geography and export

- [`build_local_geography()`](https://prigasg.github.io/locatr/reference/build_local_geography.md)
  : Build a local geography layer from Census TIGER/Line boundaries
- [`add_local_geography()`](https://prigasg.github.io/locatr/reference/add_local_geography.md)
  : Join records to local geography
- [`add_county_muni()`](https://prigasg.github.io/locatr/reference/add_county_muni.md)
  : Add county and municipality fields from Census boundaries
- [`add_muni_from_shapes()`](https://prigasg.github.io/locatr/reference/add_muni_from_shapes.md)
  : Add county and municipality fields from boundary polygons
- [`add_muni_from_key()`](https://prigasg.github.io/locatr/reference/add_muni_from_key.md)
  : Add county and municipality fields by joining on a shared key
- [`backfill_from_reference()`](https://prigasg.github.io/locatr/reference/backfill_from_reference.md)
  : Backfill verified coordinates from a trusted reference table (Tier
  0)
- [`write_geocode_review()`](https://prigasg.github.io/locatr/reference/write_geocode_review.md)
  : Export only the records that still need a human
- [`apply_manual_overrides()`](https://prigasg.github.io/locatr/reference/apply_manual_overrides.md)
  : Apply completed manual overrides
- [`export_location_crosswalk()`](https://prigasg.github.io/locatr/reference/export_location_crosswalk.md)
  : Export the location crosswalk

## App

- [`run_locatr_app()`](https://prigasg.github.io/locatr/reference/run_locatr_app.md)
  : Launch the locatr demo Shiny app
