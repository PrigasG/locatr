# locatr 0.1.0

## New package

* Initial CRAN release of `locatr`, an audit-ready workflow layer on top of
  `tidygeocoder` for cleaning, geocoding, validating, reviewing, and exporting
  messy location data.

## Geocoding and review

* Added address cleaning with optional ID, ZIP, and record-name fields.
* Added bad-address flagging for PO boxes, placeholder addresses, missing
  required fields, test records, and missing ZIP codes.
* Added a staged geocoding cascade with reference backfill, Census structured
  geocoding, ArcGIS fallback, and ArcGIS name lookup.
* Added bounding-box and polygon validation so out-of-region coordinates are
  rejected rather than silently accepted.
* Added match-confidence scoring and plain-language confidence reasons.
* Added single-address candidate lookup with score filtering, location context,
  and optional geography enrichment.
* Added manual review export, manual override application, and a map-based
  review workflow for accepting, rejecting, or relocating coordinates.

## Reproducibility and audit trail

* Added `locatr_cache()` with in-memory and persistent RDS/Parquet-backed
  response caches.
* Added cache inspection and clearing helpers.
* Added run provenance manifests, per-row placement/cache stamps, and
  `geocode_provenance()`.
* Added `geocode_report()` for methods text, status counts, cache summaries,
  and confidence summaries.

## Geography and export

* Added helpers to build Census TIGER/Line geography and attach county,
  municipality, tract, block group, ZCTA, legislative district, congressional
  district, and school district identifiers to geocoded records.
* Added spatial and attribute-key joins for user-supplied boundary layers.
* Added field-conflict flags for ZIP/state and geocoded-vs-stated county
  mismatches.
* Added reusable location crosswalk export for downstream dashboards and GIS
  workflows.

## Apps and documentation

* Added a bundled Shiny app for upload, geocoding, geography attachment,
  download, provenance review, and audit-report export.
* Added a bundled map-based review Shiny app backed by testable override-table
  generation.
* Added Hugging Face Space and Posit Connect Cloud deployment scaffolding.
* Added pkgdown reference organization, README workflows, and a getting-started
  vignette.
