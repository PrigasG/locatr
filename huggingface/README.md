---
title: locatr - Geocode + Local Geography
emoji: 🗺️
colorFrom: blue
colorTo: green
sdk: docker
app_port: 7860
pinned: false
license: mit
---

# locatr demo Space

A no-code front end for the [`locatr`](https://github.com/PrigasG/locatr) R
package. Upload messy location data and get back audit-ready coordinates, with
optional county/locality enrichment, without writing any R.

## What it does

1. **Upload and preview** - drop in a `CSV`, `Excel` (`.xlsx`/`.xls`), or
   `Parquet` file and inspect the first rows.
2. **Geocode** - map your address columns and run locatr's cascade
   (US Census -> ArcGIS -> name lookup), with score/type controls for name
   matches and a session cache so repeated addresses are not re-queried while
   you work.
3. **Download geocoded records** - export the geocoded file immediately as
   `CSV`, `Excel`, or `Parquet`, optionally removing columns first.
4. **Attach geography, if needed** - build county/locality boundaries from
   Census TIGER/Line with `tigris`, or upload a shapefile/GeoPackage/GeoJSON,
   optionally append tract/ZCTA/district/school-district GEOIDs, then download
   the geography crosswalk.
5. **Audit the run** - review the methods paragraph, provenance/cache summary,
   confidence counts, and field-conflict flags; download a Markdown report for
   project records.

Geocoding calls external services (Census, ArcGIS) and the Census geography
builder downloads boundary files, so runs need network access and can take a
little while. There is a row cap on the geocoding step to keep demo runs snappy.

## Running this Space

It is a **Docker** Space: the [`Dockerfile`](./Dockerfile) builds on
`rocker/geospatial`, installs the app dependencies and `locatr`, and serves the
app shipped inside the package on port `7860`.

## Running locally instead

```r
# install.packages("remotes")
remotes::install_github("PrigasG/locatr")
install.packages(c("shiny", "bslib", "DT", "leaflet",
                   "readxl", "writexl", "arrow"))
locatr::run_locatr_app()
```
