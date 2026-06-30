# ArcGIS address fallback pass (Google-like fuzzy matching)

For rows the Census pass could not place inside the configured region,
re-geocodes with a composite geocoder (ArcGIS by default: free, no API
key, fuzzy matching close to Google) using the single-line
`full_address_clean`. ArcGIS requests are constrained to the region bbox
when possible, and results are still guarded against the bounding box so
out-of-region false matches are discarded before coordinates are
coalesced back into `latitude`/`longitude`.

## Usage

``` r
geocode_arcgis(data, method = "arcgis", bbox = region_bbox("NJ"), ...)
```

## Arguments

- data:

  A data frame from
  [`geocode_census()`](https://prigasg.github.io/locatr/reference/geocode_census.md)
  (or after
  [`validate_geocodes()`](https://prigasg.github.io/locatr/reference/validate_geocodes.md)).

- method:

  tidygeocoder method for this pass (default `"arcgis"`). `"google"`
  also works if `GOOGLEGEOCODE_API_KEY` is set.

- bbox:

  Bounding box used to reject out-of-region matches; see
  [`region_bbox()`](https://prigasg.github.io/locatr/reference/region_bbox.md).

- ...:

  Passed through to
  [`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html).

## Value

`data` with fallback columns `fb_latitude`, `fb_longitude`, `fb_status`,
and updated `latitude`, `longitude`, `geocode_method`, `geocode_pass`,
`match_status` for rows this pass filled.

## Details

Formerly `geocode_fallback()`; renamed because this tier is specifically
the ArcGIS (composite) address pass.
