# Primary geocode pass via the US Census batch geocoder

Geocodes only the rows marked `ready_for_geocoding`, using the
*structured* Census engine (street / city / state / ZIP) rather than a
single-line string, which matches reliably more often. Rows not ready
are returned untouched with empty coordinate columns so the frame stays
rectangular.

## Usage

``` r
geocode_census(data, ..., cache = NULL, refresh = FALSE)
```

## Arguments

- data:

  A data frame from
  [`flag_bad_addresses()`](https://prigasg.github.io/locatr/reference/flag_bad_addresses.md).

- ...:

  Passed through to
  [`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)
  (e.g. `full_results`).

- cache:

  Optional
  [`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md).
  When supplied, rows whose structured query is already cached are
  filled from it instead of re-querying Census.

- refresh:

  If `TRUE`, ignore cached entries and re-query, overwriting them.
  Defaults to `FALSE`.

## Value

`data` with `latitude`, `longitude`, `geocode_method`, `geocode_pass`,
`match_status`, plus Census full-result columns when
`full_results = TRUE` (full-result columns are not stored in the cache,
so cache-filled rows omit them).

## Details

Volatile Census full-result columns (`tiger_line_id`, `id`) are coerced
to character to avoid the `bind_rows()` integer/character type clash
that the Census service triggers intermittently between batches.
