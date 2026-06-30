# Primary geocode pass via the US Census batch geocoder

Geocodes only the rows marked `ready_for_geocoding`, using the
*structured* Census engine (street / city / state / ZIP) rather than a
single-line string, which matches reliably more often. Rows not ready
are returned untouched with empty coordinate columns so the frame stays
rectangular.

## Usage

``` r
geocode_census(data, ...)
```

## Arguments

- data:

  A data frame from
  [`flag_bad_addresses()`](https://prigasg.github.io/locatr/reference/flag_bad_addresses.md).

- ...:

  Passed through to
  [`tidygeocoder::geocode()`](https://jessecambon.github.io/tidygeocoder/reference/geocode.html)
  (e.g. `full_results`).

## Value

`data` with `latitude`, `longitude`, `geocode_method`, `geocode_pass`,
`match_status`, plus Census full-result columns when
`full_results = TRUE`.

## Details

Volatile Census full-result columns (`tiger_line_id`, `id`) are coerced
to character to avoid the `bind_rows()` integer/character type clash
that the Census service triggers intermittently between batches.
