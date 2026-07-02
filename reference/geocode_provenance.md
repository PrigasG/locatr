# Read the provenance manifest from a geocoding run

[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
attaches a run manifest as an attribute of its output. This returns it:
a run id and UTC timestamp, the locatr / tidygeocoder / cache-schema
versions, the tiers run, whether a reference table was used, the cache
path, per-`review_status` counts, and cache activity (`cache_hits` /
`cache_misses` / `cache_writes`). Read it directly on the
[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
result, since later data-frame operations may drop the attribute.

## Usage

``` r
geocode_provenance(data)

# S3 method for class 'locatr_provenance'
print(x, ...)
```

## Arguments

- data:

  Output of
  [`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md).

- x:

  A `locatr_provenance` object.

- ...:

  Ignored.

## Value

A `locatr_provenance` object (a named list) describing the run.

## See also

[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md),
[`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md)
