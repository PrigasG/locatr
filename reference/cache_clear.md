# Clear a locatr cache

Empties the in-memory table. For a persistent cache this also deletes
the file, so it is guarded: a persistent cache requires
`confirm = TRUE`.

## Usage

``` r
cache_clear(cache, confirm = FALSE)
```

## Arguments

- cache:

  A `locatr_cache` from
  [`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md).

- confirm:

  Must be `TRUE` to clear a persistent (path-backed) cache and delete
  its file. Ignored for memory-only caches.

## Value

The cleared `cache`, invisibly.

## See also

[`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md)
