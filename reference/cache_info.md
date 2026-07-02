# Summarise a locatr cache

Summarise a locatr cache

## Usage

``` r
cache_info(cache)
```

## Arguments

- cache:

  A `locatr_cache` from
  [`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md).

## Value

A one-row tibble: `rows`, distinct `keys`, distinct `methods`,
`oldest`/`newest` `cached_at`, whether it is `persistent`, its `path`,
and `format`.

## See also

[`locatr_cache()`](https://prigasg.github.io/locatr/reference/locatr_cache.md)
