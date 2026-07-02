# Create a locatr response cache

A cache of parsed geocoder results that makes runs reproducible:
repeated queries are served locally instead of re-hitting the service,
and - because the parsed coordinates are stored - a cached result can be
replayed offline, even without the `httr`/`jsonlite` packages that the
live call needs. Pass the returned object to
[`geocode_address()`](https://prigasg.github.io/locatr/reference/geocode_address.md)
via its `cache` argument.

## Usage

``` r
locatr_cache(path = NULL, format = c("rds", "parquet"), store_raw = FALSE)

# S3 method for class 'locatr_cache'
print(x, ...)
```

## Arguments

- path:

  Optional file path for a persistent cache. `NULL` (default) keeps the
  cache in memory for the session only - no disk writes. When a path is
  given, the cache is loaded from it if present and flushed on every
  write.

- format:

  On-disk format when `path` is set: `"rds"` (default, no extra
  dependency) or `"parquet"` (needs `arrow`).

- store_raw:

  Reserved for storing raw service responses. `FALSE` by default; raw
  storage is only ever kept for services whose terms permit it.

- x:

  A `locatr_cache` object.

- ...:

  Ignored.

## Value

A `locatr_cache` object (an environment) to pass to geocoding functions.

## Details

The cache table is *long*: one row per candidate result (so
[`geocode_address()`](https://prigasg.github.io/locatr/reference/geocode_address.md)'s
ranked set round-trips exactly), plus a single sentinel row
(`result_rank = 0`, `status = "no_match"`) for a query that matched
nothing, so misses are replayable and never silently re-queried.

The lookup `key` is an implementation detail (a hash). The visible
`method`, `endpoint`, `query`, and `params` columns are the audit
contract - keys can always be rebuilt from them if a future `rlang`
changes its hash.

## See also

[`cache_info()`](https://prigasg.github.io/locatr/reference/cache_info.md),
[`cache_clear()`](https://prigasg.github.io/locatr/reference/cache_clear.md),
[`geocode_address()`](https://prigasg.github.io/locatr/reference/geocode_address.md)

## Examples

``` r
cache <- locatr_cache()
cache_info(cache)
#> # A tibble: 1 × 10
#>    rows  keys methods method_counts oldest newest persistent path  file_size
#>   <int> <int>   <int> <list>        <chr>  <chr>  <lgl>      <chr>     <dbl>
#> 1     0     0       0 <list [0]>    NA     NA     FALSE      NA           NA
#> # ℹ 1 more variable: format <chr>
```
