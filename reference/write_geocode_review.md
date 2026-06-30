# Export only the records that still need a human

Writes a tidy review CSV of rows whose `review_status` is
`"needs_manual_review"`, with blank `manual_*` columns for a reviewer to
fill in. Feed the completed file back through
[`apply_manual_overrides()`](https://prigasg.github.io/locatr/reference/apply_manual_overrides.md).

## Usage

``` r
write_geocode_review(data, path)
```

## Arguments

- data:

  A data frame carrying the audit columns.

- path:

  Output CSV path.

## Value

Invisibly, the review tibble that was written.
