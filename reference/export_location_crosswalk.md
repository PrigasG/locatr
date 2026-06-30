# Export the dashboard-ready location crosswalk

Selects the final, stable set of columns for Tableau (or any BI tool)
and, optionally, writes them to CSV. Audit columns are retained so a
reviewer can always see how each coordinate was produced, including
score/type/status fields from the name lookup tier when available.

## Usage

``` r
export_location_crosswalk(data, path = NULL)
```

## Arguments

- data:

  A fully processed data frame.

- path:

  Optional output CSV path. When `NULL`, nothing is written.

## Value

The crosswalk tibble (also written to `path` when supplied).
