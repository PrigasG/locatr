# Summarise a geocoding run into a provenance report

Turns a finished
[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
frame into an audit report: counts by review status, by placing tier,
and by cache status; a `match_confidence` summary; and an auto-generated
plain-language *methods paragraph* suitable for a report or a paper.
When the run manifest is present (attached by
[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)),
the methods paragraph names the package versions, run date, region
guard, and cache activity; without it, the report is built from the
audit columns alone.

## Usage

``` r
geocode_report(data, file = NULL, low_confidence_below = 0.5)

# S3 method for class 'locatr_report'
print(x, ...)
```

## Arguments

- data:

  A finished data frame from
  [`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
  (ideally still carrying its `locatr_run` manifest). At minimum,
  `review_status` and `geocode_pass` drive the summary;
  `match_confidence` and `cache_status` are used when present.

- file:

  Optional path. When given, a Markdown version of the report is written
  there and the report object is returned invisibly.

- low_confidence_below:

  Confidence threshold (0-1) used to count low-confidence rows in the
  summary. Defaults to `0.5`.

- x:

  A `locatr_report` object.

- ...:

  Ignored.

## Value

A `locatr_report` object (a named list) with the counts, confidence
summary, and `methods` paragraph. Printing it shows a formatted summary.

## See also

[`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md),
[`geocode_provenance()`](https://prigasg.github.io/locatr/reference/geocode_provenance.md),
[`add_match_confidence()`](https://prigasg.github.io/locatr/reference/add_match_confidence.md)

## Examples

``` r
df <- data.frame(
  record_id = c("a", "b", "c"),
  geocode_pass = c("pass_1_census_structured", "pass_2_fallback",
                   "pass_4_name_lookup"),
  review_status = c("auto_accepted", "auto_accepted", "needs_manual_review"),
  match_confidence = c(0.9, 0.72, 0.35)
)
geocode_report(df)
#> <locatr geocoding report> 3 record(s)
#> 
#> Methods:
#>   Addresses were cleaned and standardised, then geocoded with locatr using
#>   a validation-guarded cascade of US Census structured matching, ArcGIS
#>   address fallback, and ArcGIS name lookup. Each candidate coordinate was
#>   validated against the configured region; out-of-region matches were
#>   rejected rather than mapped. Of 3 record(s), 67% were auto-accepted, 33%
#>   were flagged for manual review, and 0% were rejected. Coordinates were
#>   placed 33% by US Census structured matching, 33% by ArcGIS address
#>   matching, and 33% by ArcGIS name lookup. Median match confidence (0-1)
#>   was 0.72, with 1 record(s) below 0.5 flagged for closer review.
#> 
#> 
#> Review status:
#>   auto_accepted            2
#>   needs_manual_review      1
#> 
#> Placed by:
#>   arcgis_address           1
#>   census                   1
#>   name_lookup              1
#> 
#> Match confidence: median 0.72, mean 0.657, 1 below 0.5
```
