# Add a unified match-confidence score

Collapses locatr's several trust signals into one calibrated
`match_confidence` on a 0-1 scale plus a short `confidence_reason`
string, so a reviewer can sort or threshold on a single column instead
of reading `match_status`, `validation_status`, `nm_status`, and
`review_status` together. Higher is more trustworthy.

## Usage

``` r
add_match_confidence(data)
```

## Arguments

- data:

  A data frame from the batch pipeline or from
  [`geocode_address()`](https://prigasg.github.io/locatr/reference/geocode_address.md).

## Value

`data` with two added columns: `match_confidence` (0-1, rounded to three
decimals) and `confidence_reason`.

## Details

The right scoring model is chosen from the columns present:

- Pipeline output (from
  [`geocode_records()`](https://prigasg.github.io/locatr/reference/geocode_records.md)
  / the crosswalk): scored from the tier that placed the row
  (`geocode_pass`), the match and validation status, and the name-tier
  confidence. Rejected or unplaced rows score near zero;
  reference-verified and manual rows score highest.

- Candidate output (from
  [`geocode_address()`](https://prigasg.github.io/locatr/reference/geocode_address.md)):
  scored from the ArcGIS match score, discounted by how coarse the
  address type is, and capped when the point falls outside a supplied
  `bbox`.

The score is a transparent, rule-based prior - deliberately explainable
rather than a black-box model - so every value can be traced to its
`confidence_reason`.

## Examples

``` r
add_match_confidence(data.frame(
  geocode_pass = c("pass_1_census_structured", "pass_4_name_lookup"),
  match_status = c("matched", "matched_low_confidence"),
  validation_status = c("coordinate_ok", "coordinate_ok"),
  latitude = c(40.2, 40.3), longitude = c(-74.7, -74.8)
))
#>               geocode_pass           match_status validation_status latitude
#> 1 pass_1_census_structured                matched     coordinate_ok     40.2
#> 2       pass_4_name_lookup matched_low_confidence     coordinate_ok     40.3
#>   longitude match_confidence         confidence_reason
#> 1     -74.7             0.90   census structured match
#> 2     -74.8             0.35 low-confidence name match
```
