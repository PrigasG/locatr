# Flag cross-field conflicts in location data

Catches a class of data-entry errors the geocoder itself will silently
accept: a ZIP that cannot belong to the stated state, and a stated
county that disagrees with the county the coordinate actually fell in.
It adds audit columns rather than changing any coordinate, so a reviewer
can decide what to do.

## Usage

``` r
flag_field_conflicts(
  data,
  zip = "zip_clean",
  state = "state_clean",
  stated_county = NULL,
  geocoded_county = "location_county"
)
```

## Arguments

- data:

  A data frame of cleaned/geocoded records.

- zip:

  Name of the ZIP column (default `"zip_clean"`). Set to `NULL` to skip
  the ZIP check.

- state:

  Name of the state column (default `"state_clean"`).

- stated_county:

  Optional name of a county column supplied in the input. The county
  check runs only when this is given.

- geocoded_county:

  Name of the geocoded county column to compare against (default
  `"location_county"`).

## Value

`data` with three added columns: `zip_state_conflict` (logical, `NA`
when indeterminate), `county_conflict` (logical, `NA` when either county
is missing), and `field_conflict` (a `"; "`-joined summary such as
`"zip_state"`, `"county"`, or `"zip_state; county"`; `NA` when clean).

## Details

The ZIP check is deliberately conservative. It compares the ZIP's
leading digit against the USPS regional assignment for the stated state,
so it only flags a ZIP that is definitively in the wrong region (for
example a `"8xxxx"` ZIP recorded in New Jersey). It never flags a
same-region near-miss, and it stays silent when the state is unknown or
the ZIP is missing, so it does not produce false positives.

The county check compares a stated county column against a geocoded
county column (for example `location_county` from
[`add_county_muni()`](https://prigasg.github.io/locatr/reference/add_county_muni.md)),
after normalising case and stripping the trailing
"County"/"Parish"/"Borough".

## Examples

``` r
df <- data.frame(
  zip_clean = c("07030", "85001"),   # 07 is NJ; 85 is AZ
  state_clean = c("NJ", "NJ")
)
flag_field_conflicts(df)
#>   zip_clean state_clean zip_state_conflict county_conflict field_conflict
#> 1     07030          NJ              FALSE              NA           <NA>
#> 2     85001          NJ               TRUE              NA      zip_state
```
