# Flag addresses that should not be blindly geocoded

Identifies PO boxes, placeholders, and missing fields so they go
straight to review instead of wasting geocoder calls (or producing
confident-but-wrong matches). Sets `bad_address_flag` and an initial
`review_status`.

## Usage

``` r
flag_bad_addresses(data)
```

## Arguments

- data:

  A data frame from
  [`clean_addresses()`](https://prigasg.github.io/locatr/reference/clean_addresses.md).

## Value

`data` with added columns `bad_address_flag` and `review_status`. Rows
fit for geocoding get `review_status == "ready_for_geocoding"`.

## Details

A missing ZIP is recorded as `bad_address_flag == "missing_zip"` for
audit, but it does **not** block geocoding: as long as the address and
city are present, the row stays `ready_for_geocoding` (Census matches on
street/city/state and ArcGIS on the single-line address). Only genuinely
unusable rows - missing address or city, PO boxes, placeholders, test
records - are routed to `needs_manual_review`.

## Examples

``` r
df <- tibble::tibble(
  record_id = c("a", "b"),
  address_clean = c("100 MAIN STREET", "PO BOX 42"),
  city_clean = c("TRENTON", "TRENTON"),
  zip_clean = c("08608", "08608"),
  record_name = c("Real Site", "Mailbox Co")
)
flag_bad_addresses(df)
#> # A tibble: 2 × 7
#>   record_id address_clean   city_clean zip_clean record_name bad_address_flag
#>   <chr>     <chr>           <chr>      <chr>     <chr>       <chr>           
#> 1 a         100 MAIN STREET TRENTON    08608     Real Site   NA              
#> 2 b         PO BOX 42       TRENTON    08608     Mailbox Co  po_box          
#> # ℹ 1 more variable: review_status <chr>
```
