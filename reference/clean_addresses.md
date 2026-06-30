# Clean and standardise address fields

Normalises raw address, city and ZIP text into geocoder-friendly columns
and builds a single-line `full_address_clean`. Column mappings are
supplied with tidy-eval (bare column names). Original address pieces are
preserved in `*_raw` columns. If the input already contains a
`full_address_clean` column in any case style (for example
`Full_Address_Clean`), locatr preserves that user-supplied value as
`full_address_raw` so there is only one canonical `full_address_clean`
column after cleaning.

## Usage

``` r
clean_addresses(
  data,
  id = NULL,
  address,
  city,
  zip = NULL,
  name = NULL,
  state = "NJ"
)
```

## Arguments

- data:

  A data frame of records with addresses.

- id:

  Optional bare column name holding a unique record identifier. When
  omitted, `record_id` is generated from the row number.

- address, city:

  Bare column names for the raw address and city. Required.

- zip:

  Optional bare column name for the raw ZIP/postal code. When omitted,
  `zip_clean` is `NA`.

- name:

  Optional bare column name for the record name (kept as `record_name`
  for review/exports). Defaults to `NULL`.

- state:

  Two-letter state used for all rows. Defaults to `"NJ"` for the first
  production workflow; pass another state abbreviation as needed.

## Value

`data` with added columns: `record_id`, `record_name`, `address_raw`,
`city_raw`, `zip_raw`, optional `full_address_raw`, `address_clean`,
`city_clean`, `state_clean`, `zip_clean`, `full_address_clean`.

## Details

Only `address` and `city` are required. When `id` is omitted, a
surrogate `record_id` is generated from the row position. When `zip` is
omitted (or empty), `zip_clean` is `NA` and `full_address_clean` is
built without a trailing ZIP, so an address + city + state row is still
geocodable. Supplying a ZIP improves Census structured-match precision
but is no longer mandatory.

## Examples

``` r
df <- tibble::tibble(
  LocationID = "NJ306100", Name = "Hackensack-UMC Mountainside",
  Address = "ONE BAY AVE", City = "Montclair", Zip = "7042"
)
clean_addresses(df, id = LocationID, address = Address,
                       city = City, zip = Zip, name = Name)
#> # A tibble: 1 × 15
#>   LocationID Name     Address City  Zip   record_id address_raw city_raw zip_raw
#>   <chr>      <chr>    <chr>   <chr> <chr> <chr>     <chr>       <chr>    <chr>  
#> 1 NJ306100   Hackens… ONE BA… Mont… 7042  NJ306100  ONE BAY AVE Montcla… 7042   
#> # ℹ 6 more variables: state_clean <chr>, address_clean <chr>, city_clean <chr>,
#> #   zip_clean <chr>, full_address_clean <chr>, record_name <chr>

# address + city only (surrogate id, no ZIP)
clean_addresses(tibble::tibble(Address = "100 Main St", City = "Trenton"),
                address = Address, city = City)
#> # A tibble: 1 × 12
#>   Address City  record_id address_raw city_raw zip_raw state_clean address_clean
#>   <chr>   <chr> <chr>     <chr>       <chr>    <chr>   <chr>       <chr>        
#> 1 100 Ma… Tren… 1         100 Main St Trenton  NA      NJ          100 MAIN STR…
#> # ℹ 4 more variables: city_clean <chr>, zip_clean <chr>,
#> #   full_address_clean <chr>, record_name <chr>
```
