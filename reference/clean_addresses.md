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
clean_addresses(data, id, address, city, zip, name = NULL, state = "NJ")
```

## Arguments

- data:

  A data frame of records with addresses.

- id:

  Bare column name holding a unique record identifier.

- address, city, zip:

  Bare column names for the raw address parts.

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
```
