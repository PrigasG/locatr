# Launch the locatr demo Shiny app

Runs the bundled web app (the same one published as a Hugging Face
Space): upload a CSV/Excel/Parquet file, geocode it with the locatr
cascade, download the geocoded records directly, or optionally attach
local geography from Census TIGER/Line/an uploaded shapefile before
downloading a crosswalk. The download step can remove selected columns
before export. The app is for demonstration and light interactive use;
production pipelines should call the package functions directly.

## Usage

``` r
run_locatr_app(...)
```

## Arguments

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.
  `port`, `host`, `launch.browser`).

## Value

Called for its side effect of starting the app; does not return.

## Details

The app depends on packages that are only listed under `Suggests`, so
they are not installed automatically. If any are missing, this function
stops with the install command you need.

## Examples

``` r
if (interactive()) {
run_locatr_app()
}
```
