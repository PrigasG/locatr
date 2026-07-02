# Internal IO helpers shared by the bundled Shiny app (inst/locatr-app/app.R).
# Kept in the package rather than the app so the app stays a thin presentation
# layer and these readers are covered by package tests. Not exported: they are
# called as locatr:::.read_location_table() / locatr:::.read_geography_layer().

# Read a tabular upload by file extension. `name` carries the original file name
# (and therefore the extension) because Shiny stores uploads under a temp path.
.read_location_table <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  switch(
    ext,
    csv     = readr::read_csv(path, show_col_types = FALSE),
    tsv     = readr::read_tsv(path, show_col_types = FALSE),
    txt     = readr::read_csv(path, show_col_types = FALSE),
    xlsx    = .read_excel(path),
    xls     = .read_excel(path),
    parquet = .read_parquet(path),
    stop("Unsupported data file type: .", ext,
         " (use csv, tsv, txt, xlsx, xls, or parquet).", call. = FALSE)
  )
}

# Read polygons from a zipped shapefile, a set of .shp sidecar files uploaded
# together, or a single .geojson/.gpkg. `upload` is the Shiny fileInput data
# frame (columns `name`, `datapath`).
.read_geography_layer <- function(upload) {
  exts <- tolower(tools::file_ext(upload$name))
  work <- file.path(
    tempdir(),
    paste0("locatr_shp_", as.integer(stats::runif(1, 1, 1e9)))
  )
  dir.create(work, showWarnings = FALSE, recursive = TRUE)

  if (any(exts == "zip")) {
    utils::unzip(upload$datapath[exts == "zip"][1], exdir = work)
  } else {
    # copy each uploaded file back to its real name so the .shp finds its
    # .dbf/.shx/.prj sidecars
    file.copy(upload$datapath, file.path(work, upload$name))
  }

  candidates <- list.files(
    work, pattern = "\\.(shp|gpkg|geojson|json)$",
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  )
  if (length(candidates) == 0) {
    stop("No .shp, .gpkg or .geojson found. For a shapefile, upload the .zip ",
         "or select the .shp together with its .dbf/.shx/.prj sidecars.",
         call. = FALSE)
  }
  sf::st_read(candidates[1], quiet = TRUE)
}

# readxl / arrow are Suggests (app-only deps); guard so the core package does
# not hard-require them.
.read_excel <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Reading Excel files needs the 'readxl' package.", call. = FALSE)
  }
  readxl::read_excel(path)
}

.read_parquet <- function(path) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Reading Parquet files needs the 'arrow' package.", call. = FALSE)
  }
  out <- arrow::read_parquet(path)
  if (requireNamespace("tibble", quietly = TRUE)) tibble::as_tibble(out) else out
}
