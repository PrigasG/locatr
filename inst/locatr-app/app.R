# locatr demo app
# -----------------------------------------------------------------------------
# A small Shiny front-end for the locatr pipeline, meant to run as a Hugging
# Face Space so people can get a geocoded file or geography-tagged crosswalk
# without writing any R. Four steps:
#   1. Upload a data file (CSV / Excel / Parquet) and preview it.
#   2. Map the address columns and geocode with locatr's cascade.
#   3. Attach local geography - either built from Census TIGER/Line, or from a
#      shapefile the user uploads (.zip, .shp + sidecars, .geojson, or .gpkg).
#   4. Optionally join, choose output columns, and download CSV / Excel / Parquet.
#
# Launch locally with locatr::run_locatr_app().
# -----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(sf)
library(leaflet)

if (!requireNamespace("locatr", quietly = TRUE)) {
  stop("The locatr package must be installed before running this app.",
       call. = FALSE)
}

# ---- helpers ----------------------------------------------------------------

# Read a tabular upload by extension.
read_table_any <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  switch(
    ext,
    csv     = readr::read_csv(path, show_col_types = FALSE),
    tsv     = readr::read_tsv(path, show_col_types = FALSE),
    txt     = readr::read_csv(path, show_col_types = FALSE),
    xlsx    = readxl::read_excel(path),
    xls     = readxl::read_excel(path),
    parquet = tibble::as_tibble(arrow::read_parquet(path)),
    stop("Unsupported data file type: .", ext,
         " (use csv, xlsx, xls, or parquet).")
  )
}

# Read polygons from a zipped shapefile, a set of .shp sidecar files uploaded
# together, or a single .geojson/.gpkg. `upload` is the fileInput data frame
# (columns name, datapath).
read_shapes_any <- function(upload) {
  exts <- tolower(tools::file_ext(upload$name))
  work <- file.path(tempdir(),
                    paste0("locatr_shp_", as.integer(stats::runif(1, 1, 1e9))))
  dir.create(work, showWarnings = FALSE, recursive = TRUE)

  if (any(exts == "zip")) {
    utils::unzip(upload$datapath[exts == "zip"][1], exdir = work)
  } else {
    # copy each uploaded file back to its real name so .shp finds its sidecars
    file.copy(upload$datapath, file.path(work, upload$name))
  }

  candidates <- list.files(
    work, pattern = "\\.(shp|gpkg|geojson|json)$",
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  )
  if (length(candidates) == 0) {
    stop("No .shp, .gpkg or .geojson found. For a shapefile, upload the .zip ",
         "or select the .shp together with its .dbf/.shx/.prj sidecars.")
  }
  sf::st_read(candidates[1], quiet = TRUE)
}

# A region bbox for validation: locatr's preset where it exists, else a
# continental-US fallback so geocoding still runs for any state.
safe_bbox <- function(state) {
  bb <- tryCatch(locatr::region_bbox(state), error = function(e) NULL)
  if (!is.null(bb)) return(bb)
  c(lat_min = 24.5, lat_max = 49.5, lon_min = -125.0, lon_max = -66.9)
}

# Guess a likely column from a set of candidate name patterns.
guess_col <- function(cols, pattern, allow_none = FALSE) {
  hit <- cols[grepl(pattern, cols, ignore.case = TRUE)]
  if (length(hit) > 0) return(hit[1])
  if (allow_none) "" else cols[1]
}

pick_optional_col <- function(cols, pattern) {
  hit <- cols[grepl(pattern, cols, ignore.case = TRUE)]
  if (length(hit) > 0) hit[1] else NULL
}

# Run clean_addresses with column names supplied as strings.
clean_with_strings <- function(data, id, address, city, zip, name, state) {
  args <- list(
    data    = data,
    id      = rlang::sym(id),
    address = rlang::sym(address),
    city    = rlang::sym(city),
    zip     = rlang::sym(zip),
    state   = state
  )
  if (!is.null(name) && nzchar(name)) args$name <- rlang::sym(name)
  do.call(locatr::clean_addresses, args)
}

drop_selected_cols <- function(data, drop_cols) {
  if (is.null(drop_cols) || length(drop_cols) == 0) {
    return(data)
  }
  dplyr::select(data, -dplyr::any_of(drop_cols))
}

# "" / NULL -> NULL, so add_local_geography falls back to auto-detection
# instead of looking for a column literally named "".
nz_or_null <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) NULL else x
}

# Non-spatial join: merge polygon attributes onto geocoded rows by a shared key
# column. This is the fallback for when point-in-polygon is not the right
# criteria (e.g. the shapefile is keyed by a region/ZIP/FIPS code the data also
# carries). Produces the same location_* columns add_local_geography() does.
attribute_join_geography <- function(geocoded, shapes, data_key, shp_key,
                                     county_col = NULL, locality_col = NULL,
                                     key_col = NULL) {
  county_col   <- nz_or_null(county_col)
  locality_col <- nz_or_null(locality_col)
  key_col      <- nz_or_null(key_col)
  shape_cols   <- names(shapes)
  statefp_col <- pick_optional_col(shape_cols, "^statefp$|state.*fips")
  county_code_col <- pick_optional_col(shape_cols, "^county_code$|^countyfp$|cnty.*code")
  county_fips_col <- pick_optional_col(shape_cols, "county.*fips|cnty.*fips")
  muni_code_col <- pick_optional_col(
    shape_cols,
    "municipality.*code|mun.*code|muni.*code|cousubfp|placefp|tractce"
  )
  muni_geoid_col <- pick_optional_col(shape_cols, "municipality.*geoid|muni.*geoid|^geoid$|gnis")
  muni_name_standard_col <- pick_optional_col(
    shape_cols,
    "municipality.*standard|namelsad|municipality.*name|mun.*name|location_locality"
  )
  muni_type_col <- pick_optional_col(shape_cols, "municipality.*type|^lsad$|^type$|classfp|mtfcc")

  attr_tbl <- sf::st_drop_geometry(shapes) %>%
    dplyr::transmute(
      .join_key         = as.character(.data[[shp_key]]),
      .statefp          = if (!is.null(statefp_col)) as.character(.data[[statefp_col]]) else NA_character_,
      location_county   = if (!is.null(county_col)) as.character(.data[[county_col]]) else NA_character_,
      location_locality = if (!is.null(locality_col)) as.character(.data[[locality_col]]) else NA_character_,
      muni_join_key     = if (!is.null(key_col)) as.character(.data[[key_col]]) else NA_character_,
      county_code       = if (!is.null(county_code_col)) as.character(.data[[county_code_col]]) else NA_character_,
      county_fips       = if (!is.null(county_fips_col)) as.character(.data[[county_fips_col]]) else NA_character_,
      municipality_code = if (!is.null(muni_code_col)) as.character(.data[[muni_code_col]]) else NA_character_,
      municipality_geoid = if (!is.null(muni_geoid_col)) as.character(.data[[muni_geoid_col]]) else NA_character_,
      municipality_name_standard = if (!is.null(muni_name_standard_col)) as.character(.data[[muni_name_standard_col]]) else NA_character_,
      municipality_type = if (!is.null(muni_type_col)) as.character(.data[[muni_type_col]]) else NA_character_
    ) %>%
    dplyr::distinct(.join_key, .keep_all = TRUE)

  geocoded %>%
    dplyr::mutate(.join_key = as.character(.data[[data_key]])) %>%
    dplyr::left_join(attr_tbl, by = ".join_key") %>%
    dplyr::mutate(
      geography_match_status = dplyr::if_else(
        is.na(.data$location_locality) & is.na(.data$location_county),
        "no_geography_match", "geography_matched"
      ),
      County = .data$location_county,
      Municipality = .data$location_locality,
      muni_join_key = dplyr::coalesce(
        .data$muni_join_key, .data$municipality_geoid,
        .data$municipality_code
      ),
      county_fips = dplyr::coalesce(
        .data$county_fips,
        dplyr::if_else(
          !is.na(.data$.statefp) & !is.na(.data$county_code),
          paste0(.data$.statefp, .data$county_code),
          NA_character_
        )
      ),
      `Muni Key` = dplyr::if_else(
        .data$geography_match_status == "geography_matched",
        dplyr::coalesce(.data$muni_join_key,
                        paste(.data$location_county, .data$location_locality, sep = "::")),
        NA_character_
      ),
      muni_match_status = dplyr::if_else(
        .data$geography_match_status == "geography_matched",
        "muni_matched", "no_muni_match"
      )
    ) %>%
    dplyr::select(-".join_key", -".statefp")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

US_STATES <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL",
               "IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
               "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
               "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC")

# ---- UI ---------------------------------------------------------------------

ui <- page_navbar(
  title = "locatr - geocode + local geography",
  theme = bs_theme(version = 5, preset = "flatly"),
  id = "nav",

  nav_panel(
    title = "1. Upload & preview",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        fileInput("data_file", "Data file (CSV, Excel, or Parquet)",
                  accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".parquet")),
        helpText("Each row should be one record with an address."),
        uiOutput("data_summary"),
        actionButton("go_geocode", "Continue to geocode",
                     class = "btn-primary", icon = icon("arrow-right"))
      ),
      card(
        card_header("Preview"),
        DT::DTOutput("data_preview")
      )
    )
  ),

  nav_panel(
    title = "2. Geocode",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        helpText("Map your columns, then geocode with locatr's cascade ",
                 "(Census -> ArcGIS -> name lookup)."),
        uiOutput("colmap_ui"),
        selectInput("state", "State", choices = US_STATES, selected = "NJ"),
        numericInput("max_rows", "Max rows to geocode", value = 200,
                     min = 1, step = 50),
        numericInput("name_min_score", "Name match score threshold", value = 90,
                     min = 0, max = 100, step = 1),
        checkboxGroupInput(
          "name_accept_types", "Name match types that can pass",
          choices = c("PointAddress", "Subaddress", "StreetAddress"),
          selected = c("PointAddress", "Subaddress", "StreetAddress")
        ),
        actionButton("run_geocode", "Geocode", class = "btn-primary",
                     icon = icon("location-dot")),
        helpText("Geocoding calls external services and can take a while.")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Geocoded records"), DT::DTOutput("geo_table")),
        card(card_header("Map"), leafletOutput("geo_map", height = 460))
      )
    )
  ),

  nav_panel(
    title = "3. Attach geography",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        radioButtons(
          "geo_source", "Geography source",
          choices = c("Census TIGER/Line (build automatically)" = "census",
                      "Upload a shapefile" = "shapefile"),
          selected = "census"
        ),
        helpText("County and municipality fields are optional. Use Census ",
                 "boundaries when you do not have a file, or upload your own ",
                 "authoritative geography when you prefer that source."),
        conditionalPanel(
          "input.geo_source == 'census'",
          selectInput("geo_state", "State", choices = US_STATES, selected = "NJ"),
          selectInput("geo_level", "Geography (becomes location_locality)",
                      choices = c("county_subdivision", "place", "county", "tract"),
                      selected = "county_subdivision"),
          actionButton("build_geo", "Build geography", class = "btn-primary",
                       icon = icon("layer-group")),
          helpText("State is enough for the automated path. Building here is ",
                   "optional; the download step can build the layer when needed.")
        ),
        conditionalPanel(
          "input.geo_source == 'shapefile'",
          fileInput("shp_file", "Shapefile (.zip preferred) / .geojson / .gpkg",
                    multiple = TRUE,
                    accept = c(".zip", ".shp", ".shx", ".dbf", ".prj",
                               ".cpg", ".geojson", ".json", ".gpkg")),
          radioButtons(
            "join_mode", "Join criteria",
            choices = c("Spatial (point in polygon)" = "spatial",
                        "Attribute key (merge on a column)" = "key"),
            selected = "spatial"
          ),
          helpText("Spatial assigns each point to the polygon it falls in. ",
                   "Use an attribute key to merge on a shared column when the ",
                   "spatial join is not the right criteria. County, locality, ",
                   "and muni key fields are optional unless you need an ",
                   "attribute-key merge, where the two key columns are required."),
          uiOutput("shp_colmap_ui")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Geography attributes"), DT::DTOutput("geo_layer_table")),
        card(card_header("Boundaries"), leafletOutput("geo_layer_map", height = 460))
      )
    )
  ),

  nav_panel(
    title = "4. Download",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        radioButtons(
          "output_source", "Output",
          choices = c("Geocoded records" = "geocoded",
                      "Geography crosswalk" = "crosswalk"),
          selected = "geocoded"
        ),
        actionButton("run_join", "Attach geography", class = "btn-primary",
                     icon = icon("object-group")),
        helpText("Geography is optional. Download geocoded records directly, ",
                 "or attach geography first."),
        uiOutput("drop_cols_ui"),
        tags$hr(),
        downloadButton("dl_csv", "Download CSV"),
        downloadButton("dl_xlsx", "Download Excel"),
        downloadButton("dl_parquet", "Download Parquet"),
        uiOutput("output_summary")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Output preview"), DT::DTOutput("output_table")),
        card(card_header("Map"), leafletOutput("output_map", height = 460))
      )
    )
  ),

  # right-aligned navbar items
  nav_spacer(),
  nav_item(actionLink("show_help", "Help", icon = icon("circle-question"))),
  nav_item(
    tags$a(icon("github"), "GitHub",
           href = "https://github.com/PrigasG/locatr", target = "_blank")
  )
)

# ---- server -----------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = NULL, geocoded = NULL, geo_layer = NULL, crosswalk = NULL
  )

  notify_error <- function(expr, msg) {
    tryCatch(expr, error = function(e) {
      showNotification(paste0(msg, ": ", conditionMessage(e)),
                       type = "error", duration = 10)
      NULL
    })
  }

  # --- Help modal -----------------------------------------------------------
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "How to use this app",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close"),
      tags$ol(
        tags$li(tags$b("Upload & preview"),
                " - load a CSV, Excel, or Parquet file; one row per record."),
        tags$li(tags$b("Geocode"),
                " - map your ID/address/city/ZIP (and optional name) columns, ",
                "pick the state, and run locatr's cascade (Census -> ArcGIS -> ",
                "name lookup). Geocoding calls external services, so it needs ",
                "network access and is capped by the row limit."),
        tags$li(tags$b("Attach geography (optional)"),
                " - build county/locality boundaries from Census TIGER/Line, or ",
                "upload your own shapefile (.zip, or .shp with its sidecars, or ",
                ".geojson/.gpkg). For a shapefile you choose the join criteria: ",
                "spatial (point-in-polygon) or an attribute key shared with your data."),
        tags$li(tags$b("Download"),
                " - export the geocoded records or the geography crosswalk as ",
                "CSV, Excel, or Parquet. You can drop columns before downloading.")
      ),
      tags$p("Low-confidence name matches are flagged for review rather than ",
             "trusted automatically."),
      tags$p(
        "Built on the ",
        tags$a("locatr", href = "https://github.com/PrigasG/locatr",
               target = "_blank"),
        " R package."
      )
    ))
  })

  # --- Step 1: upload + preview ---------------------------------------------
  observeEvent(input$data_file, {
    rv$data <- notify_error(
      read_table_any(input$data_file$datapath, input$data_file$name),
      "Could not read data file"
    )
    rv$geocoded <- NULL
    rv$crosswalk <- NULL
  })

  output$data_preview <- DT::renderDT({
    req(rv$data)
    DT::datatable(utils::head(rv$data, 200), options = list(scrollX = TRUE),
                  rownames = FALSE)
  })

  output$data_summary <- renderUI({
    req(rv$data)
    tags$div(
      tags$strong(format(nrow(rv$data), big.mark = ",")), " rows, ",
      tags$strong(ncol(rv$data)), " columns loaded."
    )
  })

  # --- Step 2: column map + geocode -----------------------------------------
  output$colmap_ui <- renderUI({
    req(rv$data)
    cols <- names(rv$data)
    tagList(
      selectInput("col_id",   "Unique ID",  choices = cols,
                  selected = guess_col(cols, "id|code|key")),
      selectInput("col_addr", "Address",     choices = cols,
                  selected = guess_col(cols, "addr|street")),
      selectInput("col_city", "City",        choices = cols,
                  selected = guess_col(cols, "city|town|munic")),
      selectInput("col_zip",  "ZIP",         choices = cols,
                  selected = guess_col(cols, "zip|postal")),
      selectInput("col_name", "Name (optional)", choices = c("(none)" = "", cols),
                  selected = guess_col(cols, "name|facility|site|provider",
                                       allow_none = TRUE))
    )
  })

  observeEvent(input$run_geocode, {
    req(rv$data, input$col_id, input$col_addr, input$col_city, input$col_zip)
    withProgress(message = "Geocoding with locatr ...", value = 0, {
      result <- notify_error({
        dat <- rv$data
        if (!is.na(input$max_rows) && nrow(dat) > input$max_rows) {
          dat <- utils::head(dat, input$max_rows)
        }
        incProgress(0.2, detail = "cleaning addresses")
        cleaned <- clean_with_strings(
          dat, input$col_id, input$col_addr, input$col_city, input$col_zip,
          input$col_name, input$state
        )
        incProgress(0.2, detail = "flagging bad addresses")
        flagged <- locatr::flag_bad_addresses(cleaned)
        incProgress(0.2, detail = "running the cascade")
        locatr::geocode_records(
          flagged, bbox = safe_bbox(input$state),
          name_min_score = input$name_min_score,
          name_accept_types = input$name_accept_types,
          verbose = FALSE
        )
      }, "Geocoding failed")
      if (!is.null(result)) {
        rv$geocoded <- result
        rv$crosswalk <- NULL
        incProgress(0.4, detail = "done")
        showNotification("Geocoding complete.", type = "message")
        bslib::nav_select("nav", "4. Download", session = session)
      }
    })
  })

  output$geo_table <- DT::renderDT({
    req(rv$geocoded)
    show <- rv$geocoded %>%
      dplyr::select(dplyr::any_of(c(
        "record_id", "record_name", "full_address_clean",
        "latitude", "longitude", "geocode_pass", "match_status",
        "nm_score", "nm_addr_type", "nm_status", "review_status"
      )))
    DT::datatable(show, options = list(scrollX = TRUE), rownames = FALSE)
  })

  output$geo_map <- renderLeaflet({
    req(rv$geocoded)
    pts <- rv$geocoded %>%
      dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))
    m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    if (nrow(pts) > 0) {
      has_name <- "record_name" %in% names(pts)
      popup <- if (has_name) {
        paste0(pts$record_name, "<br/>pass: ", pts$geocode_pass)
      } else {
        paste0("pass: ", pts$geocode_pass)
      }
      m <- m %>% addCircleMarkers(
        data = pts, lng = ~longitude, lat = ~latitude,
        radius = 5, stroke = FALSE, fillOpacity = 0.7, popup = popup
      ) %>% fitBounds(min(pts$longitude), min(pts$latitude),
                      max(pts$longitude), max(pts$latitude))
    }
    m
  })

  # --- Step 3: geography layer ----------------------------------------------
  observeEvent(input$build_geo, {
    withProgress(message = "Building Census geography ...", value = 0.5, {
      layer <- notify_error(
        locatr::build_local_geography(state = input$geo_state,
                                      geography = input$geo_level),
        "Could not build geography (is 'tigris' installed and online?)"
      )
      if (!is.null(layer)) {
        rv$geo_layer <- layer
        showNotification("Geography layer ready.", type = "message")
      }
    })
  })

  observeEvent(input$shp_file, {
    rv$geo_layer <- notify_error(
      read_shapes_any(input$shp_file), "Could not read shapefile"
    )
  })

  output$shp_colmap_ui <- renderUI({
    req(input$geo_source == "shapefile", rv$geo_layer)
    cols <- setdiff(names(rv$geo_layer), attr(rv$geo_layer, "sf_column"))
    none <- c("(none)" = "")

    mapping <- tagList(
      selectInput("shp_county", "County column (polygon attribute)",
                  choices = c(none, cols),
                  selected = guess_col(cols, "county")),
      selectInput("shp_locality", "Locality column (polygon attribute)",
                  choices = c(none, cols),
                  selected = guess_col(cols, "local|mun|name|place|town")),
      selectInput("shp_muni_key", "Muni key column (optional)",
                  choices = c(none, cols),
                  selected = guess_col(cols, "muni.*key|mun.*code|geoid|gnis"))
    )

    if (identical(input$join_mode, "key")) {
      data_cols <- if (!is.null(rv$data)) names(rv$data) else character(0)
      key_ui <- tagList(
        selectInput("data_key", "Your data column (join key)",
                    choices = data_cols,
                    selected = guess_col(data_cols, "id|code|key|zip|fips|geoid")),
        selectInput("shp_key", "Shapefile column (join key)", choices = cols,
                    selected = guess_col(cols, "id|code|key|zip|fips|geoid"))
      )
      tagList(key_ui, mapping)
    } else {
      mapping
    }
  })

  observeEvent(input$go_geocode, {
    req(rv$data)
    bslib::nav_select("nav", "2. Geocode", session = session)
  })

  output$geo_layer_table <- DT::renderDT({
    req(rv$geo_layer)
    DT::datatable(utils::head(sf::st_drop_geometry(rv$geo_layer), 200),
                  options = list(scrollX = TRUE), rownames = FALSE)
  })

  output$geo_layer_map <- renderLeaflet({
    req(rv$geo_layer)
    poly <- notify_error(sf::st_transform(rv$geo_layer, 4326),
                         "Could not project boundaries")
    req(poly)
    leaflet(poly) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(weight = 1, fillOpacity = 0.1, color = "#2c7fb8")
  })

  # --- Step 4: optional join + download -------------------------------------
  observeEvent(input$run_join, {
    req(rv$geocoded)
    withProgress(message = "Joining to geography ...", value = 0.5, {
      crosswalk <- notify_error({
        is_shp <- identical(input$geo_source, "shapefile")
        if (!is_shp && is.null(rv$geo_layer)) {
          incProgress(0.2, detail = "building Census geography")
          rv$geo_layer <- locatr::build_local_geography(
            state = input$geo_state,
            geography = input$geo_level
          )
        }
        if (is_shp) {
          req(rv$geo_layer)
        }
        if (is_shp && identical(input$join_mode, "key")) {
          req(input$data_key, input$shp_key)
          joined <- attribute_join_geography(
            rv$geocoded, rv$geo_layer,
            data_key = input$data_key, shp_key = input$shp_key,
            county_col = input$shp_county, locality_col = input$shp_locality,
            key_col = input$shp_muni_key
          )
        } else {
          county_col   <- if (is_shp) nz_or_null(input$shp_county) else NULL
          locality_col <- if (is_shp) nz_or_null(input$shp_locality) else NULL
          key_col      <- if (is_shp) nz_or_null(input$shp_muni_key) else NULL
          joined <- locatr::add_muni_from_shapes(
            rv$geocoded, muni_shapes = rv$geo_layer,
            county_col = county_col, muni_col = locality_col,
            key_col = key_col
          )
        }
        locatr::export_location_crosswalk(joined)
      }, "Join failed")
      if (!is.null(crosswalk)) {
        rv$crosswalk <- crosswalk
        updateRadioButtons(session, "output_source", selected = "crosswalk")
        matched <- sum(!is.na(crosswalk$location_locality))
        rate <- if (nrow(crosswalk) > 0) round(100 * matched / nrow(crosswalk)) else 0
        showNotification(
          sprintf("Crosswalk ready - %d%% of rows matched a locality.%s", rate,
                  if (rate == 0) " Try other join columns or switch the join criteria." else ""),
          type = if (rate == 0) "warning" else "message", duration = 8
        )
      }
    })
  })

  output_data_raw <- reactive({
    req(rv$geocoded)
    if (identical(input$output_source, "crosswalk")) {
      validate(need(!is.null(rv$crosswalk),
                    "Attach geography first, or switch Output to Geocoded records."))
      rv$crosswalk
    } else {
      rv$geocoded
    }
  })

  output$drop_cols_ui <- renderUI({
    dat <- output_data_raw()
    selectizeInput(
      "drop_cols", "Remove columns from download",
      choices = names(dat),
      selected = character(0),
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  output_data <- reactive({
    drop_selected_cols(output_data_raw(), input$drop_cols)
  })

  output$output_table <- DT::renderDT({
    dat <- output_data()
    DT::datatable(dat, options = list(scrollX = TRUE), rownames = FALSE)
  })

  output$output_summary <- renderUI({
    dat <- output_data()
    locality_count <- if ("location_locality" %in% names(dat)) {
      sum(!is.na(dat$location_locality))
    } else NA_integer_
    low_conf_count <- if ("match_status" %in% names(dat)) {
      sum(dat$match_status == "matched_low_confidence", na.rm = TRUE)
    } else NA_integer_

    tags$div(
      tags$hr(),
      tags$strong(format(nrow(dat), big.mark = ",")), " records, ",
      tags$strong(ncol(dat)), " columns selected.",
      if (!is.na(locality_count)) {
        tags$div(tags$strong(format(locality_count, big.mark = ",")),
                 " with a locality.")
      },
      if (!is.na(low_conf_count)) {
        tags$div(tags$strong(format(low_conf_count, big.mark = ",")),
                 " low-confidence name matches need review.")
      }
    )
  })

  output$output_map <- renderLeaflet({
    dat <- output_data()
    m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    if (!all(c("latitude", "longitude") %in% names(dat))) {
      return(m)
    }
    pts <- dat %>%
      dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))
    if (nrow(pts) > 0) {
      locality <- if ("location_locality" %in% names(pts)) pts$location_locality else ""
      county   <- if ("location_county" %in% names(pts)) pts$location_county else ""
      status   <- if ("match_status" %in% names(pts)) pts$match_status else ""
      m <- m %>% addCircleMarkers(
        data = pts, lng = ~longitude, lat = ~latitude,
        radius = 5, stroke = FALSE, fillOpacity = 0.7,
        popup = paste0(locality, "<br/>", county, "<br/>", status)
      ) %>% fitBounds(min(pts$longitude), min(pts$latitude),
                      max(pts$longitude), max(pts$latitude))
    }
    m
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0(input$output_source %||% "geocoded", ".csv"),
    content = function(file) {
      readr::write_csv(output_data(), file)
    }
  )
  output$dl_xlsx <- downloadHandler(
    filename = function() paste0(input$output_source %||% "geocoded", ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(output_data(), file)
    }
  )
  output$dl_parquet <- downloadHandler(
    filename = function() paste0(input$output_source %||% "geocoded", ".parquet"),
    content = function(file) {
      arrow::write_parquet(output_data(), file)
    }
  )
}

shinyApp(ui, server)
