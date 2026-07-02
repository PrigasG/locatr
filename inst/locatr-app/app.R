# locatr demo app
# -----------------------------------------------------------------------------
# A small Shiny front-end for the locatr pipeline, meant to run as a Hugging
# Face Space so people can get a geocoded file or geography-tagged crosswalk
# without writing any R. Five steps:
#   1. Upload a data file (CSV / Excel / Parquet) and preview it.
#   2. Map the address columns and geocode with locatr's cascade.
#   3. Attach local geography - either built from Census TIGER/Line, or from a
#      shapefile the user uploads (.zip, .shp + sidecars, .geojson, or .gpkg).
#   4. Optionally join, choose output columns, and download CSV / Excel / Parquet.
#   5. Review provenance and download an audit report.
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
# Tabular/shapefile reading and the attribute-key geography join now live in the
# locatr package (locatr:::.read_location_table, locatr:::.read_geography_layer,
# locatr::add_muni_from_key) so this app stays a thin presentation layer. The
# helpers below are UI-only: input guessing, navigation, and download shaping.

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

# Run clean_addresses with column names supplied as strings. Only address and
# city are required; an empty id/zip/name string ("(none)") is dropped so the
# package applies its optional-field defaults (surrogate id, NA zip).
clean_with_strings <- function(data, address, city, id = "", zip = "",
                               name = "", state = "NJ") {
  args <- list(
    data    = data,
    address = rlang::sym(address),
    city    = rlang::sym(city),
    state   = state
  )
  if (!is.null(id) && nzchar(id))     args$id   <- rlang::sym(id)
  if (!is.null(zip) && nzchar(zip))   args$zip  <- rlang::sym(zip)
  if (!is.null(name) && nzchar(name)) args$name <- rlang::sym(name)
  do.call(locatr::clean_addresses, args)
}

drop_selected_cols <- function(data, drop_cols) {
  if (is.null(drop_cols) || length(drop_cols) == 0) {
    return(data)
  }
  dplyr::select(data, -dplyr::any_of(drop_cols))
}

as_count_table <- function(x) {
  if (length(x) == 0L) {
    return(data.frame(item = character(), count = integer()))
  }
  data.frame(item = names(x), count = as.integer(x), row.names = NULL)
}

app_report_markdown <- function(report) {
  lines <- c("# locatr audit report", "")
  if (!is.null(report$run)) {
    run <- report$run
    lines <- c(
      lines,
      paste0("- Run ID: ", run$run_id),
      paste0("- Run at: ", run$run_at),
      paste0("- locatr: ", run$locatr_version),
      paste0("- tidygeocoder: ", run$tidygeocoder_version),
      paste0("- Cache: ", run$cache_path),
      ""
    )
  }
  lines <- c(lines, "## Methods", "", report$methods, "")
  add_counts <- function(title, counts) {
    if (length(counts) == 0L) {
      return(character())
    }
    c(paste0("## ", title), "",
      paste0("- ", names(counts), ": ", as.integer(counts)), "")
  }
  lines <- c(lines, add_counts("Review status", report$review_status))
  lines <- c(lines, add_counts("Placed by", report$tiers))
  lines <- c(lines, add_counts("Cache status", report$cache_status))
  if (!is.null(report$confidence) && !is.na(report$confidence$median)) {
    cf <- report$confidence
    lines <- c(
      lines,
      "## Match confidence", "",
      paste0("- Median: ", format(cf$median)),
      paste0("- Mean: ", format(cf$mean)),
      paste0("- Below ", format(cf$below_threshold), ": ", cf$n_below),
      ""
    )
  }
  lines
}

# "" / NULL -> NULL, so the spatial join (add_muni_from_shapes) falls back to
# auto-detection instead of looking for a column literally named "".
nz_or_null <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) NULL else x
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
        uiOutput("conflict_ui"),
        tags$hr(),
        checkboxInput("use_cache", "Use session cache", value = TRUE),
        checkboxInput("refresh_cache", "Refresh cached geocoder results",
                      value = FALSE),
        actionButton("run_geocode", "Geocode", class = "btn-primary",
                     icon = icon("location-dot")),
        helpText("The session cache avoids repeated Census/ArcGIS calls during ",
                 "this app session. Hosted sessions are ephemeral; export the ",
                 "audit report for a durable record.")
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
        ),
        tags$hr(),
        checkboxGroupInput(
          "extra_census_levels", "Optional extra Census geographies",
          choices = c(
            "Tract" = "tract",
            "Block group" = "block_group",
            "ZCTA" = "zcta",
            "Congressional district" = "congressional_district",
            "State senate / upper" = "state_legislative_district_upper",
            "State house / lower" = "state_legislative_district_lower",
            "Unified school district" = "school_district"
          ),
          selected = character(0)
        ),
        helpText("Extra geographies add <level>_geoid and <level>_name columns ",
                 "to the crosswalk. They use tigris and may download boundary files.")
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

  nav_panel(
    title = "5. Audit report",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        actionButton("make_report", "Refresh report", class = "btn-primary",
                     icon = icon("clipboard-list")),
        helpText("Reports summarize the geocoding run, cache/provenance, review ",
                 "statuses, and match confidence. Download the Markdown report ",
                 "for project records or methods sections."),
        tags$hr(),
        downloadButton("dl_report", "Download report (.md)"),
        downloadButton("dl_provenance", "Download provenance (.txt)")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Methods paragraph"), verbatimTextOutput("report_methods")),
        card(
          card_header("Run provenance"),
          verbatimTextOutput("provenance_text"),
          DT::DTOutput("cache_status_table")
        )
      ),
      card(card_header("Review summary"), DT::DTOutput("report_counts_table"))
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
    data = NULL, geocoded = NULL, geo_layer = NULL, crosswalk = NULL,
    cache = NULL, report = NULL
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
                " - map your address/city columns, optional ID/ZIP/name columns, ",
                "pick the state, and run locatr's cascade (Census -> ArcGIS -> ",
                "name lookup). Use the session cache to avoid repeated service ",
                "calls while you work. Geocoding calls external services, so it ",
                "needs network access and is capped by the row limit."),
        tags$li(tags$b("Attach geography (optional)"),
                " - build county/locality boundaries from Census TIGER/Line, or ",
                "upload your own shapefile (.zip, or .shp with its sidecars, or ",
                ".geojson/.gpkg). You can also append tract, ZCTA, district, or ",
                "school-district GEOIDs from Census boundaries. For a shapefile ",
                "you choose the join criteria: spatial (point-in-polygon) or an ",
                "attribute key shared with your data."),
        tags$li(tags$b("Download"),
                " - export the geocoded records or the geography crosswalk as ",
                "CSV, Excel, or Parquet. You can drop columns before downloading."),
        tags$li(tags$b("Audit report"),
                " - review the methods paragraph, provenance, cache status, ",
                "field-conflict flags, and confidence summaries; download the ",
                "Markdown report for your project records.")
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
      locatr:::.read_location_table(input$data_file$datapath, input$data_file$name),
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
      selectInput("col_id",   "Unique ID (optional)", choices = c("(auto)" = "", cols),
                  selected = guess_col(cols, "id|code|key", allow_none = TRUE)),
      selectInput("col_addr", "Address",     choices = cols,
                  selected = guess_col(cols, "addr|street")),
      selectInput("col_city", "City",        choices = cols,
                  selected = guess_col(cols, "city|town|munic")),
      selectInput("col_zip",  "ZIP (optional)", choices = c("(none)" = "", cols),
                  selected = guess_col(cols, "zip|postal", allow_none = TRUE)),
      selectInput("col_name", "Name (optional)", choices = c("(none)" = "", cols),
                  selected = guess_col(cols, "name|facility|site|provider",
                                       allow_none = TRUE))
    )
  })

  output$conflict_ui <- renderUI({
    req(rv$data)
    cols <- names(rv$data)
    selectInput(
      "stated_county", "Stated county column (optional conflict check)",
      choices = c("(none)" = "", cols),
      selected = guess_col(cols, "county|cnty", allow_none = TRUE)
    )
  })

  observeEvent(input$run_geocode, {
    req(rv$data, input$col_addr, input$col_city)
    withProgress(message = "Geocoding with locatr ...", value = 0, {
      result <- notify_error({
        dat <- rv$data
        if (!is.na(input$max_rows) && nrow(dat) > input$max_rows) {
          dat <- utils::head(dat, input$max_rows)
        }
        incProgress(0.2, detail = "cleaning addresses")
        cleaned <- clean_with_strings(
          dat, address = input$col_addr, city = input$col_city,
          id = input$col_id, zip = input$col_zip,
          name = input$col_name, state = input$state
        )
        incProgress(0.2, detail = "flagging bad addresses")
        flagged <- locatr::flag_bad_addresses(cleaned)
        cache <- if (isTRUE(input$use_cache)) {
          if (is.null(rv$cache)) {
            rv$cache <- locatr::locatr_cache()
          }
          rv$cache
        } else {
          NULL
        }
        incProgress(0.2, detail = "running the cascade")
        geocoded <- locatr::geocode_records(
          flagged, bbox = safe_bbox(input$state),
          name_min_score = input$name_min_score,
          name_accept_types = input$name_accept_types,
          cache = cache,
          refresh = isTRUE(input$refresh_cache),
          verbose = FALSE
        )
        if (!is.null(input$stated_county) && nzchar(input$stated_county)) {
          geocoded <- locatr::flag_field_conflicts(
            geocoded, stated_county = input$stated_county
          )
        } else {
          geocoded <- locatr::flag_field_conflicts(geocoded)
        }
        geocoded
      }, "Geocoding failed")
      if (!is.null(result)) {
        rv$geocoded <- result
        rv$crosswalk <- NULL
        rv$report <- locatr::geocode_report(result)
        incProgress(0.4, detail = "done")
        showNotification("Geocoding complete.", type = "message")
        bslib::nav_select("nav", "5. Audit report", session = session)
      }
    })
  })

  output$geo_table <- DT::renderDT({
    req(rv$geocoded)
    show <- rv$geocoded %>%
      dplyr::select(dplyr::any_of(c(
        "record_id", "record_name", "full_address_clean",
        "latitude", "longitude", "geocode_pass", "match_status",
        "nm_score", "nm_addr_type", "nm_status", "match_confidence",
        "cache_status", "field_conflict", "review_status"
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
      locatr:::.read_geography_layer(input$shp_file), "Could not read shapefile"
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
          joined <- locatr::add_muni_from_key(
            rv$geocoded, rv$geo_layer,
            data_key = input$data_key, shp_key = input$shp_key,
            county_col = input$shp_county, muni_col = input$shp_locality,
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
        crosswalk <- locatr::export_location_crosswalk(joined)
        if (!is.null(input$extra_census_levels) &&
            length(input$extra_census_levels) > 0L) {
          crosswalk <- locatr::add_census_geographies(
            crosswalk,
            state = input$geo_state %||% input$state,
            levels = input$extra_census_levels
          )
        }
        if (!is.null(input$stated_county) && nzchar(input$stated_county) &&
            input$stated_county %in% names(joined)) {
          conflict_source <- joined
          conflict_source <- locatr::flag_field_conflicts(
            conflict_source, stated_county = input$stated_county
          )
          crosswalk$zip_state_conflict <- conflict_source$zip_state_conflict
          crosswalk$county_conflict <- conflict_source$county_conflict
          crosswalk$field_conflict <- conflict_source$field_conflict
        } else if (!"field_conflict" %in% names(crosswalk)) {
          crosswalk <- locatr::flag_field_conflicts(crosswalk)
        }
        crosswalk
      }, "Join failed")
      if (!is.null(crosswalk)) {
        rv$crosswalk <- crosswalk
        rv$report <- locatr::geocode_report(crosswalk)
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
    conflict_count <- if ("field_conflict" %in% names(dat)) {
      sum(!is.na(dat$field_conflict))
    } else NA_integer_
    cached_count <- if ("cache_status" %in% names(dat)) {
      sum(dat$cache_status == "cached", na.rm = TRUE)
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
      },
      if (!is.na(conflict_count)) {
        tags$div(tags$strong(format(conflict_count, big.mark = ",")),
                 " field conflict flags.")
      },
      if (!is.na(cached_count)) {
        tags$div(tags$strong(format(cached_count, big.mark = ",")),
                 " coordinates replayed from cache.")
      }
    )
  })

  observeEvent(input$make_report, {
    dat <- output_data_raw()
    rv$report <- locatr::geocode_report(dat)
    showNotification("Audit report refreshed.", type = "message")
  })

  current_report <- reactive({
    if (is.null(rv$report)) {
      dat <- output_data_raw()
      rv$report <- locatr::geocode_report(dat)
    }
    rv$report
  })

  output$report_methods <- renderText({
    report <- current_report()
    paste(strwrap(report$methods, width = 90), collapse = "\n")
  })

  output$provenance_text <- renderText({
    dat <- output_data_raw()
    prov <- tryCatch(locatr::geocode_provenance(dat), error = function(e) NULL)
    if (is.null(prov) && !is.null(rv$geocoded)) {
      prov <- tryCatch(locatr::geocode_provenance(rv$geocoded),
                       error = function(e) NULL)
    }
    if (is.null(prov)) {
      return("No locatr run manifest is attached to this output.")
    }
    paste(capture.output(print(prov)), collapse = "\n")
  })

  output$cache_status_table <- DT::renderDT({
    report <- current_report()
    DT::datatable(as_count_table(report$cache_status), rownames = FALSE,
                  options = list(dom = "t"))
  })

  output$report_counts_table <- DT::renderDT({
    report <- current_report()
    rows <- rbind(
      cbind(section = "review_status", as_count_table(report$review_status)),
      cbind(section = "placed_by", as_count_table(report$tiers)),
      cbind(section = "cache_status", as_count_table(report$cache_status))
    )
    DT::datatable(rows, rownames = FALSE, options = list(pageLength = 12))
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
  output$dl_report <- downloadHandler(
    filename = function() "locatr-audit-report.md",
    content = function(file) {
      writeLines(app_report_markdown(current_report()), file)
    }
  )
  output$dl_provenance <- downloadHandler(
    filename = function() "locatr-provenance.txt",
    content = function(file) {
      dat <- output_data_raw()
      prov <- tryCatch(locatr::geocode_provenance(dat), error = function(e) NULL)
      if (is.null(prov) && !is.null(rv$geocoded)) {
        prov <- tryCatch(locatr::geocode_provenance(rv$geocoded),
                         error = function(e) NULL)
      }
      if (is.null(prov)) {
        writeLines("No locatr run manifest is attached to this output.", file)
      } else {
        writeLines(capture.output(print(prov)), file)
      }
    }
  )
}

shinyApp(ui, server)
