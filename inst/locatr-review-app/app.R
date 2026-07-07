# locatr geocode + map-review app
# -----------------------------------------------------------------------------
# The full locatr pipeline as a Shiny app, with a map-based human-review step:
#   1. Upload a data file (CSV / Excel / Parquet) and preview it.
#   2. Map the address columns and geocode with locatr's cascade.
#   3. Review the flagged records on a map: accept, reject, or relocate, then
#      apply the overrides back into the geocoded data.
#   4. Attach local geography (Census TIGER/Line or an uploaded shapefile).
#   5. Choose output columns and download CSV / Excel / Parquet.
#   6. Review provenance and download an audit report.
#
# Launch with run_locatr_review_app().
# -----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(sf)
library(leaflet)

# locatr is guaranteed installed by run_locatr_review_app(); expose its exports
# (and the two internal readers the app uses) so the app can call them by name.
if (!requireNamespace("locatr", quietly = TRUE)) {
  stop("The locatr package must be installed before running this app.",
       call. = FALSE)
}
local({
  for (nm in getNamespaceExports("locatr")) {
    assign(nm, getExportedValue("locatr", nm), envir = globalenv())
  }
  assign(".read_location_table",
         get(".read_location_table", envir = asNamespace("locatr")),
         envir = globalenv())
  assign(".read_geography_layer",
         get(".read_geography_layer", envir = asNamespace("locatr")),
         envir = globalenv())
})

# ---- helpers ----------------------------------------------------------------

# A region bbox for validation: locatr's preset where it exists, else a
# continental-US fallback so geocoding still runs for any state.
safe_bbox <- function(state) {
  bb <- tryCatch(region_bbox(state), error = function(e) NULL)
  if (!is.null(bb)) return(bb)
  c(lat_min = 24.5, lat_max = 49.5, lon_min = -125.0, lon_max = -66.9)
}

guess_col <- function(cols, pattern, allow_none = FALSE) {
  hit <- cols[grepl(pattern, cols, ignore.case = TRUE)]
  if (length(hit) > 0) return(hit[1])
  if (allow_none) "" else cols[1]
}

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
  do.call(clean_addresses, args)
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

nz_or_null <- function(x) {
  if (is.null(x) || length(x) == 0 || !nzchar(x)) NULL else x
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

# Review-tab helpers: a running decisions table and an upsert.
.empty_decisions <- function() {
  data.frame(
    record_id = character(0), decision = character(0),
    manual_latitude = double(0), manual_longitude = double(0),
    stringsAsFactors = FALSE
  )
}

.upsert_decision <- function(decisions, record_id, decision, lat, lng) {
  decisions <- decisions[decisions$record_id != record_id, , drop = FALSE]
  rbind(
    decisions,
    data.frame(
      record_id = as.character(record_id), decision = decision,
      manual_latitude = as.numeric(lat), manual_longitude = as.numeric(lng),
      stringsAsFactors = FALSE
    )
  )
}

US_STATES <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL",
               "IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT",
               "NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
               "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC")

# Build an audit report, but never let a report failure break the pipeline.
# geocode_report() is a newer locatr function; on an older installed build it
# may be absent, in which case the core geocode/review/geography/download flow
# should still work and the audit tab simply shows nothing.
safe_report <- function(data) {
  tryCatch(geocode_report(data), error = function(e) NULL)
}

# A tab title with an optional info icon that reveals a hint on hover.
tab_title <- function(label, hint = NULL) {
  if (is.null(hint)) {
    return(label)
  }
  tagList(
    label,
    tooltip(icon("circle-info", class = "text-muted ms-1"), hint,
            placement = "bottom")
  )
}

# ---- UI ---------------------------------------------------------------------

ui <- page_navbar(
  title = tagList(
    "locatr",
    tooltip(
      icon("circle-info", class = "text-muted ms-2"),
      paste0("Geocode messy addresses, review them on a map, attach local ",
             "geography, and export - built on the locatr R package."),
      placement = "bottom"
    )
  ),
  theme = bs_theme(version = 5, preset = "flatly"),
  id = "nav",

  nav_panel(
    title = tab_title("Upload & preview"),
    value = "upload",
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
    title = tab_title("Geocode",
                      paste0("Clean and flag addresses, then run the cascade ",
                             "(Census, then ArcGIS, then name lookup).")),
    value = "geocode",
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
                 "this app session.")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Geocoded records"), DT::DTOutput("geo_table")),
        card(card_header("Map"), leafletOutput("geo_map", height = 460))
      )
    )
  ),

  nav_panel(
    title = tab_title("Review",
                      paste0("Accept, reject, or relocate flagged records on ",
                             "the map, then apply the fixes back to the data.")),
    value = "review",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        helpText("Records the cascade could not auto-accept. Select one on the ",
                 "map or in the table, then accept, reject, or relocate it."),
        checkboxGroupInput("rv_status", "Show which rows?",
                           choices = c("needs_manual_review", "rejected"),
                           selected = c("needs_manual_review", "rejected")),
        sliderInput("rv_max_conf", "Confidence at or below",
                    min = 0, max = 1, value = 1, step = 0.05),
        uiOutput("rv_editor"),
        tags$hr(),
        uiOutput("rv_progress"),
        downloadButton("rv_dl", "Download override CSV"),
        actionButton("rv_apply", "Apply overrides to data",
                     class = "btn-primary", icon = icon("check")),
        helpText("Applying updates the geocoded data so the geography, download, ",
                 "and audit steps reflect your review.")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Records needing review"), DT::DTOutput("rv_table")),
        card(card_header("Map"), leafletOutput("rv_map", height = 460))
      )
    )
  ),

  nav_panel(
    title = tab_title("Attach geography",
                      paste0("Add county and municipality (and optional Census ",
                             "levels) by point-in-polygon or a key join.")),
    value = "geography",
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
    title = tab_title("Download"),
    value = "download",
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
    title = tab_title("Audit report",
                      paste0("A plain-language methods summary, cache status, ",
                             "and confidence breakdown for the run.")),
    value = "audit",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        actionButton("make_report", "Refresh report", class = "btn-primary",
                     icon = icon("clipboard-list")),
        helpText("Reports summarize the geocoding run, cache/provenance, review ",
                 "statuses, and match confidence."),
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
  rv_decisions <- reactiveVal(.empty_decisions())
  rv_last_click <- reactiveVal(NULL)

  notify_error <- function(expr, msg) {
    tryCatch(expr, error = function(e) {
      showNotification(paste0(msg, ": ", conditionMessage(e)),
                       type = "error", duration = 10)
      NULL
    })
  }

  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "How to use this app",
      easyClose = TRUE, size = "l", footer = modalButton("Close"),
      tags$ol(
        tags$li(tags$b("Upload & preview"),
                " - load a CSV, Excel, or Parquet file; one row per record."),
        tags$li(tags$b("Geocode"),
                " - map your address/city columns, optional ID/ZIP/name columns, ",
                "pick the state, and run locatr's cascade. Geocoding needs ",
                "network access and is capped by the row limit."),
        tags$li(tags$b("Review"),
                " - the records the cascade could not auto-accept appear on a ",
                "map; select one and accept, reject, or relocate it. Apply the ",
                "overrides to fold your fixes back into the geocoded data."),
        tags$li(tags$b("Attach geography (optional)"),
                " - county/locality from Census TIGER/Line or your own ",
                "shapefile, plus optional tract / ZCTA / district GEOIDs."),
        tags$li(tags$b("Download"),
                " - export the geocoded records or geography crosswalk as CSV, ",
                "Excel, or Parquet; drop columns first if you like."),
        tags$li(tags$b("Audit report"),
                " - methods paragraph, provenance, cache status, and confidence ",
                "summaries; download the Markdown report for your records.")
      ),
      tags$p("Built on the ",
             tags$a("locatr", href = "https://github.com/PrigasG/locatr",
                    target = "_blank"), " R package.")
    ))
  })

  # --- Step 1: upload + preview ---------------------------------------------
  observeEvent(input$data_file, {
    rv$data <- notify_error(
      .read_location_table(input$data_file$datapath, input$data_file$name),
      "Could not read data file"
    )
    rv$geocoded <- NULL
    rv$crosswalk <- NULL
    rv_decisions(.empty_decisions())
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

  observeEvent(input$go_geocode, {
    req(rv$data)
    bslib::nav_select("nav", "geocode", session = session)
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
        flagged <- flag_bad_addresses(cleaned)
        cache <- if (isTRUE(input$use_cache)) {
          if (is.null(rv$cache)) rv$cache <- locatr_cache()
          rv$cache
        } else {
          NULL
        }
        incProgress(0.2, detail = "running the cascade")
        geocoded <- geocode_records(
          flagged, bbox = safe_bbox(input$state),
          name_min_score = input$name_min_score,
          name_accept_types = input$name_accept_types,
          cache = cache, refresh = isTRUE(input$refresh_cache),
          verbose = FALSE
        )
        if (!is.null(input$stated_county) && nzchar(input$stated_county)) {
          geocoded <- flag_field_conflicts(
            geocoded, stated_county = input$stated_county
          )
        } else {
          geocoded <- flag_field_conflicts(geocoded)
        }
        geocoded
      }, "Geocoding failed")
      if (!is.null(result)) {
        rv$geocoded <- result
        rv$crosswalk <- NULL
        rv_decisions(.empty_decisions())
        rv$report <- safe_report(result)
        incProgress(0.4, detail = "done")
        showNotification("Geocoding complete - review flagged records.",
                         type = "message")
        bslib::nav_select("nav", "review", session = session)
      }
    })
  })

  output$geo_table <- DT::renderDT({
    req(rv$geocoded)
    show <- rv$geocoded %>%
      dplyr::select(dplyr::any_of(c(
        "record_id", "record_name", "full_address_clean",
        "latitude", "longitude", "geocode_pass", "match_status",
        "match_confidence", "cache_status", "field_conflict", "review_status"
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

  # --- Step 3: map-based review ---------------------------------------------
  review_pool <- reactive({
    req(rv$geocoded)
    dat <- rv$geocoded
    if ("review_status" %in% names(dat)) {
      dat <- dat[dat$review_status %in% input$rv_status, , drop = FALSE]
    }
    if ("match_confidence" %in% names(dat)) {
      keep <- is.na(dat$match_confidence) | dat$match_confidence <= input$rv_max_conf
      dat <- dat[keep, , drop = FALSE]
    }
    dat
  })

  rv_pool_display <- reactive({
    dat <- review_pool()
    dec <- rv_decisions()
    dat$decision <- if (nrow(dec) > 0L) {
      dec$decision[match(as.character(dat$record_id), dec$record_id)]
    } else {
      rep(NA_character_, nrow(dat))
    }
    cols <- intersect(
      c("record_id", "record_name", "full_address_clean", "latitude",
        "longitude", "match_confidence", "review_status", "field_conflict",
        "decision"),
      names(dat)
    )
    dat[, cols, drop = FALSE]
  })

  output$rv_table <- DT::renderDT({
    DT::datatable(rv_pool_display(), selection = "single", rownames = FALSE,
                  options = list(scrollX = TRUE, pageLength = 10))
  })

  rv_selected_id <- reactive({
    row <- input$rv_table_rows_selected
    dat <- review_pool()
    if (is.null(row) || length(row) == 0L || nrow(dat) == 0L) {
      return(NULL)
    }
    as.character(dat$record_id[row])
  })

  output$rv_map <- renderLeaflet({
    dat <- review_pool()
    m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    if (all(c("latitude", "longitude") %in% names(dat))) {
      has_xy <- !is.na(dat$latitude) & !is.na(dat$longitude)
      pts <- dat[has_xy, , drop = FALSE]
      if (nrow(pts) > 0) {
        nm <- if ("record_name" %in% names(pts)) {
          as.character(pts$record_name)
        } else {
          rep("", nrow(pts))
        }
        m <- m %>%
          addCircleMarkers(
            data = pts, lng = ~longitude, lat = ~latitude,
            layerId = as.character(pts$record_id), radius = 6, stroke = TRUE,
            color = "#2c7fb8", fillOpacity = 0.7,
            popup = paste0(as.character(pts$record_id), "<br/>", nm)
          ) %>%
          fitBounds(min(pts$longitude), min(pts$latitude),
                    max(pts$longitude), max(pts$latitude))
      }
    }
    m
  })

  observeEvent(input$rv_map_marker_click, {
    id <- input$rv_map_marker_click$id
    dat <- review_pool()
    row <- which(as.character(dat$record_id) == id)
    if (length(row) == 1L) {
      DT::dataTableProxy("rv_table") %>% DT::selectRows(row)
    }
  })

  observeEvent(input$rv_map_click, {
    rv_last_click(list(lat = input$rv_map_click$lat,
                       lng = input$rv_map_click$lng))
  })

  output$rv_editor <- renderUI({
    id <- rv_selected_id()
    if (is.null(id)) {
      return(helpText("Select a record in the table or on the map."))
    }
    dat <- review_pool()
    rec <- dat[as.character(dat$record_id) == id, , drop = FALSE][1, ]
    lc <- rv_last_click()
    tagList(
      tags$hr(),
      tags$strong(paste0("Record ", id)),
      tags$div(if ("record_name" %in% names(rec)) rec$record_name else ""),
      tags$div(if ("full_address_clean" %in% names(rec)) {
        rec$full_address_clean
      } else ""),
      numericInput("rv_lat", "Manual latitude",
                   value = if (!is.null(lc)) round(lc$lat, 6) else rec$latitude),
      numericInput("rv_lng", "Manual longitude",
                   value = if (!is.null(lc)) round(lc$lng, 6) else rec$longitude),
      helpText("Tip: click the map to fill these from a location."),
      div(
        actionButton("rv_accept", "Accept", class = "btn-success"),
        actionButton("rv_relocate", "Relocate", class = "btn-primary"),
        actionButton("rv_reject", "Reject", class = "btn-danger")
      )
    )
  })

  rv_record <- function(decision, lat = NA_real_, lng = NA_real_) {
    id <- rv_selected_id()
    req(id)
    rv_decisions(.upsert_decision(rv_decisions(), id, decision, lat, lng))
    showNotification(paste0("Record ", id, ": ", decision, "."),
                     type = "message", duration = 4)
  }

  observeEvent(input$rv_accept, rv_record("accept"))
  observeEvent(input$rv_reject, rv_record("reject"))
  observeEvent(input$rv_relocate, {
    lat <- input$rv_lat
    lng <- input$rv_lng
    if (is.null(lat) || is.null(lng) || is.na(lat) || is.na(lng)) {
      showNotification("Enter a manual latitude and longitude first.",
                       type = "warning")
      return(invisible())
    }
    if (!in_bbox(lat, lng, safe_bbox(input$state))) {
      showNotification("That point is outside the region; not saved.",
                       type = "error")
      return(invisible())
    }
    rv_record("relocate", lat, lng)
  })

  output$rv_progress <- renderUI({
    req(rv$geocoded)
    total <- nrow(review_pool())
    done <- sum(rv_decisions()$record_id %in%
                  as.character(review_pool()$record_id))
    tags$div(tags$strong(done), " of ", tags$strong(total), " reviewed.")
  })

  rv_overrides <- reactive({
    req(rv$geocoded)
    build_review_overrides(rv$geocoded, rv_decisions())
  })

  output$rv_dl <- downloadHandler(
    filename = function() "manual_review_completed.csv",
    content = function(file) readr::write_csv(rv_overrides(), file)
  )

  observeEvent(input$rv_apply, {
    req(rv$geocoded)
    withProgress(message = "Applying overrides ...", value = 0.5, {
      updated <- notify_error({
        tmp <- tempfile(fileext = ".csv")
        readr::write_csv(rv_overrides(), tmp)
        apply_manual_overrides(rv$geocoded, tmp, bbox = safe_bbox(input$state))
      }, "Applying overrides failed")
      if (!is.null(updated)) {
        rv$geocoded <- updated
        rv$crosswalk <- NULL
        rv_decisions(.empty_decisions())
        rv$report <- safe_report(updated)
        showNotification("Overrides applied to the geocoded data.",
                         type = "message")
      }
    })
  })

  # --- Step 4: geography layer ----------------------------------------------
  observeEvent(input$build_geo, {
    withProgress(message = "Building Census geography ...", value = 0.5, {
      layer <- notify_error(
        build_local_geography(state = input$geo_state,
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
      .read_geography_layer(input$shp_file), "Could not read shapefile"
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

  # --- Step 5: optional join + download -------------------------------------
  observeEvent(input$run_join, {
    req(rv$geocoded)
    withProgress(message = "Joining to geography ...", value = 0.5, {
      crosswalk <- notify_error({
        is_shp <- identical(input$geo_source, "shapefile")
        if (!is_shp && is.null(rv$geo_layer)) {
          incProgress(0.2, detail = "building Census geography")
          rv$geo_layer <- build_local_geography(
            state = input$geo_state, geography = input$geo_level
          )
        }
        if (is_shp) req(rv$geo_layer)
        if (is_shp && identical(input$join_mode, "key")) {
          req(input$data_key, input$shp_key)
          joined <- add_muni_from_key(
            rv$geocoded, rv$geo_layer,
            data_key = input$data_key, shp_key = input$shp_key,
            county_col = input$shp_county, muni_col = input$shp_locality,
            key_col = input$shp_muni_key
          )
        } else {
          county_col   <- if (is_shp) nz_or_null(input$shp_county) else NULL
          locality_col <- if (is_shp) nz_or_null(input$shp_locality) else NULL
          key_col      <- if (is_shp) nz_or_null(input$shp_muni_key) else NULL
          joined <- add_muni_from_shapes(
            rv$geocoded, muni_shapes = rv$geo_layer,
            county_col = county_col, muni_col = locality_col, key_col = key_col
          )
        }
        crosswalk <- export_location_crosswalk(joined)
        if (!is.null(input$extra_census_levels) &&
            length(input$extra_census_levels) > 0L) {
          crosswalk <- add_census_geographies(
            crosswalk, state = input$geo_state %||% input$state,
            levels = input$extra_census_levels
          )
        }
        if (!"field_conflict" %in% names(crosswalk)) {
          crosswalk <- flag_field_conflicts(crosswalk)
        }
        crosswalk
      }, "Join failed")
      if (!is.null(crosswalk)) {
        rv$crosswalk <- crosswalk
        rv$report <- safe_report(crosswalk)
        updateRadioButtons(session, "output_source", selected = "crosswalk")
        matched <- sum(!is.na(crosswalk$location_locality))
        rate <- if (nrow(crosswalk) > 0) round(100 * matched / nrow(crosswalk)) else 0
        showNotification(
          sprintf("Crosswalk ready - %d%% of rows matched a locality.%s", rate,
                  if (rate == 0) " Try other join columns or criteria." else ""),
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
      choices = names(dat), selected = character(0), multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  output_data <- reactive({
    drop_selected_cols(output_data_raw(), input$drop_cols)
  })

  output$output_table <- DT::renderDT({
    DT::datatable(output_data(), options = list(scrollX = TRUE), rownames = FALSE)
  })

  output$output_summary <- renderUI({
    dat <- output_data()
    locality_count <- if ("location_locality" %in% names(dat)) {
      sum(!is.na(dat$location_locality))
    } else NA_integer_
    conflict_count <- if ("field_conflict" %in% names(dat)) {
      sum(!is.na(dat$field_conflict))
    } else NA_integer_
    tags$div(
      tags$hr(),
      tags$strong(format(nrow(dat), big.mark = ",")), " records, ",
      tags$strong(ncol(dat)), " columns selected.",
      if (!is.na(locality_count)) {
        tags$div(tags$strong(format(locality_count, big.mark = ",")),
                 " with a locality.")
      },
      if (!is.na(conflict_count)) {
        tags$div(tags$strong(format(conflict_count, big.mark = ",")),
                 " field conflict flags.")
      }
    )
  })

  output$output_map <- renderLeaflet({
    dat <- output_data()
    m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    if (!all(c("latitude", "longitude") %in% names(dat))) {
      return(m)
    }
    pts <- dat %>% dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))
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
    content = function(file) readr::write_csv(output_data(), file)
  )
  output$dl_xlsx <- downloadHandler(
    filename = function() paste0(input$output_source %||% "geocoded", ".xlsx"),
    content = function(file) writexl::write_xlsx(output_data(), file)
  )
  output$dl_parquet <- downloadHandler(
    filename = function() paste0(input$output_source %||% "geocoded", ".parquet"),
    content = function(file) arrow::write_parquet(output_data(), file)
  )

  # --- Step 6: audit report -------------------------------------------------
  observeEvent(input$make_report, {
    rv$report <- safe_report(output_data_raw())
    showNotification("Audit report refreshed.", type = "message")
  })

  current_report <- reactive({
    if (is.null(rv$report)) {
      rv$report <- safe_report(output_data_raw())
    }
    rv$report
  })

  report_unavailable_msg <- paste(
    "Audit report unavailable. This usually means the installed locatr package",
    "predates geocode_report(); reinstall locatr (devtools::install() or",
    "devtools::load_all()) and re-run the report.")

  output$report_methods <- renderText({
    report <- current_report()
    if (is.null(report)) {
      return(report_unavailable_msg)
    }
    paste(strwrap(report$methods, width = 90), collapse = "\n")
  })

  output$provenance_text <- renderText({
    prov <- tryCatch(geocode_provenance(output_data_raw()),
                     error = function(e) NULL)
    if (is.null(prov) && !is.null(rv$geocoded)) {
      prov <- tryCatch(geocode_provenance(rv$geocoded), error = function(e) NULL)
    }
    if (is.null(prov)) {
      return("No locatr run manifest is attached to this output.")
    }
    paste(utils::capture.output(print(prov)), collapse = "\n")
  })

  output$cache_status_table <- DT::renderDT({
    report <- current_report()
    req(report)
    DT::datatable(as_count_table(report$cache_status),
                  rownames = FALSE, options = list(dom = "t"))
  })

  output$report_counts_table <- DT::renderDT({
    report <- current_report()
    req(report)
    rows <- rbind(
      cbind(section = "review_status", as_count_table(report$review_status)),
      cbind(section = "placed_by", as_count_table(report$tiers)),
      cbind(section = "cache_status", as_count_table(report$cache_status))
    )
    DT::datatable(rows, rownames = FALSE, options = list(pageLength = 12))
  })

  output$dl_report <- downloadHandler(
    filename = function() "locatr-audit-report.md",
    content = function(file) {
      report <- current_report()
      if (is.null(report)) {
        writeLines(report_unavailable_msg, file)
      } else {
        writeLines(app_report_markdown(report), file)
      }
    }
  )
  output$dl_provenance <- downloadHandler(
    filename = function() "locatr-provenance.txt",
    content = function(file) {
      prov <- tryCatch(geocode_provenance(output_data_raw()),
                       error = function(e) NULL)
      if (is.null(prov) && !is.null(rv$geocoded)) {
        prov <- tryCatch(geocode_provenance(rv$geocoded), error = function(e) NULL)
      }
      if (is.null(prov)) {
        writeLines("No locatr run manifest is attached to this output.", file)
      } else {
        writeLines(utils::capture.output(print(prov)), file)
      }
    }
  )
}

shinyApp(ui, server)
