# =============================================================================
# NWRFC Monthly Streamflow Forecasts (Shiny) - Revised v2
#
# Changes from v1:
#   - DT replaces tableHTML: percent formatting + white text on dark cells
#   - Fixed-width sidebar (doesn't stretch with window)
#   - Results Output removed
#   - ASCW1 excluded (station no longer exists)
#   - WRIA_NR + WRIA_NM combined into single column
# =============================================================================

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

DATA_PATH <- "data/forecast_latest.csv"
META_PATH <- "data/month_meta.csv"
TS_PATH   <- "data/last_updated.txt"

WY_MONTH_LEVELS <- c("OCT","NOV","DEC","JAN","FEB","MAR",
                     "APR","MAY","JUN","JUL","AUG","SEP")
RUNOFF_MONTHS   <- c("APR","MAY","JUN","JUL","AUG","SEP")

EXCLUDED_STATIONS <- c("ASCW1")   # decommissioned; no longer publishes ESP10

# Color scale breakpoints (0-1 scale) and their bin colors + font colors
COLOR_CUTS   <- c(0.25, 0.50, 0.75, 0.90, 1.10, 1.25, 1.50, 1.75)
COLOR_VALUES <- c("#b3070c","#fd0f17","#fda52d","#fefe43",
                  "#1cfe3e","#1dfcfc","#0fa4f9","#0104f8","#7f047d")
FONT_COLORS  <- c("white","white","black","black",
                  "black","black","white","white","white")

# -----------------------------------------------------------------------------
# Load static data once at startup (not per-session)
# -----------------------------------------------------------------------------

forecast_wide <- read_csv(DATA_PATH, show_col_types = FALSE) %>%
  filter(!NatFlowStationID %in% EXCLUDED_STATIONS)

month_meta <- read_csv(META_PATH, show_col_types = FALSE) %>%
  mutate(Month = toupper(Month))

last_updated <- tryCatch(
  readLines(TS_PATH, warn = FALSE)[1],
  error = function(e) "Unknown"
)

months_available <- intersect(WY_MONTH_LEVELS, names(forecast_wide))
obs_months       <- month_meta$Month[month_meta$is_observed]

wria_list <- forecast_wide %>%
  filter(!is.na(WRIA_NM), WRIA_NM != "") %>%
  distinct(WRIA_NR, WRIA_NM) %>%
  arrange(WRIA_NR) %>%
  pull(WRIA_NM)

# -----------------------------------------------------------------------------
# JS: shade observed-month column headers gray/italic in DT
# (built once at startup; obs_months is fixed per daily deploy)
# -----------------------------------------------------------------------------

obs_header_js <- sprintf(
  "function(thead, data, start, end, display) {
    var obs = [%s];
    $(thead).find('th').each(function() {
      var txt = $(this).text().trim();
      if (obs.indexOf(txt) > -1) {
        $(this).css({
          'background-color': '#e0e0e0',
          'font-style': 'italic',
          'color': '#555'
        });
      }
    });
  }",
  paste0('"', obs_months, '"', collapse = ", ")
)

# -------------------------
# UI
# -------------------------

ui <- fluidPage(

  # ── Polished banner ───────────────────────────────────────────────────────
  tags$div(
    style = paste0(
      "background: linear-gradient(135deg, #1a3a5c 0%, #2e6da4 100%);",
      "padding: 12px 20px;",
      "margin-bottom: 16px;",
      "border-radius: 6px;",
      "display: flex;",
      "align-items: center;",
      "gap: 16px;"
    ),
    
    # ── Icon in white circle badge ──────────────────────────────────────────
    tags$div(
      style = paste0(
        "background-color: white;",
        "border-radius: 50%;",
        "padding: 8px;",
        "width: 65px;",
        "height: 65px;",
        "display: flex;",
        "align-items: center;",
        "justify-content: center;",
        "flex-shrink: 0;",
        "box-shadow: 0 2px 8px rgba(0,0,0,0.4);"
      ),
      tags$img(
        src   = "nwrfc_forecast.svg",
        style = "width: 80px; height: 80px; object-fit: contain; display: block;"
      )
    ),
    
    # ── Title text block ────────────────────────────────────────────────────
    tags$div(
      tags$h2(
        "Monthly Runoff Forecasts — Washington State Watersheds",
        style = paste0(
          "color: white; margin: 0;",
          "font-size: 22px; font-weight: bold; line-height: 1.2;"
        )
      ),
      tags$p(
        paste0("NOAA Northwest River Forecast Center  |  Data as of: ", last_updated),
        style = paste0(
          "color: rgba(255,255,255,0.78);",
          "margin: 4px 0 0 0; font-size: 13px;"
        )
      )
    )
  ),

  tags$style(HTML("

    /* ---- Sidebar: fixed width when expanded, zero when collapsed ---------- */
    @media (min-width: 768px) {
      .col-sm-3 {
        width:     265px !important;
        flex:      0 0 265px !important;
        max-width: 265px !important;
        transition: width 0.25s ease, max-width 0.25s ease;
        overflow:  hidden;
      }
      .col-sm-9 {
        width:     calc(100% - 285px) !important;
        flex:      0 0 calc(100% - 285px) !important;
        max-width: calc(100% - 285px) !important;
        transition: width 0.25s ease, max-width 0.25s ease;
      }
      .sidebar-collapsed .col-sm-3 {
        width:     0px !important;
        flex:      0 0 0px !important;
        max-width: 0px !important;
        padding:   0 !important;
      }
      .sidebar-collapsed .col-sm-9 {
        width:     100% !important;
        flex:      0 0 100% !important;
        max-width: 100% !important;
      }
    }

    /* ---- Chevron toggle button -------------------------------------------- */
    #sidebarToggle {
      position:      fixed;
      top:           120px;
      left:          265px;
      z-index:       9999;
      background:    #4a7fb5;
      color:         white;
      border:        none;
      border-radius: 0 4px 4px 0;
      padding:       6px 5px;
      cursor:        pointer;
      font-size:     14px;
      line-height:   1;
      width:         18px;
      transition:    left 0.25s ease;
    }
    #sidebarToggle:hover { background: #2c4f70; }

    /* ---- Compact DT table rows -------------------------------------------- */
    #forecastTable table.dataTable thead th,
    #forecastTable table.dataTable tbody td {
      padding-top:    2px !important;
      padding-bottom: 2px !important;
      padding-left:   5px !important;
      padding-right:  5px !important;
      font-size:      12px !important;
      line-height:    1.2 !important;
      white-space:    nowrap !important;
    }

    /* ---- Explain panel ---------------------------------------------------- */
    .explain-panel {
      background-color: #f9f9f9;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 12px 14px;
      margin-top: 18px;
      font-size: 12px;
      line-height: 1.5;
      color: #444;
    }
    .explain-panel h5 {
      margin-top: 0; margin-bottom: 8px;
      font-size: 13px; font-weight: bold; color: #222;
    }
    .explain-panel p { margin-bottom: 6px; }

  ")),

  # Chevron toggle button (sits outside sidebarLayout so it overlays freely)
  tags$button(
    id       = "sidebarToggle",
    HTML("&#x276E;"),   # left-pointing chevron; flips to right when collapsed
    onclick  = "
      var el  = document.querySelector('.row');
      var btn = document.getElementById('sidebarToggle');
      el.classList.toggle('sidebar-collapsed');
      if (el.classList.contains('sidebar-collapsed')) {
        btn.innerHTML = '&#x276F;';
        btn.style.left = '0px';
      } else {
        btn.innerHTML = '&#x276E;';
        btn.style.left = '265px';
      }
    "
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      radioButtons(
        "viewMode", "View:",
        choices  = c("Water Supply Season (Apr\u2013Sep)" = "runoff",
                     "Full Water Year (Oct\u2013Sep)"     = "full"),
        selected = "runoff",
        inline   = TRUE
      ),

      hr(style = "margin: 8px 0;"),
      h4("Select WRIA Basin(s)"),

      checkboxGroupInput("wriaFilter", NULL,
                         choices  = wria_list,
                         selected = wria_list),

      actionButton("selectAll",      "Select All"),
      actionButton("clearSelection", "Clear Selection"),
      br(), br(),

      downloadButton("downloadData", "Download Table (CSV)"),

      div(class = "explain-panel",
        h5("About This App"),
        p("Monthly streamflow forecasts from NWRFC for Washington State WRIAs."),
        p("Values shown as % of the 1991\u20132020 normal.
          100% = average; above 100% = above normal; below 100% = below normal."),
        p("The Washington Dept. of Ecology uses 75% of normal as a
          drought indicator threshold."),
        p(tags$em("Italicized column headers"), " indicate months where observed
          (actual) runoff has been substituted for the forecast value."),
        tags$hr(style = "margin: 8px 0;"),
        p(tags$b("Source: "),
          tags$a("NOAA Northwest River Forecast Center",
                 href = "https://www.nwrfc.noaa.gov", target = "_blank")),
        p("Contact: jeffjmarti at gmail.com; no NOAA/NWRFC affiliation."),
        p(tags$b("Note: "),
          "Data refreshed daily. Download saves the currently displayed table.")
      )
    ),

    mainPanel(
      br(),
      DTOutput("forecastTable")
    )
  )
)

# -------------------------
# SERVER
# -------------------------

server <- function(input, output, session) {

  # -- Sidebar buttons --------------------------------------------------------
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "wriaFilter", selected = wria_list)
  })
  observeEvent(input$clearSelection, {
    updateCheckboxGroupInput(session, "wriaFilter", selected = character(0))
  })

  # ── Shared reactive: filtered + column-selected wide table ─────────────────
  display_tbl <- reactive({
    req(input$wriaFilter)

    mon_cols <- if (input$viewMode == "runoff") {
      intersect(RUNOFF_MONTHS, months_available)
    } else {
      months_available   # already in WY order from pipeline
    }

    forecast_wide %>%
      filter(WRIA_NM %in% input$wriaFilter) %>%
      arrange(WRIA_NR, Name) %>%
      mutate(WRIA = sprintf("%02d \u2013 %s", WRIA_NR, WRIA_NM)) %>%
      select(WRIA, Name, all_of(mon_cols))
  })

  # ── Tab 1: DT forecast table ───────────────────────────────────────────────
  # formatPercentage multiplies raw 0-1 values by 100 and appends "%".
  # styleInterval cuts still operate on the raw 0-1 scale underneath.
  output$forecastTable <- renderDT({
    tbl      <- display_tbl()
    mon_cols <- intersect(names(tbl), WY_MONTH_LEVELS)

    if (nrow(tbl) == 0) return(datatable(tbl, rownames = FALSE))

    # 0-based column indices for DT columnDefs
    # Columns: WRIA (0), Name (1), months (2+)
    mon_idx_0 <- which(names(tbl) %in% WY_MONTH_LEVELS) - 1L

    datatable(
      tbl,
      rownames = FALSE,
      class    = "compact stripe",
      options  = list(
        pageLength     = nrow(tbl),
        dom            = "ti",
        scrollX        = TRUE,
        autoWidth      = TRUE,
        headerCallback = JS(obs_header_js),
        columnDefs     = list(
          list(className = "dt-center", targets = mon_idx_0)
        ),
        drawCallback   = JS("function() { this.api().columns.adjust(); }")
      )
    ) %>%
      formatPercentage(mon_cols, digits = 0) %>%
      formatStyle(
        mon_cols,
        backgroundColor = styleInterval(COLOR_CUTS, COLOR_VALUES),
        color           = styleInterval(COLOR_CUTS, FONT_COLORS)
      )
  })

  # ── Download ────────────────────────────────────────────────────────────────
  output$downloadData <- downloadHandler(
    filename = function() {
      view_tag <- if (input$viewMode == "runoff") "apr-sep" else "full-wy"
      paste0("nwrfc_forecast_", view_tag, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      export_df <- display_tbl() %>%
        mutate(`Data as of` = last_updated) %>%
        relocate(`Data as of`, .before = 1)
      write.csv(export_df, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )
}

shinyApp(ui = ui, server = server)
