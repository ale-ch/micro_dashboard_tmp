library(shiny)
library(tmap)
library(plotly)
library(dplyr)
library(DT)
library(bslib)

setwd('/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard')

# source('/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/LOAD_DATA.r')
# source('/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/tests/LOAD_DATA_TEST.r')

source('/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/LOAD_DATA_v2.r')
# source("/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/compute_median_by_nuts.r")
source("/Volumes/T7 Shield/FRES/DB_Comunale/micro_dashboard/aggregate_by_nuts.r")

LEVELS <- c("Municipal", "NUTS3", "NUTS2", "NUTS1", "NUTS0")
AGGREGATION_CHOICES <- c("Mean", "Median", "Sum")
VARIABLES_CHOICES <- names(municipal_data_merged)[14:164]



tmap_mode("view")

ui <- fluidPage(
  theme = bslib::bs_theme(
    bootswatch = "yeti",
    "navbar-bg"            = "#0B61A4",
    "navbar-color"         = "#FFFFFF",
    "navbar-hover-color"   = "#E2E8F0",
    "navbar-active-color"  = "#E2E8F0",
    "navbar-brand-color"   = "#FFFFFF",
    
    "nav-pills-link-active-bg"    = "#4F9DD9",
    "nav-pills-link-active-color" = "#FFFFFF",
    "nav-pills-link-color"        = "#0B61A4",
    
    "btn-bg"            = "#0072B2",
    "btn-color"         = "#FFFFFF",
    "btn-border-color"  = "#0072B2",
    "btn-hover-bg"      = "#3399CC"
  ),
  
  tags$head(
    tags$style(HTML("
      /* Wrap first column body cells in benchmarking table */
      .benchmarking-table td.bm-wrap,
      .benchmarking-table .DTFC_LeftBodyLiner td.bm-wrap {
        white-space: normal !important;
        word-break: break-word;
        min-width: 300px;
      }
      
      .var-desc { font-size: 1.2rem; line-height: 1.4; }
      .var-desc strong { font-weight: 800; }
      #operation_choice_play > label.control-label {
      font-size: 20px;
      font-weight: 700;
      }

      #operation_choice_play label.radio-inline {
        font-size: 1.15rem;
      }

      /* --- Flex layout wrapper --- */
      .flex-container {
        display: flex;
        width: 100%;
        margin: 0;
        padding: 0;
      }

      /* Sidebar = 30%, Main = 70% */
      .flex-sidebar {
        flex: 0 0 30%;
        max-width: 30%;
        padding-right: 10px;   /* small, controlled gap */
      }

      .flex-main {
        flex: 0 0 70%;
        max-width: 70%;
        padding-left: 18px;    /* small, controlled gap */
      }

      /* Inside a flex-container, ignore Bootstrap's col-sm-* widths & floats */
      .flex-container .col-sm-3,
      .flex-container .col-sm-4,
      .flex-container .col-sm-5,
      .flex-container .col-sm-7,
      .flex-container .col-sm-8,
      .flex-container .col-sm-9 {
        float: none;
        width: 100%;
      }

      /* Make the .well (sidebarPanel/mainPanel wrapper) fill its flex column */
      .flex-container .well {
        width: 100%;
        max-width: none;
        margin: 0;
        box-sizing: border-box;
      }

      /* Stack vertically on smaller screens */
      @media (max-width: 992px) {
        .flex-container {
          flex-direction: column;
        }
        .flex-sidebar,
        .flex-main {
          flex: 0 0 100%;
          max-width: 100%;
          padding: 0;
        }
      }
    "))
  ),
  
  navbarPage(
    "Municipal data dashboard",
    
    tabPanel(
      "Welcome",
      div(
        style = "text-align:center; margin-top:60px;",
        tags$img(
          src = "static/FEEM_logo.png",
          style = "max-width:250px; height:auto;"
        ),
        br(), br(),
        h2("Welcome to the dashboard"),
        p("Explore municipal data and indicators.")
      )
    ),
    
    tabPanel(
      "Map",
      div(class = "flex-container",
          div(class = "flex-sidebar",
              sidebarPanel(
                uiOutput("var_select"),
                uiOutput("year_select_ui"),
                uiOutput("comune_select_wrapper_1"),
                radioButtons("level_map", "Granularity", choices = LEVELS, selected = "Municipal", inline = TRUE),
                radioButtons("aggregation", "Type of aggregation", choices = AGGREGATION_CHOICES, selected = "Median", inline = TRUE)
              )
          ),
          div(class = "flex-main",
              mainPanel(
                tabsetPanel(id = "map_tabs", type = "pills",
                            tabPanel("Map", br(), tmapOutput("map")),
                            tabPanel("Data", br(), DTOutput("map_table"))
                )
              )
          )
      )
    ),
    
    tabPanel(
      "Time series",
      div(class = "flex-container",
          div(class = "flex-sidebar",
              sidebarPanel(
                uiOutput("ts_var_select"),
                uiOutput("date_range_ui"),
                uiOutput("comune_select_wrapper")
              )
          ),
          div(class = "flex-main",
              mainPanel(
                tabsetPanel(id = "ts_tabs", type = "pills",
                            tabPanel("Chart", br(), plotlyOutput("ts_plot")),
                            tabPanel("Data", br(), DTOutput("ts_table"))
                )
              )
          )
      )
    )
  )
)

server <- function(input, output, session) {
  # MAP
  output$var_select <- renderUI({
    req(municipal_data_merged)
    selectInput("variable", "Select variable",
                choices = VARIABLES_CHOICES)
  })
  
  output$year_select_ui <- renderUI({
    req(municipal_data_merged)
    req("year" %in% names(municipal_data_merged))
    
    yrs <- sort(unique(municipal_data_merged$year))
    
    selectInput(
      inputId = "year_select",
      label = "Select year",
      choices = yrs,
      selected = NULL,
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  
  # 1. Initialize the empty widget
  output$comune_select_wrapper_1 <- renderUI({
    selectizeInput(
      "selected_comune_map", 
      "Search and Select comune:", 
      choices = sort(unique(municipal_data_merged$COMUNE)), # Leave empty initially
      multiple = TRUE,
      options = list(
        placeholder = 'Type to search...',
        loadThrottle = 100 # Wait 300ms after typing stops before searching
      )
    )
  })
  
  # 2. Update the choices from the server side
  observeEvent(municipal_data_merged, {
    req(municipal_data_merged)
    choices <- sort(unique(municipal_data_merged$COMUNE))
    
    updateSelectizeInput(
      session, 
      "selected_comune", 
      choices = choices, 
      server = TRUE # This enables fast searching/suggestions as you type
    )
  })
  
  map_table_data <- reactive({
    req(municipal_data_merged, input$variable, input$year_select, input$level_map)
    
    if(input$level_map != "Municipal") {
      aggregate_by_nuts(municipal_data_merged, input$level_map, VARIABLES_CHOICES, input$aggregation) %>% 
        filter(year == input$year_select)
    } else {
      if (is.null(input$selected_comune_map)) {
        municipal_data_merged %>% 
          filter(year == input$year_select)
      } else {
        municipal_data_merged %>% 
          filter(year == input$year_select, COMUNE == input$selected_comune_map)
      }
    }
  })
  
  output$map <- renderTmap({
    req(map_table_data(), input$variable)
    
    tm_shape(map_table_data()) +
      tm_polygons(input$variable)
  })
  
  output$map_table <- DT::renderDT({
    req(map_table_data())
    df <- map_table_data() %>% st_drop_geometry()
    
    validate(need(nrow(df) > 0, "No data available for the selected year/variable."))
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 20,
        lengthMenu = list(
          c(20, 50, 100, 500, -1),
          c("20", "50", "100", "500", "All")
        ),
        dom = "flrtip"
      )
    )
  })
  
  # TIME SERIES
  output$ts_var_select <- renderUI({
    req(municipal_data_merged)
    numeric_vars <- names(municipal_data_merged)[sapply(municipal_data_merged, is.numeric)]
    selectInput("ts_variable", "Select variable",
                choices = VARIABLES_CHOICES)
  })
  
  output$date_range_ui <- renderUI({
    req(municipal_data_merged)
    req("year" %in% names(municipal_data_merged))
    
    yrs <- sort(unique(municipal_data_merged$year))
    
    sliderInput(
      "date_range",
      "Select year range",
      min = min(yrs),
      max = max(yrs),
      value = c(min(yrs), max(yrs)),
      step = 1,
      sep = ""
    )
  })
  
  # 1. Initialize the empty widget
  output$comune_select_wrapper <- renderUI({
    selectizeInput(
      "selected_comune", 
      "Search and Select comune:", 
      choices = sort(unique(municipal_data_merged$COMUNE)), # Leave empty initially
      multiple = TRUE,
      options = list(
        placeholder = 'Type to search...',
        loadThrottle = 100 # Wait 300ms after typing stops before searching
      )
    )
  })
  
  # 2. Update the choices from the server side
  observeEvent(municipal_data_merged, {
    req(municipal_data_merged)
    choices <- sort(unique(municipal_data_merged$COMUNE))
    
    updateSelectizeInput(
      session, 
      "selected_comune", 
      choices = choices, 
      server = TRUE # This enables fast searching/suggestions as you type
    )
  })
  
  # Create a reactive dataset for the time series panel
  ts_table_data <- reactive({
    req(municipal_data_merged, input$ts_variable, input$date_range, input$selected_comune)
    
    municipal_data_merged %>%
      filter(
        year >= input$date_range[1],
        year <= input$date_range[2],
        COMUNE %in% input$selected_comune
      ) %>%
      arrange(year)
  })
  
  # Update plot to use the reactive dataset
  output$ts_plot <- renderPlotly({
    df <- ts_table_data()
    
    plot_ly(
      df,
      x = ~year,
      y = as.formula(paste0("~`", input$ts_variable, "`")),
      color = ~COMUNE,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        xaxis = list(title = "Year", tickformat = "d"),
        yaxis = list(title = input$ts_variable)
      )
  })
  
  # Render the data table
  output$ts_table <- DT::renderDT({
    req(ts_table_data())
    
    # Drop geometry if the object is an sf object
    df <- ts_table_data() 
    if(inherits(df, "sf")) {
      df <- sf::st_drop_geometry(df)
    }
    
    validate(need(nrow(df) > 0, "No data available for the selected parameters."))
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 20,
        lengthMenu = list(
          c(20, 50, 100, 500, -1),
          c("20", "50", "100", "500", "All")
        ),
        dom = "flrtip"
      )
    )
  })
}

shinyApp(ui, server)