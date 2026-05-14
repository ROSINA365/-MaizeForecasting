# ============================================================
# Kenya Maize Price Forecasting Dashboard
# Powered by WFP Live Data
# Author: Sharon Rosina Wamalwa
# Course: DSCI 725 — Data Mining for Competitive Advantage
# ============================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(forecast)
library(zoo)
library(lubridate)
library(plotly)
library(DT)
library(httr)
library(jsonlite)

# ============================================================
# FOOD SECURITY THRESHOLDS
# ============================================================
thresholds <- list(
  Marsabit    = list(warning = 70,  critical = 85),
  Mombasa     = list(warning = 50,  critical = 60),
  Nairobi     = list(warning = 55,  critical = 70),
  Turkana     = list(warning = 100, critical = 120),
  `Uasin Gishu` = list(warning = 60, critical = 80)
)

# ============================================================
# DATA LOADING FUNCTION (WFP Live Data)
# ============================================================
load_wfp_data <- function() {
  tryCatch({
    url <- "https://data.humdata.org/dataset/wfp-food-prices-for-kenya/resource/517ee1bf-2437-4f8c-aa1b-cb9925b9d437/download/wfp_food_prices_ken.csv"
    
    df <- read.csv(url, stringsAsFactors = FALSE)
    df <- df[-1, ]
    df$date <- as.Date(df$date)
    if (all(is.na(df$date))) {
      df$date <- as.Date(df$date, format = "%m/%d/%Y")
    }
    df$price <- as.numeric(df$price)
    
    maize <- df %>%
      filter(commodity %in% c("Maize (white)", 
                              "Maize (white, dry)")) %>%
      filter(admin2 %in% c("Uasin Gishu", "Turkana", 
                           "Marsabit", "Nairobi", 
                           "Mombasa")) %>%
      mutate(price_per_kg = case_when(
        unit == "90 KG" ~ price / 90,
        unit == "50 KG" ~ price / 50,
        unit == "KG"    ~ price,
        TRUE            ~ price
      )) %>%
      filter(!is.na(price_per_kg), !is.na(date))
    
    return(maize)
    
  }, error = function(e) {
    return(NULL)
  })
}

# ============================================================
# TIME SERIES CREATION FUNCTION
# ============================================================
make_ts <- function(data, county_name) {
  county_data <- data %>%
    filter(admin2 == county_name) %>%
    arrange(date) %>%
    mutate(date = floor_date(date, "month")) %>%
    group_by(date) %>%
    summarise(price_per_kg = mean(price_per_kg, 
                                  na.rm = TRUE)) %>%
    ungroup()
  
  if (nrow(county_data) < 24) return(NULL)
  
  all_months <- seq(min(county_data$date),
                    max(county_data$date),
                    by = "month")
  
  full_data <- data.frame(date = all_months) %>%
    left_join(county_data, by = "date")
  
  full_data$price_per_kg <- na.approx(
    full_data$price_per_kg, na.rm = FALSE)
  
  ts(full_data$price_per_kg,
     start = c(year(min(all_months)), 
               month(min(all_months))),
     frequency = 12)
}

# ============================================================
# FORECASTING FUNCTION — FIXED TSLM
# ============================================================
run_forecast <- function(ts_data, county, 
                         model_choice, horizon) {
  results <- list()
  
  tryCatch({
    if (model_choice == "Best Model (Recommended)") {
      if (county %in% c("Nairobi", "Turkana")) {
        t   <- seq_along(ts_data)
        mon <- factor(cycle(ts_data))
        fit <- tslm(ts_data ~ t + mon)
        last_t   <- max(t)
        last_mon <- as.numeric(tail(cycle(ts_data), 1))
        new_mon  <- factor(
          ((last_mon - 1 + 1:horizon) %% 12) + 1,
          levels = levels(mon)
        )
        fc <- forecast(fit, newdata = data.frame(
          t   = last_t + 1:horizon,
          mon = new_mon
        ))
        results$forecast   <- fc
        results$model_name <- "TSLM (Best for this county)"
        results$fitted     <- fitted(fit)
      } else {
        fc <- naive(ts_data, h = horizon)
        results$forecast   <- fc
        results$model_name <- "Naive (Best for this county)"
        results$fitted     <- fitted(fc)
      }
    } else if (model_choice == "Naive") {
      fc <- naive(ts_data, h = horizon)
      results$forecast   <- fc
      results$model_name <- "Naive Model"
      results$fitted     <- fitted(fc)
    } else if (model_choice == "ETS") {
      fit <- ets(ts_data)
      fc  <- forecast(fit, h = horizon)
      results$forecast   <- fc
      results$model_name <- paste("ETS:", fit$method)
      results$fitted     <- fitted(fit)
    } else if (model_choice == "ARIMA") {
      fit <- auto.arima(ts_data)
      fc  <- forecast(fit, h = horizon)
      results$forecast   <- fc
      results$model_name <- "ARIMA Model"
      results$fitted     <- fitted(fit)
    } else if (model_choice == "TSLM") {
      t   <- seq_along(ts_data)
      mon <- factor(cycle(ts_data))
      fit <- tslm(ts_data ~ t + mon)
      last_t   <- max(t)
      last_mon <- as.numeric(tail(cycle(ts_data), 1))
      new_mon  <- factor(
        ((last_mon - 1 + 1:horizon) %% 12) + 1,
        levels = levels(mon)
      )
      fc <- forecast(fit, newdata = data.frame(
        t   = last_t + 1:horizon,
        mon = new_mon
      ))
      results$forecast   <- fc
      results$model_name <- "TSLM Model"
      results$fitted     <- fitted(fit)
    }
  }, error = function(e) {
    results$error <- e$message
  })
  
  return(results)
}

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = "Kenya Maize Price Forecasting",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 280,
    
    tags$div(
      style = "padding: 15px; color: #ecf0f1; 
               font-size: 12px;
               border-bottom: 1px solid #2c3e50; 
               margin-bottom: 10px;",
      tags$p("Live WFP Data", 
             style = "margin: 0; font-weight: bold;"),
      tags$p("Humanitarian Data Exchange",
             style = "margin: 4px 0 0 0; 
                      font-size: 11px; opacity: 0.7;")
    ),
    
    sidebarMenu(
      menuItem("Dashboard",    
               tabName = "dashboard", 
               icon = icon("chart-line")),
      menuItem("Forecast Table", 
               tabName = "table",     
               icon = icon("table")),
      menuItem("About",        
               tabName = "about",     
               icon = icon("info-circle"))
    ),
    
    tags$hr(style = "border-color: #2c3e50;"),
    
    tags$div(
      style = "padding: 0 15px;",
      
      selectInput(
        "county",
        tags$span("Select County",
                  style = "color:white; font-weight:bold;"),
        choices  = c("Marsabit", "Mombasa", "Nairobi",
                     "Turkana", "Uasin Gishu"),
        selected = "Nairobi"
      ),
      
      selectInput(
        "model",
        tags$span("Forecasting Model",
                  style = "color:white; font-weight:bold;"),
        choices  = c("Best Model (Recommended)",
                     "Naive", "ETS", "ARIMA", "TSLM"),
        selected = "Best Model (Recommended)"
      ),
      
      sliderInput(
        "horizon",
        tags$span("Forecast Horizon (Months)",
                  style = "color:white; font-weight:bold;"),
        min = 6, max = 24, value = 12, step = 6
      ),
      
      tags$br(),
      
      actionButton(
        "forecast_btn",
        "Generate Forecast",
        style = "width:100%; background:#27ae60;
                 color:white; border:none; 
                 padding:12px; font-size:15px;
                 font-weight:bold; border-radius:8px;"
      ),
      
      tags$br(), tags$br(),
      
      actionButton(
        "refresh_btn",
        "Refresh Live Data",
        style = "width:100%; background:#2980b9;
                 color:white; border:none; 
                 padding:10px; font-size:13px;
                 border-radius:8px;"
      )
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius: 10px; 
             border-top: 3px solid #27ae60; }
      .alert-safe     { background:#d4edda; 
                        border-left:5px solid #28a745;
                        padding:15px; border-radius:8px; 
                        margin-bottom:15px; }
      .alert-warning  { background:#fff3cd; 
                        border-left:5px solid #ffc107;
                        padding:15px; border-radius:8px; 
                        margin-bottom:15px; }
      .alert-critical { background:#f8d7da; 
                        border-left:5px solid #dc3545;
                        padding:15px; border-radius:8px; 
                        margin-bottom:15px; }
      .about-section  { margin-bottom: 20px; }
      .about-section h4 { color: #27ae60; 
                          border-bottom: 2px solid #27ae60;
                          padding-bottom: 5px; }
      .about-section h5 { color: #2c3e50; 
                          margin-top: 15px; 
                          font-weight: bold; }
      .model-card     { background: #f8f9fa; 
                        border-left: 4px solid #27ae60;
                        padding: 10px 15px; 
                        margin-bottom: 10px;
                        border-radius: 0 8px 8px 0; }
      .county-card    { background: #f8f9fa;
                        border-left: 4px solid #2980b9;
                        padding: 10px 15px;
                        margin-bottom: 10px;
                        border-radius: 0 8px 8px 0; }
      .highlight-box  { background: #eafaf1;
                        border: 1px solid #27ae60;
                        padding: 12px 15px;
                        border-radius: 8px;
                        margin-bottom: 15px; }
    "))),
    
    tabItems(
      
      # ---- DASHBOARD TAB ----
      tabItem(tabName = "dashboard",
              
              fluidRow(column(12, uiOutput("status_bar"))),
              fluidRow(column(12, uiOutput("alert_box"))),
              
              fluidRow(
                infoBoxOutput("box_current",  width = 3),
                infoBoxOutput("box_forecast", width = 3),
                infoBoxOutput("box_change",   width = 3),
                infoBoxOutput("box_months",   width = 3)
              ),
              
              fluidRow(
                box(
                  title = uiOutput("chart_title"),
                  width = 12, status = "success",
                  solidHeader = TRUE,
                  plotlyOutput("forecast_plot", 
                               height = "450px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Accuracy Metrics",
                  width = 6, status = "info",
                  solidHeader = TRUE,
                  tableOutput("accuracy_table")
                ),
                box(
                  title = "Next 6 Months Forecast",
                  width = 6, status = "warning",
                  solidHeader = TRUE,
                  tableOutput("forecast_summary")
                )
              )
      ),
      
      # ---- TABLE TAB ----
      tabItem(tabName = "table",
              fluidRow(
                box(
                  title = "Full Forecast Table",
                  width = 12, status = "success",
                  solidHeader = TRUE,
                  tags$div(
                    style = "background:#eafaf1; 
                             border-left:4px solid #27ae60;
                             padding:12px 15px; 
                             border-radius:0 8px 8px 0;
                             margin-bottom:15px;",
                    tags$b("How to read this table:"),
                    tags$ul(
                      style = "margin: 8px 0 0 0;",
                      tags$li(tags$b("Forecast (KES/KG):"),
                              " The predicted maize price per
                              kilogram for that month."),
                      tags$li(tags$b("Lower 95%:"),
                              " The lowest price we would 
                              realistically expect. There is 
                              only a 2.5% chance the actual 
                              price will fall below this value.
                              Think of it as the 
                              best-case scenario."),
                      tags$li(tags$b("Upper 95%:"),
                              " The highest price we would 
                              realistically expect. There is 
                              only a 2.5% chance the actual 
                              price will go above this value. 
                              Think of it as the 
                              worst-case scenario for 
                              planning purposes."),
                      tags$li(tags$b("Status:"),
                              " \U0001f7e2 Safe = price is within 
                              normal range; \U0001f7e1 Warning = 
                              price is getting high and should 
                              be monitored; \U0001f534 Critical = 
                              price is at a level likely to 
                              cause food insecurity and 
                              requires immediate action.")
                    )
                  ),
                  DTOutput("full_table")
                )
              )
      ),
      
      # ---- ABOUT TAB ----
      tabItem(tabName = "about",
              fluidRow(
                box(
                  width = 12, status = "success",
                  solidHeader = TRUE,
                  title = "About This Dashboard",
                  
                  tags$div(
                    class = "about-section",
                    style = "padding: 10px;",
                    
                    # --- OBJECTIVE ---
                    tags$div(
                      class = "highlight-box",
                      tags$h4("Dashboard Objective"),
                      tags$p("This dashboard was developed to 
                        support proactive food security planning 
                        across five Kenyan counties. Maize is 
                        Kenya's primary staple food, consumed 
                        across all income groups. When maize 
                        prices rise sharply, low-income 
                        households are hit hardest and acute 
                        food insecurity follows. By forecasting 
                        prices up to 24 months ahead, this tool 
                        gives governments, aid organizations, 
                        and planners early warning to 
                        pre-position food reserves, target 
                        assistance, and intervene before a 
                        crisis develops — rather than reacting 
                        after it occurs.")
                    ),
                    
                    tags$hr(),
                    
                    # --- DATA SOURCE ---
                    tags$h5("Data Source"),
                    tags$p("Live data is fetched automatically 
                      from the ",
                           tags$b("World Food Programme (WFP) 
                             Kenya Food Prices dataset"),
                           " hosted on the Humanitarian Data 
                      Exchange (HDX). The dataset covers maize 
                      prices across 28 Kenyan counties from 
                      January 2006 to present, and is updated 
                      monthly. Every time you click 
                      Refresh Live Data, the dashboard pulls 
                      the most current prices directly 
                      from WFP."),
                    
                    tags$hr(),
                    
                    # --- WHY THESE 5 COUNTIES ---
                    tags$h5("Why These 5 Counties?"),
                    tags$p("The five counties were selected to 
                      represent Kenya's three main 
                      agro-ecological zones, each facing 
                      different food security challenges:"),
                    tags$div(class = "county-card",
                             tags$b("Agricultural Zone — 
                             Uasin Gishu:"),
                             " Kenya's breadbasket and a major 
                      maize-producing region. Prices here 
                      reflect harvest conditions and supply 
                      levels for the whole country."
                    ),
                    tags$div(class = "county-card",
                             tags$b("Arid and Semi-Arid Lands — 
                             Turkana and Marsabit:"),
                             " The most food-insecure counties in 
                      Kenya. These regions depend entirely on 
                      maize imports since they cannot grow it 
                      locally. Price spikes here directly 
                      translate to hunger."
                    ),
                    tags$div(class = "county-card",
                             tags$b("Urban Centers — 
                             Nairobi and Mombasa:"),
                             " Kenya's two largest cities. Urban 
                      maize prices affect millions of 
                      low-income households who purchase all 
                      their food from markets."
                    ),
                    
                    tags$hr(),
                    
                    # --- FORECASTING MODELS ---
                    tags$h5("Forecasting Models"),
                    tags$p("Four models were trained and 
                      evaluated on 20 years of historical 
                      maize price data. Each model was tested 
                      on the last 24 months of data it had 
                      never seen before, to measure how 
                      accurately it predicts real future 
                      prices. The model with the lowest 
                      forecast error for each county was 
                      selected as the Best Model."),
                    
                    tags$div(class = "model-card",
                             tags$b("Best Model (Recommended): "),
                             "Automatically uses the model that 
                      performed best for the selected county 
                      based on our analysis. For Nairobi and 
                      Turkana this is TSLM; for Marsabit, 
                      Mombasa, and Uasin Gishu this is the 
                      Naive model."
                    ),
                    tags$div(class = "model-card",
                             tags$b("Naive: "),
                             "The simplest forecast — it uses last 
                      month's price to predict this month's 
                      price, and repeats that forward for 
                      however many months you select. 
                      Surprisingly, this was the hardest 
                      model to beat for counties with 
                      volatile, shock-driven prices like 
                      Marsabit and Mombasa."
                    ),
                    tags$div(class = "model-card",
                             tags$b("ETS (Exponential Smoothing): "),
                             "Gives more weight to recent prices 
                      than older ones — similar to how we 
                      naturally pay more attention to what 
                      happened last month than five years ago. 
                      It automatically detects whether prices 
                      are trending up or down and adjusts 
                      its forecast accordingly."
                    ),
                    tags$div(class = "model-card",
                             tags$b("ARIMA: "),
                             "Uses the pattern between a month's 
                      price and previous months' prices to 
                      predict the future. It also accounts 
                      for the fact that maize prices tend to 
                      drift upward over time — confirmed 
                      statistically for all five counties 
                      using the Augmented Dickey-Fuller test."
                    ),
                    tags$div(class = "model-card",
                             tags$b("TSLM (Time Series Linear Model): "),
                             "Fits a straight-line trend through the 
                      historical maize prices and adds monthly 
                      seasonal adjustments. For example, for 
                      Nairobi it learned that prices tend to 
                      be slightly lower after the main harvest 
                      season and higher during lean months — 
                      and projects that pattern forward. 
                      This model achieved the best accuracy 
                      for Nairobi (only 4.4% average error 
                      over 24 months) and Turkana, where a 
                      clear long-run upward trend made the 
                      linear approach highly effective."
                    ),
                    
                    tags$hr(),
                    
                    # --- WHY 12 MONTHS ---
                    tags$h5("Why Does the Forecast Default 
                            to 12 Months?"),
                    tags$div(
                      class = "highlight-box",
                      tags$p("The dashboard defaults to a 
                        12-month forecast horizon because this 
                        aligns with Kenya's annual agricultural 
                        planning cycle — one full planting, 
                        growing, and harvest season. A 
                        12-month window gives food security 
                        planners enough lead time to arrange 
                        grain reserves or targeted assistance, 
                        while keeping forecasts reliable. You 
                        can extend to 24 months for longer-term 
                        strategic planning, though uncertainty 
                        — shown by the shaded confidence bands 
                        on the chart — naturally increases the 
                        further ahead we forecast.")
                    ),
                    
                    tags$hr(),
                    
                    # --- BEST MODEL PER COUNTY TABLE ---
                    tags$h5("Best Model Per County"),
                    tags$table(
                      class = "table table-bordered table-striped",
                      tags$thead(
                        tags$tr(
                          tags$th("County"),
                          tags$th("Best Model"),
                          tags$th("Why It Won")
                        )
                      ),
                      tags$tbody(
                        tags$tr(
                          tags$td("Marsabit"),
                          tags$td("Naive"),
                          tags$td("Prices are volatile and 
                            shock-driven. No complex model 
                            could reliably predict direction, 
                            so the simplest approach won.")
                        ),
                        tags$tr(
                          tags$td("Mombasa"),
                          tags$td("Naive"),
                          tags$td("Shorter data history and 
                            irregular price movements caused 
                            complex models to overfit.")
                        ),
                        tags$tr(
                          tags$td("Nairobi"),
                          tags$td("TSLM"),
                          tags$td("Prices followed a clear, 
                            steady upward trend with consistent 
                            seasonal patterns. TSLM achieved 
                            only 4.4% average forecast error 
                            over 24 months.")
                        ),
                        tags$tr(
                          tags$td("Turkana"),
                          tags$td("TSLM"),
                          tags$td("Strong persistent upward 
                            trend made the linear model 
                            effective. Prices are forecast 
                            to approach KES 100/KG — an 
                            urgent food security signal.")
                        ),
                        tags$tr(
                          tags$td("Uasin Gishu"),
                          tags$td("Naive"),
                          tags$td("An extraordinary price spike 
                            in 2022 misled all complex models. 
                            The Naive model still performed 
                            best among the four.")
                        )
                      )
                    ),
                    
                    tags$hr(),
                    
                    # --- ALERT THRESHOLDS ---
                    tags$h5("Food Security Alert Thresholds"),
                    tags$p("Thresholds were set based on 
                      historical average prices and food 
                      security assessments per county. When 
                      forecast prices cross these levels the 
                      dashboard raises an alert to 
                      prompt action."),
                    tags$table(
                      class = "table table-bordered table-striped",
                      tags$thead(
                        tags$tr(
                          tags$th("County"),
                          tags$th("Warning (KES/KG)"),
                          tags$th("Critical (KES/KG)")
                        )
                      ),
                      tags$tbody(
                        tags$tr(tags$td("Marsabit"),
                                tags$td("70"),
                                tags$td("85")),
                        tags$tr(tags$td("Mombasa"),
                                tags$td("50"),
                                tags$td("60")),
                        tags$tr(tags$td("Nairobi"),
                                tags$td("55"),
                                tags$td("70")),
                        tags$tr(tags$td("Turkana"),
                                tags$td("100"),
                                tags$td("120")),
                        tags$tr(tags$td("Uasin Gishu"),
                                tags$td("60"),
                                tags$td("80"))
                      )
                    ),
                    
                    tags$hr(),
                    
                    # --- CREDITS ---
                    tags$div(
                      style = "background:#f8f9fa; 
                               padding:15px; 
                               border-radius:8px;",
                      tags$p(
                        tags$b("Developer: "), 
                        "Sharon Rosina Wamalwa", tags$br(),
                        tags$b("Course: "), 
                        "DSCI 725 — Data Mining for 
                        Competitive Advantage", tags$br(),
                        tags$b("Instructor: "), 
                        "Thomas Tiahrt, Ph.D.", tags$br(),
                        tags$b("Institution: "), 
                        "University of South Dakota", 
                        tags$br(),
                        tags$b("Year: "), "2026"
                      )
                    )
                  )
                )
              )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  wfp_data    <- reactiveVal(NULL)
  data_loaded <- reactiveVal(FALSE)
  
  observe({
    withProgress(message = "Loading live WFP data...", {
      data <- load_wfp_data()
      if (!is.null(data)) {
        wfp_data(data)
        data_loaded(TRUE)
      }
    })
  })
  
  observeEvent(input$refresh_btn, {
    withProgress(message = "Refreshing...", {
      data <- load_wfp_data()
      if (!is.null(data)) {
        wfp_data(data)
        data_loaded(TRUE)
        showNotification("Data refreshed!", 
                         type = "message", duration = 3)
      } else {
        showNotification("Could not connect to WFP.",
                         type = "error", duration = 5)
      }
    })
  })
  
  county_ts <- eventReactive(input$forecast_btn, {
    req(wfp_data())
    make_ts(wfp_data(), input$county)
  }, ignoreNULL = FALSE)
  
  fc_results <- eventReactive(input$forecast_btn, {
    req(county_ts())
    run_forecast(county_ts(), input$county,
                 input$model, input$horizon)
  }, ignoreNULL = FALSE)
  
  output$status_bar <- renderUI({
    if (!data_loaded()) {
      tags$div(class = "alert-warning",
               "Loading live data from WFP...")
    } else {
      county <- input$county
      last_date <- wfp_data() %>%
        filter(admin2 == county) %>%
        summarise(d = max(date, na.rm = TRUE)) %>%
        pull(d)
      tags$div(class = "alert-safe",
               paste("Live WFP data loaded. Latest observation for",
                     county, ":", 
                     format(last_date, "%B %Y")))
    }
  })
  
  output$alert_box <- renderUI({
    req(fc_results(), county_ts())
    fc     <- fc_results()$forecast
    if (is.null(fc)) return(NULL)
    thresh <- thresholds[[input$county]]
    max_fc <- max(as.numeric(fc$mean), na.rm = TRUE)
    
    if (max_fc >= thresh$critical) {
      tags$div(class = "alert-critical",
               tags$h5("\U0001f534 CRITICAL FOOD SECURITY ALERT",
                       style = "margin:0 0 8px 0;"),
               paste0("Forecast prices for ", input$county,
                      " may reach KES ", round(max_fc, 1),
                      "/KG — above critical threshold of KES ",
                      thresh$critical, "/KG."))
    } else if (max_fc >= thresh$warning) {
      tags$div(class = "alert-warning",
               tags$h5("\U0001f7e1 FOOD SECURITY WARNING",
                       style = "margin:0 0 8px 0;"),
               paste0("Forecast prices for ", input$county,
                      " may reach KES ", round(max_fc, 1),
                      "/KG — above warning threshold of KES ",
                      thresh$warning, "/KG."))
    } else {
      tags$div(class = "alert-safe",
               tags$h5("\U0001f7e2 STATUS: STABLE",
                       style = "margin:0 0 8px 0;"),
               paste0("Prices for ", input$county,
                      " are forecast to remain below KES ",
                      thresh$warning, "/KG."))
    }
  })
  
  output$box_current <- renderInfoBox({
    req(county_ts())
    val <- round(tail(as.numeric(county_ts()), 1), 2)
    infoBox("Current Price", paste("KES", val, "/KG"),
            icon = icon("tag"), 
            color = "green", fill = TRUE)
  })
  
  output$box_forecast <- renderInfoBox({
    req(fc_results())
    fc  <- fc_results()$forecast
    val <- round(mean(as.numeric(fc$mean), 
                      na.rm = TRUE), 2)
    infoBox("Avg Forecast", paste("KES", val, "/KG"),
            icon = icon("chart-line"), 
            color = "blue", fill = TRUE)
  })
  
  output$box_change <- renderInfoBox({
    req(county_ts(), fc_results())
    cur    <- tail(as.numeric(county_ts()), 1)
    fut    <- mean(as.numeric(fc_results()$forecast$mean),
                   na.rm = TRUE)
    change <- round(((fut - cur) / cur) * 100, 1)
    col    <- if (change > 10) "red" else if 
    (change > 0) "yellow" else "green"
    infoBox("Expected Change",
            paste0(ifelse(change > 0, "+", ""), 
                   change, "%"),
            icon = icon("percent"), 
            color = col, fill = TRUE)
  })
  
  output$box_months <- renderInfoBox({
    req(county_ts())
    infoBox("Historical Data",
            paste(length(county_ts()), "months"),
            icon = icon("database"),
            color = "purple", fill = TRUE)
  })
  
  output$chart_title <- renderUI({
    req(fc_results())
    tags$span(
      paste0(input$county, " — Historical & ", 
             input$horizon, "-Month Forecast"),
      tags$br(),
      tags$small(style = "font-size:12px; opacity:0.8;",
                 paste("Model:", fc_results()$model_name))
    )
  })
  
  output$forecast_plot <- renderPlotly({
    req(county_ts(), fc_results())
    
    ts_data <- county_ts()
    fc      <- fc_results()$forecast
    thresh  <- thresholds[[input$county]]
    if (is.null(fc)) return(NULL)
    
    hist_dates <- seq(
      as.Date(paste(start(ts_data)[1],
                    start(ts_data)[2], "01", sep = "-")),
      by = "month", length.out = length(ts_data))
    
    fy <- end(ts_data)[1]
    fm <- end(ts_data)[2] + 1
    if (fm > 12) { fm <- 1; fy <- fy + 1 }
    fc_dates <- seq(
      as.Date(paste(fy, fm, "01", sep = "-")),
      by = "month", length.out = input$horizon)
    
    fc_mean <- as.numeric(fc$mean)
    fc_lo95 <- as.numeric(fc$lower[, 2])
    fc_hi95 <- as.numeric(fc$upper[, 2])
    fc_lo80 <- as.numeric(fc$lower[, 1])
    fc_hi80 <- as.numeric(fc$upper[, 1])
    
    plot_ly() %>%
      add_ribbons(x = ~fc_dates,
                  ymin = ~fc_lo95, ymax = ~fc_hi95,
                  fillcolor = "rgba(39,174,96,0.1)",
                  line = list(color = "transparent"),
                  name = "95% Confidence Band") %>%
      add_ribbons(x = ~fc_dates,
                  ymin = ~fc_lo80, ymax = ~fc_hi80,
                  fillcolor = "rgba(39,174,96,0.2)",
                  line = list(color = "transparent"),
                  name = "80% Confidence Band") %>%
      add_lines(x = ~hist_dates,
                y = ~as.numeric(ts_data),
                name = "Historical Price",
                line = list(color = "#2c3e50", 
                            width = 2)) %>%
      add_lines(x = ~fc_dates, y = ~fc_mean,
                name = "Forecast",
                line = list(color = "#27ae60",
                            width = 3, 
                            dash = "dash")) %>%
      add_lines(
        x = ~c(min(hist_dates), max(fc_dates)),
        y = ~rep(thresh$warning, 2),
        name = paste("Warning KES", thresh$warning),
        line = list(color = "#f39c12",
                    width = 1.5, dash = "dot")) %>%
      add_lines(
        x = ~c(min(hist_dates), max(fc_dates)),
        y = ~rep(thresh$critical, 2),
        name = paste("Critical KES", thresh$critical),
        line = list(color = "#e74c3c",
                    width = 1.5, dash = "dot")) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price (KES per KG)"),
        legend = list(orientation = "h", y = -0.25),
        hovermode = "x unified",
        paper_bgcolor = "white",
        plot_bgcolor  = "white"
      )
  })
  
  output$accuracy_table <- renderTable({
    req(county_ts(), fc_results())
    fc      <- fc_results()$forecast
    ts_data <- county_ts()
    test    <- tail(ts_data, 24)
    tryCatch({
      acc <- accuracy(fc, test)
      data.frame(
        Metric     = c("ME","RMSE","MAE","MAPE","MASE"),
        Training   = round(acc[1, c("ME","RMSE","MAE",
                                    "MAPE","MASE")], 3),
        Validation = round(acc[2, c("ME","RMSE","MAE",
                                    "MAPE","MASE")], 3)
      )
    }, error = function(e) {
      data.frame(Note = "Run forecast to see metrics")
    })
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$forecast_summary <- renderTable({
    req(fc_results(), county_ts())
    fc      <- fc_results()$forecast
    thresh  <- thresholds[[input$county]]
    ts_data <- county_ts()
    
    fy <- end(ts_data)[1]
    fm <- end(ts_data)[2] + 1
    if (fm > 12) { fm <- 1; fy <- fy + 1 }
    
    n      <- min(6, input$horizon)
    dates  <- seq(as.Date(paste(fy, fm, "01", sep="-")),
                  by = "month", length.out = n)
    prices <- round(as.numeric(fc$mean)[1:n], 2)
    status <- sapply(prices, function(p) {
      if (p >= thresh$critical) "\U0001f534 Critical"
      else if (p >= thresh$warning) "\U0001f7e1 Warning"
      else "\U0001f7e2 Safe"
    })
    data.frame(
      Month    = format(dates, "%B %Y"),
      `KES/KG` = prices,
      Status   = status,
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$full_table <- renderDT({
    req(fc_results(), county_ts())
    fc      <- fc_results()$forecast
    thresh  <- thresholds[[input$county]]
    ts_data <- county_ts()
    
    fy <- end(ts_data)[1]
    fm <- end(ts_data)[2] + 1
    if (fm > 12) { fm <- 1; fy <- fy + 1 }
    
    dates  <- seq(as.Date(paste(fy, fm, "01", sep="-")),
                  by = "month", 
                  length.out = input$horizon)
    prices <- round(as.numeric(fc$mean), 2)
    lo     <- round(as.numeric(fc$lower[, 2]), 2)
    hi     <- round(as.numeric(fc$upper[, 2]), 2)
    status <- sapply(prices, function(p) {
      if (p >= thresh$critical) "\U0001f534 Critical"
      else if (p >= thresh$warning) "\U0001f7e1 Warning"
      else "\U0001f7e2 Safe"
    })
    
    datatable(
      data.frame(
        Month               = format(dates, "%B %Y"),
        `Forecast (KES/KG)` = prices,
        `Lower 95%`         = lo,
        `Upper 95%`         = hi,
        Status              = status,
        check.names = FALSE
      ),
      options = list(pageLength = 12, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui = ui, server = server)
