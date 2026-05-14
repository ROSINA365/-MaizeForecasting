#  Kenya Maize Price Forecasting Dashboard

A Shiny web application for forecasting monthly maize prices across five Kenyan counties to support proactive food security planning.

##  Live App
 **[https://sharonrosina.shinyapps.io/MaizeForecasting](https://sharonrosina.shinyapps.io/MaizeForecasting)**

##  Overview
Maize is Kenya's primary staple food. When prices spike, low-income households are hit hardest. This dashboard provides 6 to 24-month price forecasts powered by live WFP data, giving governments and aid organizations early warning to intervene before a crisis develops.

##  Counties Covered
County - Zone - Best Model 

|Nairobi- Urban - TSLM |
|Turkana - Arid (ASAL) - TSLM| 
|Marsabit - Arid (ASAL) - Naive |
|Mombasa - Urban - Naive |
|Uasin Gishu - Agricultural - Naive |

## Forecasting Models
- **Naive** — Uses last month's price to predict forward
- **ETS** — Exponential smoothing, weights recent prices more heavily
- **ARIMA** — Uses patterns between past and present prices
- **TSLM** — Linear trend + seasonal adjustments

##  Data Source
Live data from the [WFP Kenya Food Prices dataset](https://data.humdata.org/dataset/wfp-food-prices-for-kenya) via Humanitarian Data Exchange (HDX). Updated monthly.

##  How to Run Locally
```r
# Install required packages
install.packages(c("shiny", "shinydashboard", "tidyverse",
                   "forecast", "zoo", "lubridate",
                   "plotly", "DT", "httr", "jsonlite"))

# Run the app
shiny::runApp("app.R")
```

##  Developer
- **Name:** Sharon Rosina Wamalwa
- **Course:** DSCI 725 — Data Mining for Competitive Advantage
- **Instructor:** Thomas Tiahrt, Ph.D.
- **Institution:** University of South Dakota
- **Year:** 2026
