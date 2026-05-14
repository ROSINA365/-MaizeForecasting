# ============================================================
# Project: Forecasting Monthly Maize Prices in Kenyan Counties
# Student: Sharon Rosina Wamalwa
# Note:  All plots were created in R and saved as high-resolution PNG files 
# in the working directory, then manually inserted into the Word document.
# ============================================================

# --- Load Libraries ---
library(tidyverse)
library(flextable)
library(lubridate)

# Import dataset
setwd("C:/Users/user/OneDrive - The University of South Dakota/Documents/Data Mining for Competitive Advantage/Project/Sharon")
##this checks the working directory
getwd()
maize_data <- read.csv("wfp_food_prices_ken.csv")
head(maize_data)


# Remove the metadata row (row 1)
maize_data <- maize_data[-1, ]

# Fix data types
maize_data$date      <- as.Date(maize_data$date, format = "%m/%d/%Y")
maize_data$price     <- as.numeric(maize_data$price)
maize_data$usdprice  <- as.numeric(maize_data$usdprice)
maize_data$latitude  <- as.numeric(maize_data$latitude)
maize_data$longitude <- as.numeric(maize_data$longitude)

# Confirm it worked
head(maize_data)

############################## Commodity summary ########################
commodity_summary <- data.frame(
  Commodity = c("Maize (white)", "Maize (white, dry)"),
  Period_Covered = c("January 2006 – February 2021", 
                     "January 2021 – November 2025"),
  Total_Observations = c(1229, 605)
)

ft_commodity <- flextable(commodity_summary) %>%
  set_header_labels(
    Commodity = "Commodity Label",
    Period_Covered = "Period Covered",
    Total_Observations = "Total Observations"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_commodity


######################  raw counts per county and commodity ##################
county_raw_summary <- data.frame(
  County = c("Marsabit", "Marsabit", "Mombasa", "Mombasa", 
             "Nairobi", "Nairobi", "Nakuru", "Nakuru",
             "Turkana", "Turkana", "Uasin Gishu", "Uasin Gishu"),
  Commodity = c("Maize (white)", "Maize (white, dry)",
                "Maize (white)", "Maize (white, dry)",
                "Maize (white)", "Maize (white, dry)",
                "Maize (white)", "Maize (white, dry)",
                "Maize (white)", "Maize (white, dry)",
                "Maize (white)", "Maize (white, dry)"),
  Unit = c("KG", "KG", "90 KG", "90 KG",
           "90 KG", "90 KG", "90 KG", "90 KG",
           "KG", "KG", "90 KG", "90 KG"),
  Observations = c(116, 30, 114, 11, 110, 50, 44, 22, 130, 137, 122, 34)
)

ft_raw <- flextable(county_raw_summary) %>%
  set_header_labels(
    County = "County",
    Commodity = "Commodity Label",
    Unit = "Unit",
    Observations = "Observations"
  ) %>%
  bold(part = "header") %>%
  merge_v(j = "County") %>%
  autofit()

ft_raw


 

#############  Standardizing All Prices to KES Per KG #####################
unit_standard_table <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi", "Nakuru", 
             "Turkana", "Uasin Gishu"),
  Unit = c("KG", "90 KG", "90 KG", "90 KG", "KG", "90 KG"),
  Start_Date = c("2006-01-15", "2006-01-15", "2006-01-15",
                 "2015-01-15", "2006-01-15", "2006-01-15"),
  End_Date = c("2025-03-15", "2024-06-15", "2025-10-15",
               "2024-06-15", "2025-09-15", "2025-06-15"),
  Observations = c(146, 125, 160, 66, 267, 156),
  Missing = c(0, 0, 0, 0, 0, 0)
)

ft_standard <- flextable(unit_standard_table) %>%
  set_header_labels(
    County = "County",
    Unit = "Original Unit",
    Start_Date = "Start Date",
    End_Date = "End Date",
    Observations = "Observations",
    Missing = "Missing Values"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_standard

############################################################
# Filter for Maize (white) & Maize (white, dry)
maize_combined <- maize_data %>%
  filter(commodity %in% c("Maize (white)", "Maize (white, dry)"))

# Filter for 6 counties
target_counties <- c("Uasin Gishu", "Nakuru", "Turkana",
                     "Marsabit", "Nairobi", "Mombasa")

maize_six_raw <- maize_combined %>%
  filter(admin2 %in% target_counties)

# Standardize prices to KES per KG
maize_six <- maize_six_raw %>%
  mutate(
    price_per_kg = case_when(
      unit == "90 KG" ~ price / 90,
      unit == "50 KG" ~ price / 50,
      unit == "KG"    ~ price,
      TRUE            ~ price
    )
  )

# Remove Nakuru
maize_final <- maize_six %>%
  filter(admin2 != "Nakuru")

# Confirm everything worked
cat("maize_final created successfully!\n")
cat("Total rows:", nrow(maize_final), "\n")
cat("Counties:", paste(unique(maize_final$admin2), collapse = ", "), "\n")






#################################################################
### Check Nakuru's Gap
# Check Nakuru dates specifically
nakuru_gap <- data.frame(
  Issue = c("Series start date", 
            "Last Maize (white) observation",
            "First Maize (white, dry) observation",
            "Gap between series",
            "Total observations",
            "Missing months throughout series"),
  Detail = c("January 2015 (other counties start January 2006)",
             "February 2020",
             "January 2021",
             "11 months (March 2020 to December 2020)",
             "66 (below reliable threshold for full analysis)",
             "Multiple months missing across 2015-2020")
)

ft_nakuru <- flextable(nakuru_gap) %>%
  set_header_labels(
    Issue = "Data Quality Issue",
    Detail = "Detail"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_nakuru


##################### final dataset summary ####################
final_summary <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi", 
             "Turkana", "Uasin Gishu"),
  Classification = c("ASAL", "Urban", "Urban", 
                     "ASAL", "Agricultural"),
  Start_Date = c("January 2006", "January 2006", "January 2006",
                 "January 2006", "January 2006"),
  End_Date = c("March 2025", "June 2024", "October 2025",
               "September 2025", "June 2025"),
  Observations = c(146, 125, 160, 267, 156)
)

ft1 <- flextable(final_summary) %>%
  set_header_labels(
    County = "County",
    Classification = "Classification",
    Start_Date = "Start Date",
    End_Date = "End Date",
    Observations = "Observations"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft1


########################  Overall summary statistics ######################
price_summary <- maize_final %>%
  summarise(
    Min    = round(min(price_per_kg,  na.rm = TRUE), 2),
    Q1     = round(quantile(price_per_kg, 0.25, na.rm = TRUE), 2),
    Median = round(median(price_per_kg,  na.rm = TRUE), 2),
    Mean   = round(mean(price_per_kg,    na.rm = TRUE), 2),
    Q3     = round(quantile(price_per_kg, 0.75, na.rm = TRUE), 2),
    Max    = round(max(price_per_kg,     na.rm = TRUE), 2),
    SD     = round(sd(price_per_kg,      na.rm = TRUE), 2)
  )

print(price_summary)

# Summary statistics BY county
price_table <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi", 
             "Turkana", "Uasin Gishu"),
  Min    = c(15.00, 11.50, 8.91, 26.00, 10.50),
  Median = c(47.60, 29.30, 33.00, 75.00, 29.20),
  Mean   = c(49.00, 29.00, 34.40, 75.30, 31.50),
  Max    = c(100.00, 64.90, 76.70, 150.00, 90.40),
  SD     = c(18.00, 10.70, 14.40, 23.80, 16.10)
)

ft2 <- flextable(price_table) %>%
  set_header_labels(
    County = "County",
    Min    = "Min (KES)",
    Median = "Median (KES)",
    Mean   = "Mean (KES)",
    Max    = "Max (KES)",
    SD     = "Std Dev (KES)"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft2


### Quick Visual Check
ggplot(maize_final, aes(x = date, y = price_per_kg, color = admin2)) +
  geom_line() +
  facet_wrap(~admin2, scales = "free_y", ncol = 2) +
  labs(
    title = "Maize Price Per KG by County (2006–2025)",
    x     = "Year",
    y     = "Price (KES per KG)",
    color = "County"
  ) +
  theme_minimal() +
  theme(legend.position = "none")




######################################################################################
library(zoo)
library(forecast)

# Function to fill missing months for any county
fill_ts <- function(county_name) {
  
  county_data <- maize_final %>%
    filter(admin2 == county_name) %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(price_per_kg = mean(price_per_kg, na.rm = TRUE)) %>%
    ungroup()
  
  all_months <- seq(min(county_data$date), 
                    max(county_data$date), 
                    by = "month")
  
  full_data <- data.frame(date = all_months) %>%
    left_join(county_data, by = "date")
  
  full_data$price_per_kg <- na.approx(full_data$price_per_kg, 
                                      na.rm = FALSE)
  
  ts(full_data$price_per_kg,
     start = c(year(min(all_months)), month(min(all_months))),
     frequency = 12)
}

# Apply to all five counties
ts_marsabit <- fill_ts("Marsabit")
ts_mombasa  <- fill_ts("Mombasa")
ts_nairobi  <- fill_ts("Nairobi")
ts_turkana  <- fill_ts("Turkana")
ts_uasin    <- fill_ts("Uasin Gishu")

# Confirm all created
cat("✅ Marsabit:", length(ts_marsabit), "months | End:", end(ts_marsabit), "\n")
cat("✅ Mombasa:", length(ts_mombasa), "months | End:", end(ts_mombasa), "\n")
cat("✅ Nairobi:", length(ts_nairobi), "months | End:", end(ts_nairobi), "\n")
cat("✅ Turkana:", length(ts_turkana), "months | End:", end(ts_turkana), "\n")
cat("✅ Uasin Gishu:", length(ts_uasin), "months | End:", end(ts_uasin), "\n")



##################### Data Exploration #################################################

ts_lengths <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi", 
             "Turkana", "Uasin Gishu"),
  Classification = c("ASAL", "Urban", "Urban", 
                     "ASAL", "Agricultural"),
  Time_Series_Length = c(length(ts_marsabit), length(ts_mombasa), 
                         length(ts_nairobi), length(ts_turkana), 
                         length(ts_uasin)),
  Start = c("January 2006", "January 2006", "January 2006",
            "January 2006", "January 2006"),
  Frequency = c("Monthly", "Monthly", "Monthly", 
                "Monthly", "Monthly")
)

ft_ts <- flextable(ts_lengths) %>%
  set_header_labels(
    County = "County",
    Classification = "Classification",
    Time_Series_Length = "Series Length (Months)",
    Start = "Start Date",
    Frequency = "Frequency"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_ts

###################################################################################
###### Individual County Time Series Plots ######
### TS plot for Marsabit
# Check what R thinks the end date is
end(ts_marsabit)
start(ts_marsabit)
length(ts_marsabit)

## fixing the missing gaps
# How many months should there be from Jan 2006 to Mar 2025?
marsabit_dates <- maize_final %>%
  filter(admin2 == "Marsabit") %>%
  arrange(date) %>%
  pull(date)

# Check actual first and last date
cat("First date:", format(min(marsabit_dates)), "\n")
cat("Last date:", format(max(marsabit_dates)), "\n")
cat("Actual observations:", length(marsabit_dates), "\n")

# How many months should exist between first and last date?
cat("Expected months:", 
    length(seq(min(marsabit_dates), 
               max(marsabit_dates), 
               by = "month")), "\n")

# Which months are missing?
all_months <- seq(min(marsabit_dates), 
                  max(marsabit_dates), 
                  by = "month")
missing_months <- all_months[!all_months %in% marsabit_dates]
cat("Missing months:", length(missing_months), "\n")
print(missing_months)

## this output shows: ere is a huge gap from 2021 to 2025 where almost every month
##is missing for Marsabit. This means the "Maize (white, dry)" data for Marsabit 
## only has 30 observations spread very irregularly.

##  filling the missing months using linear interpolation so the time series is 
##complete and continuous. 

library(zoo)

# Function to fill missing months for any county
fill_ts <- function(county_name) {
  
  county_data <- maize_final %>%
    filter(admin2 == county_name) %>%
    arrange(date) %>%
    select(date, price_per_kg)
  
  # Create complete monthly sequence
  all_months <- seq(min(county_data$date), 
                    max(county_data$date), 
                    by = "month")
  
  # Create full data frame with all months
  full_data <- data.frame(date = all_months) %>%
    left_join(county_data, by = "date")
  
  # Fill missing values using linear interpolation
  full_data$price_per_kg <- na.approx(full_data$price_per_kg, 
                                      na.rm = FALSE)
  
  # Convert to time series
  ts(full_data$price_per_kg,
     start = c(year(min(all_months)), month(min(all_months))),
     frequency = 12)
}

# Apply to all five counties
ts_marsabit <- fill_ts("Marsabit")
ts_mombasa  <- fill_ts("Mombasa")
ts_nairobi  <- fill_ts("Nairobi")
ts_turkana  <- fill_ts("Turkana")
ts_uasin    <- fill_ts("Uasin Gishu")

# Check new lengths and end dates
cat("Marsabit:", length(ts_marsabit), "| End:", end(ts_marsabit), "\n")
cat("Mombasa:", length(ts_mombasa), "| End:", end(ts_mombasa), "\n")
cat("Nairobi:", length(ts_nairobi), "| End:", end(ts_nairobi), "\n")
cat("Turkana:", length(ts_turkana), "| End:", end(ts_turkana), "\n")
cat("Uasin Gishu:", length(ts_uasin), "| End:", end(ts_uasin), "\n")


 ### Results
## The lengths look better but the end dates are wrong for some counties 
##Marsabit showing 2027, Nairobi 2027, and Turkana 2036 which is impossible.
##This is happening because some counties have duplicate date entries 


# Function with duplicate handling + gap filling
fill_ts <- function(county_name) {
  
  county_data <- maize_final %>%
    filter(admin2 == county_name) %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(price_per_kg = mean(price_per_kg, na.rm = TRUE)) %>%
    ungroup()
  
  # Create complete monthly sequence
  all_months <- seq(min(county_data$date), 
                    max(county_data$date), 
                    by = "month")
  
  # Create full data frame with all months
  full_data <- data.frame(date = all_months) %>%
    left_join(county_data, by = "date")
  
  # Fill missing values using linear interpolation
  full_data$price_per_kg <- na.approx(full_data$price_per_kg, 
                                      na.rm = FALSE)
  
  # Convert to time series
  ts(full_data$price_per_kg,
     start = c(year(min(all_months)), month(min(all_months))),
     frequency = 12)
}

# Apply to all five counties
ts_marsabit <- fill_ts("Marsabit")
ts_mombasa  <- fill_ts("Mombasa")
ts_nairobi  <- fill_ts("Nairobi")
ts_turkana  <- fill_ts("Turkana")
ts_uasin    <- fill_ts("Uasin Gishu")

# Check new lengths and end dates
cat("Marsabit:", length(ts_marsabit), "| Start:", start(ts_marsabit), "| End:", end(ts_marsabit), "\n")
cat("Mombasa:", length(ts_mombasa), "| Start:", start(ts_mombasa), "| End:", end(ts_mombasa), "\n")
cat("Nairobi:", length(ts_nairobi), "| Start:", start(ts_nairobi), "| End:", end(ts_nairobi), "\n")
cat("Turkana:", length(ts_turkana), "| Start:", start(ts_turkana), "| End:", end(ts_turkana), "\n")
cat("Uasin Gishu:", length(ts_uasin), "| Start:", start(ts_uasin), "| End:", end(ts_uasin), "\n")


## All counties start in January 2006 and end at the correct dates



### TS Plot for Marsabit

png("Marsabit_TS.png", width = 1400, height = 600)
autoplot(ts_marsabit) +
  labs(title = "Marsabit: Monthly Maize Price (2006–2025)",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


### TS Plot for Mombasa 

png("Mombasa_TS.png", width = 1400, height = 600)
autoplot(ts_mombasa) +
  labs(title = "Mombasa: Monthly Maize Price (2006–2024)",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


### TS Plot for Nairobi
png("Nairobi_TS.png", width = 1400, height = 600)
autoplot(ts_nairobi) +
  labs(title = "Nairobi: Monthly Maize Price (2006–2025)",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


## TS plot for Turkana
png("Turkana_TS.png", width = 1400, height = 600)
autoplot(ts_turkana) +
  labs(title = "Turkana: Monthly Maize Price (2006–2025)",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


### TS Plot for Uasin Gishu
png("UasinGishu_TS.png", width = 1400, height = 600)
autoplot(ts_uasin) +
  labs(title = "Uasin Gishu: Monthly Maize Price (2006–2025)",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


############################### Seasonal Patterns ######################################
png("Marsabit_Seasonal.png", width = 1400, height = 600)
ggseasonplot(ts_marsabit, 
             year.labels = TRUE,
             year.labels.left = TRUE) +
  labs(title = "Seasonal Plot: Marsabit Maize Price",
       x = "Month", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


png("Mombasa_Seasonal.png", width = 1400, height = 600)
ggseasonplot(ts_mombasa, 
             year.labels = TRUE,
             year.labels.left = TRUE) +
  labs(title = "Seasonal Plot: Mombasa Maize Price",
       x = "Month", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Nairobi_Seasonal.png", width = 1400, height = 600)
ggseasonplot(ts_nairobi, 
             year.labels = TRUE,
             year.labels.left = TRUE) +
  labs(title = "Seasonal Plot: Nairobi Maize Price",
       x = "Month", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Turkana_Seasonal.png", width = 1400, height = 600)
ggseasonplot(ts_turkana, 
             year.labels = TRUE,
             year.labels.left = TRUE) +
  labs(title = "Seasonal Plot: Turkana Maize Price",
       x = "Month", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("UasinGishu_Seasonal.png", width = 1400, height = 600)
ggseasonplot(ts_uasin, 
             year.labels = TRUE,
             year.labels.left = TRUE) +
  labs(title = "Seasonal Plot: Uasin Gishu Maize Price",
       x = "Month", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


############################ Time Series Decomposition ########################
png("Marsabit_Decomp.png", width = 1400, height = 800)
autoplot(decompose(ts_marsabit)) +
  labs(title = "Decomposition: Marsabit Maize Price") +
  theme_minimal()
dev.off()


png("Mombasa_Decomp.png", width = 1400, height = 800)
autoplot(decompose(ts_mombasa)) +
  labs(title = "Decomposition: Mombasa Maize Price") +
  theme_minimal()
dev.off()

png("Nairobi_Decomp.png", width = 1400, height = 800)
autoplot(decompose(ts_nairobi)) +
  labs(title = "Decomposition: Nairobi Maize Price") +
  theme_minimal()
dev.off()

png("Turkana_Decomp.png", width = 1400, height = 800)
autoplot(decompose(ts_turkana)) +
  labs(title = "Decomposition: Turkana Maize Price") +
  theme_minimal()
dev.off()

png("UasinGishu_Decomp.png", width = 1400, height = 800)
autoplot(decompose(ts_uasin)) +
  labs(title = "Decomposition: Uasin Gishu Maize Price") +
  theme_minimal()
dev.off()


 ####################################################################################
##################### Autocorrelation and Partial Autocorrelation Analysis ##############
png("Marsabit_ACF.png", width = 1400, height = 600)
par(mfrow = c(1, 2))
Acf(ts_marsabit, main = "ACF: Marsabit Maize Price")
Pacf(ts_marsabit, main = "PACF: Marsabit Maize Price")
dev.off()


png("Mombasa_ACF.png", width = 1400, height = 600)
par(mfrow = c(1, 2))
Acf(ts_mombasa, main = "ACF: Mombasa Maize Price")
Pacf(ts_mombasa, main = "PACF: Mombasa Maize Price")
dev.off()

png("Nairobi_ACF.png", width = 1400, height = 600)
par(mfrow = c(1, 2))
Acf(ts_nairobi, main = "ACF: Nairobi Maize Price")
Pacf(ts_nairobi, main = "PACF: Nairobi Maize Price")
dev.off()

png("Turkana_ACF.png", width = 1400, height = 600)
par(mfrow = c(1, 2))
Acf(ts_turkana, main = "ACF: Turkana Maize Price")
Pacf(ts_turkana, main = "PACF: Turkana Maize Price")
dev.off()

png("UasinGishu_ACF.png", width = 1400, height = 600)
par(mfrow = c(1, 2))
Acf(ts_uasin, main = "ACF: Uasin Gishu Maize Price")
Pacf(ts_uasin, main = "PACF: Uasin Gishu Maize Price")
dev.off()


 #######################################################################################
########################## Data Transformation ##################################

#### Missing Month Interpolation ####
interpolation_summary <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi", 
             "Turkana", "Uasin Gishu"),
  Original_Obs = c(146, 125, 160, 267, 156),
  Expected_Months = c(231, 222, 238, 237, 234),
  Missing_Filled = c(85, 97, 78, 0, 78),
  Final_Length = c(231, 222, 238, 237, 234)
)

ft_interp <- flextable(interpolation_summary) %>%
  set_header_labels(
    County = "County",
    Original_Obs = "Original Observations",
    Expected_Months = "Expected Monthly Observations",
    Missing_Filled = "Months Interpolated",
    Final_Length = "Final Series Length"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_interp


#################### Train and Validation Split ##############################################
# Split each county series - last 24 months for validation


# Marsabit
n_marsabit <- length(ts_marsabit)
train_marsabit <- window(ts_marsabit, 
                         end = c(2006 + (n_marsabit - 25) %/% 12,
                                 (n_marsabit - 25) %% 12 + 1))
test_marsabit <- window(ts_marsabit, 
                        start = c(2006 + (n_marsabit - 24) %/% 12,
                                  (n_marsabit - 24) %% 12 + 1))

# Mombasa
n_mombasa <- length(ts_mombasa)
train_mombasa <- window(ts_mombasa, 
                        end = c(2006 + (n_mombasa - 25) %/% 12,
                                (n_mombasa - 25) %% 12 + 1))
test_mombasa <- window(ts_mombasa, 
                       start = c(2006 + (n_mombasa - 24) %/% 12,
                                 (n_mombasa - 24) %% 12 + 1))

# Nairobi
n_nairobi <- length(ts_nairobi)
train_nairobi <- window(ts_nairobi, 
                        end = c(2006 + (n_nairobi - 25) %/% 12,
                                (n_nairobi - 25) %% 12 + 1))
test_nairobi <- window(ts_nairobi, 
                       start = c(2006 + (n_nairobi - 24) %/% 12,
                                 (n_nairobi - 24) %% 12 + 1))

# Turkana
n_turkana <- length(ts_turkana)
train_turkana <- window(ts_turkana, 
                        end = c(2006 + (n_turkana - 25) %/% 12,
                                (n_turkana - 25) %% 12 + 1))
test_turkana <- window(ts_turkana, 
                       start = c(2006 + (n_turkana - 24) %/% 12,
                                 (n_turkana - 24) %% 12 + 1))

# Uasin Gishu
n_uasin <- length(ts_uasin)
train_uasin <- window(ts_uasin, 
                      end = c(2006 + (n_uasin - 25) %/% 12,
                              (n_uasin - 25) %% 12 + 1))
test_uasin <- window(ts_uasin, 
                     start = c(2006 + (n_uasin - 24) %/% 12,
                               (n_uasin - 24) %% 12 + 1))

# Flextable showing split results
split_summary <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi", 
             "Turkana", "Uasin Gishu"),
  Total_Obs = c(length(ts_marsabit), length(ts_mombasa),
                length(ts_nairobi), length(ts_turkana),
                length(ts_uasin)),
  Training_Obs = c(length(train_marsabit), length(train_mombasa),
                   length(train_nairobi), length(train_turkana),
                   length(train_uasin)),
  Validation_Obs = c(length(test_marsabit), length(test_mombasa),
                     length(test_nairobi), length(test_turkana),
                     length(test_uasin)),
  Train_End = c(format(time(train_marsabit)[length(train_marsabit)]),
                format(time(train_mombasa)[length(train_mombasa)]),
                format(time(train_nairobi)[length(train_nairobi)]),
                format(time(train_turkana)[length(train_turkana)]),
                format(time(train_uasin)[length(train_uasin)])),
  Test_Start = c(format(time(test_marsabit)[1]),
                 format(time(test_mombasa)[1]),
                 format(time(test_nairobi)[1]),
                 format(time(test_turkana)[1]),
                 format(time(test_uasin)[1]))
)

# Fix the date formatting
split_summary <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi", 
             "Turkana", "Uasin Gishu"),
  Total_Obs = c(length(ts_marsabit), length(ts_mombasa),
                length(ts_nairobi), length(ts_turkana),
                length(ts_uasin)),
  Training_Obs = c(length(train_marsabit), length(train_mombasa),
                   length(train_nairobi), length(train_turkana),
                   length(train_uasin)),
  Validation_Obs = c(length(test_marsabit), length(test_mombasa),
                     length(test_nairobi), length(test_turkana),
                     length(test_uasin)),
  Train_End = c("February 2023", "May 2022", 
                "October 2023", "August 2023", 
                "June 2023"),
  Test_Start = c("March 2023", "June 2022", 
                 "November 2023", "September 2023", 
                 "July 2023")
)

ft_split <- flextable(split_summary) %>%
  set_header_labels(
    County = "County",
    Total_Obs = "Total Months",
    Training_Obs = "Training Months",
    Validation_Obs = "Validation Months",
    Train_End = "Training End",
    Test_Start = "Validation Start"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_split


######### Time Index and Lag Variable Creation ########################
# Create time index and lag variables for each county
maize_final <- maize_final %>%
  arrange(admin2, date) %>%
  group_by(admin2) %>%
  mutate(
    time_index = row_number(),
    month      = factor(format(date, "%m")),
    lag_1      = lag(price_per_kg, 1)
  ) %>%
  ungroup()

# Show sample for Uasin Gishu as flextable
sample_vars <- maize_final %>%
  filter(admin2 == "Uasin Gishu") %>%
  select(date, price_per_kg, time_index, month, lag_1) %>%
  head(10) %>%
  mutate(
    date = format(date, "%B %Y"),
    price_per_kg = round(price_per_kg, 2),
    lag_1 = round(lag_1, 2)
  )

ft_vars <- flextable(sample_vars) %>%
  set_header_labels(
    date        = "Date",
    price_per_kg = "Price (KES/KG)",
    time_index  = "Time Index",
    month       = "Month",
    lag_1       = "Lag 1 Price"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_vars


# Show sample for all 5 counties
sample_all_counties <- maize_final %>%
  filter(admin2 %in% c("Marsabit", "Mombasa", "Nairobi", 
                       "Turkana", "Uasin Gishu")) %>%
  group_by(admin2) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  select(admin2, date, price_per_kg, time_index, month, lag_1) %>%
  mutate(
    date         = format(date, "%B %Y"),
    price_per_kg = round(price_per_kg, 2),
    lag_1        = round(lag_1, 2)
  )

ft_all_vars <- flextable(sample_all_counties) %>%
  set_header_labels(
    admin2       = "County",
    date         = "Date",
    price_per_kg = "Price (KES/KG)",
    time_index   = "Time Index",
    month        = "Month",
    lag_1        = "Lag 1 Price"
  ) %>%
  bold(part = "header") %>%
  merge_v(j = "admin2") %>%
  autofit()

ft_all_vars
 

 ###################################### Iterative Results ########################
####################################### Naïve Model #############################

library(forecast)
library(flextable)

# Fit Naive model for all 5 counties
naive_marsabit  <- naive(train_marsabit, h = 24)
naive_mombasa   <- naive(train_mombasa,  h = 24)
naive_nairobi   <- naive(train_nairobi,  h = 24)
naive_turkana   <- naive(train_turkana,  h = 24)
naive_uasin     <- naive(train_uasin,    h = 24)

# Get accuracy for all counties
acc_naive_marsabit  <- accuracy(naive_marsabit,  test_marsabit)
acc_naive_mombasa   <- accuracy(naive_mombasa,   test_mombasa)
acc_naive_nairobi   <- accuracy(naive_nairobi,   test_nairobi)
acc_naive_turkana   <- accuracy(naive_turkana,   test_turkana)
acc_naive_uasin     <- accuracy(naive_uasin,     test_uasin)

# Build accuracy table - validation row only
naive_accuracy_table <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi",
             "Turkana", "Uasin Gishu"),
  Set = "Validation",
  ME   = round(c(acc_naive_marsabit[2,"ME"],
                 acc_naive_mombasa[2,"ME"],
                 acc_naive_nairobi[2,"ME"],
                 acc_naive_turkana[2,"ME"],
                 acc_naive_uasin[2,"ME"]), 3),
  RMSE = round(c(acc_naive_marsabit[2,"RMSE"],
                 acc_naive_mombasa[2,"RMSE"],
                 acc_naive_nairobi[2,"RMSE"],
                 acc_naive_turkana[2,"RMSE"],
                 acc_naive_uasin[2,"RMSE"]), 3),
  MAE  = round(c(acc_naive_marsabit[2,"MAE"],
                 acc_naive_mombasa[2,"MAE"],
                 acc_naive_nairobi[2,"MAE"],
                 acc_naive_turkana[2,"MAE"],
                 acc_naive_uasin[2,"MAE"]), 3),
  MAPE = round(c(acc_naive_marsabit[2,"MAPE"],
                 acc_naive_mombasa[2,"MAPE"],
                 acc_naive_nairobi[2,"MAPE"],
                 acc_naive_turkana[2,"MAPE"],
                 acc_naive_uasin[2,"MAPE"]), 3),
  MASE = round(c(acc_naive_marsabit[2,"MASE"],
                 acc_naive_mombasa[2,"MASE"],
                 acc_naive_nairobi[2,"MASE"],
                 acc_naive_turkana[2,"MASE"],
                 acc_naive_uasin[2,"MASE"]), 3)
)

ft_naive <- flextable(naive_accuracy_table) %>%
  set_header_labels(
    County = "County",
    Set    = "Dataset",
    ME     = "ME",
    RMSE   = "RMSE",
    MAE    = "MAE",
    MAPE   = "MAPE",
    MASE   = "MASE"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_naive


# Build FULL accuracy table - both training AND validation rows
naive_full_table <- data.frame(
  County = c("Marsabit", "Marsabit",
             "Mombasa", "Mombasa",
             "Nairobi", "Nairobi",
             "Turkana", "Turkana",
             "Uasin Gishu", "Uasin Gishu"),
  Set = rep(c("Training", "Validation"), 5),
  ME = round(c(
    acc_naive_marsabit[1,"ME"], acc_naive_marsabit[2,"ME"],
    acc_naive_mombasa[1,"ME"],  acc_naive_mombasa[2,"ME"],
    acc_naive_nairobi[1,"ME"],  acc_naive_nairobi[2,"ME"],
    acc_naive_turkana[1,"ME"],  acc_naive_turkana[2,"ME"],
    acc_naive_uasin[1,"ME"],    acc_naive_uasin[2,"ME"]), 3),
  RMSE = round(c(
    acc_naive_marsabit[1,"RMSE"], acc_naive_marsabit[2,"RMSE"],
    acc_naive_mombasa[1,"RMSE"],  acc_naive_mombasa[2,"RMSE"],
    acc_naive_nairobi[1,"RMSE"],  acc_naive_nairobi[2,"RMSE"],
    acc_naive_turkana[1,"RMSE"],  acc_naive_turkana[2,"RMSE"],
    acc_naive_uasin[1,"RMSE"],    acc_naive_uasin[2,"RMSE"]), 3),
  MAE = round(c(
    acc_naive_marsabit[1,"MAE"], acc_naive_marsabit[2,"MAE"],
    acc_naive_mombasa[1,"MAE"],  acc_naive_mombasa[2,"MAE"],
    acc_naive_nairobi[1,"MAE"],  acc_naive_nairobi[2,"MAE"],
    acc_naive_turkana[1,"MAE"],  acc_naive_turkana[2,"MAE"],
    acc_naive_uasin[1,"MAE"],    acc_naive_uasin[2,"MAE"]), 3),
  MAPE = round(c(
    acc_naive_marsabit[1,"MAPE"], acc_naive_marsabit[2,"MAPE"],
    acc_naive_mombasa[1,"MAPE"],  acc_naive_mombasa[2,"MAPE"],
    acc_naive_nairobi[1,"MAPE"],  acc_naive_nairobi[2,"MAPE"],
    acc_naive_turkana[1,"MAPE"],  acc_naive_turkana[2,"MAPE"],
    acc_naive_uasin[1,"MAPE"],    acc_naive_uasin[2,"MAPE"]), 3),
  MASE = round(c(
    acc_naive_marsabit[1,"MASE"], acc_naive_marsabit[2,"MASE"],
    acc_naive_mombasa[1,"MASE"],  acc_naive_mombasa[2,"MASE"],
    acc_naive_nairobi[1,"MASE"],  acc_naive_nairobi[2,"MASE"],
    acc_naive_turkana[1,"MASE"],  acc_naive_turkana[2,"MASE"],
    acc_naive_uasin[1,"MASE"],    acc_naive_uasin[2,"MASE"]), 3)
)


ft_naive_full <- flextable(naive_full_table) %>%
  set_header_labels(
    County = "County",
    Set    = "Dataset",
    ME     = "ME",
    RMSE   = "RMSE",
    MAE    = "MAE",
    MAPE   = "MAPE",
    MASE   = "MASE"
  ) %>%
  bold(part = "header") %>%
  merge_v(j = "County") %>%
  autofit()

ft_naive_full


##### Running this 
# Check if objects still exist
ls()



 ######################## Naive Model Plots #################################
png("Naive_Marsabit.png", width = 1400, height = 600)
autoplot(naive_marsabit) +
  autolayer(test_marsabit, series = "Actual") +
  labs(title = "Naïve Model: Marsabit Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Naive_Mombasa.png", width = 1400, height = 600)
autoplot(naive_mombasa) +
  autolayer(test_mombasa, series = "Actual") +
  labs(title = "Naïve Model: Mombasa Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Naive_Nairobi.png", width = 1400, height = 600)
autoplot(naive_nairobi) +
  autolayer(test_nairobi, series = "Actual") +
  labs(title = "Naïve Model: Nairobi Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Naive_Turkana.png", width = 1400, height = 600)
autoplot(naive_turkana) +
  autolayer(test_turkana, series = "Actual") +
  labs(title = "Naïve Model: Turkana Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Naive_UasinGishu.png", width = 1400, height = 600)
autoplot(naive_uasin) +
  autolayer(test_uasin, series = "Actual") +
  labs(title = "Naïve Model: Uasin Gishu Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()
#########################################################################
# Fit ETS model for all 5 counties
ets_marsabit  <- ets(train_marsabit)
ets_mombasa   <- ets(train_mombasa)
ets_nairobi   <- ets(train_nairobi)
ets_turkana   <- ets(train_turkana)
ets_uasin     <- ets(train_uasin)

# Generate forecasts
fc_ets_marsabit  <- forecast(ets_marsabit,  h = 24)
fc_ets_mombasa   <- forecast(ets_mombasa,   h = 24)
fc_ets_nairobi   <- forecast(ets_nairobi,   h = 24)
fc_ets_turkana   <- forecast(ets_turkana,   h = 24)
fc_ets_uasin     <- forecast(ets_uasin,     h = 24)

# Confirm models fitted
cat("Marsabit ETS:", ets_marsabit$method, "\n")
cat("Mombasa ETS:", ets_mombasa$method, "\n")
cat("Nairobi ETS:", ets_nairobi$method, "\n")
cat("Turkana ETS:", ets_turkana$method, "\n")
cat("Uasin Gishu ETS:", ets_uasin$method, "\n")




######################################################################################


############################### Exponential Smoothing — ETS Model ##################
## Fit ETS model for all 5 counties
ets_selected <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi", 
             "Turkana", "Uasin Gishu"),
  ETS_Model = c("ETS(A,Ad,N)", "ETS(M,Ad,M)", 
                "ETS(A,N,N)", "ETS(A,N,N)", 
                "ETS(M,Ad,N)"),
  Error = c("Additive", "Multiplicative", 
            "Additive", "Additive", 
            "Multiplicative"),
  Trend = c("Additive Damped", "Additive Damped", 
            "None", "None", 
            "Additive Damped"),
  Seasonality = c("None", "Multiplicative", 
                  "None", "None", 
                  "None")
)

ft_ets_selected <- flextable(ets_selected) %>%
  set_header_labels(
    County      = "County",
    ETS_Model   = "ETS Model Selected",
    Error       = "Error Component",
    Trend       = "Trend Component",
    Seasonality = "Seasonal Component"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_ets_selected



# Get accuracy for all counties
acc_ets_marsabit  <- accuracy(fc_ets_marsabit,  test_marsabit)
acc_ets_mombasa   <- accuracy(fc_ets_mombasa,   test_mombasa)
acc_ets_nairobi   <- accuracy(fc_ets_nairobi,   test_nairobi)
acc_ets_turkana   <- accuracy(fc_ets_turkana,   test_turkana)
acc_ets_uasin     <- accuracy(fc_ets_uasin,     test_uasin)

# Build full accuracy table
ets_full_table <- data.frame(
  County = c("Marsabit", "Marsabit",
             "Mombasa", "Mombasa",
             "Nairobi", "Nairobi",
             "Turkana", "Turkana",
             "Uasin Gishu", "Uasin Gishu"),
  Set = rep(c("Training", "Validation"), 5),
  ME = round(c(
    acc_ets_marsabit[1,"ME"],  acc_ets_marsabit[2,"ME"],
    acc_ets_mombasa[1,"ME"],   acc_ets_mombasa[2,"ME"],
    acc_ets_nairobi[1,"ME"],   acc_ets_nairobi[2,"ME"],
    acc_ets_turkana[1,"ME"],   acc_ets_turkana[2,"ME"],
    acc_ets_uasin[1,"ME"],     acc_ets_uasin[2,"ME"]), 3),
  RMSE = round(c(
    acc_ets_marsabit[1,"RMSE"], acc_ets_marsabit[2,"RMSE"],
    acc_ets_mombasa[1,"RMSE"],  acc_ets_mombasa[2,"RMSE"],
    acc_ets_nairobi[1,"RMSE"],  acc_ets_nairobi[2,"RMSE"],
    acc_ets_turkana[1,"RMSE"],  acc_ets_turkana[2,"RMSE"],
    acc_ets_uasin[1,"RMSE"],    acc_ets_uasin[2,"RMSE"]), 3),
  MAE = round(c(
    acc_ets_marsabit[1,"MAE"], acc_ets_marsabit[2,"MAE"],
    acc_ets_mombasa[1,"MAE"],  acc_ets_mombasa[2,"MAE"],
    acc_ets_nairobi[1,"MAE"],  acc_ets_nairobi[2,"MAE"],
    acc_ets_turkana[1,"MAE"],  acc_ets_turkana[2,"MAE"],
    acc_ets_uasin[1,"MAE"],    acc_ets_uasin[2,"MAE"]), 3),
  MAPE = round(c(
    acc_ets_marsabit[1,"MAPE"], acc_ets_marsabit[2,"MAPE"],
    acc_ets_mombasa[1,"MAPE"],  acc_ets_mombasa[2,"MAPE"],
    acc_ets_nairobi[1,"MAPE"],  acc_ets_nairobi[2,"MAPE"],
    acc_ets_turkana[1,"MAPE"],  acc_ets_turkana[2,"MAPE"],
    acc_ets_uasin[1,"MAPE"],    acc_ets_uasin[2,"MAPE"]), 3),
  MASE = round(c(
    acc_ets_marsabit[1,"MASE"], acc_ets_marsabit[2,"MASE"],
    acc_ets_mombasa[1,"MASE"],  acc_ets_mombasa[2,"MASE"],
    acc_ets_nairobi[1,"MASE"],  acc_ets_nairobi[2,"MASE"],
    acc_ets_turkana[1,"MASE"],  acc_ets_turkana[2,"MASE"],
    acc_ets_uasin[1,"MASE"],    acc_ets_uasin[2,"MASE"]), 3)
)

ft_ets_full <- flextable(ets_full_table) %>%
  set_header_labels(
    County = "County",
    Set    = "Dataset",
    ME     = "ME",
    RMSE   = "RMSE",
    MAE    = "MAE",
    MAPE   = "MAPE",
    MASE   = "MASE"
  ) %>%
  bold(part = "header") %>%
  merge_v(j = "County") %>%
  autofit()

ft_ets_full


################################# the ETS PLOTS #########################
png("ETS_Marsabit.png", width = 1400, height = 600)
autoplot(fc_ets_marsabit) +
  autolayer(test_marsabit, series = "Actual") +
  labs(title = "ETS Model: Marsabit Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("ETS_Mombasa.png", width = 1400, height = 600)
autoplot(fc_ets_mombasa) +
  autolayer(test_mombasa, series = "Actual") +
  labs(title = "ETS Model: Mombasa Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("ETS_Nairobi.png", width = 1400, height = 600)
autoplot(fc_ets_nairobi) +
  autolayer(test_nairobi, series = "Actual") +
  labs(title = "ETS Model: Nairobi Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("ETS_Turkana.png", width = 1400, height = 600)
autoplot(fc_ets_turkana) +
  autolayer(test_turkana, series = "Actual") +
  labs(title = "ETS Model: Turkana Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("ETS_UasinGishu.png", width = 1400, height = 600)
autoplot(fc_ets_uasin) +
  autolayer(test_uasin, series = "Actual") +
  labs(title = "ETS Model: Uasin Gishu Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


 ###############################  ARIMA Model ###########################
 ################## Augmented Dickey-Fuller Test #############################

library(tseries)

# Run ADF test on training data for all counties
adf_marsabit  <- adf.test(train_marsabit)
adf_mombasa   <- adf.test(train_mombasa)
adf_nairobi   <- adf.test(train_nairobi)
adf_turkana   <- adf.test(train_turkana)
adf_uasin     <- adf.test(train_uasin)

# Build ADF results table
adf_table <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi",
             "Turkana", "Uasin Gishu"),
  ADF_Statistic = round(c(adf_marsabit$statistic,
                          adf_mombasa$statistic,
                          adf_nairobi$statistic,
                          adf_turkana$statistic,
                          adf_uasin$statistic), 4),
  P_Value = round(c(adf_marsabit$p.value,
                    adf_mombasa$p.value,
                    adf_nairobi$p.value,
                    adf_turkana$p.value,
                    adf_uasin$p.value), 4),
  Decision = c(
    ifelse(adf_marsabit$p.value > 0.05, 
           "Non-Stationary", "Stationary"),
    ifelse(adf_mombasa$p.value > 0.05,  
           "Non-Stationary", "Stationary"),
    ifelse(adf_nairobi$p.value > 0.05,  
           "Non-Stationary", "Stationary"),
    ifelse(adf_turkana$p.value > 0.05,  
           "Non-Stationary", "Stationary"),
    ifelse(adf_uasin$p.value > 0.05,    
           "Non-Stationary", "Stationary")
  )
)

ft_adf <- flextable(adf_table) %>%
  set_header_labels(
    County        = "County",
    ADF_Statistic = "ADF Test Statistic",
    P_Value       = "P-Value",
    Decision      = "Decision (α = 0.05)"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_adf


############### Differencing step ######################
# Apply first differencing to all training series
diff_marsabit <- diff(train_marsabit, differences = 1)
diff_mombasa  <- diff(train_mombasa,  differences = 1)
diff_nairobi  <- diff(train_nairobi,  differences = 1)
diff_turkana  <- diff(train_turkana,  differences = 1)
diff_uasin    <- diff(train_uasin,    differences = 1)

# Run ADF test on differenced series
adf_diff_marsabit <- adf.test(diff_marsabit)
adf_diff_mombasa  <- adf.test(diff_mombasa)
adf_diff_nairobi  <- adf.test(diff_nairobi)
adf_diff_turkana  <- adf.test(diff_turkana)
adf_diff_uasin    <- adf.test(diff_uasin)

# Build ADF differenced results table
adf_diff_table <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi",
             "Turkana", "Uasin Gishu"),
  ADF_Statistic = round(c(adf_diff_marsabit$statistic,
                          adf_diff_mombasa$statistic,
                          adf_diff_nairobi$statistic,
                          adf_diff_turkana$statistic,
                          adf_diff_uasin$statistic), 4),
  P_Value = round(c(adf_diff_marsabit$p.value,
                    adf_diff_mombasa$p.value,
                    adf_diff_nairobi$p.value,
                    adf_diff_turkana$p.value,
                    adf_diff_uasin$p.value), 4),
  Decision = c(
    ifelse(adf_diff_marsabit$p.value > 0.05,
           "Non-Stationary", "Stationary"),
    ifelse(adf_diff_mombasa$p.value > 0.05,
           "Non-Stationary", "Stationary"),
    ifelse(adf_diff_nairobi$p.value > 0.05,
           "Non-Stationary", "Stationary"),
    ifelse(adf_diff_turkana$p.value > 0.05,
           "Non-Stationary", "Stationary"),
    ifelse(adf_diff_uasin$p.value > 0.05,
           "Non-Stationary", "Stationary")
  )
)

ft_adf_diff <- flextable(adf_diff_table) %>%
  set_header_labels(
    County        = "County",
    ADF_Statistic = "ADF Test Statistic",
    P_Value       = "P-Value",
    Decision      = "Decision (α = 0.05)"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_adf_diff


######################### Box-Cox transformation assessment: ####################
# Box-Cox lambda for each county
lambda_marsabit <- BoxCox.lambda(train_marsabit)
lambda_mombasa  <- BoxCox.lambda(train_mombasa)
lambda_nairobi  <- BoxCox.lambda(train_nairobi)
lambda_turkana  <- BoxCox.lambda(train_turkana)
lambda_uasin    <- BoxCox.lambda(train_uasin)

# Build lambda table
lambda_table <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi",
             "Turkana", "Uasin Gishu"),
  Lambda = round(c(lambda_marsabit, lambda_mombasa,
                   lambda_nairobi, lambda_turkana,
                   lambda_uasin), 4),
  Interpretation = c(
    ifelse(abs(lambda_marsabit) < 0.2, 
           "Log transformation suggested", 
           "No transformation needed"),
    ifelse(abs(lambda_mombasa) < 0.2,  
           "Log transformation suggested", 
           "No transformation needed"),
    ifelse(abs(lambda_nairobi) < 0.2,  
           "Log transformation suggested", 
           "No transformation needed"),
    ifelse(abs(lambda_turkana) < 0.2,  
           "Log transformation suggested", 
           "No transformation needed"),
    ifelse(abs(lambda_uasin) < 0.2,    
           "Log transformation suggested", 
           "No transformation needed")
  )
)

ft_lambda <- flextable(lambda_table) %>%
  set_header_labels(
    County         = "County",
    Lambda         = "Box-Cox Lambda (λ)",
    Interpretation = "Interpretation"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_lambda


#### Fitting the Arima models
# Fit ARIMA models for all 5 counties
arima_marsabit <- auto.arima(train_marsabit)
arima_mombasa  <- auto.arima(train_mombasa)
arima_nairobi  <- auto.arima(train_nairobi)
arima_turkana  <- auto.arima(train_turkana)
arima_uasin    <- auto.arima(train_uasin)

# Build ARIMA model summary flextable
arima_selected <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi",
             "Turkana", "Uasin Gishu"),
  Model = c(
    as.character(arima_marsabit),
    as.character(arima_mombasa),
    as.character(arima_nairobi),
    as.character(arima_turkana),
    as.character(arima_uasin)
  ),
  AIC = round(c(arima_marsabit$aic,
                arima_mombasa$aic,
                arima_nairobi$aic,
                arima_turkana$aic,
                arima_uasin$aic), 3),
  BIC = round(c(arima_marsabit$bic,
                arima_mombasa$bic,
                arima_nairobi$bic,
                arima_turkana$bic,
                arima_uasin$bic), 3)
)

ft_arima_selected <- flextable(arima_selected) %>%
  set_header_labels(
    County = "County",
    Model  = "ARIMA Model Selected",
    AIC    = "AIC",
    BIC    = "BIC"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_arima_selected


#######  ARIMA accuracy table ########################
# Generate ARIMA forecasts
fc_arima_marsabit <- forecast(arima_marsabit, h = 24)
fc_arima_mombasa  <- forecast(arima_mombasa,  h = 24)
fc_arima_nairobi  <- forecast(arima_nairobi,  h = 24)
fc_arima_turkana  <- forecast(arima_turkana,  h = 24)
fc_arima_uasin    <- forecast(arima_uasin,    h = 24)

# Get accuracy
acc_arima_marsabit <- accuracy(fc_arima_marsabit, test_marsabit)
acc_arima_mombasa  <- accuracy(fc_arima_mombasa,  test_mombasa)
acc_arima_nairobi  <- accuracy(fc_arima_nairobi,  test_nairobi)
acc_arima_turkana  <- accuracy(fc_arima_turkana,  test_turkana)
acc_arima_uasin    <- accuracy(fc_arima_uasin,    test_uasin)

# Build full accuracy table
arima_full_table <- data.frame(
  County = c("Marsabit", "Marsabit",
             "Mombasa", "Mombasa",
             "Nairobi", "Nairobi",
             "Turkana", "Turkana",
             "Uasin Gishu", "Uasin Gishu"),
  Set = rep(c("Training", "Validation"), 5),
  ME = round(c(
    acc_arima_marsabit[1,"ME"],  acc_arima_marsabit[2,"ME"],
    acc_arima_mombasa[1,"ME"],   acc_arima_mombasa[2,"ME"],
    acc_arima_nairobi[1,"ME"],   acc_arima_nairobi[2,"ME"],
    acc_arima_turkana[1,"ME"],   acc_arima_turkana[2,"ME"],
    acc_arima_uasin[1,"ME"],     acc_arima_uasin[2,"ME"]), 3),
  RMSE = round(c(
    acc_arima_marsabit[1,"RMSE"], acc_arima_marsabit[2,"RMSE"],
    acc_arima_mombasa[1,"RMSE"],  acc_arima_mombasa[2,"RMSE"],
    acc_arima_nairobi[1,"RMSE"],  acc_arima_nairobi[2,"RMSE"],
    acc_arima_turkana[1,"RMSE"],  acc_arima_turkana[2,"RMSE"],
    acc_arima_uasin[1,"RMSE"],    acc_arima_uasin[2,"RMSE"]), 3),
  MAE = round(c(
    acc_arima_marsabit[1,"MAE"], acc_arima_marsabit[2,"MAE"],
    acc_arima_mombasa[1,"MAE"],  acc_arima_mombasa[2,"MAE"],
    acc_arima_nairobi[1,"MAE"],  acc_arima_nairobi[2,"MAE"],
    acc_arima_turkana[1,"MAE"],  acc_arima_turkana[2,"MAE"],
    acc_arima_uasin[1,"MAE"],    acc_arima_uasin[2,"MAE"]), 3),
  MAPE = round(c(
    acc_arima_marsabit[1,"MAPE"], acc_arima_marsabit[2,"MAPE"],
    acc_arima_mombasa[1,"MAPE"],  acc_arima_mombasa[2,"MAPE"],
    acc_arima_nairobi[1,"MAPE"],  acc_arima_nairobi[2,"MAPE"],
    acc_arima_turkana[1,"MAPE"],  acc_arima_turkana[2,"MAPE"],
    acc_arima_uasin[1,"MAPE"],    acc_arima_uasin[2,"MAPE"]), 3),
  MASE = round(c(
    acc_arima_marsabit[1,"MASE"], acc_arima_marsabit[2,"MASE"],
    acc_arima_mombasa[1,"MASE"],  acc_arima_mombasa[2,"MASE"],
    acc_arima_nairobi[1,"MASE"],  acc_arima_nairobi[2,"MASE"],
    acc_arima_turkana[1,"MASE"],  acc_arima_turkana[2,"MASE"],
    acc_arima_uasin[1,"MASE"],    acc_arima_uasin[2,"MASE"]), 3)
)

ft_arima_full <- flextable(arima_full_table) %>%
  set_header_labels(
    County = "County",
    Set    = "Dataset",
    ME     = "ME",
    RMSE   = "RMSE",
    MAE    = "MAE",
    MAPE   = "MAPE",
    MASE   = "MASE"
  ) %>%
  bold(part = "header") %>%
  merge_v(j = "County") %>%
  autofit()

ft_arima_full


##### ARIMA PLOTS ########
png("ARIMA_Marsabit.png", width = 1400, height = 600)
autoplot(fc_arima_marsabit) +
  autolayer(test_marsabit, series = "Actual") +
  labs(title = "ARIMA Model: Marsabit Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("ARIMA_Mombasa.png", width = 1400, height = 600)
autoplot(fc_arima_mombasa) +
  autolayer(test_mombasa, series = "Actual") +
  labs(title = "ARIMA Model: Mombasa Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("ARIMA_Nairobi.png", width = 1400, height = 600)
autoplot(fc_arima_nairobi) +
  autolayer(test_nairobi, series = "Actual") +
  labs(title = "ARIMA Model: Nairobi Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("ARIMA_Turkana.png", width = 1400, height = 600)
autoplot(fc_arima_turkana) +
  autolayer(test_turkana, series = "Actual") +
  labs(title = "ARIMA Model: Turkana Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("ARIMA_UasinGishu.png", width = 1400, height = 600)
autoplot(fc_arima_uasin) +
  autolayer(test_uasin, series = "Actual") +
  labs(title = "ARIMA Model: Uasin Gishu Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


 ######################## Time Series Linear Model — TSLM #####################
# Fit TSLM models for all 5 counties
tslm_marsabit <- tslm(train_marsabit ~ trend + season)
tslm_mombasa  <- tslm(train_mombasa  ~ trend + season)
tslm_nairobi  <- tslm(train_nairobi  ~ trend + season)
tslm_turkana  <- tslm(train_turkana  ~ trend + season)
tslm_uasin    <- tslm(train_uasin    ~ trend + season)

# Generate forecasts
fc_tslm_marsabit <- forecast(tslm_marsabit, h = 24)
fc_tslm_mombasa  <- forecast(tslm_mombasa,  h = 24)
fc_tslm_nairobi  <- forecast(tslm_nairobi,  h = 24)
fc_tslm_turkana  <- forecast(tslm_turkana,  h = 24)
fc_tslm_uasin    <- forecast(tslm_uasin,    h = 24)

# Get accuracy
acc_tslm_marsabit <- accuracy(fc_tslm_marsabit, test_marsabit)
acc_tslm_mombasa  <- accuracy(fc_tslm_mombasa,  test_mombasa)
acc_tslm_nairobi  <- accuracy(fc_tslm_nairobi,  test_nairobi)
acc_tslm_turkana  <- accuracy(fc_tslm_turkana,  test_turkana)
acc_tslm_uasin    <- accuracy(fc_tslm_uasin,    test_uasin)

# Build full accuracy table
tslm_full_table <- data.frame(
  County = c("Marsabit", "Marsabit",
             "Mombasa", "Mombasa",
             "Nairobi", "Nairobi",
             "Turkana", "Turkana",
             "Uasin Gishu", "Uasin Gishu"),
  Set = rep(c("Training", "Validation"), 5),
  ME = round(c(
    acc_tslm_marsabit[1,"ME"],  acc_tslm_marsabit[2,"ME"],
    acc_tslm_mombasa[1,"ME"],   acc_tslm_mombasa[2,"ME"],
    acc_tslm_nairobi[1,"ME"],   acc_tslm_nairobi[2,"ME"],
    acc_tslm_turkana[1,"ME"],   acc_tslm_turkana[2,"ME"],
    acc_tslm_uasin[1,"ME"],     acc_tslm_uasin[2,"ME"]), 3),
  RMSE = round(c(
    acc_tslm_marsabit[1,"RMSE"], acc_tslm_marsabit[2,"RMSE"],
    acc_tslm_mombasa[1,"RMSE"],  acc_tslm_mombasa[2,"RMSE"],
    acc_tslm_nairobi[1,"RMSE"],  acc_tslm_nairobi[2,"RMSE"],
    acc_tslm_turkana[1,"RMSE"],  acc_tslm_turkana[2,"RMSE"],
    acc_tslm_uasin[1,"RMSE"],    acc_tslm_uasin[2,"RMSE"]), 3),
  MAE = round(c(
    acc_tslm_marsabit[1,"MAE"], acc_tslm_marsabit[2,"MAE"],
    acc_tslm_mombasa[1,"MAE"],  acc_tslm_mombasa[2,"MAE"],
    acc_tslm_nairobi[1,"MAE"],  acc_tslm_nairobi[2,"MAE"],
    acc_tslm_turkana[1,"MAE"],  acc_tslm_turkana[2,"MAE"],
    acc_tslm_uasin[1,"MAE"],    acc_tslm_uasin[2,"MAE"]), 3),
  MAPE = round(c(
    acc_tslm_marsabit[1,"MAPE"], acc_tslm_marsabit[2,"MAPE"],
    acc_tslm_mombasa[1,"MAPE"],  acc_tslm_mombasa[2,"MAPE"],
    acc_tslm_nairobi[1,"MAPE"],  acc_tslm_nairobi[2,"MAPE"],
    acc_tslm_turkana[1,"MAPE"],  acc_tslm_turkana[2,"MAPE"],
    acc_tslm_uasin[1,"MAPE"],    acc_tslm_uasin[2,"MAPE"]), 3),
  MASE = round(c(
    acc_tslm_marsabit[1,"MASE"], acc_tslm_marsabit[2,"MASE"],
    acc_tslm_mombasa[1,"MASE"],  acc_tslm_mombasa[2,"MASE"],
    acc_tslm_nairobi[1,"MASE"],  acc_tslm_nairobi[2,"MASE"],
    acc_tslm_turkana[1,"MASE"],  acc_tslm_turkana[2,"MASE"],
    acc_tslm_uasin[1,"MASE"],    acc_tslm_uasin[2,"MASE"]), 3)
)

ft_tslm_full <- flextable(tslm_full_table) %>%
  set_header_labels(
    County = "County",
    Set    = "Dataset",
    ME     = "ME",
    RMSE   = "RMSE",
    MAE    = "MAE",
    MAPE   = "MAPE",
    MASE   = "MASE"
  ) %>%
  bold(part = "header") %>%
  merge_v(j = "County") %>%
  autofit()

ft_tslm_full

#### TSLM PLots #### 
png("TSLM_Marsabit.png", width = 1400, height = 600)
autoplot(fc_tslm_marsabit) +
  autolayer(test_marsabit, series = "Actual") +
  labs(title = "TSLM Model: Marsabit Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("TSLM_Mombasa.png", width = 1400, height = 600)
autoplot(fc_tslm_mombasa) +
  autolayer(test_mombasa, series = "Actual") +
  labs(title = "TSLM Model: Mombasa Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("TSLM_Nairobi.png", width = 1400, height = 600)
autoplot(fc_tslm_nairobi) +
  autolayer(test_nairobi, series = "Actual") +
  labs(title = "TSLM Model: Nairobi Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("TSLM_Turkana.png", width = 1400, height = 600)
autoplot(fc_tslm_turkana) +
  autolayer(test_turkana, series = "Actual") +
  labs(title = "TSLM Model: Turkana Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("TSLM_UasinGishu.png", width = 1400, height = 600)
autoplot(fc_tslm_uasin) +
  autolayer(test_uasin, series = "Actual") +
  labs(title = "TSLM Model: Uasin Gishu Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()


 ################################### Residual Diagnostics ##################################
# Residual diagnostics for ARIMA models
png("Residuals_Marsabit.png", width = 1400, height = 800)
checkresiduals(arima_marsabit)
dev.off()

png("Residuals_Mombasa.png", width = 1400, height = 800)
checkresiduals(arima_mombasa)
dev.off()

png("Residuals_Nairobi.png", width = 1400, height = 800)
checkresiduals(arima_nairobi)
dev.off()

png("Residuals_Turkana.png", width = 1400, height = 800)
checkresiduals(arima_turkana)
dev.off()

png("Residuals_UasinGishu.png", width = 1400, height = 800)
checkresiduals(arima_uasin)
dev.off()


 ############################################
# Ljung-Box test for all counties
lb_marsabit <- Box.test(residuals(arima_marsabit), 
                        lag = 20, type = "Ljung-Box")
lb_mombasa  <- Box.test(residuals(arima_mombasa),  
                        lag = 20, type = "Ljung-Box")
lb_nairobi  <- Box.test(residuals(arima_nairobi),  
                        lag = 20, type = "Ljung-Box")
lb_turkana  <- Box.test(residuals(arima_turkana),  
                        lag = 20, type = "Ljung-Box")
lb_uasin    <- Box.test(residuals(arima_uasin),    
                        lag = 20, type = "Ljung-Box")

# Build Ljung-Box results table
lb_table <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi",
             "Turkana", "Uasin Gishu"),
  Test_Statistic = round(c(lb_marsabit$statistic,
                           lb_mombasa$statistic,
                           lb_nairobi$statistic,
                           lb_turkana$statistic,
                           lb_uasin$statistic), 4),
  P_Value = round(c(lb_marsabit$p.value,
                    lb_mombasa$p.value,
                    lb_nairobi$p.value,
                    lb_turkana$p.value,
                    lb_uasin$p.value), 4),
  Decision = c(
    ifelse(lb_marsabit$p.value > 0.05,
           "White Noise — Model Adequate",
           "Not White Noise — Model Inadequate"),
    ifelse(lb_mombasa$p.value > 0.05,
           "White Noise — Model Adequate",
           "Not White Noise — Model Inadequate"),
    ifelse(lb_nairobi$p.value > 0.05,
           "White Noise — Model Adequate",
           "Not White Noise — Model Inadequate"),
    ifelse(lb_turkana$p.value > 0.05,
           "White Noise — Model Adequate",
           "Not White Noise — Model Inadequate"),
    ifelse(lb_uasin$p.value > 0.05,
           "White Noise — Model Adequate",
           "Not White Noise — Model Inadequate")
  )
)

ft_lb <- flextable(lb_table) %>%
  set_header_labels(
    County         = "County",
    Test_Statistic = "Ljung-Box Statistic",
    P_Value        = "P-Value",
    Decision       = "Decision (α = 0.05)"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_lb

##################################################Model Comparison ########################
# Build comprehensive model comparison table
# Using validation MASE as the primary comparison metric

comparison_table <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi",
             "Turkana", "Uasin Gishu"),
  Naive_MASE = c(acc_naive_marsabit[2,"MASE"],
                 acc_naive_mombasa[2,"MASE"],
                 acc_naive_nairobi[2,"MASE"],
                 acc_naive_turkana[2,"MASE"],
                 acc_naive_uasin[2,"MASE"]),
  ETS_MASE = c(acc_ets_marsabit[2,"MASE"],
               acc_ets_mombasa[2,"MASE"],
               acc_ets_nairobi[2,"MASE"],
               acc_ets_turkana[2,"MASE"],
               acc_ets_uasin[2,"MASE"]),
  ARIMA_MASE = c(acc_arima_marsabit[2,"MASE"],
                 acc_arima_mombasa[2,"MASE"],
                 acc_arima_nairobi[2,"MASE"],
                 acc_arima_turkana[2,"MASE"],
                 acc_arima_uasin[2,"MASE"]),
  TSLM_MASE = c(acc_tslm_marsabit[2,"MASE"],
                acc_tslm_mombasa[2,"MASE"],
                acc_tslm_nairobi[2,"MASE"],
                acc_tslm_turkana[2,"MASE"],
                acc_tslm_uasin[2,"MASE"]),
  Best_Model = c("Naïve", "Naïve", "TSLM",
                 "TSLM", "Naïve")
)

# Round numeric columns
comparison_table[,2:5] <- round(comparison_table[,2:5], 3)

ft_comparison <- flextable(comparison_table) %>%
  set_header_labels(
    County     = "County",
    Naive_MASE = "Naïve MASE",
    ETS_MASE   = "ETS MASE",
    ARIMA_MASE = "ARIMA MASE",
    TSLM_MASE  = "TSLM MASE",
    Best_Model = "Best Model"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_comparison


 #### RMSE comparison table for completeness: ####
# RMSE comparison table
rmse_table <- data.frame(
  County = c("Marsabit", "Mombasa", "Nairobi",
             "Turkana", "Uasin Gishu"),
  Naive_RMSE = round(c(acc_naive_marsabit[2,"RMSE"],
                       acc_naive_mombasa[2,"RMSE"],
                       acc_naive_nairobi[2,"RMSE"],
                       acc_naive_turkana[2,"RMSE"],
                       acc_naive_uasin[2,"RMSE"]), 3),
  ETS_RMSE = round(c(acc_ets_marsabit[2,"RMSE"],
                     acc_ets_mombasa[2,"RMSE"],
                     acc_ets_nairobi[2,"RMSE"],
                     acc_ets_turkana[2,"RMSE"],
                     acc_ets_uasin[2,"RMSE"]), 3),
  ARIMA_RMSE = round(c(acc_arima_marsabit[2,"RMSE"],
                       acc_arima_mombasa[2,"RMSE"],
                       acc_arima_nairobi[2,"RMSE"],
                       acc_arima_turkana[2,"RMSE"],
                       acc_arima_uasin[2,"RMSE"]), 3),
  TSLM_RMSE = round(c(acc_tslm_marsabit[2,"RMSE"],
                      acc_tslm_mombasa[2,"RMSE"],
                      acc_tslm_nairobi[2,"RMSE"],
                      acc_tslm_turkana[2,"RMSE"],
                      acc_tslm_uasin[2,"RMSE"]), 3)
)

ft_rmse <- flextable(rmse_table) %>%
  set_header_labels(
    County     = "County",
    Naive_RMSE = "Naïve RMSE",
    ETS_RMSE   = "ETS RMSE",
    ARIMA_RMSE = "ARIMA RMSE",
    TSLM_RMSE  = "TSLM RMSE"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_rmse


######################################################################

######################## 24-Month Future Forecast section #######################

# Refit best models on FULL series for future forecasting

# Marsabit - Naive on full series
fc_future_marsabit <- naive(ts_marsabit, h = 24)

# Mombasa - Naive on full series
fc_future_mombasa <- naive(ts_mombasa, h = 24)

# Nairobi - TSLM on full series
tslm_nairobi_full <- tslm(ts_nairobi ~ trend + season)
fc_future_nairobi <- forecast(tslm_nairobi_full, h = 24)

# Turkana - TSLM on full series
tslm_turkana_full <- tslm(ts_turkana ~ trend + season)
fc_future_turkana <- forecast(tslm_turkana_full, h = 24)

# Uasin Gishu - Naive on full series
fc_future_uasin <- naive(ts_uasin, h = 24)

# Confirm forecasts generated
cat("Marsabit:", length(fc_future_marsabit$mean), "\n")
cat("Mombasa:", length(fc_future_mombasa$mean), "\n")
cat("Nairobi:", length(fc_future_nairobi$mean), "\n")
cat("Turkana:", length(fc_future_turkana$mean), "\n")
cat("Uasin Gishu:", length(fc_future_uasin$mean), "\n")






# Generate future date sequences for each county
dates_marsabit <- seq(as.Date("2025-04-01"), 
                      by = "month", length.out = 24)
dates_mombasa  <- seq(as.Date("2024-07-01"), 
                      by = "month", length.out = 24)
dates_nairobi  <- seq(as.Date("2025-11-01"), 
                      by = "month", length.out = 24)
dates_turkana  <- seq(as.Date("2025-10-01"), 
                      by = "month", length.out = 24)
dates_uasin    <- seq(as.Date("2025-07-01"), 
                      by = "month", length.out = 24)

# Build forecast table for all counties
forecast_all <- data.frame(
  Month = format(dates_nairobi, "%B %Y"),
  Marsabit = round(as.numeric(fc_future_marsabit$mean), 2),
  Mombasa  = round(as.numeric(fc_future_mombasa$mean),  2),
  Nairobi  = round(as.numeric(fc_future_nairobi$mean),  2),
  Turkana  = round(as.numeric(fc_future_turkana$mean),  2),
  Uasin_Gishu = round(as.numeric(fc_future_uasin$mean), 2)
)

ft_forecast <- flextable(forecast_all) %>%
  set_header_labels(
    Month       = "Month",
    Marsabit    = "Marsabit (KES/KG)",
    Mombasa     = "Mombasa (KES/KG)",
    Nairobi     = "Nairobi (KES/KG)",
    Turkana     = "Turkana (KES/KG)",
    Uasin_Gishu = "Uasin Gishu (KES/KG)"
  ) %>%
  bold(part = "header") %>%
  autofit()

ft_forecast

## saving it for best viewing
# Install officer if needed
# install.packages("officer")
library(officer)

# Save flextable as image for ease of viewing
save_as_image(ft_forecast, path = "Forecast_Table.png")


############################ forecast plots #### 

png("Future_Marsabit.png", width = 1400, height = 600)
autoplot(fc_future_marsabit) +
  labs(title = "24-Month Forecast: Marsabit Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Future_Mombasa.png", width = 1400, height = 600)
autoplot(fc_future_mombasa) +
  labs(title = "24-Month Forecast: Mombasa Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Future_Nairobi.png", width = 1400, height = 600)
autoplot(fc_future_nairobi) +
  labs(title = "24-Month Forecast: Nairobi Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Future_Turkana.png", width = 1400, height = 600)
autoplot(fc_future_turkana) +
  labs(title = "24-Month Forecast: Turkana Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

png("Future_UasinGishu.png", width = 1400, height = 600)
autoplot(fc_future_uasin) +
  labs(title = "24-Month Forecast: Uasin Gishu Maize Price",
       x = "Year", y = "Price (KES per KG)") +
  theme_minimal()
dev.off()

##############################################################################
################ Visualization of Model Fitted Values and Forecasts ###################################
# Marsabit Overlay
png("Overlay_Marsabit.png", width = 1600, height = 700)
autoplot(ts_marsabit, series = "Actual Data") +
  autolayer(fitted(naive_marsabit),   series = "Naïve Fitted") +
  autolayer(fitted(ets_marsabit),     series = "ETS Fitted") +
  autolayer(fitted(arima_marsabit),   series = "ARIMA Fitted") +
  autolayer(fitted(tslm_marsabit),    series = "TSLM Fitted") +
  autolayer(fc_arima_marsabit,        series = "ARIMA Forecast", PI = FALSE) +
  autolayer(fc_ets_marsabit,          series = "ETS Forecast", PI = FALSE) +
  autolayer(fc_future_marsabit,       series = "Naïve Forecast", PI = FALSE) +
  scale_color_manual(values = c(
    "Actual Data"    = "black",
    "Naïve Fitted"   = "gray50",
    "ETS Fitted"     = "blue",
    "ARIMA Fitted"   = "green4",
    "TSLM Fitted"    = "orange",
    "Naïve Forecast" = "gray50",
    "ETS Forecast"   = "blue",
    "ARIMA Forecast" = "green4",
    "TSLM Forecast"  = "orange"
  )) +
  labs(title = "Marsabit: Actual vs Fitted vs Forecast — All Models",
       x = "Year", y = "Price (KES per KG)", color = "Series") +
  theme_minimal()
dev.off()

# Mombasa Overlay
png("Overlay_Mombasa.png", width = 1600, height = 700)
autoplot(ts_mombasa, series = "Actual Data") +
  autolayer(fitted(naive_mombasa),   series = "Naïve Fitted") +
  autolayer(fitted(ets_mombasa),     series = "ETS Fitted") +
  autolayer(fitted(arima_mombasa),   series = "ARIMA Fitted") +
  autolayer(fitted(tslm_mombasa),    series = "TSLM Fitted") +
  autolayer(fc_arima_mombasa,        series = "ARIMA Forecast", PI = FALSE) +
  autolayer(fc_ets_mombasa,          series = "ETS Forecast", PI = FALSE) +
  autolayer(fc_future_mombasa,       series = "Naïve Forecast", PI = FALSE) +
  scale_color_manual(values = c(
    "Actual Data"    = "black",
    "Naïve Fitted"   = "gray50",
    "ETS Fitted"     = "blue",
    "ARIMA Fitted"   = "green4",
    "TSLM Fitted"    = "orange",
    "Naïve Forecast" = "gray50",
    "ETS Forecast"   = "blue",
    "ARIMA Forecast" = "green4",
    "TSLM Forecast"  = "orange"
  )) +
  labs(title = "Mombasa: Actual vs Fitted vs Forecast — All Models",
       x = "Year", y = "Price (KES per KG)", color = "Series") +
  theme_minimal()
dev.off()

# Nairobi Overlay
png("Overlay_Nairobi.png", width = 1600, height = 700)
autoplot(ts_nairobi, series = "Actual Data") +
  autolayer(fitted(naive_nairobi),   series = "Naïve Fitted") +
  autolayer(fitted(ets_nairobi),     series = "ETS Fitted") +
  autolayer(fitted(arima_nairobi),   series = "ARIMA Fitted") +
  autolayer(fitted(tslm_nairobi),    series = "TSLM Fitted") +
  autolayer(fc_arima_nairobi,        series = "ARIMA Forecast", PI = FALSE) +
  autolayer(fc_ets_nairobi,          series = "ETS Forecast", PI = FALSE) +
  autolayer(fc_future_nairobi,       series = "TSLM Forecast", PI = FALSE) +
  autolayer(naive_nairobi,           series = "Naïve Forecast", PI = FALSE) +
  scale_color_manual(values = c(
    "Actual Data"    = "black",
    "Naïve Fitted"   = "gray50",
    "ETS Fitted"     = "blue",
    "ARIMA Fitted"   = "green4",
    "TSLM Fitted"    = "orange",
    "Naïve Forecast" = "gray50",
    "ETS Forecast"   = "blue",
    "ARIMA Forecast" = "green4",
    "TSLM Forecast"  = "orange"
  )) +
  labs(title = "Nairobi: Actual vs Fitted vs Forecast — All Models",
       x = "Year", y = "Price (KES per KG)", color = "Series") +
  theme_minimal()
dev.off()

# Turkana Overlay
png("Overlay_Turkana.png", width = 1600, height = 700)
autoplot(ts_turkana, series = "Actual Data") +
  autolayer(fitted(naive_turkana),   series = "Naïve Fitted") +
  autolayer(fitted(ets_turkana),     series = "ETS Fitted") +
  autolayer(fitted(arima_turkana),   series = "ARIMA Fitted") +
  autolayer(fitted(tslm_turkana),    series = "TSLM Fitted") +
  autolayer(fc_arima_turkana,        series = "ARIMA Forecast", PI = FALSE) +
  autolayer(fc_ets_turkana,          series = "ETS Forecast", PI = FALSE) +
  autolayer(fc_future_turkana,       series = "TSLM Forecast", PI = FALSE) +
  autolayer(naive_turkana,           series = "Naïve Forecast", PI = FALSE) +
  scale_color_manual(values = c(
    "Actual Data"    = "black",
    "Naïve Fitted"   = "gray50",
    "ETS Fitted"     = "blue",
    "ARIMA Fitted"   = "green4",
    "TSLM Fitted"    = "orange",
    "Naïve Forecast" = "gray50",
    "ETS Forecast"   = "blue",
    "ARIMA Forecast" = "green4",
    "TSLM Forecast"  = "orange"
  )) +
  labs(title = "Turkana: Actual vs Fitted vs Forecast — All Models",
       x = "Year", y = "Price (KES per KG)", color = "Series") +
  theme_minimal()
dev.off()

# Uasin Gishu Overlay
png("Overlay_UasinGishu.png", width = 1600, height = 700)
autoplot(ts_uasin, series = "Actual Data") +
  autolayer(fitted(naive_uasin),   series = "Naïve Fitted") +
  autolayer(fitted(ets_uasin),     series = "ETS Fitted") +
  autolayer(fitted(arima_uasin),   series = "ARIMA Fitted") +
  autolayer(fitted(tslm_uasin),    series = "TSLM Fitted") +
  autolayer(fc_arima_uasin,        series = "ARIMA Forecast", PI = FALSE) +
  autolayer(fc_ets_uasin,          series = "ETS Forecast", PI = FALSE) +
  autolayer(fc_future_uasin,       series = "Naïve Forecast", PI = FALSE) +
  scale_color_manual(values = c(
    "Actual Data"    = "black",
    "Naïve Fitted"   = "gray50",
    "ETS Fitted"     = "blue",
    "ARIMA Fitted"   = "green4",
    "TSLM Fitted"    = "orange",
    "Naïve Forecast" = "gray50",
    "ETS Forecast"   = "blue",
    "ARIMA Forecast" = "green4",
    "TSLM Forecast"  = "orange"
  )) +
  labs(title = "Uasin Gishu: Actual vs Fitted vs Forecast — All Models",
       x = "Year", y = "Price (KES per KG)", color = "Series") +
  theme_minimal()
dev.off()

cat("All 5 overlay charts saved!\n")

