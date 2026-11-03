# ============================================================================
# ARIMA FORECASTING FOR MISSING TRAILING OBSERVATIONS
# Project: Geopolitics, Oil Shock, and Macroeconomics
# ============================================================================
# This script handles missing values (April-June 2026) using ARIMA forecasting
# and saves a complete dataset for econometric analysis.
# Run this script BEFORE the main econometric analysis.
# ============================================================================

library(tidyverse)
library(forecast)

# ----------------------------------------------------------------------------
# 1. LOAD RAW DATA
# ----------------------------------------------------------------------------

data_raw <- read.csv("C:/Users/moham/Desktop/ARIMA/final_panel_dataset.csv", stringsAsFactors = FALSE)

# Convert date
data_raw$date <- as.Date(data_raw$date, format = "%d/%m/%Y")

# ----------------------------------------------------------------------------
# 2. CLEAN AND IDENTIFY MISSING VALUES
# ----------------------------------------------------------------------------

clean_to_na <- function(x) {
  ifelse(x %in% c("#N/A", "#VALUE!"), NA, as.numeric(x))
}

data_clean <- data_raw %>%
  mutate(
    cpi = clean_to_na(cpi),
    ipi = clean_to_na(ipi),
    neer = clean_to_na(neer),
    policy_rate = clean_to_na(policy_rate),
    brent = as.numeric(brent),
    vix = as.numeric(vix)
  )

cat("\n=== MISSING VALUES BEFORE ARIMA FORECASTING ===\n")
missing_before <- data_clean %>%
  summarise(
    cpi_na = sum(is.na(cpi)),
    ipi_na = sum(is.na(ipi)),
    neer_na = sum(is.na(neer)),
    policy_na = sum(is.na(policy_rate)),
    brent_na = sum(is.na(brent)),
    vix_na = sum(is.na(vix))
  )
print(missing_before)

# ----------------------------------------------------------------------------
# 3. ARIMA FORECASTING FUNCTION
# ----------------------------------------------------------------------------

simple_arima_forecast <- function(series, h = 3) {
  if (!any(is.na(series))) return(series)
  
  y <- series[!is.na(series)]
  
  if (length(y) < 12) {
    series[is.na(series)] <- tail(y, 1)
    return(series)
  }
  
  y_ts <- ts(y, frequency = 12, start = c(2023, 1))
  fit <- auto.arima(y_ts, seasonal = TRUE, stepwise = TRUE)
  fc <- forecast(fit, h = sum(is.na(series)))
  series[is.na(series)] <- as.numeric(fc$mean)
  
  return(series)
}

# ----------------------------------------------------------------------------
# 4. APPLY ARIMA TO COUNTRY-SPECIFIC VARIABLES
# ----------------------------------------------------------------------------

data_imputed <- data_clean %>%
  group_by(Country) %>%
  mutate(
    cpi = simple_arima_forecast(cpi, h = 3),
    ipi = simple_arima_forecast(ipi, h = 3),
    neer = simple_arima_forecast(neer, h = 3),
    policy_rate = simple_arima_forecast(policy_rate, h = 3)
  ) %>%
  ungroup()

# ----------------------------------------------------------------------------
# 5. HANDLE GLOBAL VARIABLES (BRENT AND VIX)
# ----------------------------------------------------------------------------

# Brent forecast
brent_historical <- data_clean %>%
  filter(Country == "Germany", !is.na(brent)) %>%
  arrange(date) %>%
  pull(brent)

if (any(is.na(data_imputed$brent))) {
  brent_ts <- ts(brent_historical, frequency = 12, start = c(2023, 1))
  brent_fit <- auto.arima(brent_ts, seasonal = TRUE)
  brent_forecast <- forecast(brent_fit, h = 1)
  brent_june_2026 <- as.numeric(brent_forecast$mean)
  
  data_imputed <- data_imputed %>%
    mutate(brent = ifelse(is.na(brent), brent_june_2026, brent))
}

# VIX forecast
vix_historical <- data_clean %>%
  filter(Country == "Germany", !is.na(vix)) %>%
  arrange(date) %>%
  pull(vix)

if (any(is.na(data_imputed$vix))) {
  vix_ts <- ts(vix_historical, frequency = 12, start = c(2023, 1))
  vix_fit <- auto.arima(vix_ts, seasonal = TRUE)
  vix_forecast <- forecast(vix_fit, h = 1)
  vix_june_2026 <- as.numeric(vix_forecast$mean)
  
  data_imputed <- data_imputed %>%
    mutate(vix = ifelse(is.na(vix), vix_june_2026, vix))
}

# ----------------------------------------------------------------------------
# 6. VERIFY AND SAVE COMPLETE DATASET
# ----------------------------------------------------------------------------

cat("\n=== MISSING VALUES AFTER ARIMA FORECASTING ===\n")
missing_after <- data_imputed %>%
  summarise(
    cpi_na = sum(is.na(cpi)),
    ipi_na = sum(is.na(ipi)),
    neer_na = sum(is.na(neer)),
    policy_na = sum(is.na(policy_rate)),
    brent_na = sum(is.na(brent)),
    vix_na = sum(is.na(vix))
  )
print(missing_after)

# Save the complete dataset (NO transformations, just cleaned data)
write.csv(data_imputed, "final_panel_complete.csv", row.names = FALSE)

cat("\n✅ Complete dataset saved to 'final_panel_complete.csv'\n")
cat("   This file has NO missing values and is ready for econometric analysis.\n")