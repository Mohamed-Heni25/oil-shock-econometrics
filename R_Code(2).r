# =====================
# R Code for the Project - 
# =====================
# Project: Geopolitics, Oil Shock, and Macroeconomics - Panel Data Analysis
# ============================================================================

# 1. INSTALL AND LOAD LIBRARIES
# ============================================================================
# install.packages(c("tidyverse", "plm", "lmtest", "stargazer", "ggplot2", "car"))

library(tidyverse)
library(plm)
library(lmtest)
library(stargazer)
library(ggplot2)
library(car)

# ============================================================================
# 2. LOAD AND PREPARE THE DATA
# ============================================================================

# Load your CLEAN dataset (CHANGE THIS PATH if needed)
data_raw <- read.csv("final_panel_complete.csv", stringsAsFactors = FALSE)

# Rename Country to country
names(data_raw)[names(data_raw) == "Country"] <- "country"

# Inspect the data structure
str(data_raw)
head(data_raw)

# Convert date column to Date format
data_raw$date <- as.Date(data_raw$date)

# Create the natural log of dependent variables and ADD GROUPING VARIABLES
data_processed <- data_raw %>%
  mutate(
    ln_cpi = log(cpi),
    ln_ipi = log(ipi),
    ln_neer = log(neer),
    # Create a dummy variable for the "post-crisis" period (starting March 2026)
    post_crisis = ifelse(date >= as.Date("2026-03-01"), 1, 0),
    # Calculate the monthly percentage change in oil price (the shock)
    oil_pct_change = (brent - lag(brent)) / lag(brent) * 100
  ) %>%
  # Group by country to calculate the lagged oil price change correctly
  group_by(country) %>%
  mutate(oil_pct_change = ifelse(is.na(oil_pct_change), 0, oil_pct_change)) %>%
  ungroup() %>%
  # ADD GROUPING VARIABLES HERE (FIXES THE ERROR)
  mutate(
    oil_balance = case_when(
      country %in% c("Germany", "France", "India", "Turkey") ~ "importer",
      country %in% c("United states", "Canada", "Saudi Arabia", "Russia") ~ "exporter",
      TRUE ~ "other"
    ),
    group = case_when(
      country %in% c("Germany", "France") ~ "Advanced_Importer",
      country %in% c("United states", "Canada") ~ "Advanced_Exporter",
      country %in% c("India", "Turkey") ~ "Emerging_Importer",
      country %in% c("Saudi Arabia", "Russia") ~ "Emerging_Exporter",
      TRUE ~ "Other"
    )
  ) %>%
  # Your data is clean, so no filtering needed (but keep for safety)
  filter(!is.na(ln_cpi), !is.na(ln_ipi))

# ============================================================================
# 3. EXPLORATORY DATA ANALYSIS (EDA)
# ============================================================================

# Plot the trend of oil prices over time
ggplot(data_processed, aes(x = date, y = brent)) +
  geom_line(color = "darkred", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2026-02-27")), linetype = "dashed", color = "blue") +
  annotate("text", x = as.Date("2026-02-27"), y = max(data_processed$brent), 
           label = "Crisis Start", hjust = -0.1) +
  labs(title = "Brent Crude Oil Price: Pre and Post 2026 Gulf Crisis",
       x = "Date", y = "USD per Barrel") +
  theme_minimal()

# Plot the average CPI trend for different country groups (NOW group EXISTS)
ggplot(data_processed, aes(x = date, y = ln_cpi, color = group)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2026-02-27")), linetype = "dashed") +
  labs(title = "Log CPI Trends by Country Group",
       x = "Date", y = "Log(CPI)") +
  theme_minimal()

# ============================================================================
# 4. PANEL DATA REGRESSION ANALYSIS
# ============================================================================

# First, declare the data as a panel data frame
pdata <- pdata.frame(data_processed, index = c("country", "date"))

# Model 1: Pooled OLS (POLS)
pols_model <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                  data = pdata, model = "pooling")
cat("\n=== POOLED OLS RESULTS ===\n")
summary(pols_model)

# Model 2: Fixed Effects (FE)
fe_model <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                data = pdata, model = "within")
cat("\n=== FIXED EFFECTS RESULTS ===\n")
summary(fe_model)

# Model 3: Random Effects (RE)
re_model <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                data = pdata, model = "random")
cat("\n=== RANDOM EFFECTS RESULTS ===\n")
summary(re_model)

# ============================================================================
# 5. DIAGNOSTIC TESTS AND ROBUST STANDARD ERRORS
# ============================================================================

hausman_test <- phtest(fe_model, re_model)
cat("\n=== HAUSMAN TEST ===\n")
print(hausman_test)

bp_test <- plmtest(pols_model, effect = "individual", type = "bp")
cat("\n=== BREUSCH-PAGAN TEST ===\n")
print(bp_test)

cat("\n=== ROBUST STANDARD ERRORS (Fixed Effects) ===\n")
coeftest(fe_model, vcov = vcovHC(fe_model, method = "arellano", type = "HC3"))

# ============================================================================
# 6. HETEROGENEITY ANALYSIS: OIL-IMPORTERS VS. EXPORTERS
# ============================================================================

# NOW oil_balance EXISTS
importers_data <- pdata %>% filter(oil_balance == "importer")
exporters_data <- pdata %>% filter(oil_balance == "exporter")

fe_importers <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                    data = importers_data, model = "within")
fe_exporters <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix + post_crisis,
                    data = exporters_data, model = "within")

stargazer(fe_importers, fe_exporters,
          title = "Fixed Effects Results: Oil Importers vs. Exporters",
          column.labels = c("Importers", "Exporters"),
          type = "text",
          keep.stat = c("n", "rsq"))

# ============================================================================
# 7. INTERPRETATION AND CONCLUSION
# ============================================================================

cat("\n", "=" , rep("=", 70), "\n")
cat("INTERPRETATION SUMMARY")
cat("\n", "=" , rep("=", 70), "\n")

cat("\n1. OIL SHOCK COEFFICIENT:\n")
if ("oil_pct_change" %in% names(coef(fe_model))) {
  cat("   Fixed Effects:", round(coef(fe_model)["oil_pct_change"], 6), "\n")
} else {
  cat("   Note: oil_pct_change was dropped due to collinearity\n")
  cat("   → The post_crisis dummy captures the oil shock effect\n")
}

cat("\n2. MODEL SELECTION (Hausman Test):\n")
cat("   p-value =", round(hausman_test$p.value, 4), "\n")
if (hausman_test$p.value < 0.05) {
  cat("   → Fixed Effects model is preferred\n")
} else {
  cat("   → Random Effects model is appropriate\n")
}

cat("\n3. POST-CRISIS EFFECT:\n")
cat("   Coefficient:", round(coef(fe_model)["post_crisis"], 4), "\n")
cat("   → The Gulf crisis caused a", round(coef(fe_model)["post_crisis"] * 100, 2), "% increase in CPI\n")

cat("\n4. EXCHANGE RATE CHANNEL:\n")
cat("   Coefficient:", round(coef(fe_model)["ln_neer"], 4), "\n")
cat("   → 1% NEER appreciation reduces inflation by", 
    round(abs(coef(fe_model)["ln_neer"]) * 100, 2), "%\n")

cat("\n5. HETEROGENEITY (Post-Crisis Effect):\n")
cat("   Importers:", round(coef(fe_importers)["post_crisis"], 4), "\n")
cat("   Exporters:", round(coef(fe_exporters)["post_crisis"], 4), "\n")