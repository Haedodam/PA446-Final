# ==========================================================
# STEP 0: LOAD REQUIRED PACKAGES
# ==========================================================

# Data access & wrangling
library(tidycensus)
library(dplyr)
library(tidyverse)
library(rvest)
library(stringr)
library(readr)

# Missingness visualization
library(naniar)

# Modeling & tidying model output
library(broom)

# Spatial data & mapping
library(tigris)
library(sf)
library(ggplot2)

# Interactive dashboard
library(shiny)
library(DT)
library(bslib)

# Machine learning (Decision Tree)
library(rpart)
library(rpart.plot)

# Geospatial caching
options(tigris_use_cache = TRUE)

# ============================================
# STEP 2: DATA ACQUISITION 
# ============================================

# --------------------------------------------------
# 1. Register Census API Key
# --------------------------------------------------
# This key allows access to ACS data via tidycensus.
# install = TRUE writes the key into your .Renviron file.
census_api_key("80f5754d51eed4e6d3803372680db90d9eda7b5b",
               install = TRUE, overwrite = TRUE)

# Reload environment so that the key becomes active
readRenviron("~/.Renviron")

# --------------------------------------------------
# 2. Define ACS variables to pull
# --------------------------------------------------
# Selected socioeconomic indicators:
# - Poverty rate
# - Percent White (for computing % non-White)
# - Percent English-only speakers
# - Percent Bachelor’s degree or higher
vars <- c(
  poverty_rate            = "DP03_0126PE",
  pct_white               = "DP05_0071PE",
  pct_english_only        = "DP02_0111PE",
  pct_bachelors_or_higher = "DP02_0068PE"
)

# --------------------------------------------------
# 3. Pull ACS data at ZCTA (ZIP Code Tabulation Area) level
# --------------------------------------------------
# output = "wide" creates one column per variable
acs_raw <- get_acs(
  geography = "zcta",
  variables = vars,
  year = 2022,
  survey = "acs5",
  output = "wide"
)

# --------------------------------------------------
# 4. Define ZIP Codes for Chicago boundaries
# --------------------------------------------------
# This custom list ensures we only keep ZIPs located in Chicago
chicago_zips <- c(
  "60601","60602","60603","60604","60605","60606","60607","60608","60609","60610",
  "60611","60612","60613","60614","60615","60616","60617","60618","60619","60620",
  "60621","60622","60623","60624","60625","60626","60628","60629","60630","60631",
  "60632","60633","60634","60636","60637","60638","60639","60640","60641","60643",
  "60644","60645","60646","60647","60649","60651","60652","60653","60654","60655",
  "60656","60657","60659","60660","60661","60664","60666","60680","60681","60690",
  "60691","60701","60706","60707","60803","60804","60805","60827"
)

# Keep only ZCTAs within Chicago
acs_chicago <- acs_raw %>%
  filter(GEOID %in% chicago_zips)

# --------------------------------------------------
# 5. Clean ACS data and derive new socioeconomic variables
# --------------------------------------------------
# Creating derived indicators:
# - % non-White
# - % non-English
# - % non-college
acs_chicago_clean <- acs_chicago %>%
  select(
    zip = GEOID,
    poverty_rate,
    pct_white,
    pct_english_only,
    pct_bachelors_or_higher
  ) %>%
  mutate(
    pct_nonwhite    = 100 - pct_white,
    pct_non_english = 100 - pct_english_only,
    pct_noncollege  = 100 - pct_bachelors_or_higher
  ) %>%
  select(
    zip,
    poverty_rate,
    pct_nonwhite,
    pct_non_english,
    pct_noncollege
  )

# --------------------------------------------------
# 6. Load CPS Arts Education Data (Creative Schools Certification)
# --------------------------------------------------
# school-results-11-28-25.csv must be placed in your project folder
schools <- readr::read_csv("Data/school-results-11-28-25.csv")

# Extract ZIP code from address field using regular expression
schools2 <- schools %>%
  mutate(zip = str_extract(Address, "\\d{5}"))

# Keep only ZIP and Certification column
arts_chicago <- schools2 %>% 
  select(zip, `Creative Schools Certification`)

# --------------------------------------------------
# 7. Convert certification categories into numeric scale
# --------------------------------------------------
# 1 = Emerging  
# 2 = Developing  
# 3 = Strong  
# 4 = Excelling
arts_chicago <- arts_chicago %>%
  mutate(
    CSC_score = case_when(
      `Creative Schools Certification` == "Emerging"   ~ 1,
      `Creative Schools Certification` == "Developing" ~ 2,
      `Creative Schools Certification` == "Strong"     ~ 3,
      `Creative Schools Certification` == "Excelling"  ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  select(CSC_score, zip)

# --------------------------------------------------
# 8. Merge ACS socioeconomic data with school arts data
# --------------------------------------------------
arts_full <- arts_chicago %>%
  left_join(acs_chicago_clean, by = "zip")

# ============================================
# STEP 3: DATA WRANGLING & Quality Checks
# ============================================

# --------------------------------------------------
# 1. Inspect structure and summary statistics
# --------------------------------------------------
# Check variable types, number of rows/columns
glimpse(arts_full)

# Quick descriptive statistics for each variable
summary(arts_full)

# --------------------------------------------------
# 2. Examine missing data patterns
# --------------------------------------------------
# Count missing values per variable
gg_miss_var(arts_full)

# Visualize missingness matrix (dark areas = missing)
vis_miss(arts_full)

# --------------------------------------------------
# 3. Check representativeness by counting schools per ZIP
# --------------------------------------------------
# This helps identify ZIP codes with very few observations
arts_full %>%
  count(zip) %>%
  arrange(n)

# --------------------------------------------------
# 4. Fairness check:
#    Are missing CSC scores concentrated in poorer ZIPs?
# --------------------------------------------------
# Compare poverty rates between ZIPs with vs. without CSC scores
arts_full %>%
  mutate(CSC_missing = is.na(CSC_score)) %>%
  group_by(CSC_missing) %>%
  summarize(mean_poverty = mean(poverty_rate, na.rm = TRUE))

# --------------------------------------------------
# 5. Drop rows missing essential variables
# --------------------------------------------------
# Required variables for analysis:
# - CSC_score
# - poverty_rate
# - pct_nonwhite
# - pct_non_english
# - pct_noncollege
arts_full <- arts_full %>%
  drop_na(CSC_score, poverty_rate, pct_nonwhite, pct_non_english, pct_noncollege)

# ==========================================================
# STEP 4: REPRODUCIBLE REPORTING - ANALYSIS
# ==========================================================

# --------------------------------------------------
# 1. Aggregate data to ZIP level
# --------------------------------------------------
# Compute:
# - average CSC score per ZIP
# - socioeconomic indicators
# - number of schools per ZIP
zip_summary <- arts_full %>%
  group_by(zip) %>%
  summarize(
    avg_CSC        = mean(CSC_score, na.rm = TRUE),
    poverty_rate   = unique(poverty_rate),
    pct_nonwhite   = unique(pct_nonwhite),
    pct_non_english= unique(pct_non_english),
    pct_noncollege = unique(pct_noncollege),
    n_schools      = n()
  )

# --------------------------------------------------
# 2. Load Chicago ZCTA shapefile (2020)
# --------------------------------------------------
# 'zctas()' loads ZIP-level geographic boundaries
chi_zcta <- zctas(cb = TRUE, year = 2020) %>%
  mutate(zip = as.character(ZCTA5CE20)) %>%     # Convert to character ZIP code
  filter(zip %in% zip_summary$zip)              # Keep Chicago-only ZIPs

# Merge spatial data with aggregated ZIP summary
chi_map <- chi_zcta %>%
  left_join(zip_summary, by = "zip")

# --------------------------------------------------
# 3. Map: Visualize spatial distribution of CSC scores
# --------------------------------------------------
ggplot(chi_map) +
  geom_sf(aes(fill = avg_CSC), color = "white", size = 0.25) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Average CSC Score by ZIP Code in Chicago",
    fill  = "Avg CSC"
  ) +
  theme_minimal()

# --------------------------------------------------
# 4. Regression Model:
#    Which socioeconomic predictors relate to CSC?
# --------------------------------------------------
model_all <- lm(
  avg_CSC ~ poverty_rate + pct_nonwhite + pct_non_english + pct_noncollege,
  data = zip_summary
)

summary(model_all)   # Print regression output

# --------------------------------------------------
# 5. Visualize Regression Coefficients
# --------------------------------------------------
tidy(model_all) %>%
  filter(term != "(Intercept)") %>%       # Remove intercept
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = estimate - std.error,
        ymax = estimate + std.error),
    width = 0.2
  ) +
  coord_flip() +
  labs(
    title = "Effect Sizes of Predictors on CSC Score",
    x = "Predictor",
    y = "Coefficient"
  ) +
  theme_minimal()

# --------------------------------------------------
# 6. Interactive Shiny Dashboard
# --------------------------------------------------

# The dashboard allows:
# - selecting a socioeconomic predictor for scatter plot
# - viewing first N ZIP rows in a data table
# - personalized greeting using user name input

df <- zip_summary   # ZIP-level dataset for dashboard

# Numeric predictors available in scatter plot
num_cols <- c("poverty_rate", "pct_nonwhite", "pct_non_english", "pct_noncollege")

# ------------------------------
# UI layout
# ------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  div(style = "padding: 12px 0; font-weight: 600; font-size: 1.2rem;",
      textOutput("greet")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      textInput("name", "What is your name?"),
      
      selectInput(
        "xvar",
        "Select predictor (X-axis):",
        choices = num_cols,
        selected = "pct_nonwhite"
      ),
      
      numericInput(
        "nrows",
        "Number of ZIP rows to show:",
        value = 10, min = 1, max = nrow(df)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatter", height = 420)),
        tabPanel("ZIP Table", DTOutput("table"))
      )
    )
  )
)

# ------------------------------
# SERVER logic
# ------------------------------
server <- function(input, output, session) {
  
  # Greeting message updates depending on user input
  output$greet <- renderText({
    nm <- trimws(input$name)
    if (nm == "" || is.null(nm))
      "Welcome to the Chicago Arts Edu Equity Explorer!"
    else
      paste0("Hello, ", nm, "! Explore how local conditions shape CSC scores.")
  })
  
  # Scatter plot (Y-axis fixed as avg_CSC)
  output$scatter <- renderPlot({
    ggplot(df, aes(x = .data[[input$xvar]], y = avg_CSC)) +
      geom_point(size = 3, alpha = 0.8, color = "#0072B2") +
      geom_smooth(method = "lm", color = "red", se = TRUE) +
      labs(
        x = input$xvar,
        y = "Average CSC Score",
        title = "Relationship Between CSC Score and Socioeconomic Factors"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # Display first N rows of ZIP summary data
  output$table <- renderDT({
    datatable(
      df %>% head(input$nrows),
      rownames = FALSE,
      options = list(pageLength = input$nrows)
    )
  })
}

# ------------------------------
# RUN THE APP
# ------------------------------
shinyApp(ui, server)

# ==========================================================
# STEP 5: ADVANCED ANALYSIS - MACHINE LEARNING(DECISION TREE)
# ==========================================================

# --------------------------------------------------
# 1. Create binary outcome for classification
# --------------------------------------------------
# High CSC (>= 3)  → "High"
# Low CSC (< 3)    → "Low"
zip_summary$CSC_binary <- ifelse(zip_summary$avg_CSC >= 3, "High", "Low")

# --------------------------------------------------
# 2. Fit Decision Tree Model
# --------------------------------------------------
# The predictors are four socioeconomic indicators.
# The outcome variable is the binary CSC category.
tree <- rpart(
  CSC_binary ~ poverty_rate + pct_nonwhite + pct_non_english + pct_noncollege,
  data = zip_summary,
  method = "class"
)

# --------------------------------------------------
# 3. Plot the decision tree
# --------------------------------------------------
# Visualization settings:
# - type = 3: show split labels and terminal node values
# - extra = 104: display class probabilities and class predictions
# - fallen.leaves = TRUE: tidy layout with leaves at the bottom
# - box.palette = "RdBu": two-color gradient
rpart.plot(
  tree,
  type = 3,
  extra = 104,
  under = TRUE,
  fallen.leaves = TRUE,
  roundint = FALSE,
  clip.right.labs = FALSE,
  box.palette = "RdBu",
  border.col = "gray40",
  cex = 0.9,
  main = "Decision Tree Predicting High vs Low CSC Score"
)
