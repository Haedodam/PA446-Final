# ==========================================================
# STEP 4: REPRODUCIBLE REPORTING - ANALYSIS
# ==========================================================

# --------------------------------------------------
# 1. Load cleaned dataset (from Step 3)
# --------------------------------------------------
arts_full <- read_csv("arts_full_clean.csv")

# --------------------------------------------------
# 2. Aggregate data to ZIP level
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
# 3. Load Chicago ZCTA shapefile (2020)
# --------------------------------------------------
# 'zctas()' loads ZIP-level geographic boundaries
chi_zcta <- zctas(cb = TRUE, year = 2020) %>%
  mutate(zip = as.character(ZCTA5CE20)) %>%     # Convert to character ZIP code
  filter(zip %in% zip_summary$zip)              # Keep Chicago-only ZIPs

# Merge spatial data with aggregated ZIP summary
chi_map <- chi_zcta %>%
  left_join(zip_summary, by = "zip")

# --------------------------------------------------
# 4. Map: Visualize spatial distribution of CSC scores
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
# 5. Regression Model:
#    Which socioeconomic predictors relate to CSC?
# --------------------------------------------------
model_all <- lm(
  avg_CSC ~ poverty_rate + pct_nonwhite + pct_non_english + pct_noncollege,
  data = zip_summary
)

summary(model_all)   # Print regression output

# --------------------------------------------------
# 6. Visualize Regression Coefficients
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
# 7. Interactive Shiny Dashboard
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
