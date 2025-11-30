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

# --------------------------------------------------
# 6. Save cleaned dataset
# --------------------------------------------------
write_csv(arts_full, "arts_full_clean.csv")

# STEP 3 complete: data is clean and ready for ZIP-level aggregation.