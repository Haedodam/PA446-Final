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

# --------------------------------------------------
# STEP 5 COMPLETE:
# The decision tree identifies which socioeconomic factors
# most strongly split Chicago ZIP codes into high vs low CSC.
# --------------------------------------------------
