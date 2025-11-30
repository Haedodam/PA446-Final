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
schools <- readr::read_csv("school-results-11-28-25.csv")

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
      TRUE
    )
  ) %>%
  select(CSC_score, zip)

# --------------------------------------------------
# 8. Merge ACS socioeconomic data with school arts data
# --------------------------------------------------
arts_full <- arts_chicago %>%
  left_join(acs_chicago_clean, by = "zip")

# Step 2 complete: arts_full is the merged dataset