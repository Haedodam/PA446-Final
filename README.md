# PA446-Final

**1. Project Overview**
This project analyzes how neighborhood-level socioeconomic factors (poverty rate, racial composition, linguistic diversity, and educational attainment) are associated with Creative Schools Certification (CSC) outcomes across Chicago ZIP codes.
The analysis includes data acquisition, cleaning, spatial mapping, regression modeling, and a simple machine learning decision tree.

**2. Datasets Used**
1) Data/arts_full.csv
Source: ArtLook (Ingenuity + Chicago Public Schools)
Description: School-level arts education data including Creative Schools Certification (CSC) outcomes and arts access indicators.
2) Data/school-results-11-28-25.csv
Source: Merged dataset created for this project
Description: Contains ZIP code-level socioeconomic indicators (ACS 5-year estimates) matched with aggregated CSC scores.

**3. How to Replicate the Analysis**
All scripts are located in the Scripts/ directory and are numbered in order of execution.
1) Load required packages
source("Scripts/Step0_RequiredPackages.R")
2) Acquire or load data
source("Scripts/Step2_DataAcquisition.R")
3) Clean and prepare data
source("Scripts/Step3_DataWrangling_and_QualityChecks.R")
4) Run descriptive and spatial analysis
source("Scripts/Step4_ReproducibleReporting_Analysis.R")
5) Run regression and machine learning models
source("Scripts/Step5_AdvancedAnalysis_MLR.R")

**4. Reports**
1) Quarto Report (Reports/Quarto Report.html) - full reproducible analysis
2) Reflection Memo (Reports/Reflection Memo.pdf) - policy memo summarizing findings
