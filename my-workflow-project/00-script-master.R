# Date: 2026-01-15 (Use YYYY-MM-DD format)
# Name: [Your Name]
# Purpose: Master - Runs all data preparation steps in order

# 1. Setup Environment ----------------------------------------------------
library(tidyverse)
library(janitor)
library(skimr)
library(naniar) # Package for missing data visualization

# Clear memory to ensure reproducibility
rm(list = ls()) 

# Create directories if they don't exist (helpful for students)
if(!dir.exists("data-interim")) dir.create("data-interim")
if(!dir.exists("data-processed")) dir.create("data-processed")
if(!dir.exists("output")) dir.create("output")


# 2. Execute Pipeline -----------------------------------------------------

# Step 1: Import and Anonymize
message("--- Running Step 1: Import ---")
source("01-import.R", echo = TRUE)

# Step 2: Cleaning and Recoding
message("--- Running Step 2: Cleaning ---")
source("02-clean-recode.R", echo = TRUE)

# Step 3: Evaluate missing data
message("--- Running Step 3: Evaluate missing data ---")
source("03-missing-data.R", echo = TRUE)

# Step 4: Create scales
message("--- Running Step 4: Create scales ---")
source("04-create-scales.R", echo = TRUE)

# Step 5: Exclusions
message("--- Running Step 5: Excluding participants ---")
source("05-Exclusions.R", echo = TRUE)

# Step 6: Analysis Wraper
message("--- Running Step 6: Analysis and Visualization ---")
source("06-analysis-wrapper.R", echo = TRUE)


message("--- Pipeline Complete! ---")