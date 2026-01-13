# Step 2: Cleaning, Factors, and Recoding

# Load Previous Step ------------------------------------------------------
analytic_data_survey <- read_rds("data-interim/01-imported.rds")

# Factor Handling ---------------------------------------------------------
# Convert sex to factor and fix levels
analytic_data_survey <- analytic_data_survey |>
  mutate(sex = as_factor(sex)) |> 
  mutate(sex = fct_relevel(sex, "female", "intersex", "male"))

# Safety Check: Warn if unexpected sex levels appear (i.e., not in the expected sex vector)
expected_sex <- c("female", "intersex", "male")
unexpected <- setdiff(levels(analytic_data_survey$sex), expected_sex)
if (length(unexpected) > 0) {
  warning("Check Data! Unexpected sex levels found: ", paste(unexpected, collapse = ", "))
}

# Likert Recoding (Text to Numbers) ---------------------------------------

# Define Mappings
likert7_recode <- c(
  "Strongly Disagree" = 1,
  "Moderately Disagree" = 2,
  "Slightly Disagree" = 3,
  "Neither Agree nor Disagree" = 4,
  "Slightly Agree" = 5,
  "Moderately Agree" = 6,
  "Strongly Agree" = 7
)

likert5_recode <- c(
  "Strongly Disagree" = 1,
  "Disagree" = 2,
  "Neutral" = 3, 
  "Agree" = 4,
  "Strongly Agree" = 5
)

# Apply Mappings
analytic_data_survey <- analytic_data_survey |>
  # Recode 7-point scales
  mutate(across(
    .cols = contains("likert7"), 
    .fns = ~ likert7_recode[.x]
  )) |>
  # Recode 5-point scales
  mutate(across(
    .cols = contains("likert5"), 
    .fns = ~ likert5_recode[.x]
  ))

# Reverse Keying ----------------------------------------------------------
analytic_data_survey <- analytic_data_survey |>
  mutate(across(
    .cols = ends_with("_likert7rev"),
    .fns = ~ (7 + 1) - .x
  )) |>
  # Rename columns to remove the 'rev' suffix now that they are fixed
  rename_with(
    .fn = ~ str_replace(.x, "_likert7rev", "_likert7"),
    .cols = ends_with("_likert7rev")
  )

# Save Interim File -------------------------------------------------------
write_rds(analytic_data_survey, "data-interim/02-cleaned.rds")