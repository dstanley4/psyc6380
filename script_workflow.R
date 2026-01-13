# Date: YYYY-MM-DD
# Name: your name here
# Example: Single occasion survey (workflow.qmd)

# Load packages
library(tidyverse)
library(janitor)
library(skimr)

# Load data ----
my_missing_value_codes <- c("-999", "", "NA")

raw_data_survey <- read_csv(file = "data-raw/data_item_scoring.csv",
                            na = my_missing_value_codes)

analytic_data_survey <- raw_data_survey

# Initial cleaning ----
analytic_data_survey <- analytic_data_survey |>
  remove_empty("rows") |>
  remove_empty("cols") |>
  clean_names()

glimpse(analytic_data_survey)

# Create participant ID ----
# And remove old id column - in case this is something you need to do
analytic_data_survey <- analytic_data_survey |>
  mutate(participant_id = row_number()) |>
  relocate(participant_id) |>
  select(-id)
  
# Create factors ----
analytic_data_survey <- analytic_data_survey |>
  mutate(across(.cols = c(sex, eye_color),
                .fns = as_factor))

glimpse(analytic_data_survey)

# Factor screening ----
analytic_data_survey |>
  select(where(is.factor)) |>
  summary()

# Reorder sex factor levels
analytic_data_survey <- analytic_data_survey |>
  mutate(sex = fct_relevel(sex,
                           "female",
                           "intersex",
                           "male"))

# Safety check: Warn if unexpected sex levels appear
expected_sex <- c("female", "intersex", "male")
unexpected <- setdiff(levels(analytic_data_survey$sex), expected_sex)
if (length(unexpected) > 0) {
  warning("Check data! Unexpected sex levels found: ",
          paste(unexpected, collapse = ", "))
}

# Reorder eye_color by frequency
analytic_data_survey <- analytic_data_survey |>
  mutate(eye_color = fct_infreq(eye_color))

analytic_data_survey |>
  select(where(is.factor)) |>
  summary()

# Numeric screening ----
analytic_data_survey |>
  select(age) |>
  skim()

analytic_data_survey |>
  select(starts_with("esteem")) |>
  skim()

analytic_data_survey |>
  select(starts_with("jobsat")) |>
  skim()

# Scale scores ----

## Reverse-key items ----
analytic_data_survey |>
  select(ends_with("_likert5rev")) |>
  glimpse()

analytic_data_survey <- analytic_data_survey |>
  mutate(6 - across(.cols = ends_with("_likert5rev"))) |>
  rename_with(.fn = str_replace,
              .cols = ends_with("_likert5rev"),
              pattern = "_likert5rev",
              replacement = "_likert5")

glimpse(analytic_data_survey)

## Creating scores ----
# Backup item-level data for reliability analysis
analytic_data_survey_items <- analytic_data_survey

# Check items before creating scales
analytic_data_survey |>
  select(starts_with("esteem")) |>
  glimpse()

analytic_data_survey |>
  select(starts_with("jobsat")) |>
  glimpse()

# Calculate scale scores
analytic_data_survey <- analytic_data_survey |>
  rowwise() |>
  mutate(self_esteem = mean(c_across(starts_with("esteem")),
                            na.rm = TRUE)) |>
  mutate(job_sat = mean(c_across(starts_with("jobsat")),
                        na.rm = TRUE)) |>
  ungroup() |>
  select(-starts_with("esteem")) |>
  select(-starts_with("jobsat"))

glimpse(analytic_data_survey)

# Save processed data ----
write_rds(analytic_data_survey, "data-processed/analytic_data_survey.rds")
