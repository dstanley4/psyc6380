# Date: YYYY-MM-DD
# Name: your name here
# Example: Qualtrics data processing
# Load packages
library(tidyverse)
library(janitor)
library(skimr)

# read just the header row to get column names
survey_file <- "data_qualtrics.csv"
col_names <- names(read_csv(survey_file,
                            n_max = 0,
                            show_col_types = FALSE))

# skip the first 3 rows, read data using names from above
raw_data <- read_csv(survey_file,
                     col_names = col_names,
                     skip = 3,
                     show_col_types = FALSE,
                     na = c("", "NA", "999"))

# Qualtrics columns to remove (any_of() ignores names not present)
cols_to_remove <- c("StartDate",
                    "EndDate",
                    "Status",
                    "IPAddress",
                    "Progress",
                    "Finished",
                    "RecordedDate",
                    "ResponseId",
                    "RecipientLastName",
                    "RecipientFirstName",
                    "RecipientEmail",
                    "ExternalReference",
                    "LocationLatitude",
                    "LocationLongitude",
                    "DistributionChannel",
                    "UserLanguage")

# remove the unwanted Qualtrics columns
raw_data <- raw_data |>
  select(!any_of(cols_to_remove))

analytic_data_survey <- raw_data

# Initial cleaning
## Convert column names to tidyverse style guide
## Remove empty rows and columns
analytic_data_survey <- analytic_data_survey %>%
  remove_empty("rows") %>%
  remove_empty("cols") %>%
  clean_names()

glimpse(analytic_data_survey)

# Convert variables to factors as needed
## Convert sex to factor
analytic_data_survey <- analytic_data_survey %>%
  mutate(sex = as_factor(sex))


## Create participant_id using row_number() and move to first column
analytic_data_survey <- analytic_data_survey |>
  mutate(participant_id = row_number()) |>
  relocate(participant_id)

# Screen factors
## screen
analytic_data_survey %>%
  select(sex) %>%
  summary()

# Check existing levels (a different way than lines above)
levels(analytic_data_survey$sex)

## change to desired order
analytic_data_survey <- analytic_data_survey %>%
  mutate(sex = fct_relevel(sex,
                           "female",
                           "intersex",
                           "male"))

# Check for unexpected levels
expected_sex <- c("female", "intersex", "male")
unexpected <- setdiff(levels(analytic_data_survey$sex), expected_sex)
if (length(unexpected) > 0) {
  warning("Unexpected sex levels found: ", paste(unexpected, collapse = ", "))
}


# Screen numeric variables
analytic_data_survey %>%
  select(year_of_birth) %>%
  skim()

# Convert Commitment items to numeric  values
## Check levels for a likert7 item
analytic_data_survey %>%
  pull(aff_com1_likert7) %>%
  unique()

## Define word-to-number mapping 7-point scale
likert7_recode <- c(
  "Strongly Disagree" = 1,
  "Moderately Disagree" = 2,
  "Slightly Disagree" = 3,
  "Neither Agree nor Disagree" = 4,
  "Slightly Agree" = 5,
  "Moderately Agree" = 6,
  "Strongly Agree" = 7
)

## Convert text responses to numeric values
analytic_data_survey <- analytic_data_survey %>%
  mutate(across(
    .cols = contains("likert7"),
    .fns = ~ likert7_recode[.x]
  ))



# Convert Job Satisfaction items to numeric  values
## Check levels for a likert5 item
analytic_data_survey %>%
  pull(job_aff1_likert5) %>%
  unique()

## Define word-to-number mapping 5-point scale
likert5_recode <- c(
  "Strongly Disagree" = 1,
  "Disagree" = 2,
  "Neutral" = 3,
  "Agree" = 4,
  "Strongly Agree" = 5
)

## Convert text responses to numeric values
analytic_data_survey <- analytic_data_survey %>%
  mutate(across(
    .cols = contains("likert5"),
    .fns = ~ likert5_recode[.x]
  ))


# Reverse key items
## Reverse key likert7 items
analytic_data_survey <- analytic_data_survey %>%
  mutate(across(
    .cols = ends_with("_likert7rev"),
    .fns = ~ (7 + 1) - .x
  )) %>%
  rename_with(
    .fn = ~ str_replace(.x, "_likert7rev", "_likert7"),
    .cols = ends_with("_likert7rev")
  )


## No likert5 items are reverse-keyed but if they were
## You would adapt the code above replacing 8 (one higher than 7) to 6 (one higher than 5)


# Create scale scores
## mutate commands create scale scores
## select commands with "-" remove items after scale creation
analytic_data_survey <- analytic_data_survey %>%
  rowwise() %>%
  mutate(affective_commitment = mean(c_across(starts_with("aff_com")),
                                     na.rm = TRUE)) %>%
  mutate(continuance_commitment = mean(c_across(starts_with("contin_com")),
                                       na.rm = TRUE)) %>%
  mutate(normative_commitment = mean(c_across(starts_with("norm_com")),
                                     na.rm = TRUE)) %>%
  mutate(job_satisfaction = mean(c_across(starts_with("job_aff")),
                                 na.rm = TRUE)) %>%
  ungroup() %>%
  select(-starts_with("aff_com")) %>%
  select(-starts_with("contin_com")) %>%
  select(-starts_with("norm_com")) %>%
  select(-starts_with("job_aff"))


glimpse(analytic_data_survey)

# Exclusions - remove fast responders
initial_n <- nrow(analytic_data_survey)

analytic_data_survey <- analytic_data_survey |>
  filter(duration_in_seconds >= 120)

final_n <- nrow(analytic_data_survey)
message(paste("Dropped", initial_n - final_n, "participants due to speed checks."))

glimpse(analytic_data_survey)

# Save processed data
write_rds(analytic_data_survey, "data-processed/analytic-data-final.rds")
