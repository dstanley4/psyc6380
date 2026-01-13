# Step 1: Import and Initial Setup

# Load Data ---------------------------------------------------------------
survey_file <- "data-raw/data-qualtrics.csv"

# Read header row for names
col_names <- names(read_csv(survey_file, n_max = 0, show_col_types = FALSE))

# Read data (skipping Qualtrics header rows 2 and 3)
raw_data <- read_csv(survey_file, 
                     col_names = col_names, 
                     skip = 3, 
                     show_col_types = FALSE,
                     na = c("", "NA","999")) # Treat blank cells, NA, and 999 as missing values
                    

# Select Columns ----------------------------------------------------------
cols_to_remove <- c("StartDate", "EndDate", "Status", "IPAddress", "Progress",
                    "Finished", "RecordedDate", "ResponseId", "RecipientLastName",
                    "RecipientFirstName", "RecipientEmail", "ExternalReference",
                    "LocationLatitude", "LocationLongitude", "DistributionChannel",
                    "UserLanguage")

analytic_data_survey <- raw_data |> 
  select(!any_of(cols_to_remove)) |> 
  clean_names() |>        
  remove_empty("rows") |> 
  remove_empty("cols")

# Create Participant ID ---------------------------------------------------
# Best practice: Create ID immediately so we can track rows if order changes
analytic_data_survey <- analytic_data_survey %>%
  mutate(participant_id = row_number()) %>% 
  relocate(participant_id) # Move to first column

# Save Interim File -------------------------------------------------------
# We use .rds because it preserves R formatting (unlike CSV)
write_rds(analytic_data_survey, "data-interim/01-imported.rds")
