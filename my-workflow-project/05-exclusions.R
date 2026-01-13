# Step 5: Exclusions

# Rules for excluding participants should be preregistered.
# In this example, we exclude participants who completed the survey in under 2 minutes.


# Load Previous Step ------------------------------------------------------
analytic_data_survey <- read_rds("data-interim/03-scales-created.rds")

# Exclusions --------------------------------------------------------------
# Filter out speeders (Requires 'duration_in_seconds' from Step 1)
initial_n <- nrow(analytic_data_survey)

# Only keep participants with duration >= 120 seconds (2 minutes)
analytic_data_survey <- analytic_data_survey |>
  filter(duration_in_seconds >= 120)

# Print a message telling the student how many were dropped
final_n <- nrow(analytic_data_survey)
message(paste("Dropped", initial_n - final_n, "participants due to speed checks."))

# Final Save --------------------------------------------------------------
write_rds(analytic_data_survey, "data-processed/analytic-data-final.rds")

# Display final structure
glimpse(analytic_data_survey)