# Step 4: Scale Creation 

# Load Previous Step ------------------------------------------------------
analytic_data_survey <- read_rds("data-interim/02-cleaned.rds")


# Create Scale Scores -----------------------------------------------------
# Note: We use rowwise() for accurate mean calculations across columns
analytic_data_survey <- analytic_data_survey |> 
  rowwise() |> 
  mutate(
    affective_commitment = mean(c_across(starts_with("aff_com")), na.rm = TRUE),
    continuance_commitment = mean(c_across(starts_with("contin_com")), na.rm = TRUE),
    normative_commitment = mean(c_across(starts_with("norm_com")), na.rm = TRUE),
    job_satisfaction = mean(c_across(starts_with("job_aff")), na.rm = TRUE)
  ) |>
  ungroup() # Always ungroup after rowwise()!

# Clean up Item columns (Optional - keeps dataset clean)
analytic_data_survey <- analytic_data_survey |>
  select(-starts_with("aff_com"),
         -starts_with("contin_com"),
         -starts_with("norm_com"),
         -starts_with("job_aff")) 

# Save Interim File -------------------------------------------------------
write_rds(analytic_data_survey, "data-interim/03-scales-created.rds")