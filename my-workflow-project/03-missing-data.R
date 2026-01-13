# Step 3: Missing Data Evaluation 

# Load Previous Step ------------------------------------------------------
analytic_data_survey <- read_rds("data-interim/02-cleaned.rds")


# Define Scale Items ------------------------------------------------------
# We select the columns relevant for scoring to check them specifically
scale_items <- analytic_data_survey |>
  select(starts_with("aff_com"),
         starts_with("contin_com"),
         starts_with("norm_com"),
         starts_with("job_aff"))

# Missing Data Diagnosis --------------------------------------------------

# 1. Text Report: Items with the most missing data
message("--- Missing Data Report by Item ---")
missing_summary <- scale_items |>
  miss_var_summary() |>  # From naniar
  filter(n_miss > 0) 

print(missing_summary)
write_csv(missing_summary, "output/data-table-missing-by-item.csv")

# 2. Text Report: Participants with too much missing data
# Check if any participant is missing more than 20% of the scale items
message("--- Participants with > 20% Missing Data ---")
high_missing_participants <- analytic_data_survey |>
  rowwise() |>
  mutate(pct_missing = mean(is.na(c_across(c(starts_with("aff_com"),
                                             starts_with("contin_com"),
                                             starts_with("norm_com"),
                                             starts_with("job_aff"))))) * 100) |>
  ungroup() |>
  filter(pct_missing > 20) |>
  select(participant_id, pct_missing)

print(high_missing_participants)
write_csv(high_missing_participants, "output/data-table-missing-by-participant.csv")

# 3. Visual Report
# Generates a map of missingness (Black = Missing, Grey = Present)
# We use 'print()' to ensure the plot renders when running from a script
message("--- Generating Missing Data Map ---")
plot_missing <- vis_miss(scale_items) +
  labs(title = "Missing Data Map (Scale Items Only)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 0, vjust = 0, size = 16),
        axis.text.x.top = element_text(margin = margin(b = 2)),
        axis.text.y = element_text(size = 16),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10))

ggsave("output/figure-missing-data-map.png", plot = plot_missing, width = 20, height = 20, dpi = "print")

# Data not altered so no need to save