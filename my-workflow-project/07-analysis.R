# Step 7: Analysis

# Rules for excluding participants should be preregistered.
# In this example, we exclude participants who completed the survey in under 2 minutes.


# Clear memory to ensure reproducibility
rm(list = ls()) 

# Set random seed for reproducibility (e.g., geom_jitter)
set.seed(12345)

# 1. Setup Environment for Analyses ----------------------------------------------------
library(tidyverse)
library(GGally)
library(skimr)
library(apaTables)

# Load Previous Step ------------------------------------------------------
analytic_data_survey <- read_rds("data-processed/analytic-data-final.rds")

# Analysis ----------------------------------------------------------------


skim(analytic_data_survey)



# Example Analysis: Descriptive Statistics

data_plot <- analytic_data_survey |>
  select(contains("commitment")) 
  

apa.cor.table(data_plot,
                  filename = "output/table-correlation-descriptive-statistics.doc",
                  table.number = 1)


font_size <- 10
cor_plot <- ggpairs(data_plot,
                    upper = "blank",
                    lower = list(continuous = wrap("points", alpha = 0.3)),
                    diag = list(continuous = wrap("barDiag",
                                                  bins = 15,
                                                  color = "black",
                                                  fill = "black",
                                                  alpha = 1))) +
  theme_linedraw(font_size) +
  theme(panel.spacing = unit(.8, "lines"))

ggsave("output/figure-pairs-plot.png",
        plot = cor_plot,
        width = 7,
        height = 7,
        units = "in",
        dpi = 300)

print(cor_plot)