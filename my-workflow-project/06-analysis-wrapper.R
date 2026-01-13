# Step 6: Analysis Wraper

# This wrapper runs the analysis script and captures its output to a text file
# The text file is saved in the output directory
# The analysis script is assumed to be "07-analysis.R"

capture.output(source("07-analysis.R", echo =  TRUE),
                      file = "output/analysis-output.txt")



