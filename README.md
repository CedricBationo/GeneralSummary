# GeneralSummary

GeneralSummary is an R package that provides a function to generate summary statistics for various types of data. The main function, `general_summary`, can handle multiple choice, continuous, and binary variables, providing means, medians, confidence intervals, and interquartile ranges as appropriate.

## Installation

For the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("your_username/GeneralSummary")
library(GeneralSummary)

# Example usage:
# Assuming df is your data frame and vars is a vector of variable names
summary_results <- general_summary(df, vars)
