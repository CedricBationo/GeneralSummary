# GeneralSummary Version 0.1.0

`GeneralSummary` is an R package that provides a function to generate summary statistics for various types of data. The main function, `general_summary`, can handle multiple choice, continuous, and binary variables, providing means, medians, confidence intervals, and interquartile ranges as appropriate.

## Installation

For the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("CedricBationo/GeneralSummary")
library(GeneralSummary)
```
## Function Signature

```r
general_summary <- function(data, variable, group_vars = NULL, weights_var = NULL, multiple_choice = FALSE, variable_type = "binary")
```

## Arguments
- `data`: The data frame containing the data.
- `variable`: The name of the variable for which the summary statistics will be calculated.
- `group_vars (optional)`: A vector of grouping variables. Default is `NULL`.
- `weights_var (optional)`: The name of the weights variable. Default is `NULL`.
- `multiple_choice (optional)`: A logical value indicating whether the variable is multiple choice. Default is `FALSE`.
- `variable_type (optional)`: The type of the variable (binary, continuous, or other). Default is `binary`.

## Function Behavior

The behavior of the `general_summary` function differs based on the `variable_type` and `multiple_choice` arguments:

 1. Multiple Choice Variables:
 - If `multiple_choice` is `TRUE`, the function calculates the mean and confidence interval for each choice in the multiple choice variable.
 - The data is first reshaped to long format, then grouped by the specified `group_vars` and the choices in the multiple choice variable.
 - The mean and confidence interval are then calculated for each group.
 2. Continuous Variables  
 - For continuous variables, the function first tests for normality using the `Shapiro-Wilk test`.
 - If the data is normally distributed, the mean and confidence interval are calculated.
 - If the data is not normally distributed, the median and interquartile range are calculated.
 3. Binary Variables:
 - For binary variables, the function calculates the mean and confidence interval.

## Data Preparation Guidelines

Follow the steps outlined below for each indicator to ensure the data is correctly prepared for analysis:

### **Data Cleaning and Setup**
   Begin by cleaning the specified variables and setting up a dedicated dataframe for analysis.

    ### a. Binary Variables:
        i. Convert the variable into a boolean (True/False) or a dummy variable (1/0) as required.
        ii. Treat 'Don't Know', 'NA', and other such responses as instructed in the `feedback_clarifications.xlsx` file.

    ### b. Continuous Variables:
        i. If dealing with numeric variables, employ the following command to ensure correct datatype conversion:
           ```R
           mutate(as.numeric(as.character()))
           ```
           > Note: Numeric values may import differently from STATA.
        ii. Address 'NA', 'Don't Know', and 'refuse' responses as per the instructions in the `feedback_clarifications.xlsx` file.

    ### c. Multiple Choice Variables:
        i. Assemble a list of Multiple Choice Questions (MCQ) variables in advance for processing through the function argument.


        
## Usage
### Binary Variable Example
```r
# Assuming df is your data frame
summary_results <- general_summary(df, 'binary_variable', c('group_var1', 'group_var2'), 'weights_var')
```
### Continuous Variable Example
```r
# Assuming df is your data frame
summary_results <- general_summary(df, 'continuous_variable', c('group_var1', 'group_var2'), 'weights_var', FALSE, 'continuous')

```
### Multiple Choice Variable Example
```r
# Assuming df is your data frame
summary_results <- general_summary(df, 'multiple_choice_variable', c('group_var1', 'group_var2'), 'weights_var', TRUE)


```

##  Authors

   - Cédric Bationo (cbationo@worldbank.org)
   - Khushboo Gupta (kgupta6@worldbank.org)
   - Harish Saï (hramsai@worldbank.org)

## License
GPL-3
