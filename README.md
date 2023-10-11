# GeneralSummary Version 0.1.0

`GeneralSummary` is an R package that provides a function to generate summary statistics for various types of data. The main function, `general_summary`, can handle multiple choice, continuous, and binary variables, providing means, medians, confidence intervals, and interquartile ranges as appropriate.

## Installation

For the development version from GitHub:

```r
install.packages("devtools")
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
- `group_vars (optional)`: A vector of grouping variables (Region, Health facilities, etc...). Default is `NULL`.
- `weights_var (optional)`: The name of the weights variable. Default is `NULL`.
- `multiple_choice (optional)`: A logical value indicating whether the variable is multiple choice. Default is `FALSE`.
- `variable_type`: The type of the variable (binary, continuous, or multiple choice). Default is `binary`.
## Function Behavior

The `general_summary` function operates differently based on the `variable_type` and `multiple_choice` arguments, adapting its behavior to the nature of the variables being analyzed. Here's a detailed breakdown of how the function handles different types of variables and the specific summary statistics it computes:

### 1. **Multiple Choice Variables**:
   - When `multiple_choice` is set to `TRUE`, the function computes:
     - **Weighted Mean:** The weighted mean of each choice within the multiple choice variable.
     - **Wilson's Interval for 95% CI:** Utilizing the `prop.test` function in R, the function computes Wilson's interval for a 95% Confidence Interval (CI) for each choice within the multiple choice variable.
   - Initially, the data is converted into a long format, followed by grouping based on the designated `group_vars` along with the choices from the multiple choice variable.
   - Post grouping, the weighted mean and Wilson's interval are computed for each group.

### 2. **Continuous Variables**:
   - A normality check is performed using the `Shapiro-Wilk test`.
   - **If the data is found to be normally distributed**:
     - **Weighted Mean:** The weighted mean of the variable is calculated.
     - **95% CI:** A 95% Confidence Interval (CI) is derived using Logit regression, facilitated by the `lm` function in R.
   - **If the data is found to be non-normally distributed**:
     - **Weighted Median:** The weighted median of the variable is computed.
     - **IQR:** The Interquartile Range (IQR) of the variable is calculated.

### 3. **Binary Variables**:
   - For binary variables, the following summary statistics are computed:
     - **Weighted Mean:** The weighted mean of the binary variable is calculated.
     - **Wilson's Interval for 95% CI:** Utilizing the `prop.test` function in R, the function computes Wilson's interval for a 95% Confidence Interval (CI).

## Data Preparation Guidelines

Follow the steps outlined below for each indicator to ensure the data is correctly prepared for analysis:

### **Data Cleaning and Setup**
   Begin by cleaning the specified variables and setting up a dedicated dataframe for analysis.

### Binary Variables:
- Convert the variable into a `boolean` `(True/False)` or a dummy variable `(1/0)` as required.
- Treat `Don't Know`, `NA`, and other such responses as instructed in the `feedback_clarifications.xlsx` file.

### Continuous Variables:
- If dealing with numeric variables, employ the following command to ensure correct datatype conversion:
 ```r
mutate(as.numeric(as.character()))
 ```
* Note: Numeric values may import differently from STATA.
- Address `Don't Know`, `NA`, and `refuse` responses as per the instructions in the `feedback_clarifications.xlsx` file.

###  Multiple Choice Variables:
- Assemble a list of Multiple Choice Questions (MCQ) variables in advance for processing through the function argument.


        
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

## License
GPL-3
