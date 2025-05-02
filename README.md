# generalSummary Version 1.2.0

`generalSummary` is an R package that provides a function to generate summary statistics for various types of data, especially in survey or weighted datasets. The main function, `general_summary`, can handle multiple choice, continuous, categorical, and binary variables, providing means, medians, confidence intervals, or interquartile ranges as appropriate.

## Installation

For the development version from GitHub:

```r
install.packages("devtools")
devtools::install_github("CedricBationo/GeneralSummary")
library(generalSummary)
```

## Function Signature

```r
general_summary<- function(
  data,
  variable,
  group_vars = NULL,
  weights_var = NULL,
  strata_var = NULL,
  id_var = NULL,
  variable_type = "binary",
  multiple_choice = FALSE,
  CI = TRUE,
  coerce_mean = FALSE,
  coerce_median = FALSE
)
```

## Arguments

- `data`: The data frame containing the data.
- `variable`: The name of the variable (or vector of variable names for multiple choice) for which the summary statistics will be calculated.
- `group_vars (optional)`: A vector of grouping variables (Region, Health facilities, etc...). Default is `NULL`.
- `weights_var (optional)`: The name of the weights variable. Default is `NULL`.
- `strata_var (optional)`: The name of the stratification variable for the survey design. Default is `NULL`.
- `id_var (optional)`: The name of the cluster (PSU) ID variable. Default is `NULL`.
- `variable_type`: The type of the variable (binary, categorical, or continuous). Default is `binary`.
- `multiple_choice`: A logical value indicating whether the variable is multiple choice (i.e., multiple binary columns). Default is `FALSE`.
- `CI`: A logical indicating whether confidence intervals (CI) or IQR should be calculated. Default is `TRUE`.
- `coerce_mean`: A logical indicating whether the mean should be calculated (even if data are not normally distributed). Overrides the Shapiro-Wilk test check. Default is `FALSE`.
- `coerce_median`: A logical indicating whether the median should be calculated even if data are normally distributed. Default is `FALSE`.

## Function Behavior

The `general_summary` function adapts its behavior based on the type of variable, data structure, and optional survey design. Here’s a breakdown of how it handles different scenarios:

### 1. **Multiple Choice Variables**
- If `multiple_choice = TRUE`, the function:
  - Converts wide-format binary columns to long format.
  - Groups by the selected grouping variables and choice name.
  - Computes:
    - **Weighted proportion** for each choice.
    - **95% Confidence Interval** using `svyciprop()` from the `survey` package.

### 2. **Categorical Variables**
- If `variable_type = "categorical"`:
  - For groups with <30 observations, it uses **Wilson score CIs**.
  - For large groups, it uses `svytotal()` to estimate:
    - **Proportion of each category**
    - **95% Confidence Interval**

### 3. **Binary Variables**
- If `variable_type = "binary"`:
  - Computes:
    - **Weighted mean**
    - **95% Confidence Interval** using the logit method from `svyciprop()`
  - Supports disaggregation via `group_vars`.

### 4. **Continuous Variables**
- If `variable_type = "continuous"`:
  - Runs a **Shapiro-Wilk test** to assess normality.
  - If normal (or if `coerce_mean = TRUE`):
    - Returns **weighted mean** and **95% CI** (via linear model).
  - If not normal (or `coerce_median = TRUE`):
    - Returns **weighted median**, **Q1**, and **Q3**.

## Data Preparation Guidelines

Follow the steps outlined below for each indicator to ensure the data is correctly prepared for analysis:

### **Binary Variables**
- Recode responses as `1` (yes) and `0` (no).
- Treat `Don't Know`, `NA`, and similar responses according to survey protocol (e.g., `feedback_clarifications.xlsx`).

### **Continuous Variables**
- Ensure numeric format, e.g.:
 ```r
 mutate(var = as.numeric(as.character(var)))
 ```
- Handle non-numeric or special codes (`999`, `"Don't know"`, etc.) as per protocol.

### **Multiple Choice Variables**
- Use a vector of variable names corresponding to each choice (e.g., `c("reason_wait", "reason_cost", "reason_distance")`).

## Usage

### Binary Variable Example
```r
summary_results <- general_summary(
  data = df,
  variable = "binary_variable",
  group_vars = c("region", "facility_type"),
  weights_var = "weights",
  strata_var = "strata",
  id_var = "cluster",
  variable_type = "binary"
)
```

### Continuous Variable Example
```r
summary_results <- general_summary(
  data = df,
  variable = "wait_time",
  group_vars = c("region"),
  weights_var = "weights",
  variable_type = "continuous"
)
```

### Multiple Choice Variable Example
```r
summary_results <- general_summary(
  data = df,
  variable = c("reason1", "reason2", "reason3"),
  group_vars = c("region"),
  weights_var = "weights",
  strata_var = "strata",
  id_var = "cluster",
  multiple_choice = TRUE,
  variable_type = "binary"
)
```

## License
MIT License