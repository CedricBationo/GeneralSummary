#' General Summary 
#'
#' Summarizes survey data for binary, categorical, continuous, or multiple-choice variables.
#' It supports domain estimates using the \pkg{survey} package and allows for confidence intervals or robust quantile summaries.
#'
#' @param data A data frame containing the variables of interest.
#' @param variable A character string or vector of variable names. For multiple choice, supply multiple variables.
#' @param group_vars Optional character vector of variables to group the summary by.
#' @param weights_var Optional name of the variable representing sampling weights.
#' @param strata_var Optional name of the stratification variable for the survey design.
#' @param id_var Optional name of the primary sampling unit (PSU) ID variable. Defaults to row number if not provided.
#' @param variable_type The type of variable to summarize: one of `"binary"`, `"categorical"`, or `"continuous"`.
#' @param multiple_choice Logical; set to \code{TRUE} if summarizing a multiple-choice question stored across multiple binary columns.
#' @param CI Logical; if \code{TRUE}, compute confidence intervals (for binary/categorical/continuous variables).
#' @param coerce_mean Logical; if \code{TRUE}, forces mean calculation for continuous data even if non-normal.
#' @param coerce_median Logical; if \code{TRUE}, forces median/IQR calculation for continuous data regardless of normality.
#'
#' @return A data frame containing the summarized results, including means/proportions and confidence intervals or quantiles as appropriate.
#'
#' @details
#' For binary and multiple-choice variables, weighted proportions and logit-based confidence intervals are computed using \code{svyciprop}.
#' For categorical variables, either Wilson-score CIs (for small groups) or design-based totals and CIs (for large groups) are returned.
#' For continuous variables, the function uses the Shapiro-Wilk test to determine normality and returns either mean + CI or median + IQR.
#'
#' @examples
#' \dontrun{
#' general_summary_prop(
#'   data = survey_data,
#'   variable = "vaccinated",
#'   group_vars = c("region"),
#'   weights_var = "weight",
#'   strata_var = "strata",
#'   id_var = "psu",
#'   variable_type = "binary"
#' )
#' }
#'
#' @export
#' @importFrom survey svydesign svymean svyciprop svytotal svyby weights
#' @importFrom dplyr %>% select all_of mutate summarise group_by group_modify ungroup rowwise across
#' @importFrom tidyr pivot_longer
#' @importFrom stats as.formula confint lm qnorm shapiro.test weighted.mean
#' @importFrom Hmisc wtd.quantile

general_summary <- function(
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
) {
  options(survey.adjust.domain.lonely = TRUE) 
  options(survey.lonely.psu = "adjust")
  
  if (!all(variable %in% names(data))) {
    missing_vars <- variable[!variable %in% names(data)]
    stop(paste("Variable(s)", paste(missing_vars, collapse = ", "), "not found in the data."))
  }
  
  if (multiple_choice == FALSE && length(variable) != 1) {
    stop("Please provide a single variable name when multiple_choice is FALSE.")
  }
  
  if (!is.null(weights_var) && any(is.na(data[[weights_var]]))) {
    warning("NAs detected in weights variable. Consider removing or imputing missing weights.")
  }
  
  
  if (is.null(strata_var)) {
    data$Stratum <- 1
    strata_var <- "Stratum"
  }
  
  if (is.null(id_var)) {
    data$ClusterID <- 1:nrow(data)
    id_var <- "ClusterID"
  }
  
  if (!is.null(weights_var)) {
    design <- svydesign(
      ids = as.formula(paste("~", id_var)),
      strata = as.formula(paste("~", strata_var)),
      weights = as.formula(paste("~", weights_var)),
      data = data
    )
  } else {
    design <- svydesign(ids = ~0, data = data)
  }
  
  ###################################################################
  # Multiple Choice
  ###################################################################
  if (multiple_choice) {
    message("Multiple choice - Calculating proportions and CIs")
    
    data_long <- data %>%
      select(all_of(c(group_vars, variable, weights_var, strata_var, id_var))) %>%
      pivot_longer(
        cols = all_of(variable),
        names_to = "reason",
        values_to = "value"
      ) %>%
      filter(!is.na(value)) %>%
      mutate(value = as.numeric(value))
    
    if (!is.null(weights_var)) {
      design_long <- svydesign(
        ids = as.formula(paste("~", id_var)),
        strata = as.formula(paste("~", strata_var)),
        weights = as.formula(paste("~", weights_var)),
        data = data_long
      )
    } else {
      design_long <- svydesign(ids = ~0, data = data_long)
    }
    
    if (!is.null(group_vars)) {
      grouping_formula <- as.formula(paste("~", paste(c(group_vars, "reason"), collapse = "+")))
      totals <- svyby(
        ~value,
        by = grouping_formula,
        design = design_long,
        FUN = svyciprop,
        vartype = "ci",
        method = "logit",
        na.rm = TRUE
      ) %>%
        mutate(
          mean_value = value * 100,
          conf_low = ci_l * 100,
          conf_high = ci_u * 100
        ) %>%
        select(all_of(group_vars), reason, mean_value, conf_low, conf_high)
    } else {
      totals <- svyby(
        ~value,
        by = ~reason,
        design = design_long,
        FUN = svyciprop,
        vartype = "ci",
        method = "logit",
        na.rm = TRUE
      ) %>%
        mutate(
          mean_value = value * 100,
          conf_low = ci_l * 100,
          conf_high = ci_u * 100
        ) %>%
        select(reason, mean_value, conf_low, conf_high)
    }
    
    return(totals)
  }
  
  ###################################################################
  # Helper function for categorical CIs (large groups)
  ###################################################################
  calc_categorical_ci <- function(design, variable) {
    svytotal_obj <- svytotal(as.formula(paste0("~ `", variable, "`")), design, na.rm = TRUE)
    total_count <- sum(weights(design))
    prop_values <- coef(svytotal_obj) / total_count * 100
    conf_int <- confint(svytotal_obj) / total_count * 100
    
    result <- data.frame(
      reason = names(prop_values),
      mean_value = as.numeric(prop_values),
      conf_low = conf_int[, 1],
      conf_high = conf_int[, 2]
    )
    return(result)
  }
  
  ###################################################################
  # Categorical
  ###################################################################
  if (variable_type == "categorical") {
    message("Categorical - Calculating proportions")
    
    data[[variable]] <- as.factor(data[[variable]])
    
    if (CI) {
      if (!is.null(group_vars)) {
        result <- data %>%
          group_by(across(all_of(group_vars))) %>%
          group_modify(~ {
            .x_filtered <- .x[!is.na(.x[[variable]]), ]
            
            if (nrow(.x_filtered) < 30) {
              # ------------------------------
              # Small group: Weighted proportions + Wilson score CIs
              # ------------------------------
              
              if (!is.null(weights_var)) {
                weighted_counts <- tapply(.x_filtered[[weights_var]], .x_filtered[[variable]], sum, na.rm = TRUE)
                total_weight <- sum(.x_filtered[[weights_var]], na.rm = TRUE)
              } else {
                weighted_counts <- table(.x_filtered[[variable]])
                total_weight <- sum(weighted_counts)
              }
              
              prop_table <- (weighted_counts / total_weight) * 100
              result_df <- data.frame(
                reason = names(weighted_counts),
                mean_value = as.numeric(prop_table)
              )
              
              if (!is.null(weights_var)) {
                n_eff <- total_weight^2 / sum(.x_filtered[[weights_var]]^2, na.rm = TRUE)
              } else {
                n_eff <- total_weight
              }
              z <- qnorm(0.975)  
              
              ci_low <- numeric(length(weighted_counts))
              ci_high <- numeric(length(weighted_counts))
              
              for (i in seq_along(weighted_counts)) {
                p <- weighted_counts[i] / total_weight
                denominator <- 1 + z^2 / n_eff
                center <- (p + z^2 / (2 * n_eff)) / denominator
                margin <- (z / denominator) * sqrt(p * (1 - p) / n_eff + z^2 / (4 * n_eff^2))
                ci_low[i] <- max(center - margin, 0) * 100
                ci_high[i] <- min(center + margin, 1) * 100
              }
              
              result_df$conf_low <- ci_low
              result_df$conf_high <- ci_high
              return(result_df)
              
            } else {
              # ------------------------------
              # Large group: Use survey design
              # ------------------------------
              sub_design <- if (!is.null(weights_var)) {
                svydesign(
                  ids = as.formula(paste("~", id_var)),
                  strata = as.formula(paste("~", strata_var)),
                  weights = as.formula(paste("~", weights_var)),
                  data = .x
                )
              } else {
                svydesign(ids = ~0, data = .x)
              }
              
              calc_categorical_ci(sub_design, variable)
            }
          }) %>%
          ungroup()
        
      } else {
        result <- calc_categorical_ci(design, variable)
      }
    } else {
      if (!is.null(group_vars)) {
        result <- data %>%
          group_by(across(all_of(group_vars))) %>%
          group_modify(~ {
            .x_filtered <- .x[!is.na(.x[[variable]]), ]
            
            if (nrow(.x_filtered) < 30) {
              if (!is.null(weights_var)) {
                weighted_counts <- tapply(.x_filtered[[weights_var]], .x_filtered[[variable]], sum, na.rm = TRUE)
                total_weight <- sum(.x_filtered[[weights_var]], na.rm = TRUE)
              } else {
                weighted_counts <- table(.x_filtered[[variable]])
                total_weight <- sum(weighted_counts)
              }
              prop_table <- (weighted_counts / total_weight) * 100
              data.frame(
                reason = names(weighted_counts),
                mean_value = as.numeric(prop_table)
              )
            } else {
              sub_design <- if (!is.null(weights_var)) {
                svydesign(
                  ids = as.formula(paste("~", id_var)),
                  strata = as.formula(paste("~", strata_var)),
                  weights = as.formula(paste("~", weights_var)),
                  data = .x
                )
              } else {
                svydesign(ids = ~0, data = .x)
              }
              
              svytotal_obj <- svytotal(as.formula(paste0("~ `", variable, "`")), sub_design, na.rm = TRUE)
              total_count <- sum(weights(sub_design))
              prop_values <- coef(svytotal_obj) / total_count * 100
              
              data.frame(
                reason = names(prop_values),
                mean_value = as.numeric(prop_values)
              )
            }
          }) %>%
          ungroup()
      } else {
        svytotal_obj <- svytotal(as.formula(paste0("~ `", variable, "`")), design, na.rm = TRUE)
        total_count <- sum(weights(design))
        prop_values <- coef(svytotal_obj) / total_count * 100
        result <- data.frame(
          reason = names(prop_values),
          mean_value = as.numeric(prop_values)
        )
      }
    }
    
    result$reason <- gsub(paste0("^", variable), "", result$reason)
    result$reason <- gsub("\\.", " ", result$reason)
    result$reason <- trimws(result$reason)
    return(result)
  }
  
  ###################################################################
  # Binary
  ###################################################################
  if (variable_type == "binary") {
    message("Binary - Calculating proportions")
    
    data[[variable]] <- as.numeric(data[[variable]])
    
    if (CI) {
      if (!is.null(group_vars)) {
        by_formula <- as.formula(paste("~", paste(group_vars, collapse = "+")))
        result <- svyby(
          formula = as.formula(paste0("~ `", variable, "`")),
          by = by_formula,
          design = design,
          FUN = svyciprop,
          method = "logit",
          vartype = "ci",
          na.rm = TRUE
        ) %>%
          mutate(
            mean_value = get(setdiff(names(.), c(all.vars(by_formula), "ci_l", "ci_u"))) * 100,
            conf_low = ci_l * 100,
            conf_high = ci_u * 100
          ) %>%
          select(all.vars(by_formula), mean_value, conf_low, conf_high)
      } else {
        prop_result <- svyciprop(as.formula(paste0("~ `", variable, "`")), design, method = "logit", na.rm = TRUE)
        ci <- confint(prop_result)
        result <- data.frame(
          mean_value = coef(prop_result)[1] * 100,
          conf_low = ci[1] * 100,
          conf_high = ci[2] * 100
        )
      }
    } else {
      if (!is.null(group_vars)) {
        by_formula <- as.formula(paste("~", paste(group_vars, collapse = "+")))
        result <- svyby(
          formula = as.formula(paste0("~ `", variable, "`")),
          by = by_formula,
          design = design,
          FUN = svymean,
          na.rm = TRUE
        ) %>%
          mutate(
            mean_value = get(variable) * 100
          ) %>%
          select(all.vars(by_formula), mean_value)
      } else {
        mean_result <- svymean(as.formula(paste0("~ `", variable, "`")), design, na.rm = TRUE)
        result <- data.frame(mean_value = coef(mean_result) * 100)
      }
    }
    return(result)
  }
  
  ###################################################################
  # Continuous
  ###################################################################
  if (variable_type == "continuous") {
    message("Continuous - Calculating summary statistics")
    
    data[[variable]] <- as.numeric(data[[variable]])
    
    shapiro_test <- tryCatch(
      shapiro.test(data[[variable]]),
      error = function(e) {
        warning("Shapiro-Wilk test failed, assuming non-normal distribution.")
        list(p.value = 0.001)  
      }
    )
    
    if ((shapiro_test$p.value > 0.05 | coerce_mean) & !coerce_median) {
      message("Assuming normal distribution - Calculating mean and CI")
      
      if (!is.null(group_vars)) {
        result <- data %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(
            mean_value = weighted.mean(.data[[variable]], 
                                       if (is.null(weights_var)) rep(1, n()), 
                                       na.rm = TRUE),
            conf_low = if (all(is.na(.data[[variable]]))) NA_real_ else confint(lm(.data[[variable]] ~ 1, weights = if (is.null(weights_var)) rep(1, n()) else .data[[weights_var]]))[1],
            conf_high = if (all(is.na(.data[[variable]]))) NA_real_ else confint(lm(.data[[variable]] ~ 1, weights = if (is.null(weights_var)) rep(1, n()) else .data[[weights_var]]))[2]
          ) %>%
          ungroup()
      } else {
        result <- data %>%
          summarise(
            mean_value = weighted.mean(.data[[variable]], 
                                       if (is.null(weights_var)) rep(1, n()) else .data[[weights_var]], 
                                       na.rm = TRUE),
            conf_low = if (all(is.na(.data[[variable]]))) NA_real_ else confint(lm(.data[[variable]] ~ 1, weights = if (is.null(weights_var)) rep(1, n()) else .data[[weights_var]]))[1],
            conf_high = if (all(is.na(.data[[variable]]))) NA_real_ else confint(lm(.data[[variable]] ~ 1, weights = if (is.null(weights_var)) rep(1, n()) else .data[[weights_var]]))[2]
          )
      }
    } else {
      message("Assuming non-normal distribution - Calculating median and quartiles")
      
      if (!is.null(group_vars)) {
        result <- data %>%
          group_by(across(all_of(group_vars))) %>%
          summarise(
            summary_stat = if (all(is.na(.data[[variable]]))) NA_real_ else wtd.quantile(.data[[variable]], weights = .data[[weights_var]], probs = 0.50, na.rm = TRUE),
            q1 = if (all(is.na(.data[[variable]]))) NA_real_ else wtd.quantile(.data[[variable]], weights = .data[[weights_var]], probs = 0.25, na.rm = TRUE),
            q3 = if (all(is.na(.data[[variable]]))) NA_real_ else wtd.quantile(.data[[variable]], weights = .data[[weights_var]], probs = 0.75, na.rm = TRUE)
          ) %>%
          ungroup()
      } else {
        result <- data %>%
          summarise(
            summary_stat = if (all(is.na(.data[[variable]]))) NA_real_ else wtd.quantile(.data[[variable]], weights = .data[[weights_var]], probs = 0.50, na.rm = TRUE),
            q1 = if (all(is.na(.data[[variable]]))) NA_real_ else wtd.quantile(.data[[variable]], weights = .data[[weights_var]], probs = 0.25, na.rm = TRUE),
            q3 = if (all(is.na(.data[[variable]]))) NA_real_ else wtd.quantile(.data[[variable]], weights = .data[[weights_var]], probs = 0.75, na.rm = TRUE)
          )
      }
    }
    return(result)
  }
  
  stop("Invalid variable type. Please specify 'categorical', 'binary', or 'continuous'.")
}
