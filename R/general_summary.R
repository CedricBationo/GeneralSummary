#' General Summary Function
#'
#' This function provides a summary of a given variable within a data frame, optionally grouped by one or more other variables.
#' It supports binary, continuous, and multiple choice variables.
#'
#' @param data A data frame containing the data.
#' @param variable The name of the variable to summarize.
#' @param group_vars An optional vector of variable names by which to group the summary.
#' @param weights_var An optional variable name containing weights for weighted summaries.
#' @param multiple_choice A logical indicating whether the variable is a multiple choice variable. Default is FALSE.
#' @param variable_type The type of the variable: "binary", "continuous", "categorical" or other. Default is "binary".
#' @param CI  A logical indicating whether CI/ IQR need to be calculated or not. Default is TRUE. 
#' 
#' @return A data frame containing the summary.
#'
#' @examples
#' \dontrun{
#' general_summary(data = my_data, variable = "my_var", group_vars = c("group1", "group2"))
#' }
#'
#' @export
#'@importFrom tidyr pivot_longer
#' @importFrom dplyr %>% select all_of group_by across summarise rowwise mutate group_modify syms
#' @importFrom tidyr pivot_longer
#' @importFrom stats IQR as.formula confint lm prop.test shapiro.test weighted.mean
#' @importFrom broom tidy

general_summary <- function(data, variable, group_vars = NULL, weights_var = NULL, multiple_choice = FALSE, variable_type = "binary", CI = TRUE) {

  
if(CI) {
    if (multiple_choice) {
        data_long <- data %>%
            select(all_of(group_vars), all_of(variable), all_of(weights_var)) %>%
            pivot_longer(
                cols = all_of(variable),
                names_to = "reason",
                values_to = "value"
            )

        group_columns <- "reason"

        if (!is.null(group_vars)) {
            group_columns <- c(group_vars, "reason")
        }

        message("Multiple choice - calculating proportions and CI")

        data_grouped <- data_long %>%
            group_by(across(all_of(group_columns))) %>%
            summarise(
                mean_value = weighted.mean(value, !!sym(weights_var), na.rm = TRUE) * 100,
                n = sum(!!sym(weights_var), na.rm = TRUE),
                .groups = "drop"
            ) %>%
            rowwise() %>%
            mutate(
                CI_result = list(prop.test((mean_value * n / 100), n, correct = FALSE)),
                conf_low = tidy(CI_result)[['conf.low']] * 100,
                conf_high = tidy(CI_result)[['conf.high']] * 100
            ) %>%
            select(-CI_result)

        
        
    } else if (variable_type == "continuous") {
        shapiro_test <- shapiro.test(data[[variable]])

        if (shapiro_test$p.value > 0.05) {
            message("Continuous - Normally Distributed - Calculating mean and CI")

            calc_lm_ci <- function(df, weights_expr) {
                linear_model <- lm(as.formula(paste(variable, "~ 1")), data = df, weights = weights_expr)
                ci_result <- confint(linear_model, level = 0.95)
                data.frame(
                    summary_stat = weighted.mean(df[[variable]], weights_expr, na.rm = TRUE),
                    conf_low = ci_result[1, 1],
                    conf_high = ci_result[1, 2]
                )
            }

            if (!is.null(group_vars)) {
                data_grouped <- data %>%
                    group_by(!!!syms(group_vars)) %>%
                    group_modify(~ calc_lm_ci(.x, if(is.null(weights_var)) rep(1, nrow(.x)) else .x[[weights_var]]))
            } else {
                data_grouped <- calc_lm_ci(data, if(is.null(weights_var)) rep(1, nrow(data)) else data[[weights_var]])

            } }

        else if (shapiro_test$p.value < 0.05)  {
            message("Continuous - non normally distributed - calculating median and quartiles")
            if (!is.null(group_vars)) {
                data_grouped <- data %>%
                    group_by(!!!syms(group_vars)) %>%
                    summarise(
                        summary_stat = weighted.median(!!sym(variable), !!sym(weights_var)),
                        q2 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.50, na.rm = TRUE),
                        q1 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.25, na.rm = TRUE),
                        q3 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.75, na.rm = TRUE)
                        
                    )

            } else {

                data_grouped <- data %>%
                    summarise(
                        summary_stat = weighted.median(!!sym(variable), !!sym(weights_var)),
                        q2 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.50, na.rm = TRUE),
                        q1 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.25, na.rm = TRUE),
                        q3 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.75, na.rm = TRUE)
                    )
            }}}

    else if (variable_type == "binary") {
      
        message("Binary - calculating mean and CI")
        if (!is.null(group_vars)) {
            data_grouped <- data %>%
                group_by(!!!syms(group_vars)) %>%
                summarise(
                    mean_value = weighted.mean(!!sym(variable), !!sym(weights_var), na.rm = TRUE) * 100,
                    n = sum(!!sym(weights_var), na.rm = TRUE),
                    .groups = "drop"
                )
        } else {
            data_grouped <- data %>%
                summarise(
                    mean_value = weighted.mean(!!sym(variable), !!sym(weights_var), na.rm = TRUE) * 100,
                    n = sum(!!sym(weights_var), na.rm = TRUE)
                )
        }
        # Confidence Interval Calculation
        data_grouped <- data_grouped %>%
            rowwise() %>%
            mutate(
                CI_result = list(prop.test((mean_value * n / 100), n, correct = FALSE)),
                conf_low = round(tidy(CI_result)[['conf.low']] * 100, 2),
                conf_high = round(tidy(CI_result)[['conf.high']] * 100, 2)
            ) %>%
            select(-CI_result)
    }
  
  else if (variable_type == "categorical") {
    message("Categorical - calculating proportion and CI")
    
    group_columns <- variable
    
    if (!is.null(group_vars)) {
      group_columns <- c(group_vars, variable)
    }
    
    
    if (!is.null(group_vars)) {
      data_grouped <- data %>%
        group_by(!!!syms(group_vars)) %>%
        mutate(N = sum(!!sym(weights_var), na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(across(all_of(group_columns))) %>%
        summarise(
          n = sum(!!sym(weights_var), na.rm = TRUE),
          mean_value = (n/N)*100,
          .groups = "drop") %>%
        unique() 
      
    } else {
      data_grouped <- data %>%
        mutate(N = sum(!!sym(weights_var), na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(across(all_of(group_columns))) %>%
        summarise(
          n = sum(!!sym(weights_var), na.rm = TRUE),
          mean_value = (n/N)*100,
          .groups = "drop") %>%
        unique() 
    }
    # Confidence Interval Calculation
    data_grouped <- data_grouped %>%
      rowwise() %>%
      mutate(
        CI_result = list(prop.test((mean_value * n / 100), n, correct = FALSE)),
        conf_low = round(tidy(CI_result)[['conf.low']] * 100, 2),
        conf_high = round(tidy(CI_result)[['conf.high']] * 100, 2)
      ) %>%
      select(-CI_result)
  }
  
  
}
  
  
  else {
    message("No confidence intervals")
    
    
    if (multiple_choice) {
      data_long <- data %>%
        select(all_of(group_vars), all_of(variable), all_of(weights_var)) %>%
        pivot_longer(
          cols = all_of(variable),
          names_to = "reason",
          values_to = "value"
        )
      
      group_columns <- "reason"
      
      if (!is.null(group_vars)) {
        group_columns <- c(group_vars, "reason")
      }
      
      message("Multiple choice - calculating Mean")
      
      data_grouped <- data_long %>%
        group_by(across(all_of(group_columns))) %>%
        summarise(
          mean_value = weighted.mean(value, !!sym(weights_var), na.rm = TRUE) * 100,
          .groups = "drop"
        ) 
      
    } else if (variable_type == "continuous") {
      shapiro_test <- shapiro.test(data[[variable]])
      
      if (shapiro_test$p.value > 0.05) {
        message("Continuous - Normally Distributed - Calculating mean")
        
        if (!is.null(group_vars)) {
          data_grouped <- data %>%
            group_by(!!!syms(group_vars)) %>%
            summary_stat = weighted.mean(df[[variable]], weights_var, na.rm = TRUE)
        } else {
          summary_stat = weighted.mean(df[[variable]], weights_var, na.rm = TRUE)
          
        } }
      
      else if (shapiro_test$p.value < 0.05)  {
        message("Continuous - non normally distributed - calculating median and quartiles")
        if (!is.null(group_vars)) {
          data_grouped <- data %>%
            group_by(!!!syms(group_vars)) %>%
            summarise(
              summary_stat = weighted.median(!!sym(variable), !!sym(weights_var)),
              q2 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.50, na.rm = TRUE),
              q1 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.25, na.rm = TRUE),
              q3 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.75, na.rm = TRUE)
            )
          
        } else {
          
          data_grouped <- data %>%
            summarise(
              summary_stat = weighted.median(!!sym(variable), !!sym(weights_var)),
              q2 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.50, na.rm = TRUE),
              q1 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.25, na.rm = TRUE),
              q3 = wtd.quantile(!!sym(variable), weights = !!sym(weights_var), probs = 0.75, na.rm = TRUE)
              
            )
        }}}
    
    else if (variable_type == "binary") {
      message("Binary - calculating mean")
      if (!is.null(group_vars)) {
        data_grouped <- data %>%
          group_by(!!!syms(group_vars)) %>%
          summarise(
            mean_value = weighted.mean(!!sym(variable), !!sym(weights_var), na.rm = TRUE) * 100,
            .groups = "drop"
          )
      } else {
        data_grouped <- data %>%
          summarise(
            mean_value = weighted.mean(!!sym(variable), !!sym(weights_var), na.rm = TRUE) * 100
          )
      }
    }
    
    else if (variable_type == "categorical") {
      message("Categorical - calculating proportions")
      
      group_columns <- variable
      
      if (!is.null(group_vars)) {
        group_columns <- c(group_vars, variable)
      }
      
      
      if (!is.null(group_vars)) {
        data_grouped <- data %>%
          group_by(!!!syms(group_vars)) %>%
          mutate(N = sum(!!sym(weights_var), na.rm = TRUE)) %>%
          ungroup() %>%
          group_by(across(all_of(group_columns))) %>%
          summarise(
            n = sum(!!sym(weights_var), na.rm = TRUE),
            mean_value = (n/N)*100,
            .groups = "drop") %>%
          unique() 
        
      } else {
        data_grouped <- data %>%
          mutate(N = sum(!!sym(weights_var), na.rm = TRUE)) %>%
          ungroup() %>%
          group_by(across(all_of(group_columns))) %>%
          summarise(
            n = sum(!!sym(weights_var), na.rm = TRUE),
            mean_value = (n/N)*100,
            .groups = "drop") %>%
          unique() 
      }}
    
    }
  
    return(data_grouped)
}
