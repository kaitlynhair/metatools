#' Count Comparison IDs per Category and Recode Small Groups
#'
#' This function counts distinct `ComparisonId` values per category in the specified column of a dataframe.
#' Categories with fewer than 10 distinct `CohortIds` are recoded as "other". It then prints the counts of 
#' `ComparisonId` per category after recoding.
#'
#' @param data A dataframe containing the data to process.
#' @param column The name of the column to process, passed as a string.
#' @return Prints a summary dataframe showing counts of ComparisonId per category after recoding.
#' @examples
#' data <- data.frame(ComparisonId = 1:100,
#'                    Category = sample(c("A", "B", "C"), 100, replace = TRUE))
#' count_comparisons_cat(data, "Category")
count_comparisons_cat <- function(data, column) {
  
  # Count ComparisonId per category
  counts <- data %>%
    group_by(!!sym(column)) %>%
    summarise(Count = n_distinct(ComparisonId))
  
  # Identify categories with less than 10 distinct CohortIds
  small_groups <- counts %>%
    filter(Count < 10) %>%
    pull(!!sym(column))
  
  # Recode small groups to "other"
  data[[column]] <- ifelse(data[[column]] %in% small_groups, "other", data[[column]])
  
  final_counts <- data %>%
    group_by(!!sym(column)) %>%
    summarise(Count = n_distinct(ComparisonId))
  
  print(final_counts)
}

#' Convert Categories Based on Cohort ID Count
#'
#' Transforms categories in the specified column of a dataframe based on the count of distinct `ComparisonId`.
#' Categories with fewer than 10 distinct `ComparisonId` are recoded to "other". The transformed dataframe is returned.
#'
#' @param data A dataframe containing the data to be transformed.
#' @param column The name of the column to process, passed as a string.
#' @return Returns the modified dataframe with categories recoded as necessary.
#' @examples
#' data <- data.frame(ComparisonId = 1:100,
#'                    Category = sample(c("A", "B", "C"), 100, replace = TRUE))
#' newdata <- convert_comparisons_cat(data, "Category")
convert_comparisons_cat <- function(data, column) {
  
  # Count ComparisonId per category
  counts <- data %>%
    group_by(!!sym(column)) %>%
    summarise(Count = n_distinct(ComparisonId))
  
  # Identify categories with less than 10 distinct CohortIds
  small_groups <- counts %>%
    filter(Count < 10) %>%
    pull(!!sym(column))
  
  # Recode small groups to "other"
  data[[column]] <- ifelse(data[[column]] %in% small_groups, "other", data[[column]])
  data <- as.data.frame(data)
  
  return(data)
}


#' Run Meta-Regression for Categorical Variables
#'
#' This function performs a meta-regression analysis on specified categorical columns in a dataframe using the `rma` function.
#' Results for each category are stored and returned in a list.
#'
#' @param data A dataframe containing the meta-analysis data.
#' @param cat_vars A character vector of categorical variables (from column names).
#' @return A list containing the meta-regression results for each categorical variable.
#' @examples
# data <- data.frame(Nested_ES = rnorm(100),
#                    Nested_ES_SE = runif(100, 0.1, 1),
#                    Category = sample(c("Type1", "Type2"), 100, replace = TRUE))
# results <- meta_reg_cat(data, c("Category"))
meta_reg_cat <- function(data, cat_vars){
  
  # create empty list to store results
  results_cat_vars <- list()
  
  # loop through var_names
  for (i in seq_along(cat_vars)) {
    
    # extract current var_name
    var <- cat_vars[i]
    
    # create model formula
    formula <- as.formula(paste0("~`", cat_vars[i], "`"))
    
    # run meta-analysis
    metareg_res <- rma(yi = `Nested_ES`,
                       sei = `Nested_ES_SE`,
                       data = data,
                       method = "REML",
                       mods = formula,
                       test = "knha")
    
    # store result in results list
    results_cat_vars[[var]] <- metareg_res
    
  }
  return(results_cat_vars)
}

#' Run Meta-Regression for Numerical Variables
#'
#' This function performs a meta-regression analysis on specified categorical columns in a dataframe using the `rma` function.
#' Results for each category are stored and returned in a list.
#'
#' @param data A dataframe containing the meta-analysis data.
#' @param cat_vars A character vector of numerical variables (from column names).
#' @return A list containing the meta-regression results for each numerical variable.
meta_reg_num <- function(data, num_columns){
  
  # create empty list to store results
  results_num_vars <- list()
  
  # loop through var_names
  for (i in seq_along(num_columns)) {
    
    # extract current var_name
    var <- num_columns[i]
    
    # create model formula
    formula <- as.formula(paste0("~`", num_columns[i], "`"))
    
    # run meta-analysis
    metareg_res <- rma(yi = `Nested_ES`,
                       sei = `Nested_ES_SE`,
                       data = data,
                       method = "REML",
                       mods = formula,
                       test = "knha")
    
    # store result in results list
    results_num_vars[[var]] <- metareg_res
    
  }
  return(results_num_vars)
}


#' Generate a Table from Meta-Regression Numeric Output
#'
#' This function formats the output of a meta-regression analysis with numeric variables into a
#' readable table format, including standard meta-analysis metrics like the Standard Mean Difference,
#' Confidence Intervals, P-values, and heterogeneity statistics (Tau^2, I^2, R^2).
#'
#' @param x An object of class 'rma' from the 'metafor' package, typically the output from `rma()`.
#' @return A data frame formatted as a table with relevant meta-analysis metrics.
metareg_table_num <- function(x){ 
  table <- data.frame("Variable" = x$coef.na,
                      "SMD Estimate (SE)" = paste(round(x$b,2), "(", round(x$se,2), ")"), 
                      "95% CI" = paste(round(x$ci.lb,2), "to", round(x$ci.ub, 2)), 
                      "P value" = paste(round(x$QMp,4)), 
                      "Tau2" = paste(round(x$tau2,2)), 
                      "I2"= paste(round(x$I2,2)), 
                      "R2" = paste(round(x$R2,2))) %>% 
    select(-Variable) %>% 
    tibble::rownames_to_column("Variable") 
  
  colnames(table) <- c("Variable", "SMD Estimate", "95% CI", "P value", "Tau<sup>2</sup>", "I<sup>2</sup>", "R<sup>2</sup>") 
  table$Variable[] <- lapply(table$Variable, gsub, pattern="X", replacement="") 
  
  table$`SMD Estimate (SE)`[2]
  kable(table, escape = FALSE) %>% kable_classic() 
  
}
#' Generate a Table from Meta-Regression Categorical Output
#'
#' This function formats the output of a meta-regression analysis for categorical variables into a
#' readable table format. It adjusts estimates relative to the intercept and includes meta-analysis
#' metrics such as Confidence Intervals, P-values, and heterogeneity statistics (Tau^2, I^2, R^2).
#'
#' @param x An object of class 'rma' from the 'metafor' package, typically the output from `rma()`
#'          that includes categorical modifiers.
#' @return A data frame formatted as a table with adjusted estimates and relevant meta-analysis metrics.
metareg_table_cat <- function(x){ 
  
  table <- data.frame("Variable" = x$coef.na,
                      "SE" = round(x$se,2),
                      "Estimate" = round(x$b,2),
                      "CI1" = round(x$ci.lb,2),
                      "CI2" = round(x$ci.ub, 2),
                      "P value" = paste(round(x$QMp,4)), 
                      "Tau2" = paste(round(x$tau2,2)), 
                      "I2"= paste(round(x$I2,2)), 
                      "R2" = paste(round(x$R2,2))) %>% 
    select(-Variable) %>% 
    tibble::rownames_to_column("Variable") 
  
  intercept <- table %>%
    mutate("95% CI" = paste0(CI1, " to", CI2)) %>%
    mutate("SMD Estimate" = paste0(Estimate)) %>%
    filter(Variable == "X(Intercept)")
  
  
  corrected_table <- table %>%
    mutate("95% CI" = paste0(CI1 + table$Estimate[1], " to", CI2 + table$Estimate[1])) %>%
    mutate("SMD Estimate" = paste0(Estimate + table$Estimate[1])) %>%
    filter(!Variable == "X(Intercept)")
  
  final_table <- rbind(corrected_table, intercept) 
  
  final_table$Variable[] <- lapply(final_table$Variable, gsub, pattern="X", replacement="") 
  
  final_table <- final_table %>%
    select(Variable, `SMD Estimate`, `95% CI`, `P.value`, Tau2, I2, R2)
  
  colnames(final_table) <- c("Variable", "SMD Estimate", "95% CI", "P value", "Tau<sup>2</sup>", "I<sup>2</sup>", "R<sup>2</sup>") 
  
  kable(final_table, escape = FALSE) %>% kable_classic() 
  
}

#