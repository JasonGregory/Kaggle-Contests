# packages ----------------------------------------------------------------
library(plyr)
library(Hmisc)
library(xgboost)
#library(psych)
library(tidyverse)
#library(dplyr)
#library(tibble)
#library(readr)
library(moments)
# library(dataFun)
library(stringr)
library(forcats)
library(vtreat)
library(rlang)
library(broom)
options(max.print=999999)
options(scipen=999)



# Functions ---------------------------------------------------------------
# List in notes that for skew (-.5 to .5 fairly symmetrical, -1 to .5 maderatly skewed, -1 or greater highly skewed)
# List in notes that kurtosis measures the tail-heaviness (close to 0 normal distribution, less than 0 light tailed, greater than 0 heavier tails)

prep_distinct <- function(data) {
  if (colnames(select(data, 1)) == "variable") {
    distinct <- data %>%
      filter(total_count == dist_count) %>%
      #    select(variable, class, total_count, dist_count, min, max) %>%
      prep_select(variable)
    
    if (length(distinct) == 0L) return(NULL) else return(distinct)  
  } else {
    distinct <- data %>% 
      select_if(function(col) n_distinct(col) == NROW(col)) %>%
      colnames() 
    
    if (length(distinct) == 0L) return(NULL) else return(distinct)
  }
}

prep_outlier <- function(data, var, floor = .01, roof = .99) {
  q_high <- quantile(data %>% prep_select(var), roof)
  q_low <- quantile(data %>% prep_select(var), floor)
  no_outlier <- data %>%
    filter(UQ(sym(var)) <= q_high, UQ(sym(var)) >= q_low)     
  return(no_outlier)
}

# Wrapper function for select. Turns a vector into a character string.
prep_select <- function(data, column, list_name = FALSE) { 
  column <- enquo(column)
  data %>%
    select(!!column) %>%
    unlist(use.names = list_name)
}

prep_describe <- function(data) {
  
  summaryFnsNum = list(
    median = function(x) median(x, na.rm = TRUE),
    kurtosis = function(x) kurtosis(x, na.rm = TRUE),
    skew = function(x) skewness(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE),
    min = function(x) min(x, na.rm = TRUE)
  )
  
  summaryFnsAll = list(
    total_count = function(x) length(x),
    dist_count = n_distinct,
    null_count = function(x) sum(is.na(x)),
    mean  = function(x) mean(x, na.rm = TRUE),
    standard_deviation = function(x) sd(x, na.rm = TRUE)
  )    
  
  summaryFnsClass = list(
    class = class
  )
  
  class_data <- as.data.frame(sapply(summaryFnsClass, function(fn){data %>% summarise_all(fn)}))
  class_data <- class_data %>%
    rownames_to_column(var = "variable") %>%
    unnest()
  
  if (select_if(data, lubridate::is.Date) %>% ncol > 0) {
    data <- data %>% 
      mutate_if(lubridate::is.Date, function(x) as.numeric(format(x, format = "%Y%m%d")))    
  }
  
  numeric_data <- as.data.frame(sapply(summaryFnsNum, function(fn){data %>% select_if(is.numeric) %>% summarise_all(fn)}))
  if (ncol(numeric_data) == 0) {
    numeric_data <- tibble(
      variable = "none"
    )
  } else {
    numeric_data <- numeric_data %>%
      rownames_to_column(var = "variable") %>%
      unnest()
  }
  
  all_data <- as.data.frame(sapply(summaryFnsAll, function(fn){data %>% summarise_all(fn)}))
  if (ncol(all_data) == 0) {
    all_data <- tibble(
      variable = "none"
    )
  } else {
    all_data <- all_data %>%
      rownames_to_column(var = "variable") %>%
      unnest()
  }
  
  describe <- all_data %>%
    left_join(numeric_data, by = "variable") %>%
    left_join(class_data, by = "variable") %>%
    as_tibble()
  
  describe <- describe %>%
    mutate(null_perc = null_count/total_count,
           dist_perc = dist_count/total_count) %>%
    select(variable, class, total_count, dist_count, null_count, null_perc, dist_perc, everything())
  
  return(describe)
  
}

prep_table <- function(data, n_dist = 80, n_row = 15, vars = NULL, describe = NULL) {
  if (!is.null(describe)) {
    if (ncol(data) != nrow(describe)) stop("The inputs for data and describe do not have the same number of variables. Either modify or remove the input for describe.")
  }
  #  if ((ncol(data) != nrow(describe)) & !is.null(describe)) stop("The inputs for data and describe do not have the same number of variables. Either modify or remove the input for describe.")
  
  if (!is.null(vars)) data <- select(data, vars)
  
  if (n_dist == 80) {
    if (ncol(data)*nrow(data) > 90000000) {
      ncol1 <- ncol(data)
      if (!is.null(describe)) {
        vars_dist <- describe %>% filter(dist_count <= n_dist) %>% arrange(desc(dist_count)) %>% prep_select(variable)
        data <- select(data, vars_dist)
      } else {
        data <- data %>%
          select_if(function(col) n_distinct(col) <= n_dist)
      }
      ncol2 <- ncol(data)
      
      if (ncol1 - ncol2 > 0) {
        message_description <- paste(ncol1 - ncol2, "columns filtered. Only columns with <= 80 distinct values included. Use n_dist argument to modify.")
      } else {
        message_description <- NULL
      }
    }
    else {
      if (!is.null(describe)) {
        vars_dist <- describe %>% arrange(desc(dist_count)) %>% prep_select(variable)
        data <- select(data, vars_dist)
        message_description <- NULL
      } else {
        message_description <- NULL
      }
    }
  } else {
    ncol1 <- ncol(data)
    if (!is.null(describe)) {
      vars_dist <- describe %>% filter(dist_count <= n_dist) %>% arrange(desc(dist_count)) %>% prep_select(variable)
      data <- select(data, vars_dist)
    } else {
      data <- data %>%
        select_if(function(col) n_distinct(col) <= n_dist)
    }
    ncol2 <- ncol(data)
    if (ncol1 - ncol2 == 0) {
      message_description <- paste("All columns have less unique values than", n_dist)
    } else {
      message_description <- paste(ncol1 - ncol2, "columns were filtered with over", n_dist, "unique values.")
    }
  }
  
  ncol <- ncol(data)
  
  for(i in 1:ncol) {
    var <- data %>% 
      select(i) %>% colnames()  
    
    data %>%
      group_by_at(var) %>% 
      tally() %>% 
      arrange(desc(n)) %>%
      print(n = n_row)
    
    cat("\n")
  }
  
  if (!is.null(message_description)) {
    message(message_description) 
  }
}

prep_plot <- function(describe, plot_type = "all") {
  if (colnames(select(describe, 1)) == "variable") {
    if (plot_type == "all") {
      plot1 <- describe %>%
        ggplot(aes(x = reorder(variable, null_perc), y = null_perc)) +
        geom_bar(stat = "identity") + 
        scale_y_continuous(expand = c(0,0)) +
        labs(x = "Variable", y = "Null %") +
        coord_flip()
      
      plot2 <- describe %>%
        ggplot(aes(x = reorder(variable, dist_perc), y = dist_perc)) +
        geom_bar(stat = "identity") + 
        scale_y_continuous(expand = c(0,0)) +
        labs(x = "Variable", y = "Distinct %") +
        coord_flip()
      
      return(gridExtra::grid.arrange(plot1, plot2, ncol=2))
    } else if (plot_type == "null") {
      plot <- describe %>%
        ggplot(aes(x = reorder(variable, null_perc), y = null_perc)) +
        geom_bar(stat = "identity") + 
        scale_y_continuous(expand = c(0,0)) +
        labs(x = "Variable", y = "Null %") +
        coord_flip()
      
      return(plot)
    } else if (plot_type == "distinct") {
      plot <- describe %>%
        ggplot(aes(x = reorder(variable, dist_perc), y = dist_perc)) +
        geom_bar(stat = "identity") + 
        scale_y_continuous(expand = c(0,0)) +
        labs(x = "Variable", y = "Distinct %") +
        coord_flip()
      
      return(plot)
    } else {
      NULL
    }
  } else {
    NULL #data %>%
  }
}

prep_inpute <- function(data, vars = NULL, character_value = "missing", numeric_value = "mean", date_value = lubridate::ymd(19000101)) {
  
  if (!is.null(vars)) {
    var_all <- select(data, vars) %>% select_if(function(x) sum(is.na(x)) > 0) %>% colnames()
  } else {
    var_all <- select_if(data, function(x) sum(is.na(x)) > 0) %>% colnames()
  }
  var_char <- select(data, one_of(var_all)) %>% select_if(is.character) %>% colnames()
  var_factor <- select(data, one_of(var_all)) %>% select_if(is.factor) %>% colnames()  
  var_date <- select(data, one_of(var_all)) %>% select_if(lubridate::is.Date) %>% colnames()
  var_numeric <- select(data, one_of(var_all)) %>% select_if(is.numeric) %>% colnames()
  var_other <- setdiff(var_all, c(var_char, var_factor, var_date, var_numeric))
  
  treatments <- designTreatmentsZ(data, var_numeric, verbose = FALSE)
  treated <- as_tibble(prepare(treatments, data))
  
  data <- if (length(var_char) > 0) {
    if (character_value != "max") {
      data %>% mutate_at(var_char, function(x)  replace(x, is.na(x), character_value))      
    } else if (character_value == "max") {
      data %>% mutate_at(var_char, function(x)  replace(x, is.na(x), names(which.max(table(x)))))
    } else {
      data    
    }
  } else {
    data
  }
  
  data <- if (length(var_factor) > 0) {
    if (character_value != "max") {
      data %>% mutate_at(var_factor, function(x) fct_explicit_na(x, na_level = character_value))
    } else if (character_value == "max") {
      data %>% mutate_at(var_factor, function(x) fct_explicit_na(x, na_level = names(which.max(table(x)))))
    } else {
      data    
    }
  } else {
    data
  }
  
  data <- if (length(var_date) > 0) {
    if (as.character(date_value) != "max") {
      data %>% mutate_at(var_char, function(x)  replace(x, is.na(x), date_value))      
    } else if (date_value == "max") {
      data %>% mutate_at(var_char, function(x)  replace(x, is.na(x), lubridate::ymd(names(which.max(table(date))))))
    } else {
      data    
    }
  } else {
    data
  }
  
  data <- if (length(var_numeric) > 0) {
    if (numeric_value != "mean") {
      data %>% mutate_at(var_numeric, function(x)  replace(x,is.na(x), numeric_value))
    } else if (numeric_value == "mean") {
      data %>% mutate_at(var_numeric, function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))
    } else {
      data    
    }
  } else {
    data
  }
  
  data <- if (length(var_other) > 0) {
    data %>% mutate_at(var_other, function(x)  replace(x, is.na(x), names(which.max(table(x)))))
  } else {
    data
  } 
  
  data <- as_tibble(cbind(data, select(treated, contains("isBad"))))
  data %>% select(1, order(colnames(data)))
}

prep_rankColumn <- function(data, cols = NULL) {
  if (is.null(cols)) {
    cols <- colnames(data)
  }
  for (i in 1:length(cols)) {
    data <- data %>%
      mutate(!!paste(cols[i], "_index", sep="") := row_number(data %>% prep_select(cols[i])))
  }
  return(data)
}

explore_plot <- function(plot1, x_vars = NULL, y_vars = NULL, plot2 = NULL) { # Think about changing data for plot 2
  if (class(plot1)[1] == "gg" | class(plot1)[1] == "gtable") {
    if (sum(is.null(x_vars), is.null(y_vars)) == 2) {
      p1 <- plot1
      if (is.null(plot2)) {
        print(p1)
      } else {
        p2 <- plot2
        gridExtra::grid.arrange(p1, p2, ncol=2)
      } 
    } else if (is.null(x_vars)) {
      for(i in 1:length(y_vars)) {
        p1 <- plot1 + aes_string(y = y_vars[i])
        if (is.null(plot2)) {
          print(p1)
        } else {
          p2 <- plot2 + aes_string(y = y_vars[i])
          gridExtra::grid.arrange(p1, p2, ncol=2)          
        }
      }
    } else if (is.null(y_vars)) {
      for(i in 1:length(x_vars)) {
        p1 <- plot1 + aes_string(x = x_vars[i])
        if (is.null(plot2)) {
          print(p1)
        } else {
          p2 <- plot2 + aes_string(x = x_vars[i])
          gridExtra::grid.arrange(p1, p2, ncol=2)          
        }
      }      
    } else {
      for(i in 1:length(x_vars)) {
        p1 <- plot1 + aes_string(x = x_vars[i], y = y_vars[i])
        if (is.null(plot2)) {
          print(p1)
        } else {
          p2 <- plot2 + aes_string(x = x_vars[i], y = y_vars[i])
          gridExtra::grid.arrange(p1, p2, ncol=2)          
        }
      }        
    }
  } else {
    print("ggvis not supported at this point")
  }
}


explore_importance <- function(data, var_outcome) {
  # Preparing importance data
  treatment <- designTreatmentsZ(data, colnames(data), verbose = FALSE)
  treated <- as_tibble(prepare(treatment, data))
  treated <- select(treated, -contains("catP"))
  describe <- prep_describe(treated)
  importance <- describe %>%
    left_join(treatment$scoreFrame, c("variable" = "varName")) %>%
    select(variable, origName, code, class, total_count, dist_count)
  
  var_outcome <- importance %>% filter(origName == var_outcome) %>% prep_select(variable)
  
  # Individual correlation section
  num_data <- select_if(treated, is.numeric)
  
  summaryFns = list(
    spearman = function(x) cor(x, select(treated, var_outcome), method = "spearman"),
    pearson = function(x) cor(x, select(treated, var_outcome), method = "pearson"),
    r_squared = function(x) cor(x, select(treated, var_outcome), method = "pearson")^2
    # Pearson correlation evaluated the linear relationship. Spearman evaluates monotonic relationship (change together but not at the same rate).
    # Spearman is looking at one type of non-linear relationship.
  )
  
  summary_data <- as.data.frame(sapply(summaryFns, function(fn){num_data %>% summarise_all(fn)}))
  if (ncol(summary_data) == 0) {
    summary_data <- tibble(
      variable = "none"
    )
  } else {
    summary_data <- summary_data %>%
      rownames_to_column(var = "variable") %>%
      unnest()
  }
  
  lm <- tidy(lm(paste(var_outcome, "~ ."), data = treated))
  
  correlation <- summary_data %>%
    full_join(lm, by = c("variable" = "term")) %>%
    dplyr::rename(linear_stdError = std.error, linear_pValue = p.value) %>%
    mutate(spearman = round(spearman, 5),
           pearson = round(pearson, 5),
           r_squared = round(r_squared,5),
           linear_pValue = round(linear_pValue,5)) %>%
    select(variable, spearman, pearson, r_squared, linear_pValue) %>% 
    filter(variable != "(Intercept)") %>%
    as.tibble
  
  # Multi-correlation section (create a seperate function for this section)
  
  multi_correlation <- rcorr(as.matrix(treated))
  multi_correlation <- as.tibble(as.data.frame(round(multi_correlation$r,5)))
  multi_correlation <- multi_correlation %>%
    rownames_to_column(var = "variable") %>%
    gather(-variable, key = "comparison_variable", value = "correlation") %>%
    select(variable, comparison_variable, correlation, everything()) %>% 
    arrange(variable, desc(abs(correlation))) %>%
    filter(variable != comparison_variable)  
  
  multi_correlation <- multi_correlation %>%
    group_by(variable) %>%
    mutate(avg_corr = mean(correlation),
           max_corr = max(abs(correlation)),
           rank_corr = row_number(desc(abs(correlation)))
    ) %>%
    ungroup() %>%
    filter(max_corr == correlation)
  
  multi_correlation <- multi_correlation %>%
    rename(top_corrVariable = comparison_variable) %>%
    select(variable, top_corrVariable, max_corr, avg_corr)
  
  # Decision tree section
  trainIndex <- caret::createDataPartition(prep_select(treated, var_outcome), 
                                           p = .75, 
                                           list = FALSE,
                                           times = 1)
  
  dataTrain <- treated[trainIndex,]
  dataTest <- treated[-trainIndex,]
  
  dTrain <- xgb.DMatrix(data = as.matrix(dataTrain[,-1]), label = prep_select(dataTrain, var_outcome))
  dTest <- xgb.DMatrix(data = as.matrix(dataTest[,-1]), label = prep_select(dataTest, var_outcome))   
  
  # Boosted Model (will need a paramater to adjust for the objective function)
  watchlist <- list(train=dTrain, test=dTest)
  bst <- xgb.train(data=dTrain, nround=200, watchlist=watchlist, objective = "reg:linear", 
                   eval_metric = "rmse", early_stopping_rounds = 5, verbose = FALSE, eta = .1)
  
  bst_importance <- as.tibble(xgb.importance(model = bst))  
  bst_importance <- bst_importance %>%
    mutate(bst_rank = row_number(desc(Gain))) %>%
    rename(variable = Feature, bst_gain = Gain) %>%
    select(variable, bst_gain, bst_rank)
  
  
  # Random Forest Model (Useful for comparison purposes for correlated variables. Need paramater for objective function)
  rfm <- xgboost(data = dTrain, max.depth = 4, tree_method = "approx", 
                 nthread = 10, num_parallel_tree = 200, subsample = 0.5, colsample_bytree =0.5, 
                 nround = 1, objective = "reg:linear", metrics = "rmse", verbose = FALSE)
  rfm_importance <- as.tibble(xgb.importance(model = rfm))  
  rfm_importance <- rfm_importance %>%
    mutate(rfm_rank = row_number(desc(Gain))) %>%
    rename(variable = Feature, rfm_gain = Gain) %>%
    select(variable, rfm_gain, rfm_rank)
  
  importance <- importance %>%
    left_join(correlation, c("variable")) %>%
    left_join(multi_correlation, c("variable")) %>% 
    left_join(bst_importance, c("variable")) %>%
    left_join(rfm_importance, c("variable")) 
  
  return(importance)
}



dataFun_instructions <- function() { # This would provide instructions on how to use the package.
  
}

prep_report <- function() { # same as prepIt
  
}

explore_importance <- function(data) {
  
}

explore_importance_plot <- function(data) {
  
}

explore_categorical <- function() {
  
}

explore_numeric <- function() {
  
}