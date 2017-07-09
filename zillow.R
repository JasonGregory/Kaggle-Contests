# Next Tasks --------------------------------------------------------------
  # Final touches on the describe function (percentage null, make into function, etc.)
  # Look into removing variables with mostly null values & removing id variables
  # Re-categorize other variables based on disctinct values (factor, etc.)
  # Look into replacing NA rows for categorical variables & removing rows for numerical variables?
  # Think about vtreat & identifying important variables. (possibly sample if data is too large for speed)
    # Categorize variables into different sections prep_importance()
      # linear importance, non-linear, etc.
    # In a later stage look at possibly re-evaluating some of the non-important variables?
  # Do lots of different plots to evaluate the data! Think about box plots and how to do quickly.
    # Maybe think about a sample function to just sample a portion of the data.

# File notes --------------------------------------------------------------
  # properties_2016.csv; all properties with home features for 2016; 1 row per property
  # properties_2017.csv; all properties with home features for 2017 (available 10/2)
  # train_2016.csv; training set from 1/16 - 12/31/16; provides log error and transaction dates
  # train_2017.csv; training set from 1/2017 - 9/15/17 (available 10/2)

# Prediction --------------------------------------------------------------
  # logerror = log(Zestimate) − log(SalePrice)
  # Predict logerror for the months in Fall 2017

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(moments)
library(dataFun)
library(stringr)
options(max.print=999999)
options(scipen=999)

# Initial data load & write -----------------------------------------------
(trainData <- read_csv(normalizePath("data/zillow/train_2016_v2.csv")))
(propertyData <- read_csv(normalizePath("data/zillow/properties_2016.csv")))
(dataDictionary <- rio::import(normalizePath("data/zillow/zillow_data_dictionary.xlsx")))
read_csv(normalizePath("data/zillow/sample_submission.csv"))

write_rds(propertyData, normalizePath("data/zillow/propertyData.rds"))
write_rds(trainData, normalizePath("data/zillow/trainData.rds"))

# Read in data ------------------------------------------------------------
(iData <- read_rds(normalizePath("data/zillow/propertyData.rds")))
(trainData <- read_rds(normalizePath("data/zillow/trainData.rds")))
  # (dataDict <- rio::import(normalizePath("data/zillow/zillow_data_dictionary.xlsx")))

# Determine how rows are distinct -----------------------------------------

# Describe  & Other Functions ------------------------
  # List in notes that for skew (-.5 to .5 fairly symmetrical, -1 to .5 maderatly skewed, -1 or greater highly skewed)
  # List in notes that kurtosis measures the tail-heaviness (close to 0 normal distribution, less than 0 light tailed, greater than 0 heavier tails)

  dataFun_instructions <- function() { # This would provide instructions on how to use the package.

  }

  prep_describe <- function(data) {
    
  }

  prep_describe_plot <- function(data) {
    # plots various things based on describe function
    
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
  
  summaryFnsNum = list(
    median = function(x) median(x, na.rm = TRUE),
    kurtosis = function(x) kurtosis(x, na.rm = TRUE),
    skew = function(x) skewness(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE),
    min = function(x) min(x, na.rm = TRUE)
  )

  summaryFnsAll = list(
    class = class,
    total_count = function(x) length(x),
    dist_count = n_distinct,
    null_count = function(x) sum(is.na(x)),
    mean  = function(x) mean(x, na.rm = TRUE),
    standard_deviation = function(x) sd(x, na.rm = TRUE)
  )
  
  data <- iData
  
  numeric_data <- as.data.frame(sapply(summaryFnsNum, function(fn){data %>% select_if(is.numeric) %>% summarise_all(fn)}))
  numeric_data <- numeric_data %>%
    rownames_to_column(var = "variable") %>%
    unnest()
  
  all_data <- as.data.frame(sapply(summaryFnsAll, function(fn){data %>% summarise_all(fn)}))
  all_data <- all_data %>%
    rownames_to_column(var = "variable") %>%
    unnest()
  
  describe <- all_data %>%
    left_join(numeric_data, by = "variable") %>% 
    as_tibble()
  
  describe <- describe %>%
    mutate(null_perc = null_count/total_count,
           dist_perc = dist_count/total_count) %>%
    select(variable, class, total_count, dist_count, null_count, null_perc, dist_perc, everything()) %>%
    print(n = Inf)

  # misc logic to analyze data
  describe %>% filter(class %in% c("integer", "numeric"))
  describe %>% filter(!str_detect(variable, "id"), class %in% c("integer", "numeric")) %>% select(variable, kurtosis) %>% print(n = Inf)
  describe %>% filter(!str_detect(variable, "id"), null_perc < .05) %>% arrange(dist_perc) %>% print(n = Inf) # possibly do plot for this & null variables
    #Potential factor variables
      describe %>% filter(class == "character") %>% arrange(dist_perc) # possibly plot this by variable type
  select(iData, contains("id"))
  
  # logic to select based on null percentage value
  varKeep <- describe %>% 
    filter(null_perc < .05) %>%
    select(variable) %>% unlist()
  
  iData %>% select(varKeep)

  # Need to figure out how to deal with larger data. Should I sample it or should I re-code it? Use a data table?
    
    # Target methods that take 5 seconds or less. 
      # First work on summaries that work on the whole data set really quick (either using data tables or not). Look into using parallel apply, etc.
      # Second work on summaries that need to take a sample of the data (such as sample)

      # Documentation on sampling
        # https://rdrr.io/cran/dplyr/man/sample.html
        # https://stackoverflow.com/questions/21255366/sample-rows-of-subgroups-from-dataframe-with-dplyr


# Parallel Notes Section --------------------------------------------------------

library("parallel")

# max(1, detectCores() - 1)

# useful windows function for multi cores: memory.limit()/memory.size()

# parallel tips: http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

# for mac
cl <- makeCluster(detectCores(), type = "FORK")

# for windows (doesn't seem worth it to try this method)
cl <- makeCluster(detectCores(), type = "PSOCK")
clusterEvalQ(cl, {library(tidyverse); library(dplyr); library(moments)})
clusterExport(cl, list("data", "summaryFnsAll"))

parSapply(cl, summaryFnsAll, function(fn){data %>% summarise_all(fn)})

stopCluster(cl)