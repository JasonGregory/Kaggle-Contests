# File notes ---------
  # properties_2016.csv; all properties with home features for 2016; 1 row per property
  # properties_2017.csv; all properties with home features for 2017 (available 10/2)
  # train_2016.csv; training set from 1/16 - 12/31/16; provides log error and transaction dates
  # train_2017.csv; training set from 1/2017 - 9/15/17 (available 10/2)

# Prediction ---------
  # logerror = log(Zestimate) − log(SalePrice)
  # Predict logerror for the months in Fall 2017

# Load libraries ---------
library(tidyverse)
library(dplyr)
library(moments)
library(dataFun)

# Initial data load & write ----------
(trainData <- read_csv(normalizePath("data/zillow/train_2016_v2.csv")))
(propertyData <- read_csv(normalizePath("data/zillow/properties_2016.csv")))
(dataDictionary <- rio::import(normalizePath("data/zillow/zillow_data_dictionary.xlsx")))
read_csv(normalizePath("data/zillow/sample_submission.csv"))

write_rds(propertyData, normalizePath("data/zillow/propertyData.rds"))
write_rds(trainData, normalizePath("data/zillow/trainData.rds"))

# Read in data ---------
(iData <- read_rds(normalizePath("data/zillow/propertyData.rds")))
(trainData <- read_rds(normalizePath("data/zillow/trainData.rds")))
  # (dataDict <- rio::import(normalizePath("data/zillow/zillow_data_dictionary.xlsx")))

# Determine how rows are disctinct
  # Need to figure out how to deal with larger data. Should I sample it or should I re-code it? Use a data table?
    
    # Target methods that take 5 seconds or less. 
      # First work on summaries that work on the whole data set really quick (either using data tables or not). Look into using parallel apply, etc.
      # Second work on summaries that need to take a sample of the data (such as sample)

      # Documentation on sampling
        # https://rdrr.io/cran/dplyr/man/sample.html
        # https://stackoverflow.com/questions/21255366/sample-rows-of-subgroups-from-dataframe-with-dplyr

    # Comparing different methods for speed  
    library("microbenchmark")
    iDT <- as.data.table(iData)
    
    indentical(apply(iData, 2, function(x) length(unique(x))), iData %>% summarise_all(n_distinct) )
    system.time(apply(iData, 2, function(x) length(unique(x))))
    system.time(apply(iDT, 2, function(x) length(unique(x))))
    system.time(apply(iData, 2, function(x) n_distinct(x)))
    
    system.time(iData %>% summarise_all(n_distinct))
    
    tst <- iData %>% summarise_all(mean())
    tibble(ndistinct = unlist(tst))
    
    tst <- spread(Var1, value)
    
    iData %>% summarise_all(funs(n_distinct,mean))
    test <- iData %>% 
      summarise_all(funs(mean(., na.rm = TRUE)))
    
    test %>% unlist() %>% print(width = Inf)

# Description Function ----------------------------------------------------
    # List in notes that for skew (-.5 to .5 fairly symmetrical, -1 to .5 maderatly skewed, -1 or greater highly skewed)
    # List in notes that kurtosis measures the tail-heaviness (close to 0 normal distribution, less than 0 light tailed, greater than 0 heavier tails)

    
prep_describe <- function(data) {
  
}

summaryFnsNum = list(
  total_count = function(x) length(x),
  dist_count = n_distinct,
  null_count = function(x) sum(is.na(x)),
  mean  = function(x) mean(x, na.rm = TRUE),
  median = function(x) median(x, na.rm = TRUE),
  standard_deviation = function(x) sd(x, na.rm = TRUE),
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
  median = function(x) median(x, na.rm = TRUE),
  standard_deviation = function(x) sd(x, na.rm = TRUE),
  max = function(x) max(x, na.rm = TRUE),
  min = function(x) min(x, na.rm = TRUE)
)

# look into useing the multi cor version of sapply & also into compiling the function

data <- iData

numeric_data <- as.data.frame(sapply(summaryFnsNum, function(fn){data %>% select_if(is.numeric) %>% summarise_all(fn)}))
numeric_data <- numeric_data %>%
  rownames_to_column(var = "Variable") %>%
  unnest()

all_data <- as.data.frame(sapply(summaryFnsAll, function(fn){data %>% summarise_all(fn)}))
all_data <- all_data %>%
  rownames_to_column(var = "Variable") %>%
  unnest()
    
numeric_data; all_data    

unnest(all_data)


library("parallel")

?n_distinct

parLapply(); parApply(); parSapply()


file.size(iData); file.size(iDT)
    




  # Modifying the code in dataPrep (needs to be quicker)



propertyData %>%
  count(parcelid) %>%
  filter(n > 1)
  # Distinct by parcelid


