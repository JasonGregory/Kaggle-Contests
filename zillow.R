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
    
    
    summaryFns = list(
      n_distinct,
      mean)
    
    str(tst) <- apply(summaryFns, function(fn){iData %>% summarise_all(fn)})
    
    as.tibble(tst)
    
    ?vars

    
    ?funs
    ?system.time()

    ?summarise_all
    
    library("parallel")
    
    ?n_distinct
    
    parLapply(); parApply(); parSapply()
    
    
    file.size(iData); file.size(iDT)
    




  # Modifying the code in dataPrep (needs to be quicker)
    prep_summary <- function(data) {
      
    }


propertyData %>%
  count(parcelid) %>%
  filter(n > 1)
  # Distinct by parcelid


