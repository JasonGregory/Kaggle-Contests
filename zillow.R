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
# library(dataFun)
library(stringr)
library(forcats)
library(vtreat)
library(lubridate)
library(gridExtra)
options(max.print=999999)
options(scipen=999)


# Additional to-do Items --------------------------------------------------
  # The inital prep state would show plots for outliers, the variable being predicted, describe tables
    # null values & distinct values. In addition it would show any prep functions to use. Possibly date fields..


# Initial data load & write -----------------------------------------------
(trainData <- read_csv(normalizePath("data/zillow/train_2016_v2.csv")))
(propertyData <- read_csv(normalizePath("data/zillow/properties_2016.csv")))
(dataDictionary <- rio::import(normalizePath("data/zillow/zillow_data_dictionary.xlsx")))
read_csv(normalizePath("data/zillow/sample_submission.csv"))

write_rds(propertyData, normalizePath("data/zillow/propertyData.rds"))
write_rds(trainData, normalizePath("data/zillow/trainData.rds"))

# Read in data ------------------------------------------------------------
# (i_data <- read_rds(normalizePath("data/zillow/propertyData.rds")))
(i_data <- read_rds("C:/Users/jgregor1/Desktop/propertyData.rds"))
(train_data <- read_rds(normalizePath("data/zillow/trainData.rds")))
  # (dataDict <- rio::import(normalizePath("data/zillow/zillow_data_dictionary.xlsx")))

# Initial look at files ---------------------------------------------------
(t_describe <- prep_describe(train_data))

# Index plot of logerrors (good for looking for outliers)
# Add descriptions of outlier/no-outlier
plot1 <- train_data %>%
  mutate(index = row_number(logerror)) %>%
  ggplot(aes(x = index, y = logerror)) +
  geom_point(alpha = 1/12) +
  labs(title = "With outliers")

plot2 <- prep_outlier(train_data) %>%
  mutate(index = row_number(logerror)) %>%
  ggplot(aes(x = index, y = logerror)) +
  geom_point(alpha = 1/12) +
  labs(title = "Without outliers")

grid.arrange(plot1, plot2, ncol=2)

# Histogram of logerror
# Add descriptions of skew, outlier/no-outlier
plot1 <- train_data %>%
  ggplot(aes(x = logerror)) +
  geom_histogram(bins = nrow(train_data)^(1/3)) +
  labs(title = "With outliers") #+ 
  #annotate("text", x=max(train_data$logerror)*.8, y=0, label = "Test", parse=T)  

temp_data <- prep_outlier(train_data)
plot2 <- prep_outlier(temp_data) %>%
  ggplot(aes(x = logerror)) +
  geom_histogram(bins = nrow(temp_data)^(1/3)) +
  labs(title = "Without outliers") #+ 
  #annotate("text", x=max(temp_data$logerror)*.8, y=0, label = "Test", parse=T)  

grid.arrange(plot1, plot2, ncol=2)

# Annotation logic. Work on later.

# equation = function(x) {
#   lm_coef <- list(a = round(coef(x)[1], digits = 2),
#                   b = round(coef(x)[2], digits = 2),
#                   r2 = round(summary(x)$r.squared, digits = 2));
#   lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
#   as.character(as.expression(lm_eq));                 
# }

# +
#   annotate("rect", xmin = 0.00, xmax = 0.1, ymin = -0.056, ymax = -0.044, fill="white", colour="red") +
#   annotate("text", x = 0.05, y = -0.05, label = equation(fit), parse = TRUE)

# Looking at transaction dates
# Eventually have this applied to any date fields.
plot1 <- train_data %>%
  ggplot(aes(x = transactiondate)) +
  geom_bar() +
  labs(title = "By Date")  

plot2 <- train_data %>%
  mutate(month = as.factor(month(transactiondate))) %>%
  ggplot(aes(x = month)) +
  geom_bar() +
  labs(title = "By Month")

plot3 <- train_data %>%
  mutate(year = as.factor(year(transactiondate))) %>%
  ggplot(aes(x = year)) +
  geom_bar() +
  labs(title = "By Year")  

grid.arrange(plot1, plot2, plot3, nrow=3)


# Determine how rows are distinct -----------------------------------------
i_data %>% select_if(function(col) n_distinct(col) == NROW(col))

select(column) %>% unlist(use.names = list_name)

describe %>% filter(null_perc > .6) %>% select(variable) %>% unlist(use.names = FALSE)



train_data %>%
  summarise(n_distinct(logerror))


n_distinct(col)
?n_distinct()
train_data %>% nrow() 


(describe <- prep_describe(i_data))
prep_desc_distinct(describe)
variables_unique <- prep_desc_distinct(describe)$variable 

# Pull in Outcome Variable ------------------------------------------------
test <- i_data %>%
  left_join(train_data, by = "parcelid") %>% select(parcelid, logerror, transactiondate, everything()) 




# Remove parcelid since it's unique -------------
variables_explanatory <- i_data %>% select(-one_of(variables_unique)) %>% colnames()

# Analyze Null Values -----------------------------------------------------
describe %>%
  filter(variable %in% variables_explanatory) %>%
  prep_desc_plotnull()

# Remove variables with > 60% NULL. Possibly analyze later. 
variables_null <- describe %>% filter(null_perc > .6) %>% select(variable) %>% unlist(use.names = FALSE)
variables_explanatory <- setdiff(variables_explanatory, variables_null)

# Filter out variables between 30% and 40% NULL values to analyze later
variables_potential <- describe %>% filter(variable %in% variables_explanatory, null_perc > .3) %>% select(variable) %>% unlist(use.names = FALSE)
variables_explanatory <- setdiff(variables_explanatory, variables_potential)

# Analyze Variable Types --------------------------------------------------
describe %>%
  filter(variable %in% variables_explanatory) %>%
  prep_desc_plotdistinct()

# Analyze location variables later 
variables_potential <- c(variables_potential, "longitude", "latitude", "rawcensustractandblock", "censustractandblock")
variables_explanatory <- setdiff(variables_explanatory, variables_potential)

describe %>%
  filter(variable %in% variables_explanatory) %>%
  arrange(dist_perc)

# Create factor variables
variable_factors <- describe %>% filter(dist_count < 40, variable %in% variables_explanatory) %>% select(variable) %>% unlist(use.names = FALSE)
variable_factors <- c(variable_factors, "regionidcity", "propertycountylandusecode", "regionidzip")

(c_data <- i_data %>% select(one_of(variables_explanatory)) %>% mutate_at(variable_factors, as.factor))

# Replace factor variable null values (don't always need to run this section)
c_data[variable_factors] <- prep_replaceNull(c_data, variable_factors)
prep_describe(c_data)

# Re-analyze mssing values with plot
VIM::aggr(c_data, numbers=TRUE)


# Variable Importance -----------------------------------------------------
c_data



# Functions ---------------------------------------------------------------
  # List in notes that for skew (-.5 to .5 fairly symmetrical, -1 to .5 maderatly skewed, -1 or greater highly skewed)
  # List in notes that kurtosis measures the tail-heaviness (close to 0 normal distribution, less than 0 light tailed, greater than 0 heavier tails)

dataFun_instructions <- function() { # This would provide instructions on how to use the package.

}

prep_replaceNull <- function (dta, columns, null_value = "Missing") {
  dta[columns] <-
    lapply(dta[columns], function(xx){
      if (sum(I(is.na(xx))) > 0) {
        levels(xx) <- c(levels(xx), null_value)
        xx[is.na(xx)] <- null_value
      }
      xx
    }
    )
  dta[columns]
}

prep_outlier <- function(data, floor = .01, roof = .99) {
  no_outlier <- data %>%
    filter(logerror <= quantile(logerror, roof), logerror >= quantile(logerror, floor)) 
  
  return(no_outlier)
}

prep_select <- function(column, list_name = FALSE) { #Wrapper around select to return list
  # Having issues with this function. Need to figure out with lazy val what is needed.
  select(column) %>% unlist(use.names = list_name)
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
  
  data <- data %>% 
    mutate_if(is.Date, function(x) as.numeric(format(x, "%Y%m%d")))  

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
    left_join(class_data, by = "variable") %>%
    as_tibble()
  
  describe <- describe %>%
    mutate(null_perc = null_count/total_count,
           dist_perc = dist_count/total_count) %>%
    select(variable, class, total_count, dist_count, null_count, null_perc, dist_perc, everything())
  
  return(describe)

}

prep_desc_plotnull <- function(describe) { # eventually combine 2 functions below.
#    if (type == "null") {
    describe %>%
      ggplot(aes(x = null_perc, y = reorder(variable, null_perc))) +
      geom_segment(aes(yend = variable), xend = 0) +
      geom_point() +
      geom_text(aes(label=round(null_perc,2)),hjust=0, vjust=0)
#    } else {
#     describe %>%
#        ggplot(aes(x = dist_perc, y = reorder(variable, dist_perc))) +
#        geom_segment(aes(yend = variable), xend = 0) +
#        geom_point() +
#        geom_text(aes(label=dist_count),hjust=0, vjust=0)      
#    }
}

prep_desc_plotdistinct <- function(describe) {
   describe %>%
      ggplot(aes(x = dist_perc, y = reorder(variable, dist_perc))) +
      geom_segment(aes(yend = variable), xend = 0) +
      geom_point() +
      geom_text(aes(label=dist_count),hjust=0, vjust=0)      
}  

prep_distinct <- function(data) {
  data %>% 
    select_if(function(col) n_distinct(col) == NROW(col))  
  
  
}

prep_desc_distinct <- function(describe) {
    describe %>% 
      filter(total_count == dist_count) %>% 
      select(variable, class, total_count, dist_count, min, max) %>%
      return()
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