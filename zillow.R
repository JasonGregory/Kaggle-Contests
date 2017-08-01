# Misc --------------------------------------------------------------------
  # Re-organize the prepare stage. 1st pull in data; 2nd describe the data; 3rd Null and distinct values; 4th Rectegorize the variables  
  # Explore the variables relative to the explanatory variable

# Next Tasks --------------------------------------------------------------
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
i_describe <- prep_describe(i_data); print(i_describe, n = Inf)
t_describe <- prep_describe(train_data); print(t_describe, n = Inf)

(var_distinct <- prep_distinct(i_describe))
prep_distinct(t_describe)

# Join data -----------------------------------------
n1_data <- i_data %>%
  inner_join(train_data, by = "parcelid")

n1_describe <- prep_describe(n1_data) ; print(n1_describe, n = Inf)

# Variable Prep -----------------------------------------------------------

# Remove unique variables and set outcome variable-------------
var_outcome <- "logerror"
var_explanatory <- setdiff(n1_data %>% colnames(), c(var_outcome, var_distinct))

  
# Analyze null and distinct values --------------
prep_plot(filter(n1_describe, variable %in% var_explanatory))

var_null <- n1_describe %>% filter(variable %in% var_explanatory, null_perc >= .25) %>% prep_select(variable)
var_explanatory <- setdiff(var_explanatory, var_null)
prep_plot(filter(n1_describe, variable %in% var_explanatory))

# Analyze location variables later 
var_location <- c("longitude", "latitude", "rawcensustractandblock", "censustractandblock", "regionidzip", "regionidcity")
var_explanatory <- setdiff(var_explanatory, var_location)
prep_plot(filter(n1_describe, variable %in% var_explanatory))

n1_data <- n1_data %>% select(c(var_outcome, var_explanatory))
n1_describe <- prep_describe(n1_data); print(n1_describe, n = Inf)

# Replace null values (probably run the code below after the importance plot is done)
# n2_data <- prep_inpute(n1_data)
# 
# 
# n2_describe <- prep_describe(n2_data); print(n2_describe, n = Inf)
# prep_plot(n1_describe)
# prep_plot(n2_describe, plot_type = "distinct")
# 
# # Create factor variables
# n2_describe %>% arrange(desc(dist_count)) %>% print(n = Inf)
# 
#   # leave as is for now
#   # var_factor <- n2_describe %>% arrange(desc(dist_count))
#   # n2_data <- n2_data %>% mutate_at(var_factor, as.factor)


# Analyze Variable Importance ------------------------------------------------

importance <- explore_importance(n1_data, var_outcome)

non_significant <- importance %>% 
  select(variable, origName, spearman, pearson, r_squared, linear_pValue, bst_rank, rfm_rank) %>%
  filter(linear_pValue < .05 & bst_rank > length(importance$variable)*.5) %>%
  prep_select(variable)

importance %>% 
  select(variable, origName, spearman, pearson, r_squared, linear_pValue, bst_rank, rfm_rank) %>%
  filter(variable != non_significant) %>% print(n = Inf)

# look into using the plotting function to zero in on some of the variables.

# Retrieving linear model stuff

# Predicted r-squared
pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  return(pred.r.squared)
}

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}
  
pred.r.squared <- pred_r_squared(my.lm)
pred.r.squared


# Example
# Set up some data
x <- seq(from=0, to=10, by=0.5)
y1 <- 2*x + rnorm(length(x))


# We want to compare two different linear models:
my.lm1 <- lm(y1 ~ sin(x))
my.lm2 <- lm(y1 ~ x)

# We will use plyr for this.
library(plyr)

# Now call model_fit_stats() for each lm model that
# we have, using ldply. This returns the results in
# a data frame that is easily used for further 
# calculations.
ldply(list(my.lm1, my.lm2), model_fit_stats)

model_fit_stats <- function(linear.model) {
  r.sqr <- summary(linear.model)$r.squared
  adj.r.sqr <- summary(linear.model)$adj.r.squared
  pre.r.sqr <- pred_r_squared(linear.model)
  PRESS <- PRESS(linear.model)
  return.df <- data.frame(r.squared = r.sqr, adj.r.squared = adj.r.sqr, pred.r.squared = pre.r.sqr, press = PRESS)
  return(return.df)
}
pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}


# General multi plot function that is more flexible
data <- n2_data

vars <- data %>% select_if(is.numeric) %>% select(1:5) %>% colnames()

p1 <- data %>%
  ggplot(aes(x = logerror)) +
  geom_histogram(bins = nrow(data)^(1/3)) +
  labs(title = "With outliers")

p2 <- prep_outlier(data) %>%   # need to think about how to fix for outliers. Need variable to iterate. Maybe just make the second plot with no outliers.
  ggplot(aes(x = logerror)) +
  geom_histogram(bins = nrow(prep_outlier(data))^(1/3)) +
  labs(title = "Without outliers")

# index plots

data <- n2_data %>% 
  select_if(is.numeric) %>% 
  select(1, 3:5) %>%
  prep_rankColumn

x_vars <- data %>% select(contains("index")) %>% colnames()
y_vars <- setdiff(colnames(data), x_vars)

p3 <- data %>%
  ggplot(aes_string(x = x_vars[1], y = y_vars[1])) +
  geom_point(alpha = 1/12) +
  labs(title = "With outliers")

p4 <- prep_outlier(data, y_vars[2]) %>%
  ggplot(aes_string(x = x_vars[1], y = y_vars[1])) +
  geom_point(alpha = 1/12) +
  labs(title = "Without outliers")
  
explore_plot(plot1 = p1, x_vars = vars, plot2 = p2)

explore_plot(plot1 = p3, x_vars = x_vars, y_vars = y_vars, plot2 = p4)

RShowDoc('WVPlots_examples',package='WVPlots')

WVPlots::ScatterHist(prep_outlier(data, "logerror"), "taxamount", "logerror", smoothmethod="lm", "Linear fit")
WVPlots::ScatterHist(prep_outlier(data, "logerror"), "taxamount", "logerror", smoothmethod="identity", "Relation Plot")


data <- data %>% mutate(bathroomcnt = as.factor(bathroomcnt),
                bedroomcnt = as.factor(bedroomcnt)
)

p5 <- WVPlots::ScatterBoxPlot(data, "bathroomcnt", "logerror", pt_alpha=0.002, title="Scatter/Box plot")

p5 + aes(x = bedroomcnt)



# Old Stuff ---------------------------------------------------------------

# Other plotting stuff
# "%+%" overrides the data for gggplot
# ggplot_build(plot2) looks at underlying data
# q <- ggplot_build(plot2)
# q$data[[1]]$colour <- "black" # change to different color
# q <- ggplot_gtable(q) build the table
# in ggvis use something like get_data
# Use class(plot1)[1] to determine the type of plot being used 
# pltList <- list() Create a list of plots to reference later  

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

gridExtra::grid.arrange(plot1, plot2, plot3, nrow=3)


#### Pulling in correlation for all variables
treatment <- designTreatmentsZ(test, colnames(test), verbose = FALSE)
treated <- as_tibble(prepare(treatment, test))
treated <- select(treated, -contains("catP"))
treated_describe <- prep_describe(treated) #Need to add to the describe function some additional fields from the vtreat summary (like orignal name).
treated_describe <- treated_describe %>%
  left_join(treatment$scoreFrame, c("variable" = "varName")) %>%
  select(variable, origName, code, class, total_count, dist_count, dist_perc, null_count, mean)
treated_describe %>% print(n = Inf)
# rsq & sig are important when not doing the TreatmentsZ but another one.


correlations <- rcorr(as.matrix(treated))
testCor <- as.tibble(as.data.frame(correlations$r))

corTable <- testCor %>%
  rownames_to_column(var = "variable") %>%
  gather(-variable, key = "comparison_variable", value = "correlation") %>%
  select(variable, comparison_variable, correlation, everything()) %>% 
  arrange(variable, desc(abs(correlation))) %>%
  filter(variable != comparison_variable)

corTable <- corTable %>% 
  group_by(variable) %>%
  mutate(avg_corr = mean(correlation),
         max_corr = max(abs(correlation)),
         rank_corr = row_number(desc(abs(correlation)))) %>%
  ungroup()

corTable %>% 
  filter(max_corr == correlation) %>% 
  print(n = Inf)

# use this then to join on the other exploratory table analysis. Possilbe to pull the top 2 correlated variables if desired.

#### Decision tree models
trainIndex <- caret::createDataPartition(treated$logerror_clean, 
                                         p = .75, 
                                         list = FALSE,
                                         times = 1)

dataTrain <- treated[trainIndex,]
dataTest <- treated[-trainIndex,]

dTrain <- xgb.DMatrix(data = as.matrix(dataTrain[,-1]), label = dataTrain$logerror_clean)
dTest <- xgb.DMatrix(data = as.matrix(dataTest[,-1]), label = dataTest$logerror_clean) 

# Boosted Model
watchlist <- list(train=dTrain, test=dTest)
bst <- xgb.train(data=dTrain, nround=200, watchlist=watchlist, objective = "reg:linear", 
                 eval_metric = "rmse", early_stopping_rounds = 5, print_every_n = 5, eta = .1)

bst_importance <- as.tibble(xgb.importance(model = bst))

# Random Forest Model (Useful for comparison purposes for correlated variables.)
rfm <- xgboost(data = dTrain, max.depth = 4, tree_method = "approx", 
               nthread = 10, num_parallel_tree = 200, subsample = 0.5, colsample_bytree =0.5, 
               nround = 1, objective = "reg:linear", metrics = "rmse")

rfm_importance <- as.tibble(xgb.importance(model = rfm))

### Pull from the Frequency column to join with the correlation data

# Linear Model (possibly include something here. Maybe be a duplicate of an earlier process.)
model2 <- xgboost(data = sparse_matrix, label = output_vector, booster = "gblinear", nround = 15, 
                  alpha = .0001, lambda = 1, objective = "binary:logistic", metrics = "error")

####

explore_describe(n2_data, var_outcome) %>% print(n = Inf)

explore_describe <- function(data, var_outcome) {
  num_data <- select_if(data, is.numeric)
  
  summaryFns = list(
    spearman = function(x) cor(x, select(n2_data, var_outcome), method = "spearman"),
    pearson = function(x) cor(x, select(n2_data, var_outcome), method = "pearson"),
    r_squared = function(x) cor(x, select(n2_data, var_outcome), method = "pearson")^2
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
  
  lm <- tidy(lm(paste(var_outcome, "~ ."), data = data))
  
  describe <- summary_data %>%
    full_join(lm, by = c("variable" = "term")) %>%
    dplyr::rename(linear_std.error = std.error, linear_p.value = p.value) %>%
    select(variable, spearman, pearson, r_squared, linear_p.value) %>% 
    filter(variable != "(Intercept)") %>%
    as.tibble
  
  return(describe)
}

# Misc Notes --------------------------------------------------------------

# Parallel Notes Section --------------------------------------------------------

# library("parallel")
# 
# # max(1, detectCores() - 1)
# 
# # useful windows function for multi cores: memory.limit()/memory.size()
# 
# # parallel tips: http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
# 
# # for mac
# cl <- makeCluster(detectCores(), type = "FORK")
# 
# # for windows (doesn't seem worth it to try this method)
# cl <- makeCluster(detectCores(), type = "PSOCK")
# clusterEvalQ(cl, {library(tidyverse); library(dplyr); library(moments)})
# clusterExport(cl, list("data", "summaryFnsAll"))
# 
# parSapply(cl, summaryFnsAll, function(fn){data %>% summarise_all(fn)})
# 
# stopCluster(cl)
