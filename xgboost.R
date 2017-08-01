install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")


# Need to do test & training split ----------------------------------------


# Logic from dplyr -------------
test %>%
  count(bedroomcnt)

test %>% 
  group_by(bedroomcnt) %>%
  sample_frac(.5) %>%
  count(bedroomcnt)

test %>% 
  group_by(bedroomcnt) %>%
  sample_n(5, replace = TRUE) %>%
  count(bedroomcnt)

# Logic from caret -------------
treatment <- designTreatmentsZ(test, colnames(test), verbose = FALSE)
treated <- as_tibble(prepare(treatment, test))
treated <- select(treated, -contains("catP"))

trainIndex <- caret::createDataPartition(treated$logerror_clean, 
                                         p = .8, 
                                         list = FALSE,
                                         times = 1)

dataTrain <- treated[trainIndex,]
dataTest <- treated[-trainIndex,]


dTrain <- xgb.DMatrix(data = as.matrix(dataTrain[,-1]), label = dataTrain$logerror_clean)
dTest <- xgb.DMatrix(data = as.matrix(dataTest[,-1]), label = dataTest$logerror_clean) 

#xgb.cv(data = dTrain, nfold = 3, nround = 200, objective = "reg:linear", eval_metric = "rmse", 
#       early_stopping_rounds = 3, print_every_n = 5, eta = .1)


# Boosted model
watchlist <- list(train=dTrain, test=dTest)
bst <- xgb.train(data=dTrain, nround=200, watchlist=watchlist, objective = "reg:linear", 
               eval_metric = "rmse", early_stopping_rounds = 5, print_every_n = 5, eta = .1)

# Random forest model
bst <- xgboost(data = dTrain, max.depth = 4, tree_method = "approx", 
               nthread = 10, num_parallel_tree = 200, subsample = 0.5, colsample_bytree =0.5, 
               nround = 1, objective = "reg:linear", metrics = "rmse")


predict(bst, dTrain)

importance_matrix <- xgb.importance(model = bst)
xgb.plot.importance(importance_matrix = importance_matrix)

as.tibble(importance_matrix)


# General Notes -----------------------------------------------------------



# xgb.DMatrix -----------------------------
# Suggested data type to use.
# Contains preprocess training data, missing values, data weight.
dtrain <- xgb.DMatrix(data = train$data, label=train$label)


# xgboost ---------------------------------
# A more simple method
# xgboost(data = dTrain, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "reg:linear", verbose = TRUE)
# Example implimentation for random forest
  bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4, tree_method = "approx", 
                 nthread = 10, num_parallel_tree = 100, subsample = 0.5, colsample_bytree =0.5, 
                 nround = 1, objective = "binary:logistic", metrics = "error")


# xgb.cv ----------------------------------
# Use for selecting ideal paramaters
# use early.stop.round to detect contineously being worse on test set
# If overfitting is observed reduce eta and increase nround at the same time

res = xgb.cv(params = param, data = train$data, label = train$label, 
             nround = 20, nfold = 5, 
             maximize = FALSE, early_stopping_rounds = 3)

# Round 1 Example
set.seed(082017)
cv.res = xgb.cv(data = train, nfold = 3, label = y, nrounds = 100, verbose = FALSE, 
                objective = '', eval_metric = '')

# Round 2 Example
set.seed(082017)
cv.res = xgb.cv(data = train, nfold = 3, label = y, nrounds = 3000, 
                objective = '', eval_metric = '',
                eta = .005, gamma = 1, lambda = 3, nthread = 8,
                max_depth = 4, min_child_weight = 1, verose = FALSE,
                subsample = .8, colsample_bytree = .8)

# xgb.train --------------------------------
# main paramaters
  # advised to use subample with eta and increase nround
  # Tune eta (learning rate) + num_rounds (number of trees) -> target 100 trees. A lower eta allows the model to be more robust to overfitting.
  # min_child_weight (start with 1/sqrt(event rate)). For regression this is the minimum # of instances needed in each node.
  # colsample_bytree (.3-.5). Ratio of columns selected.
  # subsampling (leave at 1.0). .5 means that half of the instances are selected to grow trees.
  # gamma (usually is OK to leave at 0.0). Larger gamma is more conservative.
  
# other paramaters
  # max_delta_step. Usually not needed. Sometimes for logistic regression when the class is extremely imbalanced.
  # num_paralell_tree. The number of trees to grow each round. Useful for random forest implimentation.
  
# Paramaters to controll model complexity
  # max_depth, min_child_weight, & gamma
  
# Paramaters to make the model more robust to noise
  # subsample, colsample_bytree
  
# If the data is imblanced among classes  
  # Care about ranking order
    # Balance the positive and negative weights by scale_pos_weight. Use AUS as the evaluation metric
  # Care about predicting the right probability and cannot re-blance the dataset
    # Set max_delta_step to a finite number
params = list(booster = "gbtree", silent = 0, eta = .3, gamma = 0, max_depth = 6, min_child_weight = 1, subsample = 1, colsample_bytree = 1, num_parallel_tree = 1)
xgb.train(params)


# Predict (continue using an old model)------
# Method to continue training based on a previous model
  # bst = xgboost(params = param, data = dtrain, nround = 1)
  # ptrain = predict(bst, dtrain, outputmargin = TRUE)
  # setinfo(dtrain, "base_margin", ptrain)
  # best = xgboost(params = param, data = dtrain, nround = 1)


# Importance and tree ploting -------------
# use xgb.importance for an importance score (Count of the appearance of each variable in all the trees)
# xgb.plot.tree can be used to plot a tree

# Advanced Features ------------------------
# Define our own loss function if we can calculate the 1st and 2nd order gradient of the loss function.
# Customize the evaluation metric


# Learning Logic ---------------------------

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
str(train)
train$label
dim(train$data)
class(train$data)[1]
class(train$label)
bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bstDMatrix <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic", verbose = 2)
bst <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic", verbose = 1)
pred <- predict(bst, test$data)
length(pred)
prediction <- as.numeric(pred > 0.5)
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)

# Both xgboost (simple) and xgb.train (advanced) train models
# For xgb.train you need a watchlist which is 2 xgb.DMatrix lists for the test and train.
watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nround=2, watchlist=watchlist, objective = "binary:logistic")
bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")

# For boosting linear models use booster = "gblinear" and remove the "eta" paramater.
bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")

#xgb.DMatrix Manipulatoin
xgb.DMatrix.save(dtrain, "dtrain.buffer") # Save
dtrain2 <- xgb.DMatrix("dtrain.buffer") # Load it in

#Extracting information
label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))
importance_matrix <- xgb.importance(model = bst)
xgb.plot.importance(importance_matrix = importance_matrix)
#View trees from the model
xgb.dump(bst, with_stats = T)
xgb.plot.tree(model = bst)
# save model to binary local file
xgb.save(bst, "xgboost.model")
# load binary model to R
bst2 <- xgb.load("xgboost.model")
pred2 <- predict(bst2, test$data)

# And now the test
print(paste("sum(abs(pred2-pred))=", sum(abs(pred2-pred))))

?xgboost
