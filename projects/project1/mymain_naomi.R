# Project 1
# CS598 Practical Statistical Learning
# Naomi Brhagat - nbhgat3
# Michael Miller - msmille3
# Joe May - jemay3
# 9/17/2023

# Campuswire post: https://campuswire.com/c/G06C55090/feed/193
# PDF instructions https://liangfgithub.github.io/Proj/F23_Proj1.pdf

library(glmnet)
library(randomForest)

# TODO set this to FALSE before submitting 
DEBUG = TRUE 

# use this to select which fold to use (debug only)
# fold_num = 1

get_rmse = function(y_pred, y_actual) {
  # implement per assignment instructions 
  
  n = length(y_actual)
  
  inner_sum = sum((log(y_actual) - log(y_pred))^2)
  total = sqrt(1/n * inner_sum)
  
  return(total)
}

### Pre-Processing Functions ###

clean_data_tree = function(df_in) {
  # Remove the PID as a variable
  df_in = subset(df_in, select = -PID)
  
  # zero out the Garage Year Built missing data
  df_in$Garage_Yr_Blt[is.na(df_in$Garage_Yr_Blt)] = 0

  return(df_in)
}

for (fold_num in 1:10) {
  ### Train Model ###
  # TODO switch from DEBUG mode at some point
  file_dir =  paste0('fold', as.character(fold_num))
  test_x = read.csv(paste0(file_dir, '/test.csv'))
  test_y = read.csv(paste0(file_dir, '/test_y.csv'))
  train = read.csv(paste0(file_dir, '/train.csv'))
  
  train_tree = clean_data_tree(train)
  train_y = log(train$Sale_Price)
  
  # Tree model
  set.seed(235)
  model_rf = randomForest(Sale_Price ~ ., data=train_tree[, -1], ntree = 100)
  
  ### Test Prediction Output ###
  # Same pre-processing
  test_pid = test_x$PID
  test_tree = clean_data_tree(test_x)
  
  sapply(train_tree, class)
  sapply(test_tree, class)
  
  tree_pred = as.vector(predict(model_rf, newdata = test_tree, na.action = na.pass))
  tree_output = data.frame(PID = test_pid, Sale_Price = exp(tree_pred))
  
  # Check rmse
  y_actual = subset(test_y, PID %in% test_pid)$Sale_Price
  
  # rmse_ridge = get_rmse(y_ridge, y_actual)
  rmse_rf = get_rmse(tree_pred, y_actual)
  
  # rmse_ridge
  print(rmse_rf)
}












