# Project 1
# CS598 Practical Statistical Learning
# Naomi Brhagat - nbhgat3
# Michael Miller - msmille3
# Joe May - jemay3
# 9/17/2023

# Campuswire post: https://campuswire.com/c/G06C55090/feed/193
# PDF instructions https://liangfgithub.github.io/Proj/F23_Proj1.pdf

# TODO set this to FALSE before submitting 
DEBUG = TRUE 

# use this to select which fold to use (debug only)
fold_num = 3


clean_data = function(in_df) { 
  # in_df - dataframe matching format of Ames housing dataset
  # note that this function must work with dataframes but including and excluding the 
  # column Sale_Price
  
  #TODO: Implement me! 
  
  #temp, drop non-numeric columns
  numeric_columns = sapply(in_df, function(x) is.integer(x) || is.numeric(x))
  out_df = in_df[, numeric_columns]
  
  #temp, drop any columns with missing values 
  out_df =  out_df[complete.cases(out_df), ]
  
  return(out_df)
  }

print_formatted = function(pred, idx, file_name) {
  
  out_df = data.frame(PID=idx, Sale_Price=pred)
  
  # format output per guidance 
  write.table(out_df,
              file=file_name, 
              row.names=FALSE, 
              quote=FALSE, 
              sep = ",  ")
  }

get_rmse = function(y_pred, y_actual) {
  # implement per assignment instructions 
  
  n = length(y_actual)
  
  inner_sum = sum((log(y_actual) - log(y_pred))^2)
  total = sqrt(1/n * inner_sum)
  
  return(total)
}

############## BEGIN SCRIPT BODY ############## 
if (DEBUG) {print('Running in debug mode! Disable before submitting!')}

#######  load libraries  ####### 
library(glmnet)
library(randomForest)


#######  load data  ####### 
if(DEBUG) {
  file_dir =  paste0('fold', as.character(fold_num))
  test_x = read.csv(paste0(file_dir, '/test.csv'))
  test_y = read.csv(paste0(file_dir, '/test_y.csv'))
  train = read.csv(paste0(file_dir, '/train.csv'))
} else {
  test_x = read.csv(paste0('test.csv'))
  train = read.csv(paste0('train.csv'))
}
  
#######  clean data  ####### 
train = clean_data(train)
test_x = clean_data(test_x)

####### train models and evaluate #######
test_idx = test_x$PID

### model 1 is linear model with ridge penalty, using min lambda 

# suggested lambda sequence to increase performance
lambdas = exp(seq(-10, 1, length.out = 100))

# inputs need to be two matrices
train_x_mat = as.matrix(train[, -c(2, ncol(train))])
test_x_mat = as.matrix(test_x[, -1])
train_y = train[, ncol(train)]

model_ridge = cv.glmnet(train_x_mat, train_y, alpha=0, lambda=lambdas)

y_ridge = as.vector(predict(model_ridge, s=model_ridge$lambda.min, newx=test_x_mat))

###  model 2 is randomForest 
model_rf = randomForest(Sale_Price ~ ., data=train[, -1], ntree = 100)

y_rf = as.vector(predict(model_rf, newdata=test_x))

#######  evaluate 
if(DEBUG) {
  # first, only get the y values we actually used
  y_actual = subset(test_y, PID %in% test_idx)$Sale_Price
  
  rmse_ridge = get_rmse(y_ridge, y_actual)
  rmse_rf = get_rmse(y_rf, y_actual)
  
  sprintf('RMSE for ridge model for fold %d is %.2f', fold_num, rmse_ridge)
  sprintf('RMSE for random forrest model for fold %d is %.2f', fold_num, rmse_rf)
}

#######  write output
print_formatted(y_ridge, test_idx, 'mysubmission1.txt')
print_formatted(y_rf, test_idx, 'mysubmission2.txt')






