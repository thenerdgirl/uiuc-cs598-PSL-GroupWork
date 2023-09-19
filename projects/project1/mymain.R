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
fold_num = 2


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

print_formatted = function(df, file=file_name) {
  # format output per guidance 
  write.table(df,
              file=file, 
              row.names=FALSE, 
              quote=FALSE, 
              sep = ",  ")
  }

############## BEGIN SCRIPT BODY ############## 
if (DEBUG) {print('Running in debug mode! Disable before submitting!')}

#######  load libraries  ####### 
library(glmnet)


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

####### train models ####### 



# model 1 is linear model with ridge penalty, using min lambda 

# suggested lambda sequence to increase performance
lambdas = exp(seq(-10, 1, length.out = 100))

# get rid of first and last column
train_x = makeX(train[, -c(1, ncol(df))])
train_y = train[, ncol(train)]

model_ridge = cv.glmnet(train_x, train_y, alpha = 0, lambda = lambdas)

predictions = as.vector(predict(model_ridge, s = model_ridge$lambda.min, newx = test_x))
results[sim, 2] = mean((test_y - predictions)^2)

# model 2 is randomForest 
model_rf = randomForest(y ~ ., data = train_data, ntree = 100)


#######  get predictions
pred1 = test_y
pred2 = test_y

#######  evaluate 
if(DEBUG) {
  
  
  
}


#######  write output
print_formatted(pred1, file='mysubmission1.txt')
write.csv(pred2, file='mysubmission2.txt')






