# Project 2
# CS598 Practical Statistical Learning
# Naomi Bhagat - nbhagat3
# Michael Miller - msmille3
# Joe May - jemay3
# 10/16/2023

# Campuswire post: https://campuswire.com/c/G06C55090/feed/339
# PDF instructions https://liangfgithub.github.io/Proj/F23_Proj2.pdf

#######  load libraries  ####### 
# packages to load
# note magrittr not expressly called but we use the %<% operator
packages = c('tidyr', 'tidyverse','lubridate', 'magrittr', 'dplR')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

DEBUG = TRUE

fold_count = 1

set.seed(235)

#######  Functions Called in Main  ####### 

# Given raw input data and department d, gives us the 
# cleaned matrix X as defined https://campuswire.com/c/G06C55090/feed
get_dept_matrix = function(train_data, d){
  X = train_data %>%
    filter(Dept == d) %>%
    select(Store, Date, Weekly_Sales) %>%
    spread(Store, Weekly_Sales)
  
  # replace NAs with 0 
  X[is.na(X)] = 0 
  
  # drop date row, and take transpose
  X = X[, -1]
  X = t(X)
  
  # this is now X as described here: https://campuswire.com/c/G06C55090/feed/737
  return(X)
}

# Replaces na values with 0. Get average sales for store. Create SVD.
dept_svd = function(X){
  m = ncol(X)-1
  n = nrow(X)
  d = min(m, n, 8)
  
  # replace na's with 0s
  X[is.na(X)] = 0
  
  store_mean = rowMeans(t(X[,-1]))
  X_less_mean = t(X[,-1]) - store_mean
  #if(DEBUG) { cat("dept_svd t(X[,-1]",nrow(t(X[,-1])),"m",length(t(X[,-1])),"s",length(store_mean),"\n") } 
  #if(DEBUG) { cat("dept_svd X_less n",nrow(X_less_mean),"m",length(X_less_mean),"\n") } 
  #if(DEBUG) { cat("dept_svd data   n",nrow(data_t),"m",ncol(data_t),"\n") } 
  #if(DEBUG) { cat("dept_svd data_t n",nrow(data_t),"m",length(data_t),"\n") } 
  #if(DEBUG) { cat("dept_svd c n",class(X),"\n") } 
  #if(DEBUG) { cat("dept_svd d n",class(X_less_mean),"\n") } 
  
  # implements SVD unless there is either one row or column
  # selects top 8 dimensions or row/column width if either is less than 8
  # If rows or columns have one entry, it just returns the matrix
  # I'm really not confident this is all done right, but it was done to ensure it works.
  if(min(n,m)>1){
  svd_decom = svd(X_less_mean)
  U = svd_decom$u[,1:d]
  D = diag(svd_decom$d[1:d])
  Vt = t(svd_decom$v[,1:d])
  #if(DEBUG) { 
  #  cat("dept_svd 2 U",nrow(svd_decom$u),"x",ncol(svd_decom$u),
  #      "D",length(svd_decom$d),"x",length(svd_decom$d),
  #      "Vt",nrow(svd_decom$v),"x",ncol(svd_decom$v),"\n") } 
  X_s = U %*% D %*% Vt + store_mean
  } else {
  X_s =  X_less_mean + store_mean
  }
  
  #print(X_s)
  #if(DEBUG) { cat("dept_svd 3 n",length(X_s),"m",nrow(X_s),"\n") } 
  return(X_s)
}

# given our matrix X, generate a design matrix that we will feed into lm
get_design_matrix = function(X, column_names, i){
  
  smoothed = as.data.frame(t(Xmn))
  
  colnames(smoothed) = column_names
  #if(DEBUG) { cat("get_reshape 4 n",nrow(Xi),"m",length(Xi),"\n") } 
  #print(colnames(smoothed))
  
  smoothed$Date = Xi[,1]
  #if(DEBUG) { cat("get_reshape 5 n",nrow(smoothed),"m",length(smoothed),"\n") } 
  #print(smoothed)
  
  pivot_smooth = gather(smoothed, key = "Store", value = "Prediction", -Date)
  #if(DEBUG) { cat("get_reshape 6 n",nrow(pivot_smooth),"m",length(pivot_smooth),"\n") } 
  
  pivot_smooth$Dept = i
  #if(DEBUG) { cat("get_reshape 7 n",length(pivot_smooth),"m",nrow(pivot_smooth),"\n") }
  return(pivot_smooth)
}

############## Evaluation Function ############## 
evaluation = function(){
  test_w_label_path = paste0('Proj2_Data/test_with_label.csv')
  test_w_label = read.csv(test_w_label_path)
  wae = rep(0, fold_count)
  
  for (fold_num in 1:fold_count) {

    file_dir =  paste0('fold_', as.character(fold_num))
    test = read.csv(paste0('Proj2_Data/', file_dir, '/test.csv'))
    test =  test %>%
      select(-IsHoliday) %>%
      left_join(test_w_label, by = c('Date', 'Store', 'Dept'))
    
    test_pred = read.csv(paste0('Proj2_Data/', file_dir, '/mypred.csv'))
    
    new_test = test %>%
      left_join(test_pred, by = c('Date', 'Store', 'Dept'))
    
    actuals = new_test$Weekly_Sales
    predictions = new_test$Weekly_Pred
    weights = if_else(new_test$IsHoliday.x, 5, 1)
    wae[fold_num] = sum(weights * abs(actuals - predictions)) / sum(weights)
  }
  if(DEBUG) { cat('fold',fold_num,wae,'\n') } 
  return(wae)
}

post_process = function(prediction, threshold=1.1) { 
  # given our prediction matrix, perform post processing. 
  
  # approach is per campuswire, with default value for threshold taken from 
  # @davidthaler https://github.com/davidthaler/Walmart_competition_code/blob/master/postprocess.R
  
  #@todo figure out if we need to account for a 2.5 vs 2 day shift 
  
  # get baseline sales
  
  # get surge sales
  
  # compare surge to baseline. If surged, shift excess sales 
  
  
  
  return (prediction)
  }



############## Prediction Script Body ############## 
if (DEBUG) {print('Running in debug mode! Disable before submitting!')}

if(DEBUG) {
  # print table header
  cat('\t-----wae-----\t---Time (S)---\n')
} 

for (fold_num in 1:fold_count) {
  if(DEBUG) { cat("reached fold ",fold_num) } 
  
  # Initialize prediction frame, fold name, and gets training and testing data.
  file_dir = paste0('fold_', as.character(fold_num))
  
  train = read.csv(paste0('Proj2_Data/',file_dir, '/train.csv'))
  test = read.csv(paste0('Proj2_Data/',file_dir, '/test.csv')) 
  
  # preallocate output matrix
  out = test
  
  # mutate datasets to create our variables Wk and Yr
  train = train %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    mutate(Yr = year(Date))
  
  test = test %>% 
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    mutate(Yr = year(Date)) 
  
  for(dept in unique(test$Dept)){
    
    # filter for just the dept we want
    train_dept = train %>% filter(Dept == dept)
    test_dept = test %>% filter(Dept == dept)
    
    train_stores = unique(train_dept$Store)
    test_stores = unique(test_dept$Store)
    stores_to_use = intersect(train_stores, test_stores)
    
    for(store in stores_to_use){
      # filter for just the store we want
      train_dept_store = train_dept %>% filter(Store == store)
      test_dept_store = train_dept %>% filter(Store == store)
      
      
      # get design matrix
      train_design = model.matrix(~ Yr + Wk, train_dept_store)
      test_design = model.matrix(~ Yr + Wk, test_dept_store)
      
      # train model
      model_coef = lm(train_dept_store$Weekly_Sales ~ train_design)$coef
      
      # handle 0s for when we don't have correct data
      model_coef[is.na(model_coef)] <- 0
      
      # evaluate model and put outputs back on our df
      pred_dept_store =  model_coef[1] + test_design %*% model_coef[-1]
      test_dept_store$Weekly_Sales = pred_dept_store
      
      tmp_out = test_dept_store[c('Dept', 'Store', 'Date', 'Weekly_Sales')]
      
      out = out %>%
        left_join(tmp_out, by=c('Dept', 'Store', 'Date')) 
    }
  }
  
  # if we didn't make a prediction due to lack of input data, set to 0
  out$Weekly_Sales[is.na(out$Weekly_Sales)] = 0
  
  
  pred_path = paste0('Proj2_Data/', file_dir, '/mypred.csv')
  readr::write_csv(test_pred, pred_path)
}

############## Evaluate Forecast and Estimate Grade ############## 
wae = evaluation()
if(DEBUG) {
  # calulate and print average wae
  avg_wae=mean(wae)
  cat('Average wae', avg_wae,'\n')

  # provide max points given average wae and estimate letter grade
  if(avg_wae > 1680){
    s = 2.5 + 1
    cat('This would yield a max of',s,'points\n')
    cat('F')
  } else if(avg_wae > 1640){
    s = 2.5 + 2
    cat('This would yield a max of',s,'points\n')
    cat('D')
  } else if(avg_wae > 1610){
    s = 2.5 + 3
    cat('This would yield a max of',s,'points\n')
    cat('C')
  } else if(avg_wae > 1600){
    s = 2.5 + 4
    cat('This would yield a max of',s,'points\n')
    cat('B')
  } else if(avg_wae > 1580){
    s = 2.5 + 4.5
    cat('This would yield a max of',s,'points\n')
    cat('A')
  } else {
    s = 2.5 + 5
    cat('This would yield a max of',s,'points\n')
    cat('A+ 100% Perfection')
  }
} 

# Loop through prediction files to write to .csv for debugging.

testpredictions = data.frame()
for (fold_num in 1:fold_count) {
  file_dir = paste0('fold_', as.character(fold_num))
  test_pred = read.csv(paste0('Proj2_Data/', file_dir, '/mypred.csv'))
  testpredictions = rbind(testpredictions, test_pred)
  #print(test_pred)
}    
#readr::write_csv(  testpredictions, 'Proj2_Data/testpred.csv')


