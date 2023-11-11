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

num_folds = 1

set.seed(235)

#######  Functions Called in Main  ####### 

# Given raw input data and department dept, gives us the 
# cleaned matrix X as defined https://campuswire.com/c/G06C55090/feed/364
# rows are stores, columns are weeks 
get_dept_matrix = function(train, dept){
  X = train %>%
    filter(Dept == dept) %>%
    select(Store, Date, Weekly_Sales) %>%
    spread(Store, Weekly_Sales)
  
  # replace NAs with 0 
  #X[is.na(X)] = 0 
  
  # drop date row, and take transpose
  #X = X[, -1]
  #X = t(X)
  
  return(X)
}

# Replaces na values with 0. Get average sales for store. Create SVD.
get_svd = function(X){
  m = ncol(X)-1
  n = nrow(X)
  d = min(m, n, 8)
  
  # remove store means
  store_means = rowMeans(X)
  demeaned = X - store_means
  
  # implement SVD 
  svd_results = svd(demeaned)
  
  u * s * vt
  
  
  
  
  
  
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
myeval = function(){
  file_path = paste0('Proj2_Data/test_with_label.csv')
  test_with_label = read.csv(file_path)
  wae = rep(0, num_folds)
  
  for (i in 1:num_folds) {
    file_path = paste0('Proj2_Data/fold_', i, '/test.csv')
    test = read.csv(file_path)
    test =  test %>%
      select(-IsHoliday) %>%
      left_join(test_with_label, by = c('Date', 'Store', 'Dept'))
    
    file_path = paste0('Proj2_Data/fold_', i, '/mypred.csv')
    test_pred = read.csv(file_path)
    
    new_test <- test %>%
      left_join(test_pred, by = c('Date', 'Store', 'Dept'))
    
    actuals = new_test$Weekly_Sales
    preds = new_test$Weekly_Pred
    weights = if_else(new_test$IsHoliday.x, 5, 1)
    wae[i] = sum(weights * abs(actuals - preds)) / sum(weights)
  }
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

for (fold_num in 1:num_folds) {
  if(DEBUG) { cat("Fold",fold_num, "\n") } 
  
  # Initialize prediction frame, fold name, and gets training and testing data.
  file_dir = paste0('fold_', as.character(fold_num))
  
  train_raw = read.csv(paste0('Proj2_Data/',file_dir, '/train.csv'))
  test_raw = read.csv(paste0('Proj2_Data/',file_dir, '/test.csv')) 
  
  # preallocate output matrix
  out = test_raw
  out$Weekly_Pred = 0
  
  # we use efficient cleaning as defined https://liangfgithub.github.io/Proj/F23_R_Proj2_hints.html#Efficient_Implementation_for_III
  
  # find the unique pairs of (Store, Dept) combo that appeared in both training and test sets
  train_pairs = train_raw[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
  test_pairs = test_raw[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
  both_pairs = intersect(train_pairs[, 1:2], test_pairs[, 1:2])
  
  # pick out the needed training samples, convert to dummy coding, then put them into a list
  train = both_pairs %>% 
    left_join(train, by = c('Store', 'Dept')) %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>%
    mutate(Yr = year(Date))
  
  test = both_pairs %>% 
    left_join(test, by = c('Store', 'Dept')) %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>%
    mutate(Yr = year(Date))
  
  #counter for printing
  current_dept = 1
  full_depts = length(unique(test$Dept))
  
  # iterate through departments 
  for(dept in unique(test$Dept)){
    cat("Department", current_dept, "of", full_depts, "\n")
    current_dept = current_dept + 1
    
    # X is 
    #X = get_dept_matrix(train, dept)
    
    #X_tilde = get_svd(X)
    
    # now iterate through the stores we have
    test_dept = test %>% filter(Dept == dept)
    stores = unique(test_dept$Store)
    for(store in stores){
      
      # filter for just the store we want
      train_dept_store = train %>% filter(Store == store & Dept == dept)
      test_dept_store = test %>% filter(Store == store & Dept == dept)
      
      # get design matrix
      train_design = model.matrix(~ Yr + Wk, train_dept_store)
      test_design = model.matrix(~ Yr + Wk, test_dept_store)
      
      # train model
      model_coef = lm(train_dept_store$Weekly_Sales ~ train_design)$coef
      
      # handle 0s for when we don't have correct data
      model_coef[is.na(model_coef)] = 0
      
      # evaluate model and put outputs back on our df
      pred = model_coef[1] + test_design %*% model_coef[-1]
      
      test_dept_store$Weekly_Pred = as.numeric(pred[, 1])
      
      # now we join our results back into out
      tmp_out = test_dept_store[c('Dept', 'Store', 'Date', 'Weekly_Pred')]
      
      out = out %>% 
        left_join(tmp_out, by=c('Dept', 'Store', 'Date')) %>%
        mutate(Weekly_Pred = coalesce(Weekly_Pred.y, Weekly_Pred.x)) %>% 
        select(-Weekly_Pred.x, -Weekly_Pred.y)
    }
  }
  
  pred_path = paste0('Proj2_Data/', file_dir, '/mypred.csv')
  readr::write_csv(out, pred_path)
}

############## Evaluate Forecast and Estimate Grade ############## 
wae = myeval()
if(DEBUG) {
  cat(wae, '\n')
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
