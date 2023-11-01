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
packages = c('tidyverse','lubridate')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

DEBUG = TRUE

set.seed(235)

#######  Functions Called in Main  ####### 

# provides a unique list of departments to loop through
get_depts = function(train_data){
  depts = unique(train_data$Dept)
  return(depts)
}

# filters data to given department and pivots into matrix with date/week and store number as rows/columns
department_matrix = function(train_data, d){
  matrix = train_data %>%
    filter(Dept == d) %>%
    select(Store, Date, Weekly_Sales) %>%
    spread(Store, Weekly_Sales)
  
  #if(DEBUG) { cat("department matrix: n",nrow(matrix),"m",ncol(matrix)) } 
  return(matrix)
}

# Replaces na values with 0. Get average sales for store. Create SVD.
dept_svd = function(X){
  m = ncol(X)-1
  n = nrow(X)
  d = min(m, n, 8)
  #if(DEBUG) { cat("dept_svd 1 n",n,"m",m,"d",d,"\n") } 
  
  X[is.na(X)] = 0
  #if(DEBUG) { cat("dept_svd 2 n",nrow(X),"m",ncol(X)-1,"d\n") } 
  
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
  if(DEBUG) { 
    cat("dept_svd 2 U",nrow(svd_decom$u),"x",ncol(svd_decom$u),
        "D",length(svd_decom$d),"x",length(svd_decom$d),
        "Vt",nrow(svd_decom$v),"x",ncol(svd_decom$v),"\n") } 
  X_s = U %*% D %*% Vt + store_mean
  } else {
  X_s =  X_less_mean + store_mean
  }
  
  #print(X_s)
  #if(DEBUG) { cat("dept_svd 3 n",length(X_s),"m",nrow(X_s),"\n") } 
  return(X_s)
}

# reshapes transposed and pivoted matrix back to original form with rows for date, store, and dept
get_reshape = function(Xmn,column_names,i){
  #if(DEBUG) { cat("get_reshape 1 n",nrow(Xmn),"m",ncol(Xmn),"\n") } 
  
  smoothed = as.data.frame(t(Xmn))
  #if(DEBUG) { cat("get_reshape 2 n",nrow(smoothed),"m",length(smoothed),"\n") } 
  #if(DEBUG) { 
  #  cat("get_reshape 3 n ")
  #  print(column_names)
  #} 
  
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
  fold_count = 10
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

fold_count = 10
for (fold_num in 1:fold_count) {
  
# Initialize prediction frame, fold name, and gets training and testing data.
  file_dir = paste0('fold_', as.character(fold_num))
  predictions = data.frame()
  if(DEBUG) { cat("reached fold ",fold_num) } 
  train = read.csv(paste0('Proj2_Data/',file_dir, '/train.csv'))
  test = read.csv(paste0('Proj2_Data/',file_dir, '/test.csv')) 
  depts = get_depts(train)
  
  if(DEBUG) { cat("loaded data. Departments are",depts,"\n") } 

# Loops through departments, implements SVD for each, reshapes to original form,
# and adds SVD results as predictions
  
  for(i in depts){
    if(DEBUG) { cat("department",i,"\n") } 
    dept=i
    Xi = department_matrix(train,dept)
    cn=colnames(Xi[-1])
    X_smoothed = dept_svd(Xi)
    dept_preds = get_reshape(X_smoothed,cn,dept)
    full_dept = merge(train, dept_preds, by = c("Store", "Date", "Dept"))
    predictions = rbind(predictions, full_dept)
    #if(DEBUG) { cat("loop end n",nrow(predictions),"m",ncol(predictions),"\n") }
  }
  
# Performs offsets done in implementation #1, which seem to realign prediction weeks/dates
  start_last_year = as.Date(min(test$Date)) - 375
  end_last_year = as.Date(max(test$Date)) - 350
  
  tmp_train = predictions %>%
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    rename(Weekly_Pred = Prediction) %>%
    select(-Date, -IsHoliday, -Weekly_Sales)
  
  test_wk = test %>%
    mutate(Wk = week(Date))
  
  test_pred = test_wk %>%
    left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) %>%
    select(-Wk)
  
  id = is.na(test_pred$Weekly_Pred)
  test_pred$Weekly_Pred[id] = 0
  
# Postprocess predictions
  test_pred = post_process(test_pred)
  
# Recreates prior data frames for training and predictions to print for debugging.
  
  tmp_train1 = predictions %>%
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    rename(Weekly_Pred = Prediction) %>%
    select(-IsHoliday)
  
  test_pred1 = test_wk %>%
    left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) 
  
  #print(tmp_train1)
  #print(test_pred1)
  
  if(DEBUG) { cat("fold",fold_num,"test_wk",nrow(test_wk),"m",ncol(test_wk),"\n") }
  if(DEBUG) { cat("fold",fold_num,"test_pred",nrow(test_pred),"m",ncol(test_pred),"\n") }
  
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
fold_count = 10
for (fold_num in 1:fold_count) {
  file_dir = paste0('fold_', as.character(fold_num))
  test_pred = read.csv(paste0('Proj2_Data/', file_dir, '/mypred.csv'))
  testpredictions = rbind(testpredictions, test_pred)
  #print(test_pred)
}    
#readr::write_csv(  testpredictions, 'Proj2_Data/testpred.csv')


