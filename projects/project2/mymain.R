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
packages = c('lubridate', 'tidyverse','glmnet', 'xgboost')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

DEBUG = TRUE

set.seed(235)

get_depts = function(train_data){
  depts = unique(train_data$Dept)
  return(depts)
}

department_matrix = function(train_data, d){
  matrix = train_data %>%
    filter(Dept == d) %>%
    select(Store, Date, Weekly_Sales) %>%
    spread(Store, Weekly_Sales)
  
  if(DEBUG) {
    print(matrix)
  } 
  
  return(matrix)
}

dept_svd = function(X){
  d = min(length(X)-1, nrow(X), 8)
  cat(length(X)-1, nrow(X),d)
  X[is.na(X)] = 0
  store_mean = rowMeans(t(X[,-1]))
  X_less_mean = t(X[,-1]) - store_mean
  
  svd_decom = svd(X_less_mean)
  U = svd_decom$u[,1:d]
  D = diag(svd_decom$d[1:d])
  Vt = t(svd_decom$v[,1:d])
  
  X_s = U %*% D %*% Vt + store_mean
  return(X_s)
}

get_reshape <- function(Xmn,i){
  smoothed = as.data.frame(t(Xmn))
  colnames(smoothed) = colnames(Xi[,-1])
  smoothed$Date <- Xi[,1]
  pivot_smooth = gather(smoothed, key = "Store", value = "Prediction", -Date)
  pivot_smooth$Dept = i
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
  if(DEBUG) {
    # print table header
    cat('fold',fold_num,wae,'\n')
  } 
  return(wae)
}



############## Prediction Script Body ############## 
if (DEBUG) {print('Running in debug mode! Disable before submitting!')}

if(DEBUG) {
  # print table header
  cat('\t-----wae-----\t---Time (S)---\n')
} 

fold_count = 10
for (fold_num in 1:fold_count) {
  
  file_dir = paste0('fold_', as.character(fold_num))
  predictions = data.frame()
  
  if(DEBUG) {
    print("reached fold ")
    print(fold_num)
  } 
  
  train = read.csv(paste0('Proj2_Data/',file_dir, '/train.csv'))
  test = read.csv(paste0('Proj2_Data/',file_dir, '/test.csv')) 
  depts = get_depts(train)
  
  if(DEBUG) {
    print("loaded data. Departments are")
    print(depts)
  } 
  
  for(i in depts){
    if(DEBUG) {
      print("department")
      print(i)
    } 
    dept=i
    Xi = department_matrix(train,dept)
    X_smoothed = dept_svd(Xi)
    dept_preds = get_reshape(X_smoothed,dept)
    full_dept = merge(train, dept_preds, by = c("Store", "Date", "Dept"))
    predictions = rbind(predictions, full_dept)
  }
  
  start_last_year = as.Date(min(test$Date)) - 375
  end_last_year = as.Date(max(test$Date)) - 350
  
  tmp_train = predictions %>%
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    rename(Weekly_Pred = Prediction) %>%
    select(-Date, -IsHoliday)
  
  test_wk = test %>%
    mutate(Wk = week(Date))
  
  test_pred = test_wk %>%
    left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) %>%
    select(-Wk)
  
  id = is.na(test_pred$Weekly_Pred)
  test_pred$Weekly_Pred[id] = 0
  
  if(DEBUG) {
  # print table header
  cat('fold',fold_num,'\n')
  } 
  
  pred_path = paste0('Proj2_Data/', file_dir, '/mypred.csv')
  readr::write_csv(test_pred, pred_path)
  
  if(!DEBUG) {
    print("start_last_year")
    print(start_last_year)
    print(min(test$Date))
    print("end_last_year")    
    print(end_last_year)   
    print(max(test$Date))
    print("train")
    print(head(train))
    print("tmp_train")
    print(head(tmp_train))
    #print("test")
    #print(head(test))
    print("test_pred")
    print(head(test_pred))
  } 
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
