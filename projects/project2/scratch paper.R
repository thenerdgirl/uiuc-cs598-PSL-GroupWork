
# Approach I

library(tidyverse)
library(lubridate)

i <- 1
file_path = paste0('Proj2_Data/fold_', i, '/train.csv')
train = read.csv(file_path)

num_folds <- 10
for (i in 1:num_folds) {
  file_path = paste0('Proj2_Data/fold_', i, '/train.csv')
  train = read.csv(file_path)
  
  file_path = paste0('Proj2_Data/fold_', i, '/test.csv')
  test = read.csv(file_path)
  
  most_recent_date <- max(train$Date)
  tmp_train <- train %>%
    filter(Date == most_recent_date) %>%
    rename(Weekly_Pred = Weekly_Sales) %>%
    select(-Date, -IsHoliday)
  
  test_pred <- test %>%
    left_join(tmp_train, by = c('Dept', 'Store')) 
  
  # assign zero to missing predictions
  id = is.na(test_pred$Weekly_Pred)
  test_pred$Weekly_Pred[id] = 0
  
  file_path = paste0('Proj2_Data/fold_', i, '/mypred.csv')
  write.csv(test_pred, file_path)
}

myeval = function(){
  file_path = paste0('Proj2_Data/test_with_label.csv')
  test_with_label = read.csv(file_path)
  num_folds = 10
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
wae = myeval()
print(wae)
mean(wae)

# Delete files

for (i in 1:num_folds) {
  file_path = paste0('Proj2_Data/fold_', i, '/mypred.csv')
  file.remove(file_path)
}

# Use approach 2
num_folds <- 10
for (i in 1:num_folds) {
  file_path = paste0('Proj2_Data/fold_', i, '/train.csv')
  train = read.csv(file_path)
  
  file_path = paste0('Proj2_Data/fold_', i, '/test.csv')
  test = read.csv(file_path)
  
  start_last_year = as.Date(min(test$Date)) - 375
  end_last_year = as.Date(max(test$Date)) - 350
  
  tmp_train <- train %>%
    filter(Date > start_last_year & Date < end_last_year) %>%
    mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
    rename(Weekly_Pred = Weekly_Sales) %>%
    select(-Date, -IsHoliday)
  
  test <- test %>%
    mutate(Wk = week(Date))
  
  test_pred <- test %>%
    left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) %>%
    select(-Wk)
  
  id = is.na(test_pred$Weekly_Pred)
  test_pred$Weekly_Pred[id] = 0
  
  file_path = paste0('Proj2_Data/fold_', i, '/mypred.csv')
  readr::write_csv(test_pred, file_path)
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

get_depts = function(train_data){
  depts = unique(train_data$Dept)
  return(depts)
}

#test_w_label_path = paste0('Proj2_Data/test_with_label.csv')
#test_w_label = read.csv(test_w_label_path)

train = read.csv(paste0('Proj2_Data/fold_1/train.csv'))
test = read.csv(paste0('Proj2_Data/fold_1/test.csv')) 

traind=get_depts(train)
#testd=get_depts(test)


depts = get_depts(train)
print(depts)
print(length(depts))

predictions = data.frame()

for(i in depts){
  dept=i
  Xi = department_matrix(train,dept)
  X_smoothed = dept_svd(Xi)
  dept_preds = get_reshape(X_smoothed,dept)
  full_dept = merge(train, dept_preds, by = c("Store", "Date", "Dept"))
  predictions = rbind(predictions, full_dept)
}

predictions %>%
  arrange(Store, Dept, Date)

Xi = department_matrix(train,77)
length(Xi)
nrow(Xi)
X_smoothed = dept_svd(Xi)


start_last_year = as.Date(min(test$Date)) - 375
end_last_year = as.Date(max(test$Date)) - 350

tmp_train = train %>%
  filter(Date > start_last_year & Date < end_last_year) %>%
  mutate(Wk = ifelse(year(Date) == 2010, week(Date)-1, week(Date))) %>%
  rename(Weekly_Pred = Weekly_Sales) %>%
  select(-Date, -IsHoliday)

test_wk = test %>%
  mutate(Wk = week(Date))

test_pred = test_wk %>%
  left_join(tmp_train, by = c('Dept', 'Store', 'Wk')) %>%
  select(-Wk)

id = is.na(test_pred$Weekly_Pred)
test_pred$Weekly_Pred[id] = 0

dept_svd = function(X){
  m = length(X)
  n = nrow(X)
  d = min(m,n)
  X[is.na(X)] = 0
  store_mean = rowMeans(t(X[,-1]))
  X_less_mean = t(X[,-1])-store_mean

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


