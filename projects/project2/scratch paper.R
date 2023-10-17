
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
