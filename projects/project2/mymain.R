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
packages = c('tidyr', 'tidyverse','lubridate')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

DEBUG = FALSE
if (DEBUG) {print('Running in debug mode! Disable before submitting!')}

num_folds = 10
set.seed(235)

# amount of sales to shift for christmas edge case (see https://campuswire.com/c/G06C55090/feed)
shift = 1/14

#######  Functions Called in Main  ####### 

# Given raw input data and department dept, gives us the 
# cleaned matrix X as defined https://campuswire.com/c/G06C55090/feed/364
# rows are stores, columns are weeks 
spread_df = function(train, dept){
  
  X = train %>%
    filter(Dept == dept) %>%
    select(Store, Date, Weekly_Sales) %>%
    spread(key=Store, value=Weekly_Sales)
  
  # preserve info from X
  dates = X[, 1]
  stores = colnames(X)[-1]
  
  # replace NAs with 0 
  X[is.na(X)] = 0 
  
  # drop date row, take transpose
  X = X[, -1]
  X = t(X)
  
  spread_out = list(X=X, stores=stores, dates=dates)
  
  return(spread_out)
}

# Given X matrix (rows are stores, columns are weeks)
# pivot it back into format of test/train dataframes
gather_mat = function(X, spread_out, dept) {
  
  # Change to df and add store column names back
  X_df = data.frame(t(X))
  colnames(X_df) = spread_out$stores
  X_df$Date = spread_out$dates
  
  # now apply gather to pivot table back to tall
  gathered = X_df %>% 
    gather(key="Store", value="Weekly_Sales", -Date) %>% 
    mutate(Store = as.integer(Store))
  
  # re-add department
  gathered$Dept = dept
  gathered$Dept = as.integer(dept)
  
  # now get back Wk and Yr
  out = gathered %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>%
    mutate(Yr = year(Date))
  return(out)
}

# main execution function, given a directory, load data, train model
# make predictions and write them to mypred.csv
process_fold = function(file_dir){
  
  # load data
  train_raw = read.csv(paste0(file_dir, 'train.csv'))
  test_raw = read.csv(paste0(file_dir, 'test.csv')) 
  
  # go ahead and clean test now, we will clean train later
  test = test_raw %>%
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>%
    mutate(Yr = year(Date))
  train = train_raw
  
  # preallocate output matrix
  # note that if we don't predict a value, we just return 0
  # we don't predict a value when we don't have enough data
  out = test_raw
  out$Weekly_Pred = 0
  
  #counter for printing debug messages
  current_dept = 1
  full_depts = length(unique(test$Dept))
  
  # we will only train model for departments in both test and train 
  dept_to_eval = intersect(unique(test$Dept), unique(train$Dept))
  
  # iterate through departments 
  for(dept in dept_to_eval){
    # log for debugging
    if (DEBUG) {cat("Department", current_dept, "of", full_depts, "\n")}
    current_dept = current_dept + 1
    
    # convert tall train matrix into wide X (rows are stores, col dates)
    spread_out = spread_df(train, dept)
    X = spread_out$X
    
    # if dataset is big enough to do SVD, do it, else just use X
    # SVD keeps dimensions the same but reduces noise and populates values 
    # that would otherwise be NA
    r = min(dim(X), 8)
    if (r == 8) {
      # implement SVD 
      store_means = rowMeans(X)
      demeaned = X - store_means
      
      svd_results = svd(demeaned)
      u = svd_results$u
      vt = t(svd_results$v)
      d = diag(svd_results$d)
      x_tilde = u[, 1:r] %*% d[1:r, 1:r] %*% vt[1:r, ] + store_means
    } else {
      x_tilde = X
    }
    
    # now convert back into the format of train df
    smoothed_train = gather_mat(x_tilde, spread_out, dept)
    
    # now iterate through stores
    for(store in unique(smoothed_train$Store)){
      
      # filter for just the store we want
      train_dept_store = smoothed_train %>% filter(Store == store)
      test_dept_store = test %>% filter(Store == store & Dept == dept)
      
      # get design matrix, note that we include Yr^2 as a feature
      train_design = model.matrix(~ Yr + I(Yr^2) + Wk, train_dept_store)
      test_design = model.matrix(~ Yr + I(Yr^2) + Wk, test_dept_store)
      
      # train model
      model_coef = lm(train_dept_store$Weekly_Sales ~ train_design)$coef
      
      # handle 0s for when coef is 0 (e.g. coliniearity case for Yr^2 when data justh as 1 year)
      model_coef[is.na(model_coef)] = 0
      
      # evaluate model
      pred = model_coef[1] + test_design %*% model_coef[-1]
      test_dept_store$Weekly_Pred = as.numeric(pred[, 1])
      tmp_out = test_dept_store[c('Dept', 'Store', 'Date', 'Weekly_Pred')]
      
      # postprocess, we are looking for fold 5 edge case 
      # in fold 5, Christmas falls earlier in the week so we need to pivot some sales 
      too_high = tmp_out %>% filter(Date == '2011-12-23') 
      if (nrow(too_high) > 0) {
        shift_val = too_high$Weekly_Pred * shift
        
        # now apply the shift, remove sales from Christmas week, add them to week after
        tmp_out[tmp_out$Date == '2011-12-23', 'Weekly_Pred'] = tmp_out[tmp_out$Date == '2011-12-23', 'Weekly_Pred'] - shift_val
        tmp_out[tmp_out$Date == '2011-12-30', 'Weekly_Pred'] = tmp_out[tmp_out$Date == '2011-12-30', 'Weekly_Pred'] + shift_val
      }
      
      # now we join our results back into out dataframe for reporting
      out = out %>% 
        left_join(tmp_out, by=c('Dept', 'Store', 'Date')) %>%
        mutate(Weekly_Pred = coalesce(Weekly_Pred.y, Weekly_Pred.x)) %>% 
        select(-Weekly_Pred.x, -Weekly_Pred.y)
    }
  }
  
  pred_path = paste0(file_dir, 'mypred.csv')
  readr::write_csv(out, pred_path)
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

############## Prediction Script Body ############## 
if (!DEBUG) {
  # in production mode, we just evaluate the files in this dir and output results
  file_dir = ""
  process_fold(file_dir)
} else {
  #preallocate run time holder
  run_times = rep(0, num_folds)
  
  # in debug mode, process all folds 
  for (fold_num in 1:num_folds) {
    cat("Fold",fold_num, "\n")
    file_dir = paste0('Proj2_Data/fold_', as.character(fold_num), '/')
    
    clock_start = Sys.time()
    process_fold(file_dir)
    clock_stop = Sys.time()
    
    # save time
    run_times[fold_num] = as.numeric(difftime(clock_stop, clock_start, units = "secs"))
  }
  
  # evaluate results 
  wae = myeval()
  
  # output metrics for report
  #header
  cat('Fold\t--WAE--\t\tTIME (S)\n')
  for (fold_num in 1:num_folds) {
    # rows
    cat(sprintf('%d\t%.3f\t%.2f\n',
                fold_num, 
                wae[fold_num], 
                run_times[fold_num]))
  }
  cat('\n\n')
  
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