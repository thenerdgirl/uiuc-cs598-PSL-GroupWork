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

DEBUG = FALSE

set.seed(235)



train_and_eval = function(test_x_raw, train_raw, test_y) {
  # if train_y is not null, prints metrics to screen
  # if train_y is null. does not evaluate and instead prints output to file
  
  ############ CLEAN INPUTS   ############
  # pass through preprocessing
  train = clean_input(train_raw)
  test_x = clean_input(test_x_raw)
  

  
  ############ LINEAR MODEL  ############
  # temp model to get optimal set of variables
  start_linear = Sys.time()

  stop_linear = Sys.time()
  

  
  #######  evaluate 
  if(is.null(test_y)) {
    # print output to file 
    print_formatted(exp(y_linear), test_idx, 'mysubmission1.txt')
    print_formatted(exp(y_tree), test_idx, 'mysubmission2.txt')
  } else {
    # print metrics 
    test_y$Sale_Price = log(test_y$Sale_Price)
    
    # first, only get the y values we actually used
    y_actual = subset(test_y, PID %in% test_idx)$Sale_Price

    rmse_linear = get_rmse(y_linear, y_actual)
    rmse_tree = get_rmse(y_tree, y_actual)
    
    cat(sprintf('%d\t%.4f\t%.4f\t%.3f\t%.3f\t\n',
                fold_num, 
                rmse_linear, 
                rmse_tree, 
                time_linear, 
                time_tree ))
  }
}

############## BEGIN SCRIPT BODY ############## 
if (DEBUG) {print('Running in debug mode! Disable before submitting!')}

#######  load data  ####### 
if(DEBUG) {
  # print table header
  cat('\t-----RMSE-----\t---TIME (S)---\n')
  cat('fold\tlinear\ttree\tlinear\ttree\n')
  
  # evaluate all folds 
  for (fold_num in 1:10) {
    file_dir =  paste0('fold', as.character(fold_num))
    test_x_raw = read.csv(paste0(file_dir, '/test.csv'))
    test_y = read.csv(paste0(file_dir, '/test_y.csv'))
    train_raw = read.csv(paste0(file_dir, '/train.csv'))
    train_and_eval(test_x_raw, train_raw, test_y)
  } 
} else {
  # only evaluate data in current directory 
  test_x = read.csv(paste0('test.csv'))
  train = read.csv(paste0('train.csv'))
  train_and_eval(test_x, train, NULL)
}
