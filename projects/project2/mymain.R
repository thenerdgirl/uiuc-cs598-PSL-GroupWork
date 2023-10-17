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



train_and_eval = function(test_x_raw, train_raw, test_y) {
  # if train_y is not null, prints metrics to screen
  # if train_y is null. does not evaluate and instead prints output to file
  
  ############ CLEAN INPUTS   ############
  # pass through preprocessing
  
  #train = clean_input(train_raw)
  #test_x = clean_input(test_x_raw)
  

  
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
    file_dir =  paste0('fold_', as.character(fold_num))
    test = read.csv(paste0('Proj2_Data/',file_dir, '/test.csv'))
    train = read.csv(paste0('Proj2_Data/',file_dir, '/train.csv'))
    print(file_dir)
    print(test)
    print(train)
    #test_y = read.csv(paste0('Proj2_Data/test_with_label.csv'))
  } 
} else {
  # only evaluate data in current directory 
  test_y = read.csv(paste0('Proj2_Data/test_with_label.csv'))
}
