# Project 1
# CS598 Practical Statistical Learning
# Naomi Bhagat - nbhagat3
# Michael Miller - msmille3
# Joe May - jemay3
# 9/17/2023

# Campuswire post: https://campuswire.com/c/G06C55090/feed/193
# PDF instructions https://liangfgithub.github.io/Proj/F23_Proj1.pdf

#######  load libraries  ####### 
# packages to load
packages = c('glmnet', 'randomForest', 'xgboost')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

# TODO set this to FALSE before submitting 
DEBUG = TRUE 

set.seed(235)

# dataset constants for cleaning
winsor_vars = c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", 
                "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", 
                "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", 
                "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", 
                "Enclosed_Porch", "Three_season_porch", 
                "Screen_Porch", "Misc_Val")

# heavily biased, omitted 
vars_to_remove = c('Street', 'Utilities', 'Condition_2', 'Roof_Matl', 
                   'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 
                   'Pool_Area', 'Longitude','Latitude')

clean_input = function(df_in) {
  # Cleans an input df. Df should include ID columns. Optionally include Y
  
  # zero out the Garage Year Built missing data
  df_in$Garage_Yr_Blt[is.na(df_in$Garage_Yr_Blt)] = 0
  
  # 95% winsorization
  for(var in winsor_vars){
    tmp = df_in[ , var]
    quan_val = quantile(tmp, probs = 0.95, na.rm = TRUE)
    tmp[tmp > quan_val] = quan_val
    df_in[ , var] = tmp
  }

  # handle nominals 
  # iterate through the nominals we listed out
  for (i in 1:nrow(nominal_df)) {
    # we need a new column for each value in each nominal
    row = nominal_df[i, ]
    name = row$name
    temp_columns = matrix(0, nrow(df_in), length(row$options[[1]]))
    col_names = vector('character', length=length(row$options[[1]]))
    
    # iterate through options 
    for (j in 1:length(row$options[[1]])) {
      option = row$options[[1]][j]
      
      # temp column should be true if the value is equal to this options
      temp_columns[df_in[, name]==option, j] = 1
      col_names[j] = paste(name, '_', option, sep='')
    }
    
    colnames(temp_columns) = col_names
    df_in = cbind(df_in, temp_columns)
  }
  
  numeric_columns = sapply(df_in, function(x) is.integer(x) || is.numeric(x))
  out_df = df_in[, numeric_columns]
  
  # if there is Sale_Price , put it back at the end
  if ("Sale_Price" %in% names(out_df)) {
    order = c(names(out_df)[names(out_df) != "Sale_Price"], "Sale_Price")
    out_df = out_df[, order]
  }
  return(out_df)
}

force_col_match(source, target) {
  # forces target df to have the same columns and column order as source
  # for missing columns creates columns of 0s 
  
  # get missing columns
  missing_cols = setdiff(colnames(target), colnames(source))
  
  # make zero columns for the missing columns
  missing_df = data.frame(matrix(0, nrow=nrow(target), ncol=length(missing_cols)))
  colnames(missing_df) = missing_col
  
  # add missing columns to target
  target = cbind(target, temp_columns)
  
  # drop extra columns and put columns in order
  correct_cols = colnames(source)
  
  target = target[, correct_cols]
  
  return(target)
} 


print_formatted = function(pred, idx, file_name) {
  # prints output to file matching assignment instruction example 
  
  out_df = data.frame(PID=idx, Sale_Price=pred)
  out_df$Sale_Price = round(out_df$Sale_Price, 1)
  
  # format output per guidance 
  write.table(out_df,
              file=file_name, 
              row.names=FALSE, 
              quote=FALSE, 
              sep = ",  ")
}

get_rmse = function(y_pred, y_actual) {
  # implement per assignment instructions 
  
  n = length(y_actual)
  
  inner_sum = sum((y_actual - y_pred)^2)
  total = sqrt(1/n * inner_sum)
  
  return(total)
}

train_and_eval = function(test_x, train, test_y) {
  # if train_y is not null, prints metrics to screen
  # if train_y is null. does not evaluate and instead prints output to file
  
  ############ LINEAR MODEL  ############
  train_linear = clean_input(train)
  test_x_linear = clean_linear(test_x)
  test_idx_linear = test_x_linear$PID
  
  # inputs need to be matrices 
  train_x_mat = as.matrix(train_linear[ ,2:(ncol(train_linear)-1)]) # omit PID and price
  # we are training model to predict LOG of sale price
  train_y = log(train_linear[, ncol(train_linear)]) # price only
  test_x_mat = as.matrix(test_x_linear[ ,2:ncol(test_x_linear)]) # omit PID
  
  # temp model to get optimal <something>
  start_linear = Sys.time()
  temp_model = cv.glmnet(train_x_mat, train_y, alpha=1)
  selected_vars = predict(temp_model, type="nonzero", s=temp_model$lambda.min)$s1
  
  # actual model for eval 
  model_linear = cv.glmnet(train_x_mat[, selected_vars], train_y, alpha=0)
  stop_linear = Sys.time()
  
  time_linear = as.numeric(difftime(stop_linear, start_linear, units = "secs"))
  
  # get outputs for linear model 
  y_linear = as.vector(predict(model_linear, s=model_linear$lambda.min, newx=test_x_mat[, selected_vars]))
  
  ############ TREE MODEL  ############   
  train_tree = clean_tree(train)
  test_x_tree = clean_tree(test_x)
  test_idx_tree = test_x_tree$PID
  
  train_x_mat = as.matrix(train_tree[ ,2:(ncol(train_tree)-1)]) # omit PID and price
  # we are training model to predict LOG of sale price
  train_y = log(train_tree[, ncol(train_tree)]) # price only
  test_x_mat = as.matrix(test_x_tree[ ,2:ncol(test_x_tree)]) # omit PID
  
  # train and time tree model
  start_tree = Sys.time()
  model_tree = xgboost(data = train_x_mat,
                       label = train_y,
                       max_depth = 6,
                       eta = 0.05,
                       nthread = 2,
                       nrounds = 500,
                       verbose = 0,
                       print_every_n = 0)
  stop_tree = Sys.time()
  
  time_tree = as.numeric(difftime(stop_tree, start_tree, units = "secs"))
  y_tree = as.vector(predict(model_tree, newdata=test_x_mat))
  
  #######  evaluate 
  if(is.null(test_y)) {
    # print output to file 
    print_formatted(exp(y_linear), test_idx_linear, 'mysubmission1.txt')
    print_formatted(exp(y_tree), test_idx_tree, 'mysubmission2.txt')
  } else {
    # print metrics 
    test_y$Sale_Price = log(test_y$Sale_Price)
    
    # first, only get the y values we actually used
    y_actual_linear = subset(test_y, PID %in% test_idx_linear)$Sale_Price
    y_actual_tree = subset(test_y, PID %in% test_idx_tree)$Sale_Price

    rmse_linear = get_rmse(y_linear, y_actual_linear)
    rmse_tree = get_rmse(y_tree, y_actual_tree)
    
    cat(sprintf('%d\t%.3f\t%.3f\t%.3f\t%.3f\t\n',
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
    test_x = read.csv(paste0(file_dir, '/test.csv'))
    test_y = read.csv(paste0(file_dir, '/test_y.csv'))
    train = read.csv(paste0(file_dir, '/train.csv'))
    train_and_eval(test_x, train, test_y)
  } 
} else {
  # only evaluate data in current directory 
  test_x = read.csv(paste0('test.csv'))
  train = read.csv(paste0('train.csv'))
  train_and_eval(test_x, train, NULL)
}
