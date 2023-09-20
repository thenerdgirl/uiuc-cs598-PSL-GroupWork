# Project 1
# CS598 Practical Statistical Learning
# Naomi Bhagat - nbhagat3
# Michael Miller - msmille3
# Joe May - jemay3
# 9/17/2023

# Campuswire post: https://campuswire.com/c/G06C55090/feed/193
# PDF instructions https://liangfgithub.github.io/Proj/F23_Proj1.pdf

library(glmnet)
library(randomForest)
library(xgboost)

# TODO set this to FALSE before submitting 
DEBUG = TRUE 

# use this to select which fold to use (debug only)
# fold_num = 1

get_rmse = function(y_pred, y_actual) {
  # implement per assignment instructions 
  
  n = length(y_actual)
  
  inner_sum = sum((log(y_actual) - log(y_pred))^2)
  total = sqrt(1/n * inner_sum)
  
  return(total)
}

### Pre-Processing Functions ###

clean_data_linear = function(df_in) {
  # Remove the PID as a variable
  df_in = subset(df_in, select = -PID)
  
  # zero out the Garage Year Built missing data
  df_in$Garage_Yr_Blt[is.na(df_in$Garage_Yr_Blt)] = 0
  
  # remove categorical variables
  vars_to_remove = c('Street', 'Utilities', 'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area', 'Longitude','Latitude')
  df_in = df_in[ , -which(names(df_in) %in% vars_to_remove)]
  
  # 95% winsorization
  winsor_vars = c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")
  for(var in winsor_vars){
    tmp = df_in[ , var]
    quan_val = quantile(tmp, probs = 0.95, na.rm = TRUE)
    tmp[tmp > quan_val] = quan_val
    df_in[ , var] = tmp
  }
  
  # the k stuff??
  categorical_vars = colnames(df_in)[which(sapply(df_in, function(x) mode(x)=="character"))]
  df_out = df_in[, !colnames(df_in) %in% categorical_vars, drop=FALSE]
  n_train = nrow(df_out)
  
  for(var in categorical_vars){
    mylevels = sort(unique(df_in[, var]))
    m = length(mylevels)
    m = ifelse(m>2, m, 1)
    tmp.train = matrix(0, n_train, m)
    col.names = NULL
    for(j in 1:m){
      tmp.train[df_in[, var]==mylevels[j], j] = 1
      col.names = c(col.names, paste(var, '_', mylevels[j], sep=''))
    }
    colnames(tmp.train) = col.names
    df_out = cbind(df_out, tmp.train)
  }

  return(df_out)
}

for (fold_num in 1:10) {
  ### Train Model ###
  # TODO switch from DEBUG mode at some point
  file_dir =  paste0('fold', as.character(fold_num))
  test_x = read.csv(paste0(file_dir, '/test.csv'))
  test_y = read.csv(paste0(file_dir, '/test_y.csv'))
  train = read.csv(paste0(file_dir, '/train.csv'))
  
  train_linear = clean_data_linear(train)

  # Linear model
  set.seed(235)
  lambdas = exp(seq(-10, 1, length.out = 100))

  sapply(train_linear, class)

  # model_linear = cv.glmnet(train_x_mat, train_y, alpha=0, lambda=lambdas)
  
  # Use prof's method
  set.seed(235)
  # model 1
  temp_model = cv.glmnet(as.matrix(train_linear), log(train$Sale_Price), alpha=1)
  selected_vars = predict(temp_model, type="nonzero", s=temp_model$lambda.min)$s1
  selected_vars
  # model 2
  model_linear = cv.glmnet(as.matrix(train_linear[, selected_vars]), train_y, alpha=0)
  
  ### Test Prediction Output ###
  # Same pre-processing
  test_pid = test_x$PID
  test_linear = clean_data_linear(test_x)
  
  linear_pred = as.vector(predict(model_rf, newdata = test_tree, na.action = na.pass))
  # tree_output = data.frame(PID = test_pid, Sale_Price = exp(tree_pred))
  
  # Check rmse
  y_actual = subset(test_y, PID %in% test_pid)$Sale_Price
  
  # rmse_ridge = get_rmse(y_ridge, y_actual)
  rmse_rf = get_rmse(linear_pred, y_actual)
  
  # rmse_ridge
  print(rmse_rf)
}




