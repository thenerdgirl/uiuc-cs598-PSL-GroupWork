# Project 1
# CS598 Practical Statistical Learning
# Naomi Brhagat - nbhgat3
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

clean_all = function(in_df) { 
  # in_df - dataframe matching format of Ames housing dataset
  # note that this function must work with dataframes but including and excluding the 
  # column Sale_Price
  
  #TODO: Implement me! 
  
  #temp, drop non-numeric columns
  numeric_columns = sapply(in_df, function(x) is.integer(x) || is.numeric(x))
  out_df = in_df[, numeric_columns]
  
  #temp, drop any columns with missing values 
  out_df =  out_df[complete.cases(out_df), ]
  
  return(out_df)
}

clean_linear = function(df_in) {
  # specific cleaning steps for linear model
  
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

print_formatted = function(pred, idx, file_name) {
  
  out_df = data.frame(PID=idx, Sale_Price=pred)
  
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
  
  inner_sum = sum((log(y_actual) - log(y_pred))^2)
  total = sqrt(1/n * inner_sum)
  
  return(total)
}


train_and_eval = function(test_x, train, train_y) {
  # if train_y is not null, prints metrics to screen
  # if train_y is null. does not evaluate and instead prints output to file
  
  #######  clean data  ####### 
  train = clean_all(train)
  test_x = clean_all(test_x)
  
  ####### train models and evaluate #######
  test_idx = test_x$PID
  
  ### model 1 is linear model with ridge penalty, using min lambda 
  
  # suggested lambda sequence to increase performance
  lambdas = exp(seq(-10, 1, length.out = 100))
  
  # inputs need to be two matrices
  train_x_mat = as.matrix(train[, -c(2, ncol(train))])
  test_x_mat = as.matrix(test_x[, -1])
  train_y = train[, ncol(train)]
  
  model_ridge = cv.glmnet(train_x_mat, train_y, alpha=0, lambda=lambdas)
  
  y_ridge = as.vector(predict(model_ridge, s=model_ridge$lambda.min, newx=test_x_mat))
  
  ###  model 2 is randomForest 
  model_rf = randomForest(Sale_Price ~ ., data=train[, -1], ntree = 100)
  
  y_rf = as.vector(predict(model_rf, newdata=test_x))
  
  #######  evaluate 
  if(is.null(train_y)) {
    # print output to file 
    print_formatted(y_ridge, test_idx, 'mysubmission1.txt')
    print_formatted(y_rf, test_idx, 'mysubmission2.txt')
  } else {
    # print metrics 
    
    # first, only get the y values we actually used
    y_actual = subset(test_y, PID %in% test_idx)$Sale_Price
    
    rmse_ridge = get_rmse(y_ridge, y_actual)
    rmse_rf = get_rmse(y_rf, y_actual)
    
    cat(sprintf('%d\t%.3f\t%.3f\t%.3f\t%.3f\t\n',fold_num, rmse_ridge, rmse_rf, 4, 5 ))
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
