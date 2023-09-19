# Project 1
# CS598 Practical Statistical Learning
# Naomi Brhagat - nbhgat3
# Michael Miller - msmille3
# Joe May - jemay3
# 9/17/2023

# Campuswire post: https://campuswire.com/c/G06C55090/feed/193
# PDF instructions https://liangfgithub.github.io/Proj/F23_Proj1.pdf

# TODO set this to FALSE before submitting 
DEBUG = TRUE 
if (DEBUG) {print('Running in debug mode! Disable before submitting!')}

# use this to select which fold to use (debug only)
fold_num = 1


clean_data = function(in_df) { 
  # in_df - dataframe matching format of Ames housing dataset
  # note that this function must work with dataframes but including and excluding the 
  # column Sale_Price
  
  #TODO: Implement me! 
  out_df = in_df
  
  return(out_df)
  }

print_formatted = function(df, file=file_name) {
  # format output per guidance 
  write.table(df,
              file=file, 
              row.names=FALSE, 
              quote=FALSE, 
              sep = ",  ")
  }

# load data
if(DEBUG) {
  file_dir =  paste0('fold', as.character(fold_num))
  test_x = read.csv(paste0(file_dir, '/test.csv'))
  test_y = read.csv(paste0(file_dir, '/test_y.csv'))
  train = read.csv(paste0(file_dir, '/train.csv'))
} else {
  test_x = read.csv(paste0('test.csv'))
  train = read.csv(paste0('train.csv'))
}
  
# clean data
train = clean_data(train)
test_x = clean_data(test_x)

# train models

# get predictions
pred1 = test_y
pred2 = test_y

# evaluate 

# write output
print_formatted(pred1, file='mysubmission1.txt')
write.csv(pred2, file='mysubmission2.txt')






