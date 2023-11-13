# Project 3
# CS598 Practical Statistical Learning
# Naomi Bhagat - nbhagat3
# Michael Miller - msmille3
# Joe May - jemay3
# 12/4/2023

#######  load libraries  ####### 
# packages to load
packages = c('text2vec', 'glmnet', 'pROC')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

DEBUG = TRUE

set.seed(235)

# read the vocab file
vocab = scan(file = "myvocab.txt", what = character())

# output holder
auc_scores = rep(0, 5)

if(DEBUG) {
  for (split_num in 1:5) {
    start_time = Sys.time()
    split_dir =  paste0('split_', as.character(split_num))
    
    # Get the training data
    train = read.table(paste0(split_dir, '/train.tsv'), stringsAsFactors = FALSE, header = TRUE)
    train$review = gsub('<.*?>', ' ', train$review)
    
    it_train = itoken(train$review, preprocessor = tolower, tokenizer = word_tokenizer)
    
    # use the custom vocab to make the dt matrix for model training
    vectorizer = vocab_vectorizer(create_vocabulary(vocab, ngram = c(1L, 4L)))
    dtm_train = create_dtm(it_train, vectorizer)
    
    # train the model using lasso regression
    model = cv.glmnet(x = dtm_train, y = train$sentiment, alpha = 1, family='binomial', type.measure = c("auc"))
    
    # predict on test data
    test = read.table(paste0(split_dir, '/test.tsv'), stringsAsFactors = FALSE, header = TRUE)
    test$review = gsub('<.*?>', ' ', test$review)
    
    it_test = itoken(test$review, preprocessor = tolower, tokenizer = word_tokenizer)
    
    # use same vectorizer as before to match vocab use between train and test
    dtm_test = create_dtm(it_test, vectorizer)
    
    # get prediction
    prediction = predict(model, s = model$lambda.min, newx = dtm_test, type = 'response')
    
    # Gather the output dataframe with ID and numeric prediction
    output = data.frame(id = test$id, prob = as.numeric(paste(unlist(prediction))))
    
    # evaluate the area under curve
    test_y = read.table(paste0(split_dir, '/test_y.tsv'), header = TRUE)

    # append the test_y points to the output
    output = merge(output, test_y, by="id")

    # From https://www.geeksforgeeks.org/how-to-calculate-auc-area-under-curve-in-r/#
    roc_curve = roc(output$sentiment, output$prob)
    auc_scores[split_num] = pROC::auc(roc_curve)
    end_time = Sys.time()
    print(paste("Score: ", auc_scores[split_num], "Time: ", end_time - start_time))
  }
} else {
  train = read.table('train.tsv', stringsAsFactors = FALSE, header = TRUE)
  train$review = gsub('<.*?>', ' ', train$review)
  
  it_train = itoken(train$review, preprocessor = tolower, tokenizer = word_tokenizer)
  
  # use the custom vocab to make the dt matrix for model training
  vectorizer = vocab_vectorizer(create_vocabulary(vocab, ngram = c(1L, 4L)))
  dtm_train = create_dtm(it_train, vectorizer)
  
  # train the model using lasso regression
  model = cv.glmnet(x = dtm_train, y = train$sentiment, alpha = 1, family='binomial', type.measure = c("auc"))
  
  # predict on test data
  test = read.table('test.tsv', stringsAsFactors = FALSE, header = TRUE)
  test$review = gsub('<.*?>', ' ', test$review)
  
  it_test = itoken(test$review, preprocessor = tolower, tokenizer = word_tokenizer)
  
  # use same vectorizer as before to match vocab use between train and test
  dtm_test = create_dtm(it_test, vectorizer)
  
  # get prediction
  prediction = predict(model, s = model$lambda.min, newx = dtm_test, type = 'response')
  
  # Gather the output dataframe with ID and numeric prediction
  output = data.frame(id = test$id, prob = as.numeric(paste(unlist(prediction))))
  write.table(output, file = 'mysubmission.txt')
}

