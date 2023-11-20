# Project 3 - Vocab Generator Script
# CS598 Practical Statistical Learning
# Naomi Bhagat - nbhagat3
# Michael Miller - msmille3
# Joe May - jemay3
# 12/4/2023

#######  load libraries  ####### 
# packages to load
packages = c('text2vec', 'glmnet')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

# set seed
set.seed(235)

# set the stop words
stop_words = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "their", "they", 
               "his", "her", "she", "he", "a", "an", "and", "is", "was", "are", "were", "him", "himself", "has", "have",
               "it", "its", "the", "us")

# top points vocab size
vocab_size = 1000

train = read.table("split_1/train.tsv", stringsAsFactors = FALSE, header = TRUE)

for (split_num in 2:5) {
  split_dir =  paste0('split_', as.character(split_num))
  temp_train = read.table(paste0(split_dir, '/train.tsv'), stringsAsFactors = FALSE, header = TRUE)
  train = rbind(train, temp_train)
}

##### Professor-Provided Code #####
# clear out the html tags in the review
train$review = gsub('<.*?>', ' ', train$review)

# create an iterator over tokens
it_train = itoken(train$review, preprocessor = tolower, tokenizer = word_tokenizer)

# build the vocabulary
tmp.vocab = create_vocabulary(it_train, stopwords = stop_words, ngram = c(1L,4L))

# prune the vocabulary
tmp.vocab = prune_vocabulary(tmp.vocab, term_count_min = 10, doc_proportion_max = 0.5, doc_proportion_min = 0.001)

# construct a document-term matrix
dtm_train = create_dtm(it_train, vocab_vectorizer(tmp.vocab))
##### Professor-Provided Code #####

# fit the model
# StackOverflow: why glmnet was right here and not cv.glmnet: https://stackoverflow.com/questions/29311323/difference-between-glmnet-and-cv-glmnet-in-r
model = glmnet(x = dtm_train, y = train$sentiment, alpha = 1, family="binomial")

# remove beta values less than our intended vocab size to keep vocab size low
nonzero_beta = colSums(model$beta != 0)
col_index = unname(which.max(which(nonzero_beta < vocab_size)))
vocabulary = colnames(dtm_train)[which(model$beta[, col_index] != 0)]

write.table(vocabulary, file = "myvocab.txt", row.names = FALSE, col.names = FALSE, sep = "", quote = FALSE)
