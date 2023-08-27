
# Coding Assignment 1
# Naomi Bhagat, Michael Miller, & Joe May
# nbhagat3, msmille3, jemay3

# Set Seed
set.seed(235)

### Part 1: Generate Data ###

# import necessary libraries
library(class)

# 1. First generate the 20 centers from two-dimensional normal. You can use any
#   mean and covariance structure.  You should not regenerate the centers. Use
#   these 20 centers throughout this simulation study. 

csize = 10       
p = 2        
s = sqrt(1/5)                         
m1 = matrix(rnorm(csize*p), csize, p)*s + cbind( rep(1,csize), rep(0,csize) )
m0 = matrix(rnorm(csize*p), csize, p)*s + cbind( rep(0,csize), rep(1,csize) )

# 2. Given the 20 centers, generate a training sample of size 200 (100 from each
#   class) and a test sample of size 10,000 (5,000 from each class). 

# Training sample of size 200

n = 100
id1 = sample(1:csize, n, replace=TRUE)
id0 = sample(1:csize, n, replace=TRUE)  
traindata = matrix(rnorm(2*n*p), 2*n, p)*s + rbind(m1[id1,], m0[id0,])
dim(traindata)
#Ytrain = factor(c(rep(1,n), rep(0,n)))
Ytrain = rep(c(1, 0), each = n)

# Test sample of size 10,000

N = 5000
# Allocate the n samples for class 1  to the 10 clusters
id1 = sample(1:csize, N, replace=TRUE)
# Allocate the n samples for class 1 to the 10 clusters
id0 = sample(1:csize, N, replace=TRUE)  
testdata = matrix(rnorm(2*N*p), 2*N, p)*s + rbind(m1[id1,], m0[id0,])
dim(testdata)
#Ytest = factor(c(rep(1,N), rep(0,N)))
Ytest = rep(c(1, 0), each = N)

# 3. Produce a scatter plot of the training data:
# a. assign different colors to the two classes of data points;
# b. overlay the 20 centers on this scatter plot, using a distinguishing marker
#   (e.g., a star or a different shape) and color them according to their
#   respective class.

plot(traindata[,1], traindata[,2], type="n", xlab="", ylab="")
points(traindata[1:n,1], traindata[1:n,2], col="green")
points(traindata[(n+1):(2*n),1], traindata[(n+1):(2*n),2], col="red")
points(m1[1:csize,1], m1[1:csize,2], pch="+", cex=1.5, col="green")
points(m0[1:csize,1], m0[1:csize,2], pch="+", cex=1.5, col="red");   
legend("bottomright", pch = c(1,1), col = c("red", "green"), legend = c("class 1", "class 0"))

### Part 2: kNN ###

# 1. Implement kNN from scratch; use Euclidean Distance. Your implementation
#   should meet the following requirements:
# Input: Your kNN function should accept three input parameters: training data, 
#   test data, and k. No need to write your kNN function to handle any general
#   input; it suffices to write a function that is able to handle the data for
#   this specific simulation study: binary classification; features are
#   two-dimensional numerical vectors.
# Output: Your function should return a vector of predictions for the test data.
# Vectorization: Efficiently compute distances between all test points and
#   training points simultaneously. Make predictions for all test points in a
#   single operation.
# No Loops: Do not use explicit loops like for or while inside your kNN function
#   to compute distances or make predictions. Instead, harness the power of
#   vectorized operations for efficient computations. For example, you can use
#   broadcasting in Numpy or command outer in R.

# Euclidean helper function
euclidean_distance = function(point1, point2) {
  if (length(point1) == length(point2)) {
    return(sqrt(sum((point1 - point2)^2)))
  } else {
    stop('Vectors must be of the same length')
  }
}


knn_from_scratch = function(train, test, truth, k) {
  neighbor_distances = outer(1:nrow(test), 1:nrow(train), Vectorize(function(i, j) euclidean_distance(test[i,], train[j,])))
  # Calculate each point's distance to neighbors
  
  nearest_neighbor_address = function(vector, j){
    order(vector)[1:j]
  }
  # Function to get k nearest neighbors (temp variable j for k)
  
  nearest_neighbor_address = t(apply(neighbor_distances, 1, nearest_neighbor_address, j = k))
  # Apply function to get k nearest neighbors for each point in a vectorized manner
  # nearest_neighbor_address
  
  get_vote = function(indices, vote){
    vote[indices]
  }
  # Function to collect votes for each point's k nearest neighbors
  
  neighborhood_votes = t(apply(nearest_neighbor_address, 1, get_vote, vote = truth))
  # Apply function for each point to collect votes for each point's 
  # k nearest neighbors in a vectorized manner
  # neighborhood_votes
  
  determine_winner = function(point){
    if (sum(point) > k/2) {
      return(1)
    } else if (sum(point) < k/2) {
      return(0)
    } else {
      return(sample(0:1, 1))
    }
  }
  # Determine winner given votes of k nearest neighbors. Ties are broken randomly.
  
  predictions = apply(neighborhood_votes, 1, determine_winner)
  return(predictions)
}

# 2. Explain how you handle distance ties and voting ties; distance ties may
#   occur when you have multiple (training) observations that are equidistant
#   from a test observation. voting ties may occur when K is an even number and
#   you have 50% of the k-nearest-neighbors from each of the two classes.
  
# Ans: If the number of votes for each point is the same, we have decided to 
#   randomly generate a class for that particular test point because the two
#   classes at that point are equally as likely.

#  3. Test your code with the training/test data you just generated when 
#   K = 1, 3, 5; and compare your results with knn in R or sklearn.neighbors in
#   Python. Report your results (on the test data) as a 2-by-2 table
#   (confusion matrix) for each K value. Report the results from knn or
#   sklearn.neighbors as a 2-by-2 table (confusion matrix) for each K value

# k = 1
baseline_test.pred = knn(traindata, testdata, Ytrain, k=1)
table(Ytest, baseline_test.pred)
implementation_test.pred = knn_from_scratch(traindata, testdata, Ytrain, k=1)
table(Ytest, implementation_test.pred)

# k = 3
baseline_test.pred = knn(traindata, testdata, Ytrain, k=3)
table(Ytest, baseline_test.pred)
implementation_test.pred = knn_from_scratch(traindata, testdata, Ytrain, k=3)
table(Ytest, implementation_test.pred)

# k = 5
baseline_test.pred = knn(traindata, testdata, Ytrain, k=5)
table(Ytest, baseline_test.pred)
implementation_test.pred = knn_from_scratch(traindata, testdata, Ytrain, k=5)
table(Ytest, implementation_test.pred)

### Part 3: cvkNN ###

### Using this as reference? https://medium.com/@aspengulley/write-an-r-function-to-implement-k-fold-cross-validation-ff9d92e97ce3 ###

# 1. Implement KNN classification with K chosen by 10-fold cross-validation from
#   scratch. Set the candidate K values from 1 to 180. (The maximum candidate K
#   value is 180. Why?) From now on, you are allowed to use the built-in kNN
#   function from R or Python instead of your own implementation from Part 2.
#   It is possible that multiple K values give the (same) smallest CV error;
#   when this happens, pick the largest K value among them, since the larger the
#   K value, the simpler the model.

cvkNN = function(train, truth, maxk = 180, nfolds = 10) {
  # set a vector cv.err to store the CV error for each k. 
  cv.err = rep(0,maxk);
  
  # set a vector for k values
  kvalues = seq(1, maxk, 1)
  
  # Set variables
  n = nrow(traindata)
  foldSize = floor(n/nfolds)
  
  for(k in 1:maxk) {
    # cumulative error for averaging out at the end
    error = 0
    
    for(fold in 1:nfolds) {
      # Sample the data
      data_sample = # TODO ??? something like ith.fold in the example code from campuswire #3?
        
      # Set the knn function variables
      temp_train = train[-data_sample,]
      temp_train_truth = truth[-data_sample]
      temp_test = train[data_sample,]
      temp_test_truth = truth[data_sample]
      
      # Make prediction
      temp_predict = knn(temp_train, temp_test, temp_train_truth, k=k)
      
      # Sum the error
      error = error + sum(temp_predict != temp_test_truth)
    }
    
    # Set the error in the array
    cv.err[k] = error/n
  }
  
  # Return the best k and associated error here
  # TODO remember to add a tiebreaker for if there are 2 best k's...we want the big one
  
}

# 2. Test your code with the training/test data you just generated. Report your
#   results (on the test data) as a 2-by-2 table and also report the value of
#   the selected K.

# TODO I think this is the function call we can use to test if this works
cvkNN(traindata, Ytrain)

# This is the code from Campuswire #3...I am truly unsure if it works
# myk = seq(1,180,1)
# cv.err=rep(0,m);
# 
# id=sample(1:(2*n),(2*n), replace=FALSE);
# fold=c(0,  40,  80, 120, 160, 200)
# for(i in 1:5)
#   for(j in 1:m){
#     
#     ## ith.fold = rows which are in the i-th fold
#     ith.fold = id[(fold[i]+1):fold[i+1]];   
#     tmp=knn(traindata[-ith.fold,], traindata[ith.fold,], Ytrain[-ith.fold], k=myk[j]);
#     cv.err[j]=cv.err[j]+sum(tmp != Ytrain[ith.fold])
#   }
# 
# ## find the best k value based 5-fold CV
# k.star=myk[order(cv.err)[1]]
# k.star
# 
# ## Error of KNN where K is chosen by 5-fold CV
# Ytrain.pred = knn(traindata, traindata, Ytrain, k=k.star)
# train.err.knn.CV= sum(Ytrain != Ytrain.pred)/(2*n)
# Ytest.pred = knn(traindata, testdata, Ytrain,k=k.star)
# test.err.knn.CV = sum(Ytest != Ytest.pred)/(2*N) 
# 
# train.err.knn.CV
# test.err.knn.CV
