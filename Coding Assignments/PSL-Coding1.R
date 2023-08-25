
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
Ytrain = factor(c(rep(1,n), rep(0,n)))

# Test sample of size 10,000

N = 5000
# Allocate the n samples for class 1  to the 10 clusters
id1 = sample(1:csize, N, replace=TRUE)
# Allocate the n samples for class 1 to the 10 clusters
id0 = sample(1:csize, N, replace=TRUE)  
testdata = matrix(rnorm(2*N*p), 2*N, p)*s + rbind(m1[id1,], m0[id0,])
dim(testdata)
Ytest = factor(c(rep(1,N), rep(0,N)))

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
  # Make the return vector
  predictions = rep(0, length(test[, 1]))

  # Distance calculations for each point
  # TODO vectorize this?
  for(i in 1:length(test[, 1])) {
    distances = apply(train, 1, euclidean_distance, test[i,])

    # Get nearest neighbors
    neighbors = which(distances %in% sort(distances)[1:k])

    # Get votes for the nearest neighbors from truth data
    votes = table(truth[neighbors])

    # Assign predictions
    if(votes[[1]] > votes[[2]]) {
      predictions[i] = 0
    } else if(votes[[1]] < votes[[2]]) {
      predictions[i] = 1
    } else {
      predictions[i] = sample(0:1, 1)
    }
  }
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
myknn_test.pred = myknn(traindata, testdata, Ytrain, k=1)
table(Ytest, myknn_test.pred)

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


