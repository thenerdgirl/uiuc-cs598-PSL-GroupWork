
# Coding Assignment 1
# Naomi Bhagat, Michael Miller, & Joe May
# nbhagat3, msmille3, jemay3


# Part 1: Generate Data

# 1. First generate the 20 centers from two-dimensional normal. 
# You can use any mean and covariance structure.  You should not 
# regenerate the centers. Use these 20 centers throughout this 
# simulation study. 

csize = 10       
p = 2        
s = 1                         
m1 = matrix(rnorm(csize*p), csize, p)*s + cbind( rep(1,csize), rep(0,csize) )
m0 = matrix(rnorm(csize*p), csize, p)*s + cbind( rep(0,csize), rep(1,csize) )

# 2. Given the 20 centers, generate a training sample of size 200 
# (100 from each class) and a test sample of size 10,000 (5,000 
# from each class). 

# Training sample of size 200

n = 100

id1 = sample(1:csize, n, replace=TRUE)
id0 = sample(1:csize, n, replace=TRUE)  
s = sqrt(1/5);              

traindata = matrix(rnorm(2*n*p), 2*n, p)*s + 
  rbind(m1[id1,], m0[id0,])
dim(traindata)
Ytrain = factor(c(rep(1,n), rep(0,n)))

# Training sample of size 10,000

N = 5000

# Allocate the n samples for class 1  to the 10 clusters
id1 = sample(1:csize, n, replace=TRUE)
# Allocate the n samples for class 1 to the 10 clusters
id0 = sample(1:csize, n, replace=TRUE)  
s = sqrt(1/5);              # sd for generating data. 

testdata = matrix(rnorm(2*n*p), 2*n, p)*s + 
  rbind(m1[id1,], m0[id0,])
dim(testdata)
Ytest = factor(c(rep(1,n), rep(0,n)))

# 3. Produce a scatter plot of the training data:
# a. assign different colors to the two classes of data points;
# b. overlay the 20 centers on this scatter plot, using a distinguishing 
# marker (e.g., a star or a different shape) and color them according to their respective class.

plot(traindata[,1], traindata[,2], type="n", xlab="", ylab="")
points(traindata[1:n,1], traindata[1:n,2], col="green")
points(traindata[(n+1):(2*n),1], traindata[(n+1):(2*n),2], col="red")
points(m1[1:csize,1], m1[1:csize,2], pch="+", cex=1.5, col="green")
points(m0[1:csize,1], m0[1:csize,2], pch="+", cex=1.5, col="red");   
legend("bottomright", pch = c(1,1), col = c("red", "green"), legend = c("class 1", "class 0"))

# Part 2: kNN

# 1. Implement kNN from scratch; use Euclidean Distance. 
# Your implementation should meet the following requirements:


euclidean_distance <- function(point1, point2) {
  sqrt(sum((point1 - point2)^2))
}

variable.dotcom. <- 1
variable.dotcom.


knn <- function(new_point, dataset, k) {
  distances <- numeric(nrow(dataset))
  
  # Calculate Euclidean distances between the new_point and all points in the dataset
  for (i in 1:nrow(dataset)) {
    distances[i] <- euclidean_distance(new_point, dataset[i, c("X1", "X2")])
  }
  










