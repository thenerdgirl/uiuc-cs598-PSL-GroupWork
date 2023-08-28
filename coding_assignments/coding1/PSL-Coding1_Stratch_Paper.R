

# Euclidean helper function
euclidean_distance = function(point1, point2) {
  if (length(point1) == length(point2)) {
    return(sqrt(sum((point1 - point2)^2)))
  } else {
    stop('Vectors must be of the same length')
  }
}


#A = matrix(runif(3), ncol = 2)
#A = matrix(c(0,0,2,2), ncol=2, byrow=TRUE)
A = matrix(c(0,0,3,3,5,5,2,2,1,1), ncol=2, byrow=TRUE)
A

outer(A, A, Vectorize(function(x, y) euclidean_distance(x, y)))


dist(A)


distances <- outer(1:nrow(A), 1:nrow(A), Vectorize(function(i, j) euclidean_distance(A[i,], A[j,])))
distances
distances[1,]
distances[,1]


euclidean_distance(A[1,], A[2,])
euclidean_distance(A[1,], A[3,])

a=c(0,1,1,0,0)
a
b=c(1,2,3,4,5)

p <- which(distances[1,] %in% sort(distances[1,])[1:4])
p
a[p]

table(a[p])
table(a[p])[[1]]
table(a[p])[[2]]

#outer(1:1, Vectorize(function(i) which(distances[i,] %in% sort(distances[i,])[1:4])))
outer(1:1, Vectorize(function(i) order(distances[i,])[1:4]))

# Error in as.vector(x, mode):
# cannot coerc type 'closure' to vector of type 'any'

neighbor_address <- function(vector, n){
  order(vector)[1:n]
}

neighbor_address(distances[1,],3)

neighborhood_address <- t(apply(distances, 1, neighbor_address, n = 3))
neighborhood_address


col_indices <- matrix(neighborhood_address, ncol = 3)

distances[cbind(1:5, neighborhood_address[,1])]


c1 <- distances[cbind(1:5, neighborhood_address[,1])]
c2 <- distances[cbind(1:5, neighborhood_address[,2])]
c3 <- distances[cbind(1:5, neighborhood_address[,3])]

cbind(c1, c2, c3)

c1 <- b[neighborhood_address[,1]]
c2 <- b[neighborhood_address[,2]]
c3 <- b[neighborhood_address[,3]]
cbind(c1, c2, c3)

c1 <- a[neighborhood_address[,1]]
c2 <- a[neighborhood_address[,2]]
c3 <- a[neighborhood_address[,3]]
cbind(c1, c2, c3)

outer(1:3, Vectorize(function(i) a[neighborhood_address[,i]]))
i <- 1

get_vote <- function(indices){
  a[indices]
}
get_vote(b, neighborhood_address[,1])
get_vote(b, neighborhood_address[,2])
get_vote(b, neighborhood_address[,3])

neighborhood_votes <- t(apply(neighborhood_address, 1, get_vote))

ifelse(rowSums(neighborhood_votes) >= 3/2, 1, 0)

determine_winner <- function(point){
  if (sum(point) > 3/2) {
    return(1)
  } else if (sum(point) < 3/2) {
    return(0)
  } else {
    return(0)
  }
}
apply(neighborhood_votes, 1, determine_winner)

# Alright, redoing with vectorizing

B = matrix(c(0,0,3,3,5,5,2,2,1,1), ncol=2, byrow=TRUE)
C = matrix(c(0,0,3,3), ncol=2, byrow=TRUE)
Y = c(1,2,3,4,5)
Y = c(0,1,1,0,0)
dis = outer(1:nrow(C), 1:nrow(B), Vectorize(function(i, j) euclidean_distance(C[i,], B[j,])))

m=outer(1:nrow(C), 1:nrow(B), function(i, j) euclidean_distance(C[i,], B[j,]))

B
C
Bdiff = sweep(B, 2, C[1,],"-")
Cdiff = sweep(B, 2, C[2,],"-")
Bsmsq = rowSums(Bdiff^2)
Csmsq = rowSums(Cdiff^2)
Bdist = sqrt(Bsmsq)
Cdist = sqrt(Csmsq)

subtract_rows <- function(x, y,i) {
  sweep(x, MARGIN = 2, STATS = y[i,], FUN = "-")
}
subtract_rows(B,C,2)

#=============
nrow(B)
Cx=matrix(rep(C,each = nrow(B)), ncol=2)
Bx=matrix(rep(B,nrow(C)), ncol=2,byrow=FALSE)
delta = Bx-Cx
delta  
deltasmsq=rowSums(delta^2)
deltasmsq
dis=matrix(sqrt(deltasmsq),nrow=2, byrow=TRUE)
#=============


#i=2
#outer(1:2,1:2, function(i,j) sweep(B, MARGIN = 2, STATS = C[i,],FUN = "-"))

B = traindata
C = testdata

k=3
t(apply(dis, 1, function(row) order(row)[1:k]))

x = function(row_indices) {
  sorted = order(dis[row_indices,])[1:k]
  Y[sorted]
}
vf = Vectorize(x)
t(vf(1:2))

k=3

order(dis[1,])[1:k]
order(dis[2,])[1:k]



get_vote = function(indices) {
  nearest_neighbor_address = order(dis[indices,])[1:k]
  Y[nearest_neighbor_address]
}

neighborhood_votes = Vectorize(get_vote)
votes = t(neighborhood_votes(1:nrow(C)))
votes

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

predictions = apply(votes, 1, determine_winner)



# Redoing again vectorizing and also 

B = traindata
C = testdata
#Y = c(1,2,3,4,5)
Y = Ytrain
dis = outer(1:nrow(B), 1:nrow(C), Vectorize(function(i, j) euclidean_distance(B[i,], C[j,])))
head(dis)

# vectorized logic?
#=============
traindata = B
testdata = C
Ytrain = Y
+
expanded_test_data=matrix(rep(testdata,each = nrow(traindata)), ncol=2)
expanded_train_data=matrix(rep(traindata,nrow(testdata)), ncol=2,byrow=FALSE)
train_less_test = expanded_train_data - expanded_test_data
train_less_test 
train_less_test_sum_squares=rowSums(train_less_test^2)
train_less_test_sum_squares
neighbor_distances=matrix(sqrt(train_less_test_sum_squares),nrow=2, byrow=TRUE)
neighbor_distances
#=============



order(dis[1,])[1:k]
Y[order(dis[1,])]

get_vote = function(indices) {
  nearest_address = order(dis[indices,])[1:k]
  Y[nearest_address]
}

neighborhood_votes = Vectorize(get_vote)
votes = t(neighborhood_votes(1:nrow(C)))
votes
tail(votes)

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

predictions = apply(votes, 1, determine_winner)
predictions



# Now recode the whole thing in a consolidated form to copy and paste in original code to not 
# ruin everything




#========================================================================

set.seed(235)
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

#========================================================================


knn_from_scratch = function(train, test, truth, k) {
  
  expanded_test_data=matrix(rep(test,each = nrow(train)), ncol=2)
  expanded_train_data=matrix(rep(train,nrow(test)), ncol=2,byrow=FALSE)
  train_less_test = expanded_train_data - expanded_test_data
  train_less_test_sum_squares=rowSums(train_less_test^2)
  neighbor_distances=matrix(sqrt(train_less_test_sum_squares),nrow=2, byrow=TRUE)
  
  #neighbor_distances = outer(1:nrow(test), 1:nrow(train), Vectorize(function(i, j) euclidean_distance(test[i,], train[j,])))
  # Calculate each point's distance to neighbors
  
  nearest_neighbor_address = function(j) {
    nearest_neighbors = order(neighbor_distances[j,])[1:k]
    return(truth[nearest_neighbors])
  }
  # Function to get k nearest neighbors (temp variable j for k)
  neighborhood_votes = Vectorize(nearest_neighbor_address)
  neighborhood_votes = t(neighborhood_votes(1:nrow(test)))
  
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



#========================================================================

# k = 1
baseline_test.pred = knn(traindata, testdata, Ytrain, k=1)
table(Ytest, baseline_test.pred)
implementation_test.pred = knn_from_scratch(traindata, testdata, Ytrain, k=1)
table(Ytest, implementation_test.pred)



baseline_test.pred = knn(traindata, testdata, Ytrain, k=3)
table(Ytest, baseline_test.pred)
implementation_test.pred = knn_from_scratch(traindata, testdata, Ytrain, k=3)
table(Ytest, implementation_test.pred)


