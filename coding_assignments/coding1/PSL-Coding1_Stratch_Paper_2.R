



set.seed(235)

# import necessary libraries
library(class)

csize = 10
p = 2        
s_centers = 1
#s_x = 1
s_x = sqrt(1/10); 

m1 = matrix(rnorm(csize*p), csize, p)*s_centers + cbind( rep(0.5,csize), rep(0.5,csize) )
m0 = matrix(rnorm(csize*p), csize, p)*s_centers + cbind( rep(-0.5,csize), rep(-0.5,csize) )



n = 10
id1 = sample(1:csize, n, replace=TRUE)
id0 = sample(1:csize, n, replace=TRUE)  
traindata = matrix(rnorm(2*n*p), 2*n, p)*s_x + rbind(m1[id1,], m0[id0,])
dim(traindata)
Ytrain = rep(c(1, 0), each = n)

# Test sample of size 10,000

N = 50
# Allocate the n samples for class 1  to the 10 clusters
id1 = sample(1:csize, N, replace=TRUE)
# Allocate the n samples for class 1 to the 10 clusters
id0 = sample(1:csize, N, replace=TRUE)  
testdata = matrix(rnorm(2*N*p), 2*N, p)*s_x + rbind(m1[id1,], m0[id0,])
dim(testdata)
Ytest = rep(c(1, 0), each = N)



euclidean_distance = function(point1, point2) {
  if (length(point1) == length(point2)) {
    return(sqrt(sum((point1 - point2)^2)))
  } else {
    stop('Vectors must be of the same length')
  }
}

csize=2
n=3
N=2
traindata = matrix(c(1.5,1.2,1.0,0.5,0.5,1.0,-1.25,-1.25,-1.0,-0.5,-2,-1.5), ncol=2, byrow=TRUE)
testdata = matrix(c(0,0,2,2), ncol=2, byrow=TRUE)
Ytrain = c(1,1,1,0,0,0)
Ytest = c(1,1)

plot(traindata[,1], traindata[,2], type="n", xlab="", ylab="")
points(traindata[1:n,1], traindata[1:n,2], col="green")
points(traindata[(n+1):(2*n),1], traindata[(n+1):(2*n),2], col="red")
points(m1[1:csize,1], m1[1:csize,2], pch="+", cex=1.5, col="green")
points(m0[1:csize,1], m0[1:csize,2], pch="+", cex=1.5, col="red");   
legend("bottomright", pch = c(1,1), col = c("red", "green"), legend = c("class 1", "class 0"))


knn_from_scratch = function(train, test, truth, k) {
  
  expanded_test_data = matrix(rep(test,each = nrow(train)), ncol=2)
  expanded_train_data = matrix(rep(t(train), times = nrow(test)), ncol=2, byrow = TRUE)
  # Repeat test rows to subtract from corresponding training rows
  
  train_less_test = expanded_train_data - expanded_test_data
  train_less_test_sum_squares = rowSums(train_less_test^2)
  neighbor_distances = matrix(sqrt(train_less_test_sum_squares),nrow=nrow(test), byrow=TRUE)
  # Take the sum of squares of the difference between test points from training points
  # Uses vector functionality to calculate each point's distance to neighbors
  print(neighbor_distances)
  print(train)
  nearest_neighbor_address = function(j) {
    # Handle distance ties by including all distance ties in the vote count
    extra_k = 0
    unique_j = unique(j)
    nearest = head(sort(unique_j),k)

    print(paste0("j ",j))
    print(paste0("unique_j ",unique_j))
    print("nearest ")
    print(nearest)
    
    test_table = as.data.frame(table(j))
    dupes = test_table[test_table$Freq > 1,]
    print("dupes")
    print(dupes)
    print(dupes[1,1])
    

    
    get_distance_ties <- function(i) {
      print(paste0("j ",j," i ",i," sum(j == i) ",sum(j == i)))
      x = extra_k + (sum(j == i) - 1)
      #extra_k = sum(j == i)
      return(x)
    }
    print(paste0("extra_k r1 ",extra_k))
    temp_check = sapply(nearest, get_distance_ties)
    print("temp_check ")
    print(temp_check)
    
    tie_distance = dupes[1,1]
    print("hard coded distance tie passed thru get_dist_ties")
    print(get_distance_ties(tie_distance))
    tie_index=which(j %in% tie_distance)
    print( "tie_index ")
    print( tie_index)
    
    truther = truth[tie_index]
    print("truther")
    print(truther)
    
    extra_k = sapply(nearest, get_distance_ties)
    print(paste0("extra_k r2 ",extra_k))
    
    check1=k+extra_k
    check2=order(j)
    print("check1")
    print(check1)
    print("check2")
    print(check2)
    print("head(order(j),k+extra_k) ")
    print(head(order(j),k+extra_k))
    y=head(order(j),k+extra_k)
    print("truth")
    print(truth)
    print(truth[y])
    return(head(order(j),k+extra_k))
  }
  
  # Function to get k nearest neighbors (temp variable j for k)
  
  neighborhood_votes = apply(neighbor_distances, 1, nearest_neighbor_address)
  # Return address of each row's nearest neighbor
  
  neighborhood_votes_v = matrix(truth[neighborhood_votes], nrow=nrow(test), byrow=TRUE)
  # Collect votes from nearest neighbor
  
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
  
  predictions = apply(neighborhood_votes_v, 1, determine_winner)
  return(predictions)
}

print("k = 1")
baseline_test.pred = knn(traindata, testdata, Ytrain, k=1)
table(Ytest, baseline_test.pred)
implementation_test.pred = knn_from_scratch(traindata, testdata, Ytrain, k=1)
table(Ytest, implementation_test.pred)


# for(n in nearest) {
#   extra_k = extra_k + (sum(j == n) - 1)
# }

my_do_something <- function(i) {
  extra_k = extra_k + (sum(j == i) - 1)
}

for (n in nearest) {
  my_do_something(n)
}

sapply(nearest, my_do_something)


test = c(0.8791893, 1.7848809, 0.8791893, 1.8745119, 2.3749307, 2.3526441, 3.176593, 1.1847304, 3.7154846, 1.9995022, 3.4136126, 2.7707655, 0.6056586, 2.7790663, 2.4914583, 2.6027233, 2.8103019, 2.4129525, 2.2555611, 0.6056556)

extra_k = 0
unique_j = unique(test)
nearest = head(sort(unique_j),3)

my_do_something <- function(i) {
  extra_k = extra_k + (sum(test == i) - 1)
}

blah = sum(sapply(nearest, my_do_something))
blah

which(c(0,1,2,2.2,1,5,2.2,3) %in% 2.2)

# 
# 
# # test_table = table(test)
# # dupes = test_table[test_table > 1]
# # no_dupes = unique(test)
# maxes = head(sort(no_dupes), 3)
# extra_k = 0
# for(max in maxes) {
#   extra_k = extra_k + (sum(test == max) - 1)
# }
# 
# extra_k

# eh = apply(maxes, 0, getsum)
# eh
# sum(test == 0.6056556)

# This gets indices of duplicated values
# which(duplicated(test) | duplicated(test, fromLast = TRUE))


# part 3

get_best_k = function(x, y) {
  
  # init variables for looping through k values
  max_k = 180
  fold_count = 10
  k_values = 1:max_k
  average_errors = rep(0, max_k)
  fold_size = floor(nrow(x) / fold_count)
  random_idx = sample(1:nrow(x))
  
  # loop through values of k, for each, calculate n-fold cv error
  for (k in k_values){
    # cumulative error for averaging out at the end
    error = 0
    
    for (fold in 1:fold_count){
      # calculate the index for train/validate for this fold 
      # do this by getting ordered idx (e.g. 1:10), then using that to get rand idx
      ordered_val_idx = ((fold - 1) * fold_size + 1) : (fold * fold_size)
      val_idx = random_idx[ordered_val_idx]
      
      # split data into train/val. Use negative index to exclude ones we don't want
      fold_train_x = x[-val_idx, ]
      fold_train_y = y[-val_idx]
      fold_val_x = x[val_idx, ]
      fold_val_y = y[val_idx]
      
      # Make prediction
      fold_predicted_y = knn(fold_train_x, fold_val_x, fold_train_y, k=k)
      
      # Sum the error
      error = error + sum(fold_predicted_y != fold_val_y)
    }
    
    # Add error to array to track
    average_errors[k] = error / fold_count
  }
  
  # uncomment to debug
  # plot(average_errors, xlab="k", ylab="Average error count", main="Error vs K")
  
  # Find lowest error (larger k breaks tie) which.max returns first, so reversing
  # array will get us last
  min_idx = length(average_errors) - which.min(rev(average_errors)) + 1
  min_error = average_errors[min_idx]
  best_k = k_values[min_idx]
  
  return(best_k)
}


best_k = get_best_k(traindata, Ytrain)
predicted = knn(traindata, testdata, Ytrain, k=best_k)
actual = Ytest
print(table(actual, predicted))

# Part 4 Bayes Rule

bayes_rule = function(x, s, m1, m0) {
  # x = x values 
  # s = sigma used to generate x
  # m0 = means of class 0 (for each center)
  # m1 - means of class 1 (for each center)
  
  # from assignment 1 (campuswire #36)
  
  # initialize variables for loop
  center_count = nrow(m1)
  numerator = rep(0, nrow(x))
  denominator  = rep(0, nrow(x))
  print(paste0("x ",x,"s ",s))
  print(paste0("center_count ",head(center_count),", numerator ",head(numerator),", denominator ",head(denominator)))
  
  # iterate through centers, and sum for each
  for(center in center_count){
    current_m1 = m1[center, ]
    current_m0 = m0[center, ]
    print(paste0("current_m1 ",current_m1," current_m0 ",current_m0))
    
    # get norm of each row
    m1_norm = apply(x - current_m1, 1, function(row) sqrt(sum(row^2)))
    m0_norm = apply(x - current_m0, 1, function(row) sqrt(sum(row^2)))
    
    # calculate numerator and denominator portions for each center
    numerator = sum(exp(-m1_norm / (2 * s^2)))
    denominator = sum(exp(-m0_norm / (2 * s^2)))
  }
  
  # return 1 if numerator is bigger else 0
  
  return(as.numeric(numerator >= denominator))
}


predicted = bayes_rule(testdata, s_x, m1, m0)
actual = Ytest
print(table(actual, predicted))


m0=matrix(c(1.0,1.0,1.5,2), ncol=2, byrow=TRUE)
m1=matrix(c(-1.0,-1.0,-.5,-.25), ncol=2, byrow=TRUE)
m0
m1
x=testdata
s=s_x

x=matrix(c(.5,.75,2,3,-.25,0), ncol=2, byrow=TRUE)
s=s_x
x
s
# from assignment 1 (campuswire #36)

# initialize variables for loop
center_count = nrow(m1)
numerator = rep(0, nrow(x))
denominator  = rep(0, nrow(x))
print(x)
print(s)
print(paste0("center_count ",head(center_count),", numerator ",head(numerator),", denominator ",head(denominator)))


for(center in 1:center_count){
#for(center in 1){
  center=1
  print(paste0("center ",center))
  current_m1 = m1[center, ]
  current_m0 = m0[center, ]
  #print(paste0("current_m1 ",current_m1," current_m0 ",current_m0))
  
  print(paste0("current_m0 ",current_m0))
  print(paste0("current_m1 ",current_m1))
  
  # get norm of each row
  m1_norm = apply(x - current_m1, 1, function(row) sqrt(sum(row^2)))
  m0_norm = apply(x - current_m0, 1, function(row) sqrt(sum(row^2)))
  
  #print(paste0("m1_norm ",m1_norm, "m0_norm ",m0_norm))
  print(x)
  print(paste0("m0_norm ",m0_norm))
  print(paste0("m1_norm ",m1_norm))
  
  print("numerator ")
  print(numerator)
  print(" denominator ")
  print(denominator)
  
  # calculate numerator and denominator portions for each center
  denominator = denominator + exp(-m1_norm/(2 * s^2))
  numerator = numerator + exp(-m0_norm / (2 * s^2))

  
  #numerator = sum(exp(-m1_norm / (2 * s^2)))
  #denominator = sum(exp(-m0_norm / (2 * s^2)))
  print(center)
  print("numerator ")
  print(numerator)
  print(" denominator ")
  print(denominator)
  print(as.numeric(numerator >= denominator))
}
print("numerator ")
print(numerator)
print(" denominator ")
print(denominator)
print(as.numeric(numerator >= denominator))
