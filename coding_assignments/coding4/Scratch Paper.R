

# add required packages
packages = c()

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}



Estep = function(data, G, init_params) {
  # Use Approach 2 (https://campuswire.com/c/G06C55090/feed)
  A = t(data) # transpose of the data matrix
  
  # our n-by-G output matrix
  out = matrix(0, nrow=nrow(data), ncol=G)
  
  # iterate through G
  for (k in 1:G) {
    # these steps are outlined in the campuswire post linked above
    demeaned = (A - init_params$mean[, k])
    matrix_multiplied = solve(init_params$Sigma) %*% demeaned
    element_multiplied = matrix_multiplied * demeaned
    summed = colSums(element_multiplied)
    
    # now concat the columns
    out[, k] = summed 
  }
  
  return(out)
}




Mstep = function(data, G, init_params, post_prob) {
  
  out_params = init_params
  
  # update means
  numerator = t(data) %*% post_prob
  denom = colSums(post_prob)
  # we want to perform elementwise division, but by rows instead of cols
  new_mean = sweep(numerator, 2, denom, "/")
  out_params[['mean']] = new_mean
  
  
  
  S = matrix(0, nrow=ncol(data), ncol=G)
  n = nrow(data)
  
  # iterate through G
  for (k in 1:G) {
    u = init_params$mean[, k]
    p = post_prob[, k]
    
    tmp = data - u
    
    S = S + t(tmp) %*% diag(p) %*% tmp
  }
  
  sigma_max = solve(n / S)
}





loglik = function(data, G, init_params) {
  
  # iterate over G and sum
  g_sum = rep(0, nrow(data))
  for (k in 1:G) {
    # rename vars to make equation align better
    x = data
    u = init_params$mean[, k]
    sigma = init_params$Sigma
    p = init_params$prob[k]cd
    d = ncol(x) # data dimensions 
    
    # Once again use Approach 2 to calculate gaussian (https://campuswire.com/c/G06C55090/feed)
    
    # these steps are outlined in the campuswire post linked above
    A = t(x)
    demeaned = (A - init_params$mean[, k])
    matrix_multiplied = solve(sigma) %*% demeaned
    element_multiplied = matrix_multiplied * demeaned
    summed = colSums(element_multiplied)
    
    # equation from assigment
    numerator = exp(-(1/2) * summed)
    denom = sqrt(2 * pi^d * det(sigma))
    
    likelihood = p * numerator / denom
    
    g_sum = g_sum + likelihood
  }
  
  # now return sum of all log liks
  logged = log(g_sum)
  
  out = sum(logged)
  return(out)
}





myEM = function(data, G, in_params, itmax) {
  for(i in 1:itmax) {
    post_prob = Estep(data, G, in_params)
    params = Mstep(data, G, in_params, post_prob)
  }
  
  out_params = params
  
  # add loglik to list of outputs and return 
  out_params[['loglik']] = loglik(data, G, in_params)
  
  return(out_params)
}


# load datset
test_df = read.table('faithful.dat')
n = nrow(test_df)
data=as.matrix(test_df)


# Use Approach 2 (https://campuswire.com/c/G06C55090/feed)
A = t(data) # transpose of the data matrix

# our n-by-G output matrix
out = matrix(0, nrow=nrow(data), ncol=G)

# iterate through G
for (k in 1:G) {
  # these steps are outlined in the campuswire post linked above
  demeaned = (A - init_params$mean[, k])
  matrix_multiplied = solve(init_params$Sigma) %*% demeaned
  element_multiplied = matrix_multiplied * demeaned
  summed = colSums(element_multiplied)
  
  # now concat the columns
  out[, k] = summed 
}

###### Case 1: G = 2. Using variables from assignment ###### 
G = 2
p1 = 10/n
p2 = 1 - p1

# slice x into x1 and x2 to simplify code later
x1 = as.matrix(test_df[1:10, ])
x2 = as.matrix(test_df[11:n,])

# u1 is mean of first 10 samples, u2 is mean of remaining
u1 = colMeans(x1)
u2 = colMeans(x2)

# calculate sigma using provided eqn
sigma = 1 / n * (t(x1 - u1) %*% (x1 - u1) + t(x2 - u2) %*% (x2 - u2))

t(t(x1) - u1)
(t(x1) - u1) # transposing x_i to subtract mean from corresponding column

t(t(x1) - u1)[,1]
sum(t(t(x1) - u1)[,1])
sum(t(t(x1) - u1)[,2])

(t(x1) - u1) %*% t(t(x1) - u1) # produces 2x2 matrix which seems correct o.O
t(t(x1) - u1) %*% (t(x1) - u1)  # 10x10 wrong matrix

(t(x2) - u2) %*% t(t(x2) - u2) # produces 2x2 matrix which seems correct o.O

1/n*((t(x1) - u1) %*% t(t(x1) - u1) + (t(x2) - u2) %*% t(t(x2) - u2) ) # This appears closer to correct

###### Probably use this as sigma instead ######
sigma = 1/n*((t(x1) - u1) %*% t(t(x1) - u1) + (t(x2) - u2) %*% t(t(x2) - u2) )

itmax = 20

# gather initial params into list
init_params2 = list(prob=c(p1, p2), mean=cbind(u1, u2), Sigma=sigma)

# evaluate 
out = myEM(data=data, 
           G=G, 
           in_params=init_params2, 
           itmax=itmax)





# Use Approach 2 (https://campuswire.com/c/G06C55090/feed)
A = t(data) # transpose of the data matrix

# our n-by-G output matrix
out = matrix(0, nrow=nrow(data), ncol=G)

nrow(data)
ncol(data)
nrow(A)
ncol(A)
# iterate through G
#for (k in 1:G) {
for (k in 1:1) {
  # these steps are outlined in the campuswire post linked above
  demeaned = (A - init_params$mean[, k])
  #print(demeaned)
  matrix_multiplied = solve(init_params$Sigma) %*% demeaned
  #print(matrix_multiplied)
  element_multiplied = matrix_multiplied * demeaned
  #print(element_multiplied)
  summed = colSums(element_multiplied)
  #print(summed)
  # now concat the columns
  out= matrix(summed, ncol=1)
  print(out)
  print(matrix_multiplied)
  #print(demeaned)
  #print(solve(init_params$Sigma))
}

for(i in 1:itmax) {
  post_prob = Estep(data, G, init_params2)
  params = Mstep(data, G, init_params2, post_prob)
}

Estep(data, G, in_params)

######  Case 2: G = 3 ###### 
G = 3
p1 = 10/n
p2 = 20/n
p3 = 1 - p1 - p2

# slice x into x1 x2, and x3 to simplify code later
x1 = as.matrix(test_df[1:10, ])
x2 = as.matrix(test_df[11:30, ])
x3 = as.matrix(test_df[31:n, ])


# u1 is mean of first 10 samples, u2 is mean of remaining
u1 = colMeans(x1)
u2 = colMeans(x2)
u3 = colMeans(x3)

# calculate epsilon using provided eqn
epsilon = 1 / n * (t(x1 - u1) %*% (x1 - u1) + t(x2 - u2) %*% (x2 - u2) + t(x3 - u3) %*% (x3 - u3))

max_iteration = 20




