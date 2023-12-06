



# add required packages
packages = c('tidyverse')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

# set seed
set.seed(235)

# download and preprocess raw movie data
# note that this code is from example code from this class 
# https://liangfgithub.github.io/Rcode_W13_Movie_EDA.nb.html

source_url = "https://liangfgithub.github.io/MovieData/"

ratings = read.csv(paste0(source_url, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)

colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# load movies. Same source
movies = readLines(paste0(source_url, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

# convert accented characters
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

# 
users = read.csv(paste0(source_url, 'users.dat?raw=true'),
                 sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')




ratings = read.csv('Rmat.csv')
ratings
print(row.names(ratings))
View(ratings)

ratings["u1",1:10]


n_users = length(unique(ratings$UserID))
m_movies = length(unique(ratings$MovieID))
names_users = sort(unique(ratings$UserID))
names_movies = sort(unique(ratings$MovieID))



R = ratings

# Normalize R by taking average user ratings and subtracting from R matrix
r_avg = rowMeans(R, na.rm = TRUE)
R_norm = R-r_avg
View(R_norm)

# Create R vectors RIi and RIj as indexed by Iij, the set of users who rated i and j
Ri = R_norm[,'m1']
Rj = R_norm[,'m10']
ri = Ri[!is.na(Ri) & !is.na(Rj)]
rj = Rj[!is.na(Ri) & !is.na(Rj)]

print(length(rj))

# Calculate Cosine Similarity
Sij =.5+ .5*(sum(ri * rj)/(sqrt(sum(ri^2)) * sqrt(sum(rj^2))))
Sij

get_cosine(Ri, Rj)

library(data.table)


View(R)
View(R_norm)

# Function to implement Cosine Similarity Calculation takes vectors of same length as arguments
get_cosine = function(Ri,Rj){

  # Create index for set Iij, get cardinality of set, create subsets of Ri and Rj indexed by Iij
  Iij = !is.na(Ri) & !is.na(Rj)
  Iij_count = sum(Iij, na.rm=TRUE)
  ri = Ri[Iij]
  rj = Rj[Iij]
  # Perform initial calculations in order to separate cases
  RiRj = sum(ri * rj)
  Ri2 = sqrt(sum(ri^2))
  Rj2 = sqrt(sum(rj^2))

  # Return NA, if less than three of the same people rated the same movie 
  # if or one sum square equals zero. Otherwise, calculat cosine similarity.
  if((Iij_count<3)|((Ri2*Rj2)==0)){
    Sij = NA
  } else {
    Sij = .5+ .5*(RiRj/(Ri2 * Rj2))
  }
  return(Sij)
}

y = get_cosine(R_norm[,1],R_norm[,2])
y

create_similarity_matrix = function(R) {
  t0 = Sys.time()
  m_movies = ncol(R)
  movie_names = colnames(R)
  S = matrix(NA, nrow = m_movies, ncol = m_movies,
            dimnames = list(movie_names, movie_names))
  
  for (i in 1:m_movies) {
  print(i)
    ti0=Sys.time()
    for (j in 1:m_movies) {
      if (i != j) {
        x = get_cosine(R[, i], R[, j])
        S[i, j] = x
        #S[j, i] = x
      }
    }
    #if(i==100){ break }  
  #print(S[, i])
  tif = Sys.time()
  time = tif - ti0
  print(time)
  
  }
  tf = Sys.time()
  time = tf - t0
  print(time)
  return(S)
}
#x=create_similarity_matrix(R_norm[,c('m1','m10','m100')])
#x=create_similarity_matrix(R_norm[,1:5])
x=create_similarity_matrix(R_norm)
print(x)




# Function to calculate cosine similarity between two vectors
cosine_similarity <- function(x, y) {
  # Get indices where both x and y are not NA
  idx <- !is.na(x) & !is.na(y)
  
  # Calculate dot product and magnitudes
  dot_product <- sum(x[idx] * y[idx])
  magnitude_x <- sqrt(sum(x[idx]^2))
  magnitude_y <- sqrt(sum(y[idx]^2))
  
  # Calculate cosine similarity
  if (magnitude_x != 0 && magnitude_y != 0) {
    return(dot_product / (magnitude_x * magnitude_y))
  } else {
    return(0) # Return 0 for vectors with insufficient numerical values
  }
}

cosine_similarity(R_norm[,1], R_norm[, 2])

m <- ncol(R_norm)
result <- sapply(1:m, function(j) {
  cosine_similarity(R_norm[,1], R_norm[, j])
})
result


keep_top_30 <- function(r) {
  k=30
  ranked_row = rank(-r, na.last = "keep", ties.method = "min")
  r[ranked_row > k] = NA
  return(r)
}
S = t(apply(x, 1, keep_top_30))
View(S)



S = t(apply(Y, 1, keep_top_30))
write.csv(S, file = "S.csv")



myIBCF = function(newuser){
  similarity_matrix = read.csv("S.csv", row.names = 1)
  #print(similarity_matrix)
  #print(newuser)
  recommend_limit = 3
  names(newuser) = colnames(similarity_matrix)
  sr = colSums(similarity_matrix*newuser, na.rm = TRUE)
  s = rowSums(similarity_matrix, na.rm = TRUE)
  predicted_ranks = ifelse(s != 0, sr/s, NA)
  
  #print(predicted_ranks )
  ordered_predictions = order(predicted_ranks, decreasing = TRUE)
  ordered_predictions_index = ordered_predictions[1:min(recommend_limit,length(newuser))]
  return(names(newuser)[ordered_predictions_index])
}

display_movies = c('m1','m10','m100','m1510','m260','m3212')
Y=x[display_movies,display_movies]
U = c(5,2,2,4,1,5)
myIBCF(U)






View(x)




















R = R_norm[,c('m1','m10','m100')]
cosine_similarity = function(R) {

  

  
Ri = R[,'m1']
M = R
Mi = data.frame(replicate(3,Ri))

Ii = !is.na(Mi) & !is.na(M)

M = M[Ii]
Mi = Mi[Ii]

MiM = colSums(Mi * M, na.rm = TRUE)
Mi2 = sqrt(colSums(Mi^2, na.rm=TRUE))
M2 = sqrt(colSums(M^2, na.rm=TRUE))

y=.5+.5*(MiM/(Mi2*M2))
y

  return(S)
}














