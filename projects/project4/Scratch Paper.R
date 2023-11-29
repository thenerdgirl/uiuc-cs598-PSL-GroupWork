



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
ratings$Rating
ratings %>%
  filter(UserID == "1") %>%
  summarize(x=mean(Rating))

ratings[,2]

ratings %>%
  filter(UserID == "2") %>%
  summarize(x=mean(Rating))

n_users = length(unique(ratings$UserID))
m_movies = length(unique(ratings$MovieID))
names_users = sort(unique(ratings$UserID))
names_movies = sort(unique(ratings$MovieID))



R = ratings

# Normalize R by taking average user ratings and subtracting from R matrix
r_avg = rowMeans(R, na.rm = TRUE)
R_norm = R-r_avg

# Create R vectors RIi and RIj as indexed by Iij, the set of users who rated i and j
Ri = R_norm[,'m1']
Rj = R_norm[,'m1510']
ri = Ri[!is.na(Ri) & !is.na(Rj)]
rj = Rj[!is.na(Ri) & !is.na(Rj)]

print(length(rj))

# Calculate Cosine Similarity
Sij =.5+ .5*(sum(ri * rj)/(sqrt(sum(ri^2)) * sqrt(sum(rj^2))))
Sij


library(data.table)


View(R)
View(R_norm)

