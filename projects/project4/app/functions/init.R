# read in data
myurl = "https://liangfgithub.github.io/MovieData/"

# movie data
# movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = readLines(paste0('data/', 'movies.dat'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

# extract year
movies$Year = as.numeric(unlist(lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# print(">>> INIT.R")
# print(movies)

# ratings data
# ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), sep = ':', colClasses = c('integer', 'NULL'), header = FALSE)
ratings = read.csv(paste0('data/', 'ratings.dat'), sep = ':', colClasses = c('integer', 'NULL'), header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# user data
# users = read.csv(paste0(myurl, 'users.dat?raw=true'), sep = ':', header = FALSE)
users = read.csv(paste0('data/', 'users.dat'), sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

# Create the genre_recommendation csv
set.seed(235)

# define the quantity of movies for each genre for each type 
n_all_time_fav = 50
n_popular = 40
n_hidden = 10
n_total_recs = 10

# define params for establishing popular movies vs hidden gems
min_reviews_popular = 100
min_reviews_hidden = 5

# first, unpack and duplicate genres so that each movie-genre combo is one row
movie_genres = movies %>% 
mutate(Genres = strsplit(Genres, '\\|')) %>%
rename(Genre = Genres) %>%
unnest(Genre)

# get number of 5 star reviews for calculating all time favorites
perfect_reviews = ratings %>%
filter(Rating == 5) %>%
group_by(MovieID) %>%
summarize(PerfectReviews = n())

# get average review for popular movies
review_percent_popular = ratings %>%
group_by(MovieID) %>%
filter(n() >= min_reviews_popular) %>%
summarize(AvgRating = mean(Rating))

# get average review for hidden gems
review_percent_hidden = ratings %>%
group_by(MovieID) %>%
filter(n() >= min_reviews_hidden) %>%
filter(n() < min_reviews_popular) %>%
summarize(AvgRating = mean(Rating))

# join perfect reviews with movie genres, take top n to get all time favorites
all_time_favorites = movie_genres %>%
inner_join(perfect_reviews, by="MovieID") %>%
arrange(desc(PerfectReviews)) %>%
group_by(Genre) %>%
slice_head(n=n_all_time_fav)

all_time_favorites['Rationale'] = 'All time favorite'

# we will track our final set of movies as out
out = all_time_favorites[c('MovieID', 'Title', 'Genre', 'Year', 'Rationale')]

# join average review with movie genres, take top n to get popular movies
# note we are excluding any movie that might already be all time favorites
popular = movie_genres %>%
anti_join(out, by="MovieID") %>%
inner_join(review_percent_popular, by="MovieID") %>%
arrange(desc(AvgRating)) %>%
group_by(Genre) %>%
slice_head(n=n_popular)
popular['Rationale'] = 'Popular'

out = bind_rows(out, popular[c('MovieID', 'Title', 'Genre', 'Year', 'Rationale')])

# join average hidden gem reviews with movie genres, take top n to get hiden gems
# note we are excluding any movies already chosen
hidden = movie_genres %>%
anti_join(out, by="MovieID") %>%
inner_join(review_percent_hidden, by="MovieID") %>%
arrange(desc(AvgRating)) %>%
group_by(Genre) %>%
slice_head(n=n_hidden)
hidden['Rationale'] = 'Hidden gem'

out = bind_rows(out, hidden[c('MovieID', 'Title', 'Genre', 'Year', 'Rationale')])

# next, we randomly sample n_total_rec films from out for each genre, and we save these for use in the app
to_csv = out %>%
arrange(sample(n())) %>%
group_by(Genre) %>%
slice_head(n=n_total_recs)

write_csv(to_csv, 'data/genre_recommendations.csv')
