## server.R

# load functions
library(dplyr)

# define functions
# get_user_ratings <- function(value_list) {
#   dat <- data.table(book_id = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
#                     rating = unlist(as.character(value_list)))
#   dat <- dat[!is.null(rating) & !is.na(book_id)]
#   dat[rating == " ", rating := 0]
#   dat[, ':=' (book_id = as.numeric(book_id), rating = as.numeric(rating))]
#   dat <- dat[rating > 0]
  
#   # get the indices of the ratings
#   # add the user ratings to the existing rating matrix
#   user_ratings <- sparseMatrix(i = dat$book_id, 
#                                j = rep(1,nrow(dat)), 
#                                x = dat$rating, 
#                                dims = c(nrow(ratingmat), 1))
# }

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"

# movie data
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
# extract year
movies$Year = as.numeric(unlist(lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# ratings data
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), sep = ':', colClasses = c('integer', 'NULL'), header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# user data
users = read.csv(paste0(myurl, 'users.dat?raw=true'), sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

# dynamically grab genres
suppressMessages({
  rec_tibble = read_csv('data/genre_recommendations.csv')
})
genres = unique(rec_tibble$Genre)

##########
# SERVER #
##########

shinyServer(function(input, output, session) {
  # dynamically update the genre dropdown menu in the UI
  observe({
    updateSelectInput(session, "genre_dropdown", choices = unique(genres), label = "Select a genre:")
  })

  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = books$image_url[(i - 1) * num_movies + j], style = "max-height:150")),
                 div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_movies + j]),
                 div(style = "text-align:center", strong(books$title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", books$book_id[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  ###############################################################################################################
  #                                      Genre recommendations and results                                      #
  ###############################################################################################################

  # Calculate recommendations when the sbumbutton is clicked
  genre_df <- eventReactive(input$genreBtn, {
    withBusyIndicatorServer("genreBtn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        recommendations = get_genre_recommendations(rec_tibble, input$genre_dropdown)

        return(recommendations)
        
    }) # still busy
    
  }) # clicked on button
  

  # display the recommendations
  output$genreResults <- renderUI({
    # top 10 recommendations
    num_rows <- 2
    num_movies <- 5
    recommendations <- genre_df()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
          div(style = "text-align:center", 
              a(img(src = movies$image_url[recommendations$MovieID[(i - 1) * num_movies + j]], height = 150))
          ),
          div(style="text-align:center; font-size: 100%", 
              strong(movies$Title[recommendations$MovieID[(i - 1) * num_movies + j]])
          )
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function

  ###############################################################################################################
  #                                     Rating recommendations and results                                      #
  ###############################################################################################################
  
}) # server function
