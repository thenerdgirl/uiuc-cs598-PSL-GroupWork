## server.R

# load functions
library(dplyr)

# system 1 helper
suppressMessages({
  genre_rec_list = read_csv('data/genre_recommendations.csv')
})

##########
# SERVER #
##########

function(input, output) {}

shinyServer(function(input, output, session) {

  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
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
        
        recommendations = get_genre_recommendations(genre_rec_list, input$genre_dropdown)

        return(recommendations)
        
    }) # still busy
    
  }) # clicked on button
  

  # display the recommendations
  output$genreResults <- renderUI({
    # top 10 recommendations
    num_rows <- 2
    num_movies <- 5
    recommendations <- genre_df()

    small_image_url = "https://liangfgithub.github.io/MovieImages/"

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
          div(style = "text-align:center", 
              img(src = paste0(small_image_url, recommendations$MovieID[(i - 1) * num_movies + j], '.jpg?raw=true'), height = 150)
          ),
          div(style="text-align:center; font-size: 100%", 
              strong(recommendations$Title[(i - 1) * num_movies + j])
          )
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function

  ###############################################################################################################
  #                                     Rating recommendations and results                                      #
  ###############################################################################################################
  
  ratings_df <- eventReactive(input$genreBtn, {
    withBusyIndicatorServer("genreBtn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # TODO add stuff here for getting the ratings recommendations

        # get the user's rating data
        input_values = reactiveValuesToList(input)
        user_ratings = get_user_ratings(input_values)

        print(user_ratings)

        # return(recommendations)
        
    }) # still busy
    
  }) # clicked on button

  # display the recommendations
  output$ratingResults <- renderUI({
    # top 10 recommendations
    num_rows <- 2
    num_movies <- 5
    recommendations <- genre_df()

    small_image_url = "https://liangfgithub.github.io/MovieImages/"

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
          div(style = "text-align:center", 
              img(src = paste0(small_image_url, recommendations$MovieID[(i - 1) * num_movies + j], '.jpg?raw=true'), height = 150)
          ),
          div(style="text-align:center; font-size: 100%", 
              strong(recommendations$Title[(i - 1) * num_movies + j])
          )
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function

}) # server function
