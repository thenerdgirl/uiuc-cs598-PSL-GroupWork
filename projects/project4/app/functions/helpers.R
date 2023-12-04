# All the code in this file needs to be copied to your Shiny app, and you need
# to call `withBusyIndicatorUI()` and `withBusyIndicatorServer()` in your app.
# You can also include the `appCSS` in your UI, as the example app shows.

# =============================================

# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                     time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

appCSS <- "
.btn-loading-container {
  margin-left: 10px;
  font-size: 1.2em;
}
.btn-done-indicator {
  color: green;
}
.btn-err {
  margin-top: 10px;
  color: red;
}
"

# From professor
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

############
# System 1 #
############

get_genres = function() {
  suppressMessages({
    rec_tibble = read_csv('data/genre_recommendations.csv')
  })
  genres = unique(rec_tibble$Genre)
  return(genres)
}

get_genre_recommendations = function(movies, genre) {
  return(movies %>% filter(Genre == genre))
}

############
# System 2 #
############

myIBCF = function(newuser){
  # Download similarity matrix S, limit predictions, and add labels to the newuser's ratings
  similarity_matrix = read.csv("data/S.csv", row.names = 1)
  recommend_limit = 10
  names(newuser) = colnames(similarity_matrix)
  
  # Get sum product of Sli and wi for movie l (right hand side/numerator of equation)
  sr = colSums(similarity_matrix * t(newuser), na.rm = TRUE)
  
  # Get sum of similarity values for movie l (left hand side/denominator of equation)
  s = rowSums(similarity_matrix, na.rm = TRUE)
  
  # Compute predicted rank, so long as denominator =/= zero
  predicted_ranks = ifelse(s != 0, sr/s, NA)
  
  # Sort predicted ranks and count number of predictions
  ordered_predictions = order(predicted_ranks, decreasing = TRUE)
  predictions=sum(predicted_ranks>0, na.rm = TRUE)

  # Determine if there are enough predictions or if an alternative method must be determined
  if(predictions >= recommend_limit){
    # If there are enough predictions, get the index for the predictions, and return the movieID
    ordered_predictions_index = ordered_predictions[1:recommend_limit]  
    suggestion_names = names(newuser)[ordered_predictions_index]
  } else {
    # If there aren't enough predictions, append alphabetically sorted movies, and return the movieID
    ordered_predictions_index = ordered_predictions[1:predictions]
    prediction_names = names(newuser)[ordered_predictions_index]
    alphabetical_movies = movies$MovieID[order(movies$Title)]
    suggestion_names = c(prediction_names, alphabetical_movies)[1:recommend_limit] 
  }
  return(suggestion_names)
}

get_rating_recommendations = function(user) {
  # Get name of dataframe input for purposes of printing name
  username = deparse(substitute(user))
  # cat("The top 10 recommended movies for",substitute(username),"are:\n")
  
  # Run user rating vector through IBCF
  IBCF = myIBCF(user)

  # Remove m to create ID vector compatible with above movie dataframe
  rec_addresses = sub("^m", "", IBCF)
  
  # Get movie titles, corresponding rank, and print top 10
  titles = movies$Title[match(rec_addresses, movies$MovieID)]

  print(rec_addresses)
  
  # listed_titles = paste0(seq_along(titles), ") ", titles)
  # cat(listed_titles, sep = "\n")
  # return(titles)
}
