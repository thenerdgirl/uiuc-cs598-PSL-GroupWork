## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/init.R')
source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          # Icons: https://rstudio.github.io/shinydashboard/appearance.html#icons
          dashboardSidebar(sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("house")),
            menuItem("System I: Genre", tabName = "genre", icon = icon("film")),
            menuItem("System II: Rating", tabName = "rating", icon = icon("star"))
          )),

          dashboardBody(includeCSS("css/movies.css"),
              tabItems(
                tabItem(
                  tabName="home",
                  titlePanel("Welcome to the Movie Recommender!"),
                  mainPanel(
                    h3("Usage"),
                    p("There are two systems available to help recommend movies. Select a System from the tabs on the left and follow the steps on each page to get your movie recommendations!")
                  ),
                  fluidRow(
                    box(width = 12, title = "Meet The Authors!", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "authors",
                          box(width = 3, status = "success", solidHeader = TRUE, title = "Naomi Bhagat",
                            div(style = "text-align:center",
                                tags$img(src = 'test.jpg?raw=true', height = 150)
                            )
                          ),
                          box(width = 3, status = "success", solidHeader = TRUE, title = "Joe May",
                            div(style = "text-align:center",
                                tags$img(src = 'test.jpg?raw=true', height = 150)
                            )
                          ),
                          box(width = 3, status = "success", solidHeader = TRUE, title = "Michael Miller",
                            div(style = "text-align:center",
                                tags$img(src = 'test.jpg?raw=true', height = 150)
                            )
                          )
                      )
                    )
                  )
                ),
                tabItem(
                  tabName = "genre",
                  mainPanel(
                    h3("System I: Genre"),
                    p("In order to provide a variety of recommendations, we suggest movies from three separate categories. Our implementation generates parameterized number of recommendations from each category, then randomly samples the set for the final recommendations."),
                  ),
                  fluidRow(
                    box(width = 12, title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "selectgenre",
                            selectInput("genre_dropdown", 
                                        label = "Select a genre from the dropdown below",
                                        choices = get_genres(),
                                        selected = "Action"),
                        )
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies within this genre",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("genreBtn", "Click here to get your recommendations", class = "btn-primary")
                      ),
                      br(),
                      tableOutput("genreResults")
                    )
                  ),
                ),
                tabItem(
                  tabName = "rating",
                  fluidRow(
                    box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btn", "Click here to get your recommendations", class = "btn-primary")
                      ),
                      br(),
                      tableOutput("ratingResults")
                    )
                  ),
                )
              )
          )
    )
) 