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
                    p("There are two systems available to help recommend movies.")
                  )
                ),
                tabItem(
                  tabName = "genre",
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