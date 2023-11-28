packages = c('tidyverse','shiny','recommenderlab','data.table','ShinyRatingInput','shinyjs')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

install.packages("shinydashboard")

runApp("app")