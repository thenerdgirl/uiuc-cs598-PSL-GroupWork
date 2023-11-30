packages = c('tidyverse','shiny','recommenderlab','data.table','shinyjs','devtools')

# if packages don't exist, install. Then call library on them
for (package in packages) {
  if (!requireNamespace(package, quietly=TRUE)) {
    install.packages(package)
  }
  library(package, character.only=TRUE)
}

install.packages("shinydashboard")
devtools::install_github("stefanwilhelm/ShinyRatingInput")

runApp("app")