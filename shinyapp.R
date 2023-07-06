## app.R ##
library(shiny)
library(shinyBS)
source("myUI.R")
source("myserver.R")


shinyApp(ui = myUI, server = server)

