## app.R ##
library(shiny)
source("myUI.R")
source("myserver.R")


shinyApp(ui = myUI, server = server)

