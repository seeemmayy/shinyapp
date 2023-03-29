library(shiny)
source("myUI.R")
source("myserver.R")

"""
Puts all the elements together and creates shiny app.
"""


shinyApp(ui = myUI, server = server)

