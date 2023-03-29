library(shiny)
library(shinydashboard)
library(shinythemes)
source("homepageplots.R")


"""
The User Interface creation for the different pages which houses all the text,
the plots from the homepage, the different selections for main presenting complaint
and region and the slider timescale.
"""

myUI <- shinyUI({
  navbarPage("Dashboard", theme = shinytheme("yeti"),
                  tabPanel("Home",
                        sidebarLayout(
                          sidebarPanel(h3("SAVSNet Consultation data results"),
                             h4("This dashboard displays the statistically analysed
                             results of SAVSNet veterinary consultation data for the 3 
                             main presenting complaints (Gastroenteric, Respiratory and Pruritus) 
                             for both cats and dogs."),
                             br(),
                             h4("This homepage displays the topline view of each of the illnesses
                             for the species displayed on each map. They have been split into
                             region and country with a traffic light system to display the 
                             higher than average regions."),
                             br(),
                             h4("For more information about SAVSNet please visit", uiOutput("tab")),
                             h3("Interpreting the maps"),
                             h4("The maps are divided into Countries of the United Kingdom and regions of England, 
                             also known as ITL 1 (International Terrotorial
                             Level) and NUTS 1 (Nomenclature of Territorial Units of Statistics).
                             They display a traffic light system and can be interpreted as:"),
                             br(),
                             h4("Green: Area is fine, no cause for concern."),
                             h4("Orange: Monitor."),
                             h4("Red: Above credible intervals, cause for concern."),
                             h4("Grey: No data available for selected region this week."),
                             width=3
                           ),
                           mainPanel(
                             h3("Consults of the 3 main presenting complaints within dogs and cats in Savsnet Veterinary surgeries as of ",  as.character(max(df$date_week_beginning), "%d-%m-%Y \n \n \n" )),
                             plotlyOutput("homepageplot1", height = "700px"),
                             width = 9
                           )
                  )
                ),
                  tabPanel("Methodology",
                           mainPanel(
                               h3("Methodology"),
                               h3("Harmonic Regression"),
                               h4('text here')
                               
                             )
                           ),
                  tabPanel("Dog",
                           sidebarLayout(
                             sidebarPanel(
                               h3("Lockdown Bars"),
                               h4("Between March 2020 and March 2021, the UK was under some lockdown restrictions
                                  which affected the dataset. The bars represent an extra weight added.
                                  The lighter bar represents at least one lockdown restriction in place and the darker
                                  blue bar represents national lockdowns."),
                               radioButtons("mpcchoicedog", label = h3("MPC"),
                                            choices = list("Gastroenteric" = 'gastroenteric', "Respiratory" = 'respiratory', "Pruritus" = 'pruritus'),
                                            selected = "gastroenteric"),
                               radioButtons("regionchoicedog", label = h3("Region"),
                                            choices = unique(df$id),
                                            selected = "Nationwide"),
                               sliderInput("sliderdog", label = "Date Range",
                                           min = min(df$date_week_beginning),
                                           max = max(df$date_week_beginning),
                                           value=c(min(df$date_week_beginning),max(df$date_week_beginning)),
                                           timeFormat="%Y-%m-%d"),
                               width = 2
                             ),
                             mainPanel(
                               plotOutput("dogtabgp", click = "plot_click"),
                               width = 10
                             )
                           )
                  ),
                  tabPanel("Cat",
                           sidebarLayout(
                             sidebarPanel(
                               h3("Lockdown Bars"),
                               h4("Between March 2020 and March 2021, the UK was under some lockdown restrictions
                                  which affected the dataset. The bars represent an extra weight added.
                                  The lighter bar represents at least one lockdown restriction in place and the darker
                                  blue bar represents national lockdowns."),
                               radioButtons("mpcchoicecat", label = h3("MPC"),
                                            choices = list("Gastroenteric" = 'gastroenteric', "Respiratory" = 'respiratory', "Pruritus" = 'pruritus'),
                                            selected = "gastroenteric"),
                               radioButtons("regionchoicecat", label = h3("Region"),
                                            choices = unique(df$id),
                                            selected = "Nationwide"),
                               sliderInput("slidercat", label = "Date Range",
                                           min = min(df$date_week_beginning),
                                           max = max(df$date_week_beginning),
                                           value=c(min(df$date_week_beginning),max(df$date_week_beginning)),
                                           timeFormat="%Y-%m-%d"),
                               width = 2
                             ),
                             mainPanel(
                               plotOutput("cattabgp", click = "plot_click"),
                               width = 10
                             )
                           )
                  ),
                  
  )
})
