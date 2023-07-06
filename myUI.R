library(shiny)
library(shinydashboard)
library(shinythemes)
source("homepageplots.R")


myUI <- shinyUI({navbarPage(windowTitle = "MPC Results", "Dashboard", theme = shinytheme("sandstone"),
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
                             h5("DISCLAIMER: These visualisations are for guidance only. If you are worried about the health of your pet then please contact your vet."),
                             imageOutput("bannerimage"),
                             width = 9
                           )
                  )
                ),
                  tabPanel("Methodology",
                           mainPanel(
                             p(h1("Methodology", align="center")),
                             wellPanel(style="border-color: #8b8b8b; background-color: #ffffff;", h4("These results are obtained from a combination of two methodologies: a Harmonic
                                Regression then a Gaussian Process performed on the results of the Harmonic regression."),
                             h5("Harmonic Regression is used to remove any seasonal effects of the data and then the Gaussian
                                Process is modelled on this."),
                             h5("The seasonal effects are removed prior to modelling as we can be sure the results are that of
                                other factors and not heavily influenced by a known seasonal component."),
                             br(),
                             h5("If you would like to know more about the methodologies or the model applied to the data then please expand the below boxes")),
                             checkboxInput('showPanel1','Methodology Descriptions', FALSE),
                             conditionalPanel(condition='input.showPanel1',
                               wellPanel(span(style = "background: grey", h3("Harmonic Regression"),
                                         h5("Harmonic regression is based on the principle that periodicity can be modelled using
                                            sin and cosine functions. Periodicity in this context is defined as there being a known
                                            and expected increases in cases at a specific time of the year e.g. Christmas time. It
                                            is used to remove any seasonal effects of the data.")),
                                         span(h3("Gaussian Process"),
                                         h5("A Gaussian Process is an unsupervised continuous stochastic process used for the
                                             prediction of data and provides a thorough method for classification. The mean and
                                             covariance functions determine the Gaussian Process (Davis, 2006). The Gaussian Processes
                                             are incredibly flexible in that we can have multiple priors all with different sensible
                                             distributions and they would still be able to maintain structural integrity (Brahim-Belhouari and Bermak, 2004)."),
                                          h5("Gaussian Processes work by defining a prior over functions which convert to a
                                             posterior in the presence of data. A distribution need only be defined over the
                                             functions values at a finite arbitrary set of points,\\(\\ x_1, ..., x_N\\) . They assume that
                                             \\(\\ p(f(x_1),...f(x_N)) \\) is jointly Gaussian with mean\\( \\mu(x) \\) and covariate\\( \\Sigma(x) \\) given by\\( \\Sigma_{ij} = k(x_i, x_j) \\) 
                                             where\\(\\ k \\) is a positive definite kernel function (Murphy, 2013).")))
                             ),
                             checkboxInput('showPanel2','The model applied', FALSE),
                             conditionalPanel(condition='input.showPanel2',
                                wellPanel(h4("Model:"),
                                         uiOutput('ex1'),
                                         uiOutput('ex2'),
                                         uiOutput('ex3'),
                                         h5("The model was ran on Python using PyMC3 for 6000 Markov Chain Monte Carlo iterations,
                                            using a No U-Turn Sampling method."))),
                             checkboxInput('showPanel3','References', FALSE),
                             conditionalPanel(condition='input.showPanel3',
                                wellPanel(style = "background:lightgrey", h4("References:"),
                                         h5("Richard A. Davis. “Gaussian Process”. In: Encyclopedia of Environmetrics
                                            (2006). doi: 10.1002/9780470057339.vag002"),
                                         h5("Sofiane Brahim-Belhouari and Amine Bermak. “Gaussian process for nonstationary
                                             time series prediction”. In: Computational Statistics and Data Analysis
                                             47.4 (2004), pp. 705–712. issn: 0167-9473. doi: https://doi.org/10.1016/j.csda.2004.02.006. 
                                             url: https://www.sciencedirect.com/science/article/pii/S0167947304000301."),
                                         h5("Kevin P. Murphy. Machine learning : a probabilistic perspective. Cambridge, 
                                         Mass. [u.a.]: MIT Press, 2013. isbn: 9780262044660. 
                                         url: https://www.amazon.com/Machine-Learning-Probabilistic-Perspective-Computation/dp/0262018020/ref=sr_1_2?ie=UTF8%5C&qid=1336857747%5C&sr=8-2"))))),
                
                  tabPanel("Dog",
                           sidebarLayout(
                             sidebarPanel(sliderInput("sliderdog", label = "Date Range",
                                           min = min(df$date_week_beginning),
                                           max = max(df$date_week_beginning),
                                           value=c(min(df$date_week_beginning),max(df$date_week_beginning)),
                                           timeFormat="%Y-%m-%d"),
                               radioButtons("mpcchoicedog", label = h3("MPC"),
                                            choices = list("Gastroenteric" = 'gastroenteric', "Respiratory" = 'respiratory', "Pruritus" = 'pruritus'),
                                            selected = "gastroenteric"),
                               radioButtons("regionchoicedog", label = h3("Region"),
                                            choices = unique(df$id),
                                            selected = "Nationwide"),
                               width = 2
                             ),
                             mainPanel(
                               plotOutput("dogtabgp", click = "plot_click"),
                               wellPanel("Lockdown Bars:
                                  Between March 2020 and March 2021, the UK was under some lockdown restrictions
                                  which affected the dataset. The bars represent an extra weight added.
                                  The lighter bar represents at least one lockdown restriction in place and the darker
                                  blue bar represents national lockdowns."),
                               h5("DISCLAIMER: These visualisations are for guidance only. If you are worried about the health of your pet then please contact your vet."),
                               width = 10
                             )
                           )
                  ),
                  tabPanel("Cat",
                           sidebarLayout(
                             sidebarPanel(sliderInput("slidercat", label = "Date Range",
                                           min = min(df$date_week_beginning),
                                           max = max(df$date_week_beginning),
                                           value=c(min(df$date_week_beginning),max(df$date_week_beginning)),
                                           timeFormat="%Y-%m-%d"),
                               radioButtons("mpcchoicecat", label = h3("MPC"),
                                            choices = list("Gastroenteric" = 'gastroenteric', "Respiratory" = 'respiratory', "Pruritus" = 'pruritus'),
                                            selected = "gastroenteric"),
                               radioButtons("regionchoicecat", label = h3("Region"),
                                            choices = unique(df$id),
                                            selected = "Nationwide"),
                               width = 2
                             ),
                             mainPanel(
                               plotOutput("cattabgp", click = "plot_click"),
                               wellPanel("Lockdown Bars:
                                  Between March 2020 and March 2021, the UK was under some lockdown restrictions
                                  which affected the dataset. The bars represent an extra weight added.
                                  The lighter bar represents at least one lockdown restriction in place and the darker
                                  blue bar represents national lockdowns."),
                               h5("DISCLAIMER: These visualisations are for guidance only. If you are worried about the health of your pet then please contact your vet."),
                               width = 10
                             )
                           )
                  ),
                  
  )
})
