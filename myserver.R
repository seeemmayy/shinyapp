
server <- function(input, output) {
  
  ############ dog tab plots
  
  
  df_subsetdog <- reactive({
    a <- subset(df, species == 'dog')
    b <- subset(a, mpc == input$mpcchoicedog)
    c <- subset(b, id == input$regionchoicedog)
    return(c)
  })
  
  
  
  output$dogtabgp <- renderPlot({
    ggplot(df_subsetdog(), aes(date_week_beginning,prediction, linetype="Prediction"), group=1) + 
      geom_line()+
      coord_cartesian(xlim=input$sliderdog) +
      annotate(geom = "rect", xmin = as.Date('2020-03-21'), xmax = as.Date('2020-07-03'), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)+
      annotate(geom = "rect", xmin = as.Date('2020-09-17'), xmax = as.Date('2020-12-03'), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)+
      annotate(geom = "rect", xmin = as.Date('2021-01-02'), xmax = as.Date('2021-03-31'), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)+
      annotate(geom = "rect", xmin = as.Date('2020-03-21'), xmax = as.Date('2021-03-31'), ymin = -Inf, ymax = Inf, fill = "light blue", alpha = 0.4)+
      geom_ribbon(aes(x=date_week_beginning, ymin=percentile1, ymax=percentile4, fill = "99% Credible Interval"), alpha=.4) +
      geom_ribbon(aes(x=date_week_beginning, ymin=percentile2, ymax=percentile3, fill = "95% Credible Interval"), alpha=.4) +
      geom_point(aes(y = mpc_value, colour=legendlabels))+
      scale_x_date(labels=date_format("%b %y"), date_breaks = "1 month")+
      ggtitle(paste("Plot of",input$mpcchoicedog,"main presenting complaint in Dogs",input$regionchoicedog))+
      xlab("Date")+
      ylab("Prevalence per 1000 consults")+
      theme(legend.margin = margin(t = -35), text = element_text(size=20), axis.text.x=element_text(size=rel(0.75), angle=90),
                            legend.text=element_text(size=15), legend.background=element_rect(fill = alpha("white", 0.5)))+
      scale_fill_manual(name='', values=c("95% Credible Interval" = "grey", "99% Credible Interval" = "light grey"))+
      scale_colour_manual(name="", values=c("orange","palegreen","red",NA))+
      scale_linetype(name="")
    
  })
  
  
  
  ######## cat tab plots
  
  
  df_subsetcat <- reactive({
    a <- subset(df, species == 'cat')
    b <- subset(a, mpc == input$mpcchoicecat)
    c <- subset(b, id == input$regionchoicecat)
    return(c)
  })
  
  
  output$cattabgp <- renderPlot({
    ggplot(df_subsetcat(), aes(date_week_beginning,prediction, linetype="Prediction"), group=1) + 
      geom_line()+
      coord_cartesian(xlim=input$slidercat) +
      annotate(geom = "rect", xmin = as.Date('2020-03-21'), xmax = as.Date('2020-07-03'), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)+
      annotate(geom = "rect", xmin = as.Date('2020-09-17'), xmax = as.Date('2020-12-03'), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)+
      annotate(geom = "rect", xmin = as.Date('2021-01-02'), xmax = as.Date('2021-03-31'), ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.3)+
      annotate(geom = "rect", xmin = as.Date('2020-03-21'), xmax = as.Date('2021-03-31'), ymin = -Inf, ymax = Inf, fill = "light blue", alpha = 0.4)+
      geom_ribbon(aes(x=date_week_beginning, ymin=percentile1, ymax=percentile4, fill = "99% Credible Interval"), alpha=.4) +
      geom_ribbon(aes(x=date_week_beginning, ymin=percentile2, ymax=percentile3, fill = "95% Credible Interval"), alpha=.4) +
      geom_point(aes(y = mpc_value, colour=legendlabels))+
      scale_x_date(labels=date_format("%b %y"), date_breaks = "1 month")+
      ggtitle(paste("Plot of",input$mpcchoicecat,"main presenting complaint in Cats",input$regionchoicecat))+
      xlab("Date")+
      ylab("Prevalence per 1000 consults")+
      theme(legend.margin = margin(t = -35), text = element_text(size=20), axis.text.x=element_text(size=rel(0.75), angle=90),
            legend.text=element_text(size=15), legend.background=element_rect(fill = alpha("white", 0.5)))+
      scale_fill_manual(name='', values=c("95% Credible Interval" = "grey", "99% Credible Interval" = "light grey"))+
      scale_colour_manual(name="", values=c("orange","palegreen","red",NA))+
      scale_linetype(name="")
    
  })
  
  ############### home page plots & links
  
  output$homepageplot1 <-renderPlotly({
    subplot(plpdoggi,plpdogresp,plpdogprur,plpcatgi,plpcatresp,plpcatprur, nrows=2, titleX=TRUE, titleY = TRUE, margin = 0.01)
     
    })
  
  url <- a("SAVSNet Homepage", href="https://www.liverpool.ac.uk/savsnet/")
  output$tab <- renderUI({
    tagList(url)
  })
  
  
}