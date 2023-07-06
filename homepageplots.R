library(ggplot2)
library(ggpubr)
library(tidyverse)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(rgeos)
library(broom)
library(plyr)
library(sf)
library(shinythemes)
library(plotly)
library(scales)


df <- read.csv("https://fhm-chicas-storage.lancs.ac.uk/savsnet-agile-artefacts/public/plot_data_for_alan/full_dog_cat_plot.csv")
df$date_week_beginning = as.Date(df$date_week_beginning,"%Y-%m-%d")
names(df)[names(df) == 'region'] <- 'id'
df$id <- ifelse(df$id == "East", "East of England", df$id)
df$mapstatus <- factor(df$mapstatus, levels=c("low","medium","high"))
df$legendlabels <- fct_collapse(df$gpcolour, "0.01<p<=0.05" = "orange", "p<=0.01" = "red", "0.05<p" = "palegreen")

shapefile <- st_read(".", layer="NUTS1_Jan_2018_SGCB_in_the_UKedited")
names(shapefile)[names(shapefile) == "nuts118nm"] <- "id"



filter_data_species_mpc <- function(df, species_name, mpc_name){
  finaldata1 <- df %>% filter(species==species_name, mpc == mpc_name) %>% 
    group_by(id) %>%
    filter(date_week_beginning == max(date_week_beginning))
  return(finaldata1)
}
mergedata <- function(shapefile, df){
  join <- left_join(shapefile, df, by="id")
  return(join)
}

plotdata <- function(df, species, mpc_name, shapefile){
  df1 <- filter_data_species_mpc(df, species, mpc_name)
  mergedf <- mergedata(shapefile,df1)
  return(mergedf)
}

doggi <- plotdata(df,"dog", "gastroenteric", shapefile)
dogresp <- plotdata(df,"dog", "respiratory", shapefile)
dogprur <- plotdata(df,"dog", "pruritus", shapefile)
catgi <- plotdata(df,"cat", "gastroenteric", shapefile)
catresp <- plotdata(df,"cat", "respiratory", shapefile)
catprur <- plotdata(df,"cat", "pruritus", shapefile)



pdoggi <- ggplot()+
  geom_sf(data=doggi, aes(text = paste("<b>Date: </b>", date_week_beginning,"<br>",
                                             "<b>Region: </b>", id,"<br>",
                                             "<b>MPC Value: </b> ", round(mpc_value), "<br>"),
                          fill=mapstatus))+
  coord_sf()+
  scale_fill_manual(values=c("#28A197","#F46A25","#801650"), limits=levels(doggi$mapstatus))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

plpdoggi <- ggplotly(pdoggi, tooltip = "text") %>% 
  config(displayModeBar = FALSE) %>%
  layout(hoverlabel = list(bgcolor = "white",
                           font = list(family = "Georgia")),
         yaxis=list(title=c("Dog")))



pdogresp <- ggplot()+
  geom_sf(data=dogresp, aes(text = paste("<b>Date: </b>", date_week_beginning,"<br>",
                                         "<b>Region: </b>", id,"<br>",
                                         "<b>MPC Value: </b> ", round(mpc_value), "<br>"),
                            fill=mapstatus))+
  coord_sf()+
  scale_fill_manual(values=c("#28A197","#F46A25","#801650"), limits=levels(dogresp$mapstatus))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

plpdogresp <- ggplotly(pdogresp, tooltip = "text") %>% 
  config(displayModeBar = FALSE) %>%
  layout(hoverlabel = list(bgcolor = "white",
                           font = list(family = "Georgia")))


pdogprur <- ggplot()+
  geom_sf(data=dogprur, aes(text = paste("<b>Date: </b>", date_week_beginning,"<br>",
                                         "<b>Region: </b>", id,"<br>",
                                         "<b>MPC Value: </b> ", round(mpc_value), "<br>"),
                            fill=mapstatus))+
  coord_sf()+
  scale_fill_manual(values=c("#28A197","#F46A25","#801650"), limits=levels(dogprur$mapstatus))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

plpdogprur <- ggplotly(pdogprur, tooltip = "text") %>% 
  config(displayModeBar = FALSE) %>%
  layout(hoverlabel = list(bgcolor = "white",
                           font = list(family = "Georgia")))



pcatgi <- ggplot()+
  geom_sf(data=catgi, aes(text = paste("<b>Date: </b>", date_week_beginning,"<br>",
                                       "<b>Region: </b>", id,"<br>",
                                       "<b>MPC Value: </b> ", round(mpc_value), "<br>"),
                          fill=mapstatus))+
  coord_sf()+
  scale_fill_manual(values=c("#28A197","#F46A25","#801650"), limits=levels(catgi$mapstatus))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

plpcatgi <- ggplotly(pcatgi, tooltip = "text") %>% 
  config(displayModeBar = FALSE) %>%
  layout(hoverlabel = list(bgcolor = "white",
                           font = list(family = "Georgia")),
         yaxis=list(title=c("Cat")), xaxis=list(title=c("Gastroenteric")))


pcatresp <- ggplot()+
  geom_sf(data=catresp, aes(text = paste("<b>Date: </b>", date_week_beginning,"<br>",
                                        "<b>Region: </b>", id,"<br>",
                                        "<b>MPC Value: </b> ", round(mpc_value), "<br>"),
                            fill=mapstatus))+
  coord_sf()+
  scale_fill_manual(values=c("#28A197","#F46A25","#801650"), limits=levels(catresp$mapstatus))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

plpcatresp <- ggplotly(pcatresp, tooltip = "text") %>% 
  config(displayModeBar = FALSE) %>%
  layout(hoverlabel = list(bgcolor = "white",
                           font = list(family = "Georgia")), xaxis=list(title=c("Respiratory")))


pcatprur <- ggplot()+
  geom_sf(data=catprur, aes(text = paste("<b>Date: </b>", date_week_beginning,"<br>",
                                         "<b>Region: </b>", id,"<br>",
                                         "<b>MPC Value: </b> ", round(mpc_value), "<br>"),
                            fill=mapstatus))+
  coord_sf()+
  scale_fill_manual(values=c("#28A197","#F46A25","#801650"), limits=levels(catprur$mapstatus))+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'none')+
  theme(axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

plpcatprur <- ggplotly(pcatprur, tooltip = "text") %>% 
  config(displayModeBar = FALSE) %>%
  layout(hoverlabel = list(bgcolor = "white",
                           font = list(family = "Georgia")), xaxis=list(title=c("Pruritus")))



