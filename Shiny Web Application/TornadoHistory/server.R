library(shiny)
library(leaflet)
library(lubridate)
library(nycflights13)
library(tidyverse)
library(dplyr)
library(DT)

shinyServer(function(input, output) {

Datap <- read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE_P.csv", stringsAsFactors = TRUE )
Datap <- data.frame(Datap[sample(1:nrow(Datap),200),])  #On prend juste 200 pour le test
Datap$BEGIN_LAT <-  as.numeric(Datap$BEGIN_LAT)
Datap$BEGIN_LON <-  as.numeric(Datap$BEGIN_LON)


points <- eventReactive(input$idYearRange, {
  #On selectionne les tornades dans la plage de dates
  cond1 = (Datap$YEAR>=year(input$idYearRange[1])) &
                 (Datap$YEAR<=year(input$idYearRange[2]))
  #cond2 = (Datap$TOR_F_SCALE==input$idCheckGroup)
  cond2 = c()
  for (i in 1:nrow(Datap)) {
    cond2 = c(cond2,any(Datap$TOR_F_SCALE[i]==input$idCheckGroup))
  }
  Datap[which(cond1),]
  })




output$mymap <- renderLeaflet({
  #leaflet() %>% 
 # addProviderTiles(providers$Esri.NatGeoWorldMap)  %>%
#  addTiles() %>%
 # addCircleMarkers( lng=points()$BEGIN_LON, lat=points()$BEGIN_LAT, fillOpacity = 0.5, radius=3.5)
  
  leaflet(points()) %>% 
    addCircles(lng = ~BEGIN_LON, lat = ~BEGIN_LAT) %>% 
    addTiles() %>%
    addCircleMarkers(data = points(), lat =  ~BEGIN_LAT, lng =~BEGIN_LON, 
                     radius = 3, 
                     color = ~TOR_F_SCALE,
                     stroke = FALSE, fillOpacity = 0.8)
 
  
})






 })