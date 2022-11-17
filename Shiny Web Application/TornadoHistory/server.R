library(shiny)
library(leaflet)
library(lubridate)
library(nycflights13)


shinyServer(function(input, output) {

Datap <- read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE_P.csv", stringsAsFactors = TRUE )
Datap <- data.frame(Datap[sample(1:nrow(Datap),400),])
Datap$BEGIN_LAT <-  as.numeric(Datap$BEGIN_LAT)
Datap$BEGIN_LON <-  as.numeric(Datap$BEGIN_LON)


points <- eventReactive(input$idYearRange, {
  #On selectionne les tornades dans la plage de dattes
  select = which((Datap$YEAR>=input$idYearRange[1]) & (Datap$YEAR<=input$idYearRange[2]))
  cbind(Datap$BEGIN_LAT[select], Datap$BEGIN_LON[select])
}, ignoreNULL = FALSE)

output$mymap <- renderLeaflet({
  leaflet(data = Datap[1:100,]) %>% 
  addTiles() %>%
  addCircleMarkers(Datap$BEGIN_LON, Datap$BEGIN_LAT,stroke = FALSE, fillOpacity = 0.6, radius=5)
  
  
  #renderLeaflet({
  #leaflet() %>% addTiles() #%>%
    #addMarkers(datap = points())
})






 })