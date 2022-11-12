library(shiny)
library(leaflet)
library(lubridate)
library(nycflights13)


shinyServer(function(input, output) {

Data <- read.csv("data/Base_BE_P.csv", stringsAsFactors = TRUE )
Data <- data.frame(Data)
Data$BEGIN_LAT <-  as.numeric(Data$BEGIN_LAT)
Data$BEGIN_LON <-  as.numeric(Data$BEGIN_LON)


points <- eventReactive(input$idYearRange, {
  #On selectionne les tornades dans la plage de dattes
  select = which((Data$YEAR>=input$idYearRange[1]) & (Data$YEAR<=input$idYearRange[2]))
  cbind(Data$BEGIN_LAT[select], Data$BEGIN_LON[select])
}, ignoreNULL = FALSE)

output$mymap <- renderLeaflet({
  leaflet() %>% addTiles() %>%
    addMarkers(data = points())
})






})