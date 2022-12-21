#library(shiny)
#library(leaflet)
#library(lubridate)
#library(nycflights13)
#library(tidyverse)
#library(dplyr)
#library(DT)

#shinyServer(function(input, output) {

Datap <- read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE_P.csv", stringsAsFactors = TRUE )
Datap <- data.frame(Datap[sample(1:nrow(Datap),300),])  #On prend juste 300 pour le test
Datap$BEGIN_LAT <-  as.numeric(Datap$BEGIN_LAT)
Datap$BEGIN_LON <-  as.numeric(Datap$BEGIN_LON)


#points <- eventReactive(input$idYearRange, {
  #On selectionne les tornades dans la plage de dates
#  cond1 = (Datap$YEAR>=year(input$idYearRange[1])) &
 #                (Datap$YEAR<=year(input$idYearRange[2]))
  #cond2 = (Datap$TOR_F_SCALE==input$idCheckGroup)
#  cond2 = c()
#  for (i in 1:nrow(Datap)) {
#    cond2 = c(cond2,any(Datap$TOR_F_SCALE[i]==input$idCheckGroup))
#  }
#  p = which(cond1 & cond2)
#  Datap[p,]
#  })




#output$mymap <- renderLeaflet({
  #leaflet() %>% 
 # addProviderTiles(providers$Esri.NatGeoWorldMap)  %>%
#  addTiles() %>%
 # addCircleMarkers( lng=points()$BEGIN_LON, lat=points()$BEGIN_LAT, fillOpacity = 0.5, radius=3.5)
  
#  leaflet(points()) %>% 
#    addCircles(lng = ~BEGIN_LON, lat = ~BEGIN_LAT) %>% 
#    addTiles() %>%
#    addCircleMarkers(data = points(), lat =  ~BEGIN_LAT, lng =~BEGIN_LON, 
#                     radius = 3, 
#                     color = ~TOR_F_SCALE,
#                     stroke = FALSE, fillOpacity = 0.8)
 
  
#})






# })








function(input, output, session) {
  
  selectedData1 <- reactive({
    Datap %>%
      filter(Datap$MONTH_NAME %in% input$tri_mois)
  })
  
  
  
  selectedData2 <- reactive({
    selectedData1() %>%
 #     select(1,4,5,6,7,10,39,26,34,24,25,12,13,14,15,16,23,17,
 #            18,19,20,28,27,22,21,29,30,31,32,33) %>%
      filter(selectedData1()$TOR_F_SCALE %in% input$Echelle)  # %>%
 #     filter(selectedData1()$YEAR >= input$year_inf) %>%
  #    filter(selectedData1()$YEAR <= input$year_sup)      # %>%
 #     filter(height >= input$height[1])  %>%
 #     filter(height <= input$height[2]) 
  })
  
  
  selectedData3 <- reactive({
    selectedData2() %>% 
  filter(selectedData2()$YEAR >= input$year_inf)
  })
  
  selectedData4 <- reactive({
    selectedData3() %>% 
      filter(selectedData3()$YEAR <= input$year_sup)    
  })
  
  
#  selectedData3 <- reactive({
#    data %>%
#      select(1,4,5,6,7,10,39,26,34,24,25,12,13,14,15,16,23,17,
#             18,19,20,28,27,22,21,29,30,31,32,33) %>%
#      filter(data$player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player))) 
#    
#  })
  
#  selectedData4 <- reactive({
#    rbind(selectedData3(),selectedData2())
    
#  })
  

  
output$mymap <- renderLeaflet({

    leaflet(selectedData4()) %>% 
      addCircles(lng = ~BEGIN_LON, lat = ~BEGIN_LAT) %>% 
      addTiles() %>%
      addCircleMarkers(data = selectedData4(), lat =  ~BEGIN_LAT, lng = ~BEGIN_LON, 
                       radius = 3, 
                       color = ~TOR_F_SCALE,
                       stroke = FALSE, fillOpacity = 0.8)
  
  
  })
  
  
  
  
  
  
  

  
}















