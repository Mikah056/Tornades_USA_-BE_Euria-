
Datap <- read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE_P.csv", stringsAsFactors = TRUE )
Datap <- data.frame(Datap[sample(1:nrow(Datap),300),])  #On prend juste 300 pour le test
Datap$BEGIN_LAT <-  as.numeric(Datap$BEGIN_LAT)
Datap$BEGIN_LON <-  as.numeric(Datap$BEGIN_LON)


function(input, output, session) {
  
  selectedData1 <- reactive({
    Datap %>%
      filter(Datap$MONTH_NAME %in% input$tri_mois)
  })
  
  
  selectedData2 <- reactive({
    selectedData1() %>%
      filter(selectedData1()$TOR_F_SCALE %in% input$Echelle)
  })
  
  
  selectedData3 <- reactive({
    selectedData2() %>% 
  filter(selectedData2()$YEAR >= input$year_inf)
  })
  
  selectedData4 <- reactive({
    selectedData3() %>% 
      filter(selectedData3()$YEAR <= input$year_sup)    
  })
 
  
 
  # L'observe permet de maintenir la réactivité sur les couleurs
  observe({  
  colorBy <- c("EF0","EF1","EF2","EF3","EF4","EF5") 
  pal <- colorFactor(c("#26C4EC","#00FF00","#C2F732","#ED7F10","#FF0000","#F9429E"), colorBy, ordered = TRUE) 
  
output$mymap <- renderLeaflet({

    leaflet() %>% 
      addTiles() %>%
      addCircleMarkers(data = selectedData4(), lat =  ~BEGIN_LAT, lng = ~BEGIN_LON, 
                       radius = 3, 
                       fillColor = pal(selectedData4()$TOR_F_SCALE),
                       stroke = FALSE, fillOpacity = 0.8) %>%
     addLegend("bottomleft", pal=pal, values=selectedData4()$TOR_F_SCALE,
              layerId="colorLegend")
  
  })
  })
  


  
  
  
  

  
}





