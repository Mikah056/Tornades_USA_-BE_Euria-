
#Téléchargement des données
Datap <- read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE_P.csv", stringsAsFactors = TRUE )
Datap <- data.frame(Datap[sample(1:nrow(Datap),1000),])  #On prend juste 300 pour le test
Datap$BEGIN_LAT <-  as.numeric(Datap$BEGIN_LAT)
Datap$BEGIN_LON <-  as.numeric(Datap$BEGIN_LON)


function(input, output, session) {
 
  #Prise en compte du tri suivant les mois 
  selectedData1 <- reactive({
    Datap %>%
      filter(Datap$MONTH_NAME %in% input$tri_mois)
  })
  
 #Prise en compte du tri suivant les échelles 
  selectedData2 <- reactive({
    selectedData1() %>%
      filter(selectedData1()$TOR_F_SCALE %in% input$Echelle)
  })
  
 #Prise en compte du tri suivant les années 
  selectedData3 <- reactive({
    selectedData2() %>% 
  filter(selectedData2()$YEAR >= input$year_inf)
  })
  
  selectedData4 <- reactive({
    selectedData3() %>% 
      filter(selectedData3()$YEAR <= input$year_sup)    
  })
  
  #PARTIE ROMAIN
  
  #graphique des stats descriptive
  datasetInput<- reactive({
    Datap 
  })
  
  output$graph<-renderPlot({
    library(gridExtra)
    library(cowplot)
    vaz= datasetInput()
    #Evolution des fréquence au fil des années
    library(ggplot2)
    f1 = as.data.frame(table(vaz$YEAR))
    f1[,1] = as.factor(f1[,1])
    f = ggplot(data=f1,aes(x=Var1,y=Freq,group=1)) +
      geom_line(color="blue") +
      geom_point() +
      xlab("Annees") + ylab("Fréquence des tornades") +
      theme(axis.text.x = element_text(angle=70, vjust=0.5))
    f 
    
  })
  output$graph1=renderPlot({
    vaz=datasetInput()
    
    a = ggplot(vaz, aes(x = TOR_F_SCALE)) +
      geom_bar(stat = "count") +                      # Bâtons
      theme(legend.position = "none") +               # Enlève la légende
      xlab("Echelle de Fujita") +                                      # Axe des x, non pertinent
      ylab("fréquence")                               # Axe des y
    a
    
  })
 

  
  # Contenu du popup en HTML
  selectedData5 <- reactive({
    selectedData4() %>% 
      mutate(
        popup = paste0(
          "<b>","State : ", STATE,
          "<br>","<b>","Begin date : " , BEGIN_DATE_TIME,
          "<br>","<b>","Begin location : ", BEGIN_LOCATION,
          "<br>","<b>","Ene date : " , END_DATE_TIME,
          "<br>","<b>","End location : ", END_LOCATION
        )
      )
  })
  

  
  
  ############################### Colorier les etats par niveau de sinistralité
  
  
  
  
  
  
  
  
  ########################################### FIN
  
  
  
  # L'observe permet principalement de maintenir la réactivité sur les couleurs
  observe({  
    colorBy <- c("EF0","EF1","EF2","EF3","EF4","EF5") 
    pal <- colorFactor(c("#26C4EC","#00FF00","#C2F732","#ED7F10","#FF0000","#F9429E"), colorBy, ordered = TRUE) 
    
    
  ########### Construction des vecteurs de trajectoire    
    lines = list()
    for (i in 1:nrow(selectedData5())) { 
      # Créer un objet de points pour chaque ligne
      pointsi <- sp::SpatialPoints(coords = data.frame(lon = c(selectedData5()$BEGIN_LON[i], selectedData5()$END_LON[i]), 
                                                       lat = c(selectedData5()$BEGIN_LAT[i], selectedData5()$END_LAT[i])))
      
      # Créer un objet SpatialLines pour chaque ligne
      lines[[i]] <- sp::Lines(list(sp::Line(pointsi)), ID = paste0("ligne",i))
    }
    
    # Créer un objet SpatialLines qui contient toutes les lignes
    lines <- sp::SpatialLines(lines)
    
    # Créer un data.frame avec les données attributaires associées à chaque ligne
    attribs <- data.frame(couleur = selectedData5()$TOR_F_SCALE)
    row.names(attribs) = row.names(lines)
    
    # Créer un objet SpatialLinesDataFrame qui contient les lignes et les données attributaires
    sldf <- sp::SpatialLinesDataFrame(lines, data = attribs) 
  
    #Construction du Geojson de Linestring
    geojson <- geojson_json(sldf)
    
    
  #  lines_sf <- geojsonio::geojson_sf(geojson)
    # Créer une liste de couleurs
   # couleurs <- c("EF0" = "#26C4EC", "EF1" = "#00FF00", "EF2" = "#C2F732", "EF3" = "ED7F10","EF4" = "#FF0000", "EF5" = "#F9429E")
   # colorScale = c("#26C4EC","#00FF00","#C2F732","#ED7F10","#FF0000","#F9429E")
    
    # Créer une palette de couleurs pour l'intensité
   
    
    # intensity_colors <- ggplot2::scale_color_manual(values = c("EF0" = "#26C4EC",
     #    "EF1" = "#00FF00", "EF2" = "#C2F732", "EF3" = "#ED7F10","EF4" = "#FF0000", "EF5" = "#F9429E"))
    
    
    
    getColor = function(d) {
      return (d == "EF0" ? '#26C4EC' :
        d == "EF1"  ? '#00FF00' :
        d == "EF2"  ? '#C2F732' :
        d == "EF3"  ? '#ED7F10' :
        d == "EF4"   ? '#FF0000' :
        d == "EF5"   ? '#F9429E'
        )
    } 
    
    
    style = function (feature) {
      return ( 
        fillColor: getColor(feature.properties.couleur),
        weight: 2,
        opacity: 1,
        color: 'white',
        dashArray: '3',
        fillOpacity: 0.7
      );
    }
    
    
    
    
######## FIN Vecteurs   
    
    
    
################ Construction de la base pour colorier les etats    
    
    
    
    
    
    
 ##################### Fin    
    
    
    output$mymap <- renderLeaflet({
      
    leaflet(selectedData5()) %>% setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
        addProviderTiles("Esri.OceanBasemap") %>%
     #   addPolylines(data = sldf, color = ~colorFactor(sldf$couleur, palette = colorScale))  %>%
        addGeoJSON(geojson,
                 weight = 4) %>%
     #   addPolylines(data = lines_sf, color = ~couleur, weight = 4) %>%
        addCircleMarkers(data = selectedData5(), lat =  ~BEGIN_LAT, lng = ~BEGIN_LON, 
                         radius = 3, 
                         fillColor = pal(selectedData5()$TOR_F_SCALE),
                         popup = ~popup, popupOptions = 
                           list(maxHeight = 150, maxWidth = 200),
                         stroke = FALSE, fillOpacity = 0.8) %>%
        addLegend("bottomleft", pal=pal, values=selectedData5()$TOR_F_SCALE,
                  layerId="colorLegend") 
      
      
      ############################### Colorier les etats par niveau de sinistralité 
      
      
      
      ########################################### FIN
      
      
    })
  })
  
  
  
  
  
  
  
  
  
}





