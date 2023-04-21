
#Téléchargement des données
Datap <- read.csv("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE.csv", stringsAsFactors = TRUE )
Datap <- data.frame(Datap[which(Datap$YEAR>1996),])  #On prend des 1996 pour eviter les donnees manquantes dans un premier temps
Datap$BEGIN_LAT <-  as.numeric(Datap$BEGIN_LAT)
Datap$BEGIN_LON <-  as.numeric(Datap$BEGIN_LON)

# Charger le fichier GeoJSON des formes des etats
states = geojson_read("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/State_geometry.geojson", what = "sp")

#Mettre les noms des etats en majuscule
states$name <- toupper(states$name)


function(input, output, session) {
 
######################################################################
##################################### PRISE EN COMPTE DES TRIS
  
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
  

################################# Fin de la prise en compte des tris 
###########################################################################  
  
  
  
  
  
  # L'observe permet principalement de maintenir la réactivité sur les couleurs
  observe({  
    colorBy <- c("EF0","EF1","EF2","EF3","EF4","EF5") 
    pal <- colorFactor(c("#26C4EC","#00FF00","#C2F732","#ED7F10","#FF0000","#F9429E"), colorBy, ordered = TRUE) 
    
#################################################################
###################### Construction des vecteurs de trajectoire    
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
    
######################## FIN de la construction des Vecteurs
#################################################################
       
    
#################################################################    
############################ Cette partie defini le chronoplet
    
    #Calculer la table de proportion par etat
    tab <-  table(selectedData5()$STATE)/(max(selectedData5()$YEAR)-min(selectedData5()$YEAR)+1)
    
    # Récupérer les noms des états qui ne se trouvent pas dans tab
    etats_manquants <-  setdiff(states$name, names(tab) )
    
    # Ajouter les états manquants à ma_table avec une valeur de fréquence de 0
      for (etat in etats_manquants) {
        tab[etat] <- 0
      }
    tab <- tab[-which(names(tab)=="VIRGIN ISLANDS")]
      
    # Ajouter les fréquences de tornade a la table 
    states$freq <- tab[match(states$name, names(tab))]
    
    # Créer la carte avec addPolygons() et l'échelle de couleur
    labels <- sprintf(
      "<strong>%s</strong><br/>%g tornade(s) en moyenne,<br/> entre %s et %s",
      states$name, states$freq, min(selectedData5()$YEAR), max(selectedData5()$YEAR)
    ) %>% lapply(htmltools::HTML)
    
    #Definition de l'echelle de couleur
    l=(max(states$freq)-min(states$freq))/10
    bins <- c(0, l, 2*l, 3*l, 4*l, 5*l, 6*l, 7*l, 8*l, 9*l, max(states$freq))
    pal2 <- colorBin("Reds", domain = states$freq, bins = bins)
    # "YlOrRd" / "Reds" / ...
   
############################### Fin du chronopleth   
###################################################    
    
   
    
     
########################################################
########################## Construction de la carte

  output$mymap <- renderLeaflet({
      
    map = leaflet() %>% setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
        addProviderTiles("Esri.OceanBasemap")  
    
    if (any(input$MapDetails == "Frequencies by state")) {
      map = map %>%
        addPolygons(data = states,
                    fillColor = ~pal2(freq),
                    weight = 1,
                    opacity = 2,
                    color = "black",
                    dashArray = "2",
                    fillOpacity = 7,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = pal2, values = states$freq, opacity = 0.7, title = NULL, position = "bottomright") 
    } 
    
    
    if (any(input$MapDetails == "Tornadoe's trajectories")) { 
        map = map %>%
              addGeoJSON(geojson,
                           weight = 4) 
      } 
    
    if (any(input$MapDetails == "Starting points")) { 
      map = map %>% 
        addCircleMarkers(data = selectedData5(), lat =  ~BEGIN_LAT, lng = ~BEGIN_LON, 
                         radius = 3, 
                         fillColor = pal(selectedData5()$TOR_F_SCALE),
                         popup = ~popup, popupOptions = 
                           list(maxHeight = 150, maxWidth = 200),
                         stroke = FALSE, fillOpacity = 0.8) %>%
        addLegend("bottomleft", pal=pal, values=selectedData5()$TOR_F_SCALE,
                  layerId="colorLegend")
    } 
                      
      
    map
      
      
    #   addProviderTiles("MapBox", options = providerTileOptions(
   #     id = "mapbox.light",
   #     accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      
      
      
     #   addPolylines(data = sldf, color = ~colorFactor(sldf$couleur, palette = colorScale))  %>%
        
     #   addPolylines(data = lines_sf, color = ~couleur, weight = 4) %>%
        
      
      
    
    
    
      
    })
  })
  
  
  
  
  
  
  
  
  
}





