library(shiny)
library(leaflet)
library(geojsonio)
library(plotly)
library(rgdal)

# Chargement de la base de données de tornades
tornadoes <- read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE_P.csv", stringsAsFactors = TRUE, header = TRUE)

# Création de la colonne "start" avec les coordonnées de départ
tornadoes$start <- cbind(tornadoes$BEGIN_LON, tornadoes$BEGIN_LAT)

# Création de la colonne "end" avec les coordonnées de fin
tornadoes$end <- cbind(tornadoes$END_LON, tornadoes$END_LAT)

# Fonction qui crée un fichier GeoJSON à partir de la base de données de tornades filtrée
# et qui ajoute des couleurs en fonction de l'intensité de la tornade
createGeoJSON <- function(tornadoes) {
  features <- lapply(1:nrow(tornadoes), function(i) {
    feature(geometry = list(type = "LineString", coordinates = list(tornadoes$start[i], tornadoes$end[i])),
            properties = list(magnitude = tornadoes$TOR_F_SCALE[i]))
  })
  geojson <- geojson_list(features)
  geojson_write(geojson, "temp.geojson")
}

# Fonction qui crée une couche GeoJSON avec des couleurs en fonction de l'intensité de la tornade
createGeoJSONLayer <- function(file) {
  # Dictionnaire de couleurs en fonction de l'intensité de la tornade
  colors <- c("EF0" = "#26C4EC", "EF1" = "#00FF00", "EF2" = "#FFFF00", "EF3" = "#CC5500", "EF4" = "#FF0000", "EF5" = "#A10684")
  
  # Chargement du fichier GeoJSON
  geojson <- geojson_read(file, what = "sp")
  
  # Création de la couche GeoJSON
  leaflet::addGeoJSON(geojson, color = ~colors[magnitude])
}

# Fonction qui crée une couche de comtés colorée en fonction du nombre de tornades par comté
createCountiesLayer <- function(counts) {
  # Chargement de la base de données de comtés
  comtés <- readOGR("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Test/Shiny/cb_2018_us_cd116_20m.shp")
  
  # Attribution des comptes aux comtés
  comtés$count <- counts
  
  # Création de la couche de comtés avec les comptes
  toGeoJSON(comtés, style = styleCounties)
}










# Fonction de style pour la couche de comtés
styleCounties <- function(feature) {
  fillColor <- colorBin(c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"), feature$count)
  list(color = "#777", weight = 1, fillColor = fillColor, fillOpacity = 0.8)
}










# Fonction qui crée une couche Plotly avec des points colorés en fonction de l'intensité de la tornade
createPlotlyLayer <- function(tornadoes) {
  # Dictionnaire de couleurs en fonction de l'intensité de la tornade
  colors <- c("EF0" = "#26C4EC", "EF1" = "#00FF00", "EF2" = "#FFFF00", "EF3" = "#CC5500", "EF4" = "#FF0000", "EF5" = "#A10684")
  
  # Création de la couche Plotly
  plot_ly(tornadoes, x = ~BEGIN_LON, y = ~BEGIN_LAT, type = "scatter", mode = "markers", color = ~colors[TOR_F_SCALE])
}

# Fonction qui crée un popup avec des informations sur chaque point
createPopup <- function(tornadoes) {
  # Création du popup
  paste0("<b>Lieu :</b> ", tornadoes$BEGIN_LOCATION, "<br>",
         "<b>Date :</b> ", paste(tornadoes$STATE, tornadoes$MONTH_NAME, tornadoes$YEAR, sep = "/"))
}

# Définition du serveur Shiny
server <- function(input, output) {
  # Chargement de la base de données de comtés
  comtés <- readOGR("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Test/Shiny/cb_2018_us_cd116_20m.shp")
  
  # Filtrage de la base de données de tornades en fonction des filtres sélectionnés
  tornadoes_filtered <- tornadoes[tornadoes$year >= input$year_range[1] &
                                    tornadoes$year <= input$year_range[2] &
                                    tornadoes$magn %in% input$magnitudes &
                                    tornadoes$month %in% input$months, ]
  
  # Transformation des coordonnées des points géographiques en objets de type SpatialPoints
  points <- SpatialPoints(cbind(tornadoes_filtered$BEGIN_LON, tornadoes_filteredBEGIN_LAT))
  
  # Détermination du comté le plus proche de chaque point
  comtés.proches <- over(points, comtés)
  
  # Groupement de la base de données de tornades par comté
  counts <- table(comtés.proches)
  
  # Attribution des comptes aux comtés
  comtés$count <- counts
  
  # Création de la couche de comtés avec les comptes
  comtés_geojson <- toGeoJSON(comtés, style = styleCounties)
  
  # Création d'un fichier GeoJSON avec les tornades filtrées
  createGeoJSON(tornadoes_filtered)
  
  # Mise à jour de la carte Leaflet avec les tornades filtrées
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addGeoJSON(data = createGeoJSONLayer("temp.geojson")) %>%
      addGeoJSON(data = comtés_geojson) %>%
      addPlotly(data = createPlotlyLayer(tornadoes_filtered), popup = createPopup())
  })
  
}
  