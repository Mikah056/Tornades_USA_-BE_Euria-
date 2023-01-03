library(shiny)
library(leaflet)
library(geojsonio)

# Chargement de la base de données de tornades
tornadoes <- read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE_P.csv", stringsAsFactors = TRUE, header = TRUE)

# Création de la colonne "start" avec les coordonnées de départ
tornadoes$start <- cbind(tornadoes$B_L, tornadoes$B_lat)

# Création de la colonne "end" avec les coordonnées de fin
tornadoes$end <- cbind(tornadoes$E_L, tornadoes$E_lat)

# Création de l'interface utilisateur Shiny
ui <- fluidPage(
  # Sortie de carte Leaflet
  leafletOutput("map"),
  
  # Boîte de contrôle pour spécifier la plage d'années
  box(title = "Années",
      sliderInput("year_range", "Plage d'années :",
                  min = min(tornadoes$year), max = max(tornadoes$year),
                  value = c(min(tornadoes$year), max(tornadoes$year)), step = 1, sep = "")),
  
  # Boîte de contrôle pour cocher les intensités à représenter
  box(title = "Intensités",
      checkboxGroupInput("magnitudes", "Intensités :",
                         unique(tornadoes$magn), selected = unique(tornadoes$magn))),
  
  # Boîte de contrôle pour sélectionner les mois à représenter
  box(title = "Mois",
      selectInput("months", "Mois :",
                  choices = 1:12, multiple = TRUE))
)

# Création du serveur Shiny
server <- function(input, output) {
  # Fonction qui crée un fich
  # Fonction qui crée un fichier GeoJSON à partir de la base de données de tornades filtrée
  # et qui ajoute des couleurs en fonction de l'intensité de la tornade
  createGeoJSON <- function(tornadoes) {
    features <- lapply(1:nrow(tornadoes), function(i) {
      feature(geometry = list(type = "LineString", coordinates = list(tornadoes$start[i], tornadoes$end[i])),
              properties = list(magnitude = tornadoes$magn[i]))
    })
    geojson <- geojson_list(features)
    geojson_write(geojson, "temp.geojson")
  }
  
  # Fonction qui crée une couche GeoJSON avec des couleurs en fonction de l'intensité de la tornade
  createGeoJSONLayer <- function(file) {
    # Dictionnaire de couleurs en fonction de l'intensité de la tornade
    colors <- c("EF1" = "#ffffb2", "EF2" = "#fecc5c", "EF3" = "#fd8d3c")
    
    # Chargement du fichier GeoJSON
    geojson <- geojson_read(file, what = "sp")
    
    # Création de la couche GeoJSON avec des couleurs en fonction de l'intensité de la tornade
    layer <- geojson_layer(geojson, stroke = TRUE, color = ~colors[magnitude], weight = 1)
    
    return(layer)
  }
  
  # Fonction qui crée une couche de comtés avec des couleurs en fonction du nombre de tornades
  createCountiesLayer <- function(tornadoes) {
    # Chargement de la base de données de comtés
    counties <- geojson_read("https://raw.githubusercontent.com/fraxen/tornadoes/master/data/counties.geojson", what = "sp")
    
    # Création de la colonne "count" avec le nombre de tornades par comté
    counties$count <- sapply(counties@data$NAME, function(x) sum(tornadoes$county == x))
    
    # Création de la couche de comtés avec des couleurs en fonction du nombre de tornades
    layer <- leaflet::geojson_color_scale(counties, "count",
                                          palette = "YlOrRd",
                                          labels = sprintf("%.f", 0:10))
    
    return(layer)
  }
  
  # Mise à jour de la carte Leaflet lorsque les filtres sont modifiés
  observeEvent(c(input$year_range, input$magnitudes, input$months), {
    # Filtrage de la base de données de tornades en fonction des
    # Filtrage de la base de données de tornades en fonction des filtres sélectionnés
    tornadoes_filtered <- tornadoes[tornadoes$year >= input$year_range[1] &
                                      tornadoes$year <= input$year_range[2] &
                                      tornadoes$magn %in% input$magnitudes &
                                      tornadoes$month %in% input$months, ]
    
    # Création d'un fichier GeoJSON avec les tornades filtrées
    createGeoJSON(tornadoes_filtered)
    
    # Mise à jour de la carte Leaflet avec les tornades filtrées
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addGeoJSON(data = createGeoJSONLayer("temp.geojson")) %>%
        addGeoJSON(data = createCountiesLayer(tornadoes_filtered))
    })
    