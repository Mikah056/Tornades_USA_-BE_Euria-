library(shiny)
library(leaflet)
library(plotly)
library(geojsonio)

# Charger les données
data <- read.csv("data.csv")

# Créer une échelle de couleur en fonction de l'intensité de la tornade
tornado_colors <- c("#009900", "#00FF00", "#FFFF00", "#FF6600", "#FF0000", "#660000")
names(tornado_colors) <- unique(data$TOR_F_SCALE)

# Créer une échelle de couleur pour les comtés en fonction du nombre de tornades
county_colors <- colorRamp(c("#00FF00", "#FFFF00", "#FF6600", "#FF0000", "#660000"))

# Créer l'interface utilisateur
ui <- fluidPage(
  
  # Ajouter un titre
  titlePanel("Tornades aux États-Unis"),
  
  # Ajouter une barre de navigation latérale
  sidebarLayout(
    sidebarPanel(
      
      # Ajouter un filtre par plage d'années
      sliderInput("year_range", "Plage d'années :",
                  min = 1990, max = 2022, value = c(1990, 2022)),
      
      # Ajouter un filtre par intensité de la tornade
      checkboxGroupInput("tornado_intensity", "Intensité de la tornade :",
                         choices = names(tornado_colors),
                         selected = names(tornado_colors)),
      
      # Ajouter un filtre par mois
      checkboxGroupInput("month", "Mois :",
                         choices = month.name,
                         selected = month.name)
    ),
    
    # Ajouter la carte à l'interface utilisateur
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  
  # Filtrer les données en fonction des paramètres de l'utilisateur
  filtered_data <- reactive({
    data[data$YEAR >= input$year_range[1] & data$YEAR <= input$year_range[2] &
           data$TOR_F_SCALE %in% input$tornado_intensity &
           data$MONTH %in% input$month,]
  })
  
  # Charger le fichier de données géographiques des comtés américains
  counties <- geojsonio::geojson_read("counties.geojson", what = "sp")
  
  # Créer une colonne de comtés dans les données filtrées
  filtered_data <- sp::over(counties, filtered_data)
  
  # Compter le nombre de tornades par comté
  county_tornado_count <- as.data.frame(table(filtered_data$county))
  names(county_tornado_count) <- c("county", "tornado_count")
  
  # Créer un objet de type SpatialPolygonsDataFrame avec les comtés et le nombre de tornades
  counties_spdf <- sp::SpatialPolygonsDataFrame(counties@geometry, county_tornado_count)
  
  # Créer une couche de comtés colorée en fonction du nombre de tornades
  county_layer <- leaflet::addPolygons(
    data = counties_spdf,
    weight = 1,
    color = "#000000",
    fillColor = ~county_colors(max(0, min(tornado_count, max(county_tornado_count$tornado_count)))),
    fillOpacity = 0.7,
    smoothFactor = 0.5
  )
  
  # Créer une couche de points pour les débuts de tornades
  tornado_start_points <- leaflet::addCircleMarkers(
    data = filtered_data,
    lng = ~BEGIN_LON,
    lat = ~BEGIN_LAT,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.7,
    color = ~tornado_colors[TOR_F_SCALE],
    popup = plotly::ggplotly(
      ggplot2::ggplot(filtered_data, ggplot2::aes(x = BEGIN_LOCATION, y = YEAR)) +
        ggplot2::geom_point(ggplot2::aes(color = TOR_F_SCALE))
    )
  )
  
  # Créer une couche de lignes pour les trajectoires des tornades
  tornado_trajectories <- leaflet::addLines(
    data = filtered_data,
    lng = ~c(BEGIN_LON, END_LON),
    lat = ~c(BEGIN_LAT, END_LAT),
    weight = 1,
    color = ~tornado_colors[TOR_F_SCALE],
    smoothFactor = 0.5
  )
  
  # Afficher la carte
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = counties_spdf, fill = FALSE, color = "black", weight = 1) %>%
      addCircleMarkers(data = filtered_data,
                       lng = ~BEGIN_LON,
                       lat = ~BEGIN_LAT,
                       radius = 3,
                       stroke = FALSE,
                       fillOpacity = 0.7,
                       color = ~tornado_colors[TOR_F_SCALE],
                       popup = plotly::ggplotly(
                         ggplot2::ggplot(filtered_data, ggplot2::aes(x = BEGIN_LOCATION, y = YEAR)) +
                           ggplot2::geom_point(ggplot2::aes(color = TOR_F_SCALE))
                       )
      ) %>%
      addLines(data = filtered_data,
               lng = ~c(BEGIN_LON, END_LON),
               lat = ~c(BEGIN_LAT, END_LAT),
               weight = 1,
               color = ~tornado_colors[TOR_F_SCALE],
               smoothFactor = 0.5)
  })
  
}