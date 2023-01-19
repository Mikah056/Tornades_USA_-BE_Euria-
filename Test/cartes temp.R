library(geojson)

setwd(choose.dir())
counties = geojson_read(file.choose(), what = "sp")
counties$name <- toupper(counties$name)


# Charger les bibliothèques nécessaires
library(leaflet)
library(RColorBrewer)

# Charger votre fichier GeoJSON
counties = geojson_read(file.choose(), what = "sp")

#Mettre les noms des etats en majuscule
counties$name <- toupper(counties$name)

#Calculer la table de frequence par etat
tab = 100*table(Datap$STATE)/sum(table(Datap$STATE))
tab = table(Datap$STATE)

# Récupérer les noms des états qui ne se trouvent pas dans ma_table
etats_manquants <- setdiff(counties$name, names(tab))

# Ajouter les états manquants à ma_table avec une valeur de fréquence de 0
for (etat in etats_manquants) {
  tab[etat] <- 0
}
tab=tab[-which(names(tab)=="VIRGIN ISLANDS")]

# Ajouter les fréquences de tornade à la table 
counties$freq <- tab[match(counties$name, names(tab))]

# Créer une échelle de couleur en utilisant colorNumeric()
palette <- colorNumeric(palette = "YlOrRd", counties$freq)

# Créer la carte avec addPolygons() et l'échelle de couleur
labels <- sprintf(
  "<strong>%s</strong><br/>%g %% des tornades",
  counties$name, counties$freq
) %>% lapply(htmltools::HTML)

bins <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 10)
pal <- colorBin("Reds", domain = counties$freq, bins = bins)

m <- leaflet(counties) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
addPolygons(
  fillColor = ~pal(freq),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>%
  addLegend(pal = pal, values = ~freq, opacity = 0.7, title = NULL,
            position = "bottomright")

m





















