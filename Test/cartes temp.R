library(geojsonio)

# Charger les bibliothèques nécessaires
library(leaflet)
library(RColorBrewer)

########################################
## Avec tigris
# Charger les données de limites administratives des états américains
library(tigris)
states <- states(cb = TRUE)

#######################################
## Avec json
# Charger votre fichier GeoJSON
states = geojson_read("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/State_geometry.geojson", what = "sp")
#######################################


#Mettre les noms des etats en majuscule
states$NAME <- toupper(states$NAME)

#Calculer la table de frequence par etat
tab = table(Data$STATE)/(max(Data$YEAR)-min(Data$YEAR)+1)

# Récupérer les noms des états qui ne se trouvent pas dans ma_table
etats_manquants <- setdiff(states$NAME, names(tab))

# Ajouter les états manquants à ma_table avec une valeur de fréquence de 0
for (etat in etats_manquants) {
  tab[etat] <- 0
}
tab=tab[-which(names(tab)=="VIRGIN ISLANDS")]

# Ajouter les fréquences de tornade à la table 
states$freq <- tab[match(states$NAME, names(tab))]

# Créer une échelle de couleur en utilisant colorNumeric()
palette <- colorNumeric(palette = "Reds", states$freq)

# Créer la carte avec addPolygons() et l'échelle de couleur
labels <- sprintf(
  "<strong>%s</strong><br/>%g tornades en moyenne,<br/> entre %s et %s",
  states$NAME, states$freq, min(Data$YEAR), max(Data$YEAR)
) %>% lapply(htmltools::HTML)

bins <- c(0, 10, 25, 50, Inf)
pal <- colorBin(c("#26C4EC","#C2F732","#ED7F10","#FF0000"), domain = states$freq, bins = bins)

m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
addPolygons(label = ~STUSPS,labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
  fillColor = ~pal(freq),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7) %>%
  addLegend(pal = pal, values = ~freq, opacity = 0.7, title = NULL,
            position = "bottomright") 

m



#######################################################################
#######################################################################



######################################## Graphique pour les comtes

### Avec tigris
# Charger les données de limites administratives des comtes americains
library(tigris)
counties=counties(cb = TRUE)

## On utilisera les codes FIPS parceque certains comtes de deux etats differents ont le meme nom

#Determinons le nombre de tornade par comte ayant deja ete touche
#etat, numero et nom de comte ayant deja ete touche au moins une fois
x=unique(Data[,c(2,5,6)])
nrow(unique(Data[,c(2,5,6)]))   #3011 comtes
freq = c()
for (i in 1:nrow(unique(Data[,c(2,5,6)]))) {
  u=0
  for(j in 1:nrow(Data)) {
    if (all(Data[j,c(2,5,6)]==x[i,])) {
      u=u+1
    }
  }
  freq=c(freq,u)                        #Nombre de tornade par circonscription
}

base_freq=cbind(unique(Data[,c(2,5,6)]),freq)

#Exportation de la base des effectif par comte sinistre
write.csv(base_freq,file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_freq_comte.csv',row.names = FALSE)
base_freq = read.csv(file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_freq_comte.csv')

#On rajoute ces frequences a la base des comtes
#frequence moyenne
base_freq=cbind(unique(Data[,c(2,5,6)]),freq/32)
max(base_freq[,4])    #32 annees d'observation
min(base_freq[,4])

#Mettre les noms des etats et des comtes en majuscule
counties$STATE_NAME <- toupper(counties$STATE_NAME)
counties$NAME <- toupper(counties$NAME)

#Mettre les codes fips sous format numeric
counties$COUNTYFP <- as.numeric(counties$COUNTYFP)

counties$freq = 0
for (i in 1:nrow(base_freq)) {
  v1 = which(base_freq[i,1]==counties$STATE_NAME)
  v2 = which(base_freq[i,2]==counties$COUNTYFP)
  v3 = which(base_freq[i,3]==counties$NAME)       
  if (length(v1)>length(v2)) {v4 = setdiff(v1,v2);v4=setdiff(v1,v4)
                                } else {v4 = setdiff(v2,v1);v4=setdiff(v2,v4)}
  if (length(v4)==1) {counties$freq[v4] = base_freq[i,4]
  } else {
    if (length(v3)>length(v4)) {v5 = setdiff(v3,v4);v5=setdiff(v3,v5)
    } else {v5 = setdiff(v4,v3);v5=setdiff(v4,v5)}  
    counties$freq[v5] = base_freq[i,4]
  }
}

max(counties$freq)
min(counties$freq)
#Mettre les codes FIPS de comtes en numeric
  counties$COUNTYFP <- as.numeric(counties$COUNTYFP)
  counties$STATE_NAME <- toupper(counties$STATE_NAME)
  

# Créer une échelle de couleur en utilisant colorNumeric()
palette <- colorNumeric(palette = "YlOrBr", counties$freq)

# Créer la carte avec addPolygons() et l'échelle de couleur
labels <- sprintf(
  "<strong>%s</strong><br/>%g tornades en moyenne,<br/> entre %s et %s",
  counties$NAME, counties$freq, min(Data$YEAR), max(Data$YEAR)
) %>% lapply(htmltools::HTML)

bins <- c(0, 1, 2, 3, 4, 5)
pal <- colorBin(c("#26C4EC","#00FF00","#ED7F10","#FF0000"), domain = counties$freq, bins = bins)

m <- leaflet(counties) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(label = labels, labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"),
              fillColor = ~pal(freq),
              weight = 1,
              opacity = 1,
              color = "black",
              dashArray = "5",
              fillOpacity = 0.7) %>%
  addLegend(pal = pal, values = ~freq, opacity = 0.7, title = NULL,
            position = "bottomright") %>%
  addPolygons(data=states,label = ~STUSPS)

m












