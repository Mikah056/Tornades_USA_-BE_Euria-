############################################
########## LISSAGE SPATIAL



Datap <- read.csv("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE.csv", stringsAsFactors = TRUE )
#Datap <- data.frame(Datap[which(Datap$YEAR>1996),])  #On prend des 1996 pour eviter les donnees manquantes dans un premier temps
Datap$BEGIN_LAT <-  as.numeric(Datap$BEGIN_LAT)
Datap$BEGIN_LON <-  as.numeric(Datap$BEGIN_LON)

## Si R ne télécharge pas directement le package mapsf qui est une dépendance de 
## btb sur les nouvelles versions, les telecharger préalablement sur le CRAN
#install.packages("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/mapsf_0.6.1.tar.gz", repos=NULL, type="source")
#install.packages("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/spatstat.utils_3.0-2.tar.gz", repos=NULL, type="source")
#install.packages("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/spatstat.utils_3.0-2.tar.gz", repos=NULL, type="source")
library(spatstat.utils)
#install.packages("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/spatstat.geom_3.1-0.tar.gz", repos=NULL, type="source")
library(spatstat.geom)
#nstall.packages("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/spatstat.explore_3.1-0.tar.gz", repos=NULL, type="source")
library(spatstat.explore)



#####################################################################
library(sp)

# Charger les coordonnées depuis le data frame
coords <- data.frame(longitude = Datap$BEGIN_LON, latitude = Datap$BEGIN_LAT)

# Créer un objet "SpatialPoints" à partir des coordonnées
sp_points <- SpatialPoints(coords = coords, proj4string = sp::CRS("+proj=longlat +ellps=WGS84")) #Non projet


# Définir le système de projection Albers pour les États-Unis
us_albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

# Projeter les coordonnées sur le système de projection Albers
albers_points <- spTransform(sp_points, us_albers)

# Afficher les coordonnées projetées
albers_points@coords

## Verifier le système de projection
#proj4string(albers_points)

################



#lissage avec btb : calcul de la part de ménages pauvres
#on lisse séparément le numérateur (nombre de ménages pauvres), et le dénominateur (nombre total de ménages)
library(btb)
#chargement des données
data(reunion)
#lissage
#définition des paramètres
pas <- 50000 #carreau de 200m de cote
rayon <- 100000 #bande passante de 400m
#appel de la fonction de lissage
#la fonction lisse automatiquement l’ensemble des variables contenues dansla base
#ici on lisse phouhold et houhold
bb = Datap[,c(24,25,28)]
bb[,3]=as.numeric(bb[,3])
colnames(bb)[c(1,2)]=c("x","y")


tornades_utm = albers_points@coords
tornades_utm = as.data.frame(cbind(tornades_utm,sample(1:10,nrow(tornades_utm),replace=TRUE)))
names(tornades_utm)[1:2] = c("x","y")

dfLisse <- btb::btb_smooth(pts = tornades_utm, iCellSize = pas,
                           iBandwidth = rayon, sEPSG="4326")
#taux de ménages pauvres : ratio des variables lissées
#dfLisse$txmenpa = dfLisse$houhold
#aperçu dans R
library(sp)
library(cartography)
#affichage de la carte
cartography::choroLayer(dfLisse, var = "V3", nclass = 5, method = "fisher-jenks", border = NA, legend.pos = "topright", legend.title.txt ="txmenpa (%)")
#ajout du titre et d’un contour
cartography::layoutLayer(title = "La Réunion : taux de ménages pauvres",
                         sources = "",
                         author = "",
                         scale = NULL,
                         frame = TRUE,
                         col = "black",
                         coltitle = "white")





############################################################################
#########################    LISSAGE SPATIAL    ############################
############################################################################

# Charger les données
datap <- read.csv("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE.csv", stringsAsFactors = TRUE )

#########Recodage de l'échelle de Fujita
Intensity = c(rep(0,nrow(datap)))
Intensity[which(datap$TOR_F_SCALE=="EF1")] = 1
Intensity[which(datap$TOR_F_SCALE=="EF2")] = 2
Intensity[which(datap$TOR_F_SCALE=="EF3")] = 3
Intensity[which(datap$TOR_F_SCALE=="EF4")] = 4
Intensity[which(datap$TOR_F_SCALE=="EF5")] = 5
datap$Intensity = Intensity
datap = datap[,c(25,24,28)]

#Chargement des packages
library(sp)
library(btb)
library(sp)
library(cartography)
library(spatstat)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(tidyr)


#### Nous allons fabriquer des données carreauyés de 60 km x 60 km ####

# Charger les données des limites des États des USA
usa_states <- st_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json")

# Filtrer pour ne conserver que les États-Unis contigus (supprimer l'Alaska, Hawaï et les îles)
usa_states_contiguous <- usa_states %>% 
  filter(!NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))

# Créer une grille régulière de 15 km x 15 km (15/111 = 0.14)
usa_grid <- st_make_grid(usa_states_contiguous, cellsize = c(0.14, 0.14)) %>% 
  st_sf() %>% 
  st_intersection(usa_states_contiguous)

#Dupliquer les coordonnées, car l'opération ci-dessous va les supprimer
datap = cbind(datap, LON = datap$BEGIN_LON, LAT=datap$BEGIN_LAT)

# Transformer les données en objet sf
datap_sf <- st_as_sf(datap, coords = c("BEGIN_LON", "BEGIN_LAT"), crs = 4326)

usa_grid$id = 1:nrow(usa_grid)
# Effectuer une jointure spatiale entre les carreaux et les données des tornades
tornado_joined <- st_join(usa_grid, datap_sf, join = st_intersects)

# Compter le nombre de tornades dans chaque carreau et calculer l'intensité moyenne
tornado_count <- tornado_joined %>% 
  group_by(id) %>% 
  summarise(
    tornado_count = sum(!is.na(Intensity)),
    avg_intensity = ifelse(sum(!is.na(Intensity)) == 0, 0, mean(Intensity, na.rm = TRUE))
  )

# Calculer les centroïdes de chaque cellule
usa_grid_centroids <- st_centroid(usa_grid)

# Extraire les coordonnées des centroïdes
coords <- st_coordinates(usa_grid_centroids)

# Ajouter les coordonnées des centroïdes aux résultats
tornado_count$center_lon1 <- coords[, "X"]
tornado_count$center_lat1 <- coords[, "Y"]

# Charger les coordonnées depuis le data frame
coords <- data.frame(longitude = tornado_count$center_lon1, latitude = tornado_count$center_lat1)

# Créer un objet "SpatialPoints" à partir des coordonnées
sp_points <- SpatialPoints(coords = coords, proj4string = sp::CRS("+proj=longlat +ellps=WGS84"))

# Définir le système de projection Albers pour les États-Unis
us_albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

# Projeter les coordonnées sur le système de projection Albers
albers_points <- spTransform(sp_points, us_albers)
#x = as.data.frame(albers_points)

# Ajouter les coordonnées des centroïdes aux résultats
tornado_count$center_lon <- albers_points$longitude
tornado_count$center_lat <- albers_points$latitude

# Afficher les coordonnées projetées
#albers_points@coords

## Verifier le système de projection
#proj4string(albers_points)


####### Determination de la bande passante
# transformation des x,y en objet .ppp
base.ppp = ppp(tornado_count$center_lon, tornado_count$center_lat,
                         c(min(tornado_count$center_lon), max(tornado_count$center_lon)),
                         c(min(tornado_count$center_lat), max(tornado_count$center_lat)) )


# test bw.diggle de bande passante optimale
bw_diggle <- bw.diggle(base.ppp)
plot(bw_diggle, main = "")
bw_diggle
#sigma = 10401.87










#Définition des paramètres
pas <- 15000 #carreau de 200m de cote
rayon <- 20802 #bande passante de 400m

#Transformation des données
tornades_utm = as.data.frame(tornado_count)
names(tornades_utm)[7:8] = c("x","y")
tornades_utm = tornades_utm[,c(7,8,2,3)]

#Réalisation du lissage
#la fonction lisse automatiquement l’ensemble des variables contenues dans la base
dfLisse <- btb::btb_smooth(pts = tornades_utm, iCellSize = pas,
                           iBandwidth = rayon, sEPSG="4326")

#Rajout de la variable cible
dfLisse$cible = dfLisse$tornado_count*dfLisse$avg_intensity

#Affichage de la carte
cartography::choroLayer(dfLisse, var = "cible", nclass = 6, method = "fisher-jenks", border = NA, legend.pos = "bottomleft", legend.title.txt ="Cible")





