#### Clustering base sur la densite ####

# Installation des packages
install.packages("dbscan")
install.packages("fastcluster")
install.packages("sp")

# Chargement des packages
library(fastcluster)
library(dbscan)
library(sp)
#?dbscan
#?spDists

# Chargement des données sur les tornades
Base <- read.csv2("C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE.csv", sep = ",", stringsAsFactors = TRUE,  dec = ".")
#On considère les données à partir de 1990, et on garde juste les longitudes et depart,
#car elles n'ont pas de valeurs manquantes
apply(apply(Base[which(Base$YEAR>=1990),c(24,25,26,27)],2,is.na),2,sum)
Base = Base[which(Base$YEAR>=2020),c(2,15,25,24)]
#View(Base)


### Recuperation des coordonnees. L'ordre long / lat est important
coords <- Base[,c(3,4)]


## Creation d'un objet spatial
dtest <- sp::SpatialPoints(coords = coords, proj4string = sp::CRS("+proj=longlat +ellps=WGS84")) #Non projet
plot(dtest@coords)


## Calcul de la matrice des distances geographiques (calculee avec une fonction du package sp)
# Attention!!! Relativement long en temps de calcul, pour notre base d'environ 40000
distmatrix <- sp::spDists(x = dtest, longlat = T) #Renvoie la matrice des distances en kilometres
 dd=distmatrix

#Exportation
filename = "C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/distmatrix.csv"
#write.csv2(distmatrix,filename, sep = ";", dec = ".")
D0 = read.csv(filename,header=TRUE)

#conversion de la matrice en objet dist. Sinon pas utilisable par hclust
distmatrix <- stats::as.dist(distmatrix) 


###############################################################################
###############################################################################

                        ################
                           # DBSCAN #
                        ################


### INFLUENCE DES PARAMETRES DE DBSCAN ###

#####   Influence de MinPts
Eps = 200 #eps fixe Ã 20 km, puis 200 km
#Liste des valeurs de MinPts Ã tester
MinPts = list(10,50,120,250,500,1000,2000,5000)
#Generation de matrices et dataframe similaires aux precedents
#Creation d'une matrice stockant les valeurs de nbre de clusters et nbre de points isoles

ClustersPtsIso <- matrix(nrow = length(MinPts), ncol = 2, dimnames = 
list(c("MinPts = 10", "MinPts = 50","MinPts = 120",
       "MinPts = 250", "MinPts = 500", "MinPts = 1000", "MinPts = 2000","MinPts = 5000"), 
     c("NbClusters","NbIsoPts")))

#Creation d'un dataframe stockant les N° de clusters pour chaque point selon differentes methodes
results = data.frame(Base) #Stockage de state / scale / lon / lat
ClusteringData <- as.data.frame(matrix(nrow = length(Base$STATE), ncol = length(MinPts), 
   dimnames = list(NULL, c("MinPts = 10", "MinPts = 50","MinPts = 120",
                           "MinPts = 250", "MinPts = 500", "MinPts = 1000", "MinPts = 2000","MinPts = 5000"))))


for (i in (1:length(MinPts))) {
  db <- dbscan::dbscan(distmatrix, minPts = MinPts[[i]], eps = Eps) 
  #clustering
  #Stockage des resultats : N° de cluster pour chaque point
  ClusteringData[,i] <- db$cluster
  #Nb de clusters
  NbClusters = max(db$cluster)
  #Nb de points isoles
  ClusterSize <- as.data.frame(table(db$cluster)) #compte le nombre de points par cluster
  if (ClusterSize$Var1[1] == 0) {       #Si le premier indice trouve est 0 alors il y a des points isoles
    NbIsolatedPts <- ClusterSize$Freq[1] 
  } else {             #Sinon pas de points bruits
    NbIsolatedPts <- 0
  }
  #Stockage dans la matrice du nbre de clusters et de points isoles
  ClustersPtsIso[i,1] = NbClusters #1ere colonne = Nb de clusters
  ClustersPtsIso[i,2] = NbIsolatedPts #2e colonne = Nb de points isoles
}

#Recuperation des dernieres colonnnes
results <- cbind(results, ClusteringData) #Stockage des resultats des clustering

#Exportation des resultats sous format csv
#Fichier stockant les coordonnees et N° de clusters
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANMinPts_results2.csv"
write.csv2(results,filename, sep = ";", dec = ".")
#Fichier stockant le nbre de clusters et de points isoles obtenus pour chaque methode
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANMinPts_ClustersIsoPts2.csv"
write.csv2(ClustersPtsIso,filename, sep = ";", dec = ".")



#######  Influence de Eps
MinPts = 400 #MinPts fixe a 4, puis 400
#Lite des valeurs de Eps a tester
Eps = list(50,100,250,500,800,1200,1500)

#Generation de matrices et dataframe similaires aux precedents
#Creation d'une matrice stockant les valeurs de nbre de clusters et nbre de points isoles
ClustersPtsIso <- matrix(nrow = length(Eps), ncol = 2, dimnames = list(c("Eps = 50", "Eps = 100", 
          "Eps = 250", "Eps = 500","Eps = 800","Eps = 1200","Eps = 1400"), c("NbClusters","NbIsoPts")))

#Creation d'un dataframe stockant les N° de clusters pour chaque  point selon differentes methodes
results = data.frame(Base) #Stockage de state / scale / lon / lat

ClusteringData <- as.data.frame(matrix(nrow = length(Base$STATE), ncol = length(Eps), 
                   dimnames = list(NULL, c("Eps = 50", "Eps = 100", 
                                           "Eps = 250", "Eps = 500","Eps = 800","Eps = 1200","Eps = 1400"))))


for (i in (1:length(Eps))) {
  db <- dbscan::dbscan(distmatrix, minPts = MinPts, eps = Eps[[i]]) 
  #clustering
  #Stockage des resultats : N° de cluster pour chaque point
  ClusteringData[,i] <- db$cluster
  #Nb de clusters
  NbClusters = max(db$cluster)
  #Nb de points isoles
  ClusterSize <- as.data.frame(table(db$cluster)) #compte le nombre de points par cluster
  if (ClusterSize$Var1[1] == 0) {     #Si le premier indice trouve est 0 alors il y a des points isoles
    NbIsolatedPts <- ClusterSize$Freq[1] 
  } else {       #Sinon pas de points bruits
    NbIsolatedPts <- 0
  }
  #Stockage dans la matrice du nbre de clusters et de points isoles
  ClustersPtsIso[i,1] = NbClusters #1ere colonne = Nb de clusters
  ClustersPtsIso[i,2] = NbIsolatedPts #2e colonne = Nb de points isoles
}

#Recuperation des dernieres colonnnes
results <- cbind(results, ClusteringData) #Stockage des resultats des clustering


#Exportation sous csv des resultats
#Fichier stockant les coordonnees et N° de clusters
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANEps_results2.csv"
write.csv2(results,filename, sep = ";", dec = ".")
#Fichier stockant le nbre de clusters et de points isoles otenus pour chaque methode
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANEps_ClustersIsoPts2.csv"
write.csv2(ClustersPtsIso,filename, sep = ";", dec = ".")









###############################################################################
###############################################################################

         #########################################################
           # Determination des parametres optimaux pour DBSCAN #
         #########################################################

### Analyse de l'influence de Minpts
#Importation des resultats
#Minpts=4
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANEps_results2.csv"
DBSCANEps_results = read.csv(filename, sep = ";",dec=",")
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANEps_ClustersIsoPts2.csv"
DBSCANEps_ClustersIsoPts = read.csv(filename, sep = ";")
DBSCANEps_ClustersIsoPts
#On peut voir que l'augmentation de EPS a pour effet de reduire le nombre de clusters
#De meme, ca permet de reduire le nombre de points isoles
#On a donc le double effet positif recherche

x = cbind(DBSCANEps_ClustersIsoPts,Eps=c(5,15,40,75,100,200,400))
p1 = ggplot(data=x, aes(Eps, NbClusters)) +
        geom_line(color = "blue", size = 1) +
  ylab("Nombre de clusters") + xlab("") +
  labs(title = "         MinPts = 4")
p2 = ggplot(data=x, aes(Eps, NbIsoPts)) +
  geom_line(color = "blue", size = 1) +
  ylab("Nombre de points isolés")



filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANMinPts_results2.csv"
DBSCANMinPts_results = read.csv(filename, sep = ";",dec=",")
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANMinPts_ClustersIsoPts2.csv"
DBSCANMinPts_ClustersIsoPts = read.csv(filename, sep = ";")
DBSCANMinPts_ClustersIsoPts
#On constate que l'augmentation de Minpts diminue considerablement le nombre de clusters
#et que des valeurs de plus en plus eleve de Minpts conduisent a classer de plus en plus de points comme des points isoles







############ Cas particulier

#Il semble que les Etats loin de la partie continentale genent la convergence
#HAWAII et PUERTO RICO apparaissent 14 et 11 fois,
#on va fixer les parametres en fonction de ca





  

  
  


##############################################################################
##############################################################################

                            ################
                               # HDBSCAN #
                            ################


### INFLUENCE DES PARAMETRES DE HDBSCAN : MinPts
#Liste des valeurs de MinPts & tester
MinPts = list(2,5,15,40,90,150,350,700,1000,2000)
#Generation de matrices et dataframe similaires aux precedents
#Creation d'une matrice stockant les valeurs de nbre de clusters et nbre de points isoles
ClustersPtsIso <- matrix(nrow = length(MinPts), ncol = 2, 
                         dimnames = list(c("MinPts = 2", "MinPts = 5", "MinPts = 15", "MinPts = 40","MinPts = 90",
                                           "MinPts = 150", "MinPts = 350", "MinPts = 700", "MinPts = 1000","MinPts = 2000"),
                                         c("NbClusters","NbIsoPts")))

#Creation d'un dataframe stockant les N° de clusters pour chaque point selon differentes methodes
results = data.frame(bigwos_filt[,1:5]) #Stockage de state / scale / lon / lat
ClusteringData <- as.data.frame(matrix(nrow = length(Base$STATE), ncol = length(MinPts), 
                                       dimnames = list(NULL, c("MinPts = 1", "MinPts = 5", "MinPts = 15", "MinPts = 40","MinPts = 90",
                                                               "MinPts = 150", "MinPts = 350", "MinPts = 700", "MinPts = 1000","MinPts = 2000"))))


for (i in 1:length(MinPts)) {
  hdb <- dbscan::hdbscan(distmatrix, minPts = MinPts[[i]]) 
  #clustering
  #Stockage des resultats : N° de cluster pour chaque point
  ClusteringData[,i] <- hdb$cluster
  #Nb de clusters
  NbClusters = max(hdb$cluster)
  #Nb de points isoles
  ClusterSize <- as.data.frame(table(hdb$cluster)) #compte le nombre de points par cluster
  if (ClusterSize$Var1[1] == 0) {   #Si le premier indice trouve est 0 alors il y a des points isoles
    NbIsolatedPts <- ClusterSize$Freq[1] 
  } else {   #Sinon pas de points bruits
    NbIsolatedPts <- 0
  }
  #Stockage dans la matrice du nbre de clusters et de points isoles
  ClustersPtsIso[i,1] = NbClusters       #1ere colonne = Nb de clusters
  ClustersPtsIso[i,2] = NbIsolatedPts    #2e colonne = Nb de points isoles
}

#Recuperation des dernieres colonnnes
results <- cbind(results, ClusteringData) #Stockage des resultats des clustering

#Exportation sous csv des resultats
#Fichier stockant les coordonnees et N° de clusters
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/HDBSCANMinPts_results.csv"
write.csv2(results,filename, sep = ";", dec = ".")
#Fichier stockant le nbre de clusters et de points isoles otenus pour chaque methode
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/HDBSCANMinPts_ClustersIsoPts.csv"
write.csv2(ClustersPtsIso,filename, sep = ";", dec = ".")


#######################################################

#Importation des resultats
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/HDBSCANMinPts_results.csv"
HDBSCANMinPts_results = read.csv(filename, sep = ";",dec=",")

filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/HDBSCANMinPts_ClustersIsoPts.csv"
HDBSCANMinPts_ClustersIsoPts = read.csv(filename, sep = ";",dec=",")
################################################

###############
######Parametres optimaux


########################################
############################################OPTIMISATION

#Liste des valeurs de MinPts & tester
MinPts = 20:85

#Creation d'une matrice stockant les valeurs de nbre de clusters et nbre de points isoles
ClustersPtsIso <- matrix(nrow = length(MinPts), ncol = 5, 
                         dimnames = list(as.character(20:85),
                                         c("NbClusters","NbIsoPts","Mean_cluster_scores",
                                           "Mean_membership_prob","Mean_outlier_scores")))


#On va stocher juste les mesures qui nous interessent
for (i in 46:length(MinPts)) {
  #clustering
  hdb <- dbscan::hdbscan(distmatrix, minPts = MinPts[i]) 
  #Nb de clusters
  NbClusters = max(hdb$cluster)
  #Nb de points isoles
  ClusterSize <- as.data.frame(table(hdb$cluster)) #compte le nombre de points par cluster
  if (ClusterSize$Var1[1] == 0) {   #Si le premier indice trouve est 0 alors il y a des points isoles
    NbIsolatedPts <- ClusterSize$Freq[1] 
  } else {   #Sinon pas de points bruits
    NbIsolatedPts <- 0
  }
  #Stockage dans la matrice des indicateurs
  ClustersPtsIso[i,1] = NbClusters       #1ere colonne = Nb de clusters
  ClustersPtsIso[i,2] = NbIsolatedPts    #2e colonne = Nb de points isoles
  ClustersPtsIso[i,3] = mean(hdb$cluster_scores)    #indice de stabilite
  ClustersPtsIso[i,4] = mean(hdb$membership_prob)   #probabilite d'appartenance
  ClustersPtsIso[i,5] = mean(hdb$outlier_scores)
  }

#Fichier stockant le nbre de clusters et de points isoles otenus pour chaque methode
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/HDBSCANMinPts_ClustersIsoPtsFinal.csv"
write.csv2(ClustersPtsIso,filename, sep = ";", dec = ".")

# Importation
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/HDBSCANMinPts_ClustersIsoPtsFinal.csv"
x = read.csv(filename, sep = ";", dec = ",")
x
summary(x)

par(mfrow=c(3,1))

######################################
##### Parametre optimal (qui maximise la probabilité d'appartenance)
## 84, choisi par observation graphique
######################################


#################################################
#################################################

##################### Implementation

hdb <- dbscan::hdbscan(distmatrix, minPts = 84,
                       gen_hdbscan_tree = TRUE,
                       gen_simplified_tree = TRUE,
                       verbose = TRUE)

#Recuperation des clusters
results = data.frame(Base)
x <- cbind(results, hdb$cluster)
#Stockage
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/HDBSCAN_Clustering_Final.csv"
#write.csv2(x,filename, sep = ";", dec = ".")

View(x)
nrow(x)
colnames(x)[5]='dd'
unique(x$dd)
colorBy <- unique(x$dd) 
# c("#26C4EC","#00FF00","#C2F732","#ED7F10","#FF0000","#F9429E")
# "YlOrRd"
pal <- colorFactor(c("#FF0000","#26C4EC","#00FF00","#C2F732","#ED7F10","#F9429E"), colorBy, ordered = TRUE) 

leaflet() %>% setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
  addProviderTiles("Esri.OceanBasemap") %>% 
  addCircleMarkers(data = x, lat =  ~BEGIN_LAT, lng = ~BEGIN_LON, 
                   radius = 3, 
                   fillColor = pal(x$dd),
                   stroke = FALSE, fillOpacity = 0.8) %>%
  addLegend("bottomleft", pal=pal, values=x$dd,
            layerId="colorLegend")


#Arbre simplifie
plot(hdb, show_flat = T, gradient = c("purple", "red", "green", "yellow","blue"), scale=0.5)
plot(x[,c(3,4)],col=hdb$cluster+1L)

## Arbre simplifie, avec les clusters les plus stables
plot(hdb, scale = "suggest",
     gradient = c("yellow","green","blue"), show_flat = TRUE)





###########################################################
######### Sous clustering ############

#Importation
filename = "C:/Users/User/OneDrive_ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/HDBSCAN_Clustering_Final.csv"
base = read.csv2(filename, sep = ";", dec = ",")
#View(base)
base = base[which(base$hdb.cluster==4),-c(1,6)]
dim(base)

#####Construction de la matrice de dissimilarité
coords <- base[,c(3,4)]

## Creation d'un objet spatial
dtest <- sp::SpatialPoints(coords = coords, proj4string = sp::CRS("+proj=longlat +ellps=WGS84")) #Non projet
plot(dtest@coords)

## Calcul de la matrice des distances geographiques (calculee avec une fonction du package sp)
# Attention!!! Relativement long en temps de calcul, pour notre base d'environ 40000
distmatrix <- sp::spDists(x = dtest, longlat = T) #Renvoie la matrice des distances en kilometres
dd=distmatrix

#conversion de la matrice en objet dist. Sinon pas utilisable par hclust
distmatrix <- stats::as.dist(distmatrix) 


#### Réalisation du clustering
hdb <- dbscan::hdbscan(distmatrix, minPts = 20,
                       gen_hdbscan_tree = TRUE,
                       gen_simplified_tree = TRUE,
                       verbose = TRUE)
#21 donne 3 clusters
#Recuperation des clusters
results = data.frame(base)
x <- cbind(results, hdb$cluster)
#Stockage
#filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/HDBSCAN_Clustering_Final.csv"
#write.csv2(x,filename, sep = ";", dec = ".")

#View(x)
nrow(x)
colnames(x)[5]='dd'
unique(x$dd)
colorBy <- unique(x$dd) 
# c("#26C4EC","#00FF00","#C2F732","#ED7F10","#FF0000","#F9429E")
# "YlOrRd"
pal <- colorFactor(c("#FF0000","#26C4EC","#00FF00","#C2F732","#ED7F10","#F9429E"), colorBy, ordered = TRUE) 

leaflet() %>% setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
  addProviderTiles("Esri.OceanBasemap") %>% 
  addCircleMarkers(data = x, lat =  ~BEGIN_LAT, lng = ~BEGIN_LON, 
                   radius = 3, 
                   fillColor = pal(x$dd),
                   stroke = FALSE, fillOpacity = 0.8) %>%
  addLegend("bottomleft", pal=pal, values=x$dd,
            layerId="colorLegend")


#Arbre simplifie
plot(hdb, show_flat = T, gradient = c("purple", "red", "green", "yellow","blue"), scale=0.5)
plot(x[,c(3,4)],col=hdb$cluster+1L)

## Arbre simplifie, avec les clusters les plus stables
plot(hdb, scale = "suggest",
     gradient = c("yellow","green","blue"), show_flat = TRUE)

































########
#############################################################################
#############################################################################

############# Carte de chaleur
# Installation de la bibliothèque leaflet.extras
install.packages("leaflet.extras")

# Chargement de la bibliothèque leaflet.extras
library(leaflet.extras)

library(leaflet)
library(dplyr)

# Chargement des données de tornades
data <- read.csv("chemin/vers/le/fichier.csv")

# Création de la carte
map <- leaflet() %>%
  setView(lng = -98.5795, lat = 39.8283, zoom = 4)

# Ajout de la carte de chaleur
map <- map %>% addTiles() %>% addHeatmap(
  data = data,
  lng = ~BEGIN_LON,
  lat = ~BEGIN_LAT,
  blur = 40,
  max = 0.05
)

# Affichage de la carte
map


#######################


library(ggplot2)
library(maps)

# Chargement des données de tornades
data <- read.csv("chemin/vers/le/fichier.csv")

# Création de la carte de chaleur
usa_map <- map_data("state")

ggplot(data, aes(x = BEGIN_LON, y = BEGIN_LAT)) +
  geom_density2d(alpha = 0.4, bins = 30) +
  scale_fill_gradient(low = "white", high = "red") +
  geom_path(data = usa_map, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  coord_fixed(xlim = c(-130, -60), ylim = c(25, 50)) +
  ggtitle("Carte de chaleur des tornades aux États-Unis")



########################################
