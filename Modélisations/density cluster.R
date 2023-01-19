#### Clustering base sur la densite ####

# Installation des packages
install.packages("dbscan")
install.packages("fastcluster")
install.packages("sp")

# Chargement des packages
library(fastcluster)
library(dbscan)
library(sp)

?dbscan
?spDists

# Chargement des données sur les tornades
Base <- read.csv2("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE.csv", sep = ",", stringsAsFactors = TRUE,  dec = ".")
#On considère les données à partir de 1990, et on garde juste les longitudes et depart,
#car elles n'ont pas de valeurs manquantes
apply(apply(Base[which(Base$YEAR>=1990),c(22,23,24,25)],2,is.na),2,sum)
Base = Base[which(Base$YEAR>=1990),c(2,13,23,22)]
View(Base)
set.seed(99)
ba=sample(1:nrow(Base),37000)


### Recuperation des coordonnees. L'ordre long / lat est important
coords <- Base[ba,c(3,4)]

Base_depart = Base
Base=Base[ba,]

## Creation d'un objet spatial
dtest <- sp::SpatialPoints(coords = coords, proj4string = sp::CRS("+proj=longlat +ellps=WGS84")) #Non projet
plot(dtest@coords)


## Calcul de la matrice des distances geographiques (calculee avec une fonction du package sp)
# Attention!!! Long en temps de calcul, pour notre base d'environ 40000
distmatrix <- sp::spDists(x = dtest, longlat = T) #Renvoie la matrice des distances en kilometres
dd=distmatrix
#traitement d'une sous partie de dtest
distmatrix <- stats::as.dist(distmatrix) #conversion de la matrice en objet dist. Sinon pas utilisable par hclust


############################################################################
############################################################################

                          ###############################
                            # METHODES AGGLOMERATIVES #
                          ###############################


### INFLUENCE DE LA METHODE D'AGGLOMERATION
#Liste des methodes testees
Lstmethodes = list("complete", "single", "average", "ward.D")

#Creation d'une matrice stockant les valeurs de nbre de clusters et  nbre de points isoles
ClustersPtsIso <- matrix(nrow = 4, ncol = 2, dimnames = list(c("complete", "single", 
                               "average", "ward.D"), c("NbClusters","NbIsoPts")))

#Creation d'un dataframe stockant les N° de clusters pour chaque  point selon differentes methodes
#Tableau ou chaque ligne represente un point et ayant comme colonnes : 
  #state / scale / lon / lat
results = data.frame(Base) #Stockage de state/scale/lon/lat
ClusteringData <- as.data.frame(matrix(nrow = length(Base$STATE), ncol = 4, 
                                       dimnames = list(NULL, c("complete", "single", 
                                                               "average", "ward.D"))))

for (methode in (1:length(Lstmethodes))) {
  #clustering
  fast <- fastcluster::hclust(distmatrix, method = Lstmethodes[[methode]])
  #methode de clustering conditionne le fait de pouvoir utiliser  height comme argument pour couper l'arbre
 #Marche avec complete / single (mais pas g?nial) / average / ward.D / ward.D2
 #fast #dÃ©tail du clustering
 #plot(fast) #afficher le dendrogramme
 
 #Creation des clusters : coupe du dendrogramme a une hauteur relative
fastcl <- stats::cutree(fast, h = max(fast$height)*20/100) 
#Hauteur laquelle on coupe l'arbre = 20% de la hauteur max pour matrice distances geographiques
  
  #Stockage des resultats : N° de cluster pour chaque point
  ClusteringData[,methode] <- fastcl
  
  NbClusters <- max(fastcl) #nb de clusters
  ClusterSize <- as.data.frame(table(fastcl)) #compte le nombre de points par cluster
  #Pour fastcluster il faut reperer les clusters de 1 points
  IsolatedPts <- ClusterSize[which(ClusterSize$Freq <= 1),] #Clusters de 1 pts = Points isoles
  NbIsolatedPts <- length(IsolatedPts$Freq) #Nb de points isoles
  #Stockage dans la matrice du nbre de clusters et de points isoles
  ClustersPtsIso[methode,1] = NbClusters #1ere colonne = Nb de clusters
  ClustersPtsIso[methode,2] = NbIsolatedPts #2e colonne = Nb de points isolÃ©s
}

#Recuperation des dernieres colonnnes
results <- cbind(results, ClusteringData) #Stockage des resultats des clustering
                                          #par point et par methode 

#Exportation des resultats sous format csv
#Fichier stockant les coordonnees et N° de clusters
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/methodeagglo_results.csv"
write.csv2(results,filename, sep = ";", dec = ".")
#Fichier stockant le nbre de clusters et de points isoles otenus pour chaque methode
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/methodeagglo_ClustersIsoPts.csv"
write.csv2(ClustersPtsIso,filename, sep = ";", dec = ".")




###############################################################################
###############################################################################

                        ################
                           # DBSCAN #
                        ################


### INFLUENCE DES PARAMETRES DE DBSCAN ###

#####   Influence de MinPts
Eps = 20 #eps fixe Ã 20 km
#Liste des valeurs de MinPts Ã tester
MinPts = list(1,5,15,40,90,150,350,700,1000,2000)
#Generation de matrices et dataframe similaires aux precedents
#Creation d'une matrice stockant les valeurs de nbre de clusters et nbre de points isoles

ClustersPtsIso <- matrix(nrow = length(MinPts), ncol = 2, dimnames = 
list(c("MinPts = 1", "MinPts = 5", "MinPts = 15", "MinPts = 40","MinPts = 90",
       "MinPts = 150", "MinPts = 350", "MinPts = 700", "MinPts = 1000","MinPts = 2000"), 
     c("NbClusters","NbIsoPts")))

#Creation d'un dataframe stockant les N° de clusters pour chaque point selon differentes methodes
results = data.frame(Base) #Stockage de state / scale / lon / lat
ClusteringData <- as.data.frame(matrix(nrow = length(Base$STATE), ncol = length(MinPts), 
   dimnames = list(NULL, c("MinPts = 1", "MinPts = 5", "MinPts = 15", "MinPts = 40","MinPts = 90",
                           "MinPts = 150", "MinPts = 350", "MinPts = 700", "MinPts = 1000","MinPts = 2000"))))


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
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANMinPts_results.csv"
write.csv2(results,filename, sep = ";", dec = ".")
#Fichier stockant le nbre de clusters et de points isoles obtenus pour chaque methode
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANMinPts_ClustersIsoPts.csv"
write.csv2(ClustersPtsIso,filename, sep = ";", dec = ".")



#######  Influence de Eps
MinPts = 4 #MinPts fixe a 4
#Lite des valeurs de Eps a tester
Eps = list(5,15,40,75,100,200,400)

#Generation de matrices et dataframe similaires aux precedents
#Creation d'une matrice stockant les valeurs de nbre de clusters et nbre de points isoles
ClustersPtsIso <- matrix(nrow = length(Eps), ncol = 2, dimnames = list(c("Eps = 5", "Eps = 15", 
          "Eps = 40", "Eps = 75","Eps = 100","Eps = 200","Eps = 400"), c("NbClusters","NbIsoPts")))

#Creation d'un dataframe stockant les N° de clusters pour chaque  point selon differentes methodes
results = data.frame(Base) #Stockage de state / scale / lon / lat

ClusteringData <- as.data.frame(matrix(nrow = length(Base$STATE), ncol = length(Eps), 
                   dimnames = list(NULL, c("Eps = 5", "Eps = 15", "Eps = 40", "Eps = 75",
                                           "Eps = 100","Eps = 200","Eps = 400"))))


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
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANEps_results.csv"
write.csv2(results,filename, sep = ";", dec = ".")
#Fichier stockant le nbre de clusters et de points isoles otenus pour chaque methode
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANEps_ClustersIsoPts.csv"
write.csv2(ClustersPtsIso,filename, sep = ";", dec = ".")


###############################################################################
###############################################################################

         #########################################################
           # Determination des parametres optimaux pour DBSCAN #
         #########################################################

#Importation des resultats
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANEps_results.csv"
DBSCANEps_results = read.csv(filename, sep = ";")
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANEps_ClustersIsoPts.csv"
DBSCANEps_ClustersIsoPts = read.csv(filename, sep = ";")
DBSCANEps_ClustersIsoPts
#On peut voir que l'augmentation de EPS a pour effet de reduire le nombre de clusters
#De meme, ca permet de reduire le nombre de points isoles
#On a donc le double effet positif recherche

filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANMinPts_results.csv"
DBSCANMinPts_results = read.csv(filename, sep = ";")
filename = "C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tornades_USA_BE_Euria/Modélisations/DBSCANMinPts_ClustersIsoPts.csv"
DBSCANMinPts_ClustersIsoPts = read.csv(filename, sep = ";")
DBSCANMinPts_ClustersIsoPts
#On constate que l'augmentation de Minpts diminue considerablement le nombre de clusters
#et que des valeurs de plus en plus eleve de Minpts conduisent a classer de plus en plus de points comme des points isoles






















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




#############################################################################
#############################################################################



#Influence nombre du nombre des points

### CHARGEMENT DU JEU DE DONNEES

#Nom des ensembles de points Ã traiter : correspond Ã©galement au nom des dossiers stockant les sorties
#On a choisi de ne pas garder en mÃ©moire chaque ensemble de points
 
  ### Recuperation des coordonnees et calcul du nombre de publications ###
  #necessaire de mettre dans le bon ordre : long / lat 
  coords <- na.omit(matrix(c(Base_filt$lng,Base_filt$lat),ncol = 
                             2, dimnames = list(NULL,c("lng","lat")))) #lon lat
  coords <- as.data.frame(coords)
  #on recupere le nb de publi par an pour tous les points qui possedent des coordonnees
  data = Base_filt[,6:21]
  
  
  ### Creation d'un objet spatial ###
  dtest <- sp::SpatialPoints(coords = coords, proj4string = sp::CRS("+proj=longlat +ellps=WGS84")) #pas projet
  #plot(dtest@coords)
    
    
    ### Calcul de la matrice des distances ###
    #Matrice des distances geographiques (calcule avec une fonction du package sp)
distmatrix <- sp::spDists(x = dtest, longlat = T) 
  
distmatrix <- stats::as.dist(distmatrix)    #conversion de la matrice en objet dist. Sinon pas utilisable par hclust



#####################################################################
#############################  CLUSTERING  ##########################
#####################################################################

### AGNES : fastcluster ###
fast <- fastcluster::hclust(distmatrix, method = "complete") 

              ##### clustering
 fast
#plot(fast)           #afficher le dendrogramme
 #Creation des clusters : coupe du dendrogramme a une hauteur 
relative
 fastcl <- stats::cutree(fast, h = max(fast$height)*20/100) 
#Hauteur a laquelle on coupe l'arbre = 20% de la hauteur max pour matrice distances geographiques


             ### HDBSCAN ###
hdb <- dbscan::hdbscan(distmatrix, minPts = 5, gen_hdbscan_tree = 
                         T, gen_simplified_tree = T) #generer les arbres


             ### DBSCAN ###
db <- dbscan::dbscan(distmatrix, minPts = 5, eps = 20) #distmatrix au lieu de coords. Ici eps = 20 km

#Stockage des clusters : regroupement des listes du N° de clusters pour chaque point selon chaque methode
clusters <- list(fastcl, hdb$cluster, db$cluster) 


### Creation du csv stockant le nb de clusters et de points isoles par methode ###
#Creation d'une matrice stockant les valeurs
ClustersPtsIso <- matrix(nrow = 3, ncol = 2, 
                         dimnames = 
                           list(c("fastcluster","HDBSCAN","DBSCAN"),
                                c("NbClusters","NbIsoPts")))

#Remplissage de la matrice
for (row in (1:length(clusters))) {
  NbClusters = max(clusters[[row]]) #Nb de clusters
  ClusterSize <- as.data.frame(table(clusters[row])) #compte le nombre de points par cluster
  #Pour fastcluster il faut reperer les clusters de 1 points
  if (row == 1) {
    IsolatedPts <- ClusterSize[which(ClusterSize$Freq <= 1),] 
    #Clusters de 1 pts = Points isoles
    NbIsolatedPts <- length(IsolatedPts$Freq) #Nb de points isolÃ©s
  }
  #Pour HDBSCAN et DBSCAN, il suffit simplement recuperer le nb de points du cluster d'indice 0
 else {
 if (ClusterSize$Var1[1] == 0) {     #Si le premier indice trouve est 0 alors il y a des points isoles
 NbIsolatedPts <- ClusterSize$Freq[1] 
 } else {                            #Sinon pas de points bruits
 NbIsolatedPts <- 0
 }
 }
 
 ClustersPtsIso[row,1] = NbClusters       #1ere colonne = Nb de clusters
 ClustersPtsIso[row,2] = NbIsolatedPts    #2e colonne = Nb de points isoles
}

 #Ecriture du csv
 #La variable ensemble sert a la fois d'indication de repertoire et de nom de fichier
  filename = 
    paste("/home/sigma/Bureau/Scripts/InfluenceNbPts/",ensemble,"/",ense
          mble,"_ClustersIsoPts.csv", sep = "")
  write.csv2(ClustersPtsIso,filename, sep = ";", dec = ".")
  
  
  ### Export des resultats sous csv ### 
  #Tableau ou chaque ligne represente un point et ayant comme colonnes : 
    #state / scale / lon / lat /
    #N° de cluster pour fastcluster / N° pour HDBSCAN / N° DBSCAN 
  results = data.frame(Base) #Stockage de state / scale / lon / lat
  results$fastcluster = fastcl
  results$HDBSCAN = hdb$cluster
  results$DBSCAN = db$cluster
 
 
 filename = 
paste("/home/sigma/Bureau/Scripts/InfluenceNbPts/",ensemble,"/",ense
mble,"_results.csv", sep = "")
 write.csv2(results, filename, sep = ";", dec = ".")
 
  
#fin de la grosse boucle for




 
 
 
#Note!! Une partie du code a ete inspire du document :
#"Recherche et tests d’algorithmes de clustering spatial pour webdataviz."
# De Emna CHIKHAOUI et Etienne DUPERRON 

