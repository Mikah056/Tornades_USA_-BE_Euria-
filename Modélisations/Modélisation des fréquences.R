
###################################################################
#################    Statistiques descriptives
  library(gridExtra)
library(cowplot)

#Evolution des fréquence au fil des années
library(ggplot2)
f1 = as.data.frame(table(Data$YEAR))
f1[,1] = as.factor(f1[,1])
f = ggplot(data=f1,aes(x=Var1,y=Freq,group=1)) +
  geom_line(color="blue") +
  geom_point() +
  xlab("Annees") + ylab("Fréquence des tornades") +
  theme(axis.text.x = element_text(angle=70, vjust=0.5))
f 

#Fréquences par mois et par année
unique(Data$MONTH_NAME)
Tab=table(Data$MONTH_NAME,Data$YEAR)
Recap = data.frame()
Recap = rbind(Recap,Tab[which(row.names(Tab)=="January"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="February"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="March"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="April"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="May"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="June"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="July"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="August"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="September"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="October"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="November"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="December"),])
row.names(Recap) = c("Janvier","Fevrier","Mars","Avril","Mai","Juin","Juillet","Août","Septembre","Octobre","Novembre","Decembre")
colnames(Recap) = 1990:2022
Recap

#Exportation du tableau de contingence des fréquences par mois et par année
write.csv(Recap,file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tableaux/T1.csv',row.names = TRUE)


f1 = as.data.frame(table(Data$YEAR))
f1[,1] = as.factor(f1[,1])
f = ggplot(data=f1,aes(x=Var1,y=Freq,group=1)) +
  geom_line(color="blue") +
  geom_point() +
  xlab("") + ylab("") + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())
f 

i=12
f0 = as.data.frame(t(Recap[i,]))
v2=1990:2022
f0 = cbind(f0,v2)
colnames(f0)[1] = "v1"
f0[,2] = as.factor(f0[,2])
g12 = ggplot(data=f0,aes(x=v2,y=v1,group=1)) +
  geom_line(color="blue") +
  geom_point() + xlab("") + ylab("") +
  labs(title = row.names(Recap)[i]) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank())
g12
x11()
grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12, ncol=2, nrow = 6)

#Stat desc par mois
f2 = as.data.frame(table(Data$MONTH_NAME))
write.csv(f2,file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tableaux/T2.csv',row.names = TRUE)

f3=matrix()
f3 = t(rbind(apply(t(Recap),2,min),apply(t(Recap),2,median),apply(t(Recap),2,mean),rbind(apply(t(Recap),2,sd)),apply(t(Recap),2,max),apply(t(Recap),2,sum)))
write.csv(f3,file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tableaux/T3.csv',row.names = TRUE)

xtick<-row.names(Recap)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3], 
     labels = xtick, srt = 45, pos = 1, xpd = TRUE)
boxplot(t(Recap),las=3)

## Statistiques par intensité
Tab=table(Data$MONTH_NAME,Data$TOR_F_SCALE)
colnames(Recap) = c("EF0","EF1","EF2","EF3","EF4","EF5")
Recap
#Exportation du tableau de contingence des fréquences par mois et par année
write.csv(Recap,file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Tableaux/T2.csv',row.names = TRUE)

a = ggplot(Data, aes(x = TOR_F_SCALE)) +
  geom_bar(stat = "count") +                      # Bâtons
  theme(legend.position = "none") +               # Enlève la légende
  xlab("Echelle de Fujita") +                                      # Axe des x, non pertinent
  ylab("fréquence")                               # Axe des y
a



#################################################################
#################################################################"
## Analyse suivant l'echelle
unique(Data$TOR_F_SCALE)
Tab=table(Data$TOR_F_SCALE,Data$YEAR)
Recap = data.frame()
Recap = rbind(Recap,Tab[which(row.names(Tab)=="EF0"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="EF1"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="EF2"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="EF3"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="EF4"),])
Recap = rbind(Recap,Tab[which(row.names(Tab)=="EF5"),])

row.names(Recap) = c("EF0","EF1","EF2","EF3","EF4","EF5")
colnames(Recap) = 1990:2022

R=t(Recap)
library(scales)
ggplot(Data) +
  aes(x = YEAR, fill = TOR_F_SCALE) +
  geom_bar(position = "fill") +
  xlab("Années") +
  ylab("Proportion") +
  labs(fill = "Echelle de Fujita") +
  scale_y_continuous(labels = percent)

#######################################################
Dmap = Data[which(Data$TOR_F_SCALE=="EF5"),]

library(tigris)
states <- states(cb = TRUE)


map = leaflet() %>% setView(lng = -98.583, lat = 39.833, zoom = 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(data=states,weight = 2,
              opacity = 1,
              color = "grey",
              dashArray = "3",
              fillOpacity = 0.4) %>%
            addCircleMarkers(data = Dmap, lat =  ~BEGIN_LAT, lng = ~BEGIN_LON, 
                   radius = 1, color="red")
map


##################################################

library(tigris)
countries <- states(cb = TRUE)
countries$NAME=toupper(countries$NAME)

map <- leaflet(countries)

G = table(Data$STATE)
names(G)
countries$freq=0
countries$freq[na.omit(match(names(G), countries$NAME))] = G[-c(18,49)]

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = countries$freq
)
map %>%
  addPolygons(label = ~STUSPS,labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
              stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(freq)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~freq,
            title = "Frequence",
            opacity = 1
  )




############################################
















library(leaflet)
library(spatstat)

data <- read.csv("tornades.csv", header = TRUE)
tornades_ppp <- ppp(Data$BEGIN_LON, Data$BEGIN_LAT, window = owin(xrange = c(-130, -65), yrange = c(25, 50)))



divideIntoCells <- function(win, n) {
  xrange <- as.vector(xrange(win))
  yrange <- as.vector(yrange(win))
  nx <- n
  ny <- n
  cellwidth <- (xrange[2] - xrange[1])/nx
  cellheight <- (yrange[2] - yrange[1])/ny
  cells <- list()
  for (i in 1:nx) {
    for (j in 1:ny) {
      xleft <- xrange[1] + cellwidth * (i - 1)
      ybottom <- yrange[1] + cellheight * (j - 1)
      cells[[j+(i-1)*ny]] <- owin(xrange = c(xleft, xleft + cellwidth), yrange = c(ybottom, ybottom + cellheight))
    }
  }
  cells
}



divideIntoCells <- function(xmin, xmax, ymin, ymax, n) {
  cellwidth <- (xmax - xmin)/n
  cellheight <- (ymax - ymin)/n
  cells <- list()
  for (i in 1:n) {
    for (j in 1:n) {
      xleft <- xmin + cellwidth * (i - 1)
      ybottom <- ymin + cellheight * (j - 1)
      cells[[j+(i-1)*n]] <- list(xrange = c(xleft, xleft + cellwidth), yrange = c(ybottom, ybottom + cellheight))
    }
  }
  cells
}













fenetres <- divideIntoCells(tornades_ppp, n = 50)
densites <- lapply(fenetres, FUN = function(w) {
  ppm(kappamethod = "reed.young", tornades_ppp[in.window(w)]) 
})
probas <- lapply(densites, FUN = function(d) {
  predict(d, tornades_ppp, type = "density") 
})
k <- 5
clusters <- lapply(probas, FUN = function(p) {
  kmeans(p, centers = k)$cluster 
})

colors <- c("red", "green", "blue", "orange", "purple")

map <- leaflet() %>% 
  setView(lng = -98.5795, lat = 39.8283, zoom = 4)

map <- map %>% 
  addTiles()

for (i in seq_along(fenetres)) {
  cluster_ids <- clusters[[i]]
  color <- colors[cluster_ids[1]]
  cluster_data <- data[which(in.psp(tornades_ppp, window = fenetres[[i]])),]
  cluster_markers <- apply(cluster_data, 1, function(x) {
    marker(lng = x["longitude"], lat = x["latitude"], color = color, radius = 3)
  })
  map <- map %>% 
    addMarkers(cluster_markers)
}

map






Dat = na.omit(Data[,c(24,25)])


# Charger les bibliothèques requises
library(dplyr)
library(leaflet)

# Charger les données de tornades
tornadoes <- read.csv("https://raw.githubusercontent.com/canghirla/curso-R/gh-pages/data/tornadoes.csv")

# Diviser la fenêtre spatiale en une grille de cellules carrées
n <- 20
xrange <- c(min(Dat$BEGIN_LON), max(Dat$BEGIN_LON))
yrange <- c(min(Dat$BEGIN_LAT), max(Dat$BEGIN_LAT))
cells <- divideIntoCells(xrange[1], xrange[2], yrange[1], yrange[2], n)

# Compter le nombre de tornades dans chaque cellule
counts <- lapply(cells, function(cell) {
  Dat %>%
    filter(BEGIN_LON >= cell$xrange[1] & BEGIN_LON < cell$xrange[2] &
             BEGIN_LAT >= cell$yrange[1] & BEGIN_LAT < cell$yrange[2]) %>%
    nrow()
})

# Trouver la cellule avec le nombre maximum de tornades
maxcount <- max(unlist(counts))
maxcell <- cells[[which(unlist(counts) == maxcount)]]

# Créer un objet 'leaflet' centré sur la carte des États-Unis
map <- leaflet() %>%
  setView(lng = -96, lat = 37.5, zoom = 4)

# Ajouter une couche de tuiles pour la carte
map <- map %>% addTiles()

# Ajouter une couche pour chaque cellule de la grille, en couleur en fonction du nombre de tornades qu'elle contient
for (i in 1:length(cells)) {
  cell <- cells[[i]]
  count <- counts[[i]]
  if (count > 0) {
    map <- map %>%
      addPolygons(
        lng = c(cell$xrange[1], cell$xrange[2], cell$xrange[2], cell$xrange[1]),
        lat = c(cell$yrange[1], cell$yrange[1], cell$yrange[2], cell$yrange[2]),
        fillColor = "red",
        fillOpacity = 0.5 * count/maxcount,
        stroke = FALSE
      )
  }
}

# Ajouter un marqueur pour la cellule avec le nombre maximum de tornades
map <- map %>%
  addMarkers(
    lng = mean(maxcell$xrange),
    lat = mean(maxcell$yrange),
    popup = sprintf("%d tornades", maxcount)
  )

# Afficher la carte
map








##################################################################





# Charger les bibliothèques requises
library(dplyr)
library(leaflet)

# Charger les données de tornades
tornadoes <- read.csv("https://raw.githubusercontent.com/canghirla/curso-R/gh-pages/data/tornadoes.csv")

# Diviser la fenêtre spatiale en une grille de cellules carrées
n <- 10
xrange <- c(min(Dat$BEGIN_LON), max(Dat$BEGIN_LON))
yrange <- c(min(Dat$BEGIN_LAT), max(Dat$BEGIN_LAT))
cells <- divideIntoCells(xrange[1], xrange[2], yrange[1], yrange[2], n)

# Compter le nombre de tornades dans chaque cellule
counts <- lapply(cells, function(cell) {
  Dat %>%
    filter(BEGIN_LON >= cell$xrange[1] & BEGIN_LON < cell$xrange[2] &
             BEGIN_LAT >= cell$yrange[1] & BEGIN_LAT < cell$yrange[2]) %>%
    nrow()
})

# Trouver la cellule avec le nombre maximum de tornades
maxcount <- max(unlist(counts))
maxcell <- cells[[which(unlist(counts) == maxcount)]]

# Créer une fonction de couleur numérique en fonction du nombre de points dans chaque cellule
pal <- colorNumeric(palette = "Reds", domain = c(0, maxcount))

# Créer un objet 'leaflet' centré sur la carte des États-Unis
map <- leaflet() %>%
  setView(lng = -96, lat = 37.5, zoom = 4)

# Ajouter une couche de tuiles pour la carte
map <- map %>% addTiles()

# Ajouter une couche pour chaque cellule de la grille, en couleur en fonction du nombre de points qu'elle contient
for (i in 1:length(cells)) {
  cell <- cells[[i]]
  count <- counts[[i]]
  if (count > 0) {
    map <- map %>%
      addPolygons(
        lng = c(cell$xrange[1], cell$xrange[2], cell$xrange[2], cell$xrange[1]),
        lat = c(cell$yrange[1], cell$yrange[1], cell$yrange[2], cell$yrange[2]),
        fillColor = pal(count),
        fillOpacity = 0.7,
        stroke = FALSE
      )
  }
}

# Ajouter un marqueur pour la cellule avec le nombre maximum de points
map <- map %>%
  addMarkers(
    lng = mean(maxcell$xrange),
    lat = mean(maxcell$yrange),
    popup = sprintf("%d tornades", maxcount)
  ) %>%
  addPolygons(data=states, label = ~STUSPS,labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
              weight = 2,
              opacity = 1,
              color = "black",
              dashArray = "3",
              fillOpacity = 0.7) 

# Ajouter une échelle de couleur à la carte
map <- addLegend(
  map = map,
  position = "bottomright",
  pal = pal,
  values = unlist(counts),
  title = "Nombre de tornades"
)

# Afficher la carte
map








###############################################################


# Charger les bibliothèques requises
library(dplyr)
library(leaflet)

# Charger les données de tornades
tornadoes <- read.csv("https://raw.githubusercontent.com/canghirla/curso-R/gh-pages/data/tornadoes.csv")

# Diviser la fenêtre spatiale en une grille de cellules carrées
n <- 20
xrange <- c(min(Dat$BEGIN_LON), max(Dat$BEGIN_LON))
yrange <- c(min(Dat$BEGIN_LAT), max(Dat$BEGIN_LAT))
cells <- divideIntoCells(xrange[1], xrange[2], yrange[1], yrange[2], n)

# Compter le nombre de tornades dans chaque cellule
counts <- lapply(cells, function(cell) {
  Dat %>%
    filter(BEGIN_LON >= cell$xrange[1] & BEGIN_LON < cell$xrange[2] &
             BEGIN_LAT >= cell$yrange[1] & BEGIN_LAT < cell$yrange[2]) %>%
    nrow()
})

# Calculer la densité pour chaque cellule
densities <- lapply(counts, function(count) {
  count / (n^2)
})

# Trouver la cellule avec la densité maximale
maxdensity <- max(unlist(densities))
maxcell <- cells[[which(unlist(densities) == maxdensity)]]

# Créer une fonction de couleur numérique en fonction de la densité de points dans chaque cellule
pal <- colorNumeric(palette = "Reds", domain = c(0, maxdensity))

# Créer un objet 'leaflet' centré sur la carte des États-Unis
map <- leaflet() %>%
  setView(lng = -96, lat = 37.5, zoom = 4)

# Ajouter une couche de tuiles pour la carte
map <- map %>% addTiles()

# Ajouter une couche pour chaque cellule de la grille, en couleur en fonction de la densité de points qu'elle contient
for (i in 1:length(cells)) {
  cell <- cells[[i]]
  density <- densities[[i]]
  if (density > 0) {
    map <- map %>%
      addPolygons(
        lng = c(cell$xrange[1], cell$xrange[2], cell$xrange[2], cell$xrange[1]),
        lat = c(cell$yrange[1], cell$yrange[1], cell$yrange[2], cell$yrange[2]),
        fillColor = pal(density),
        fillOpacity = 0.5,
        stroke = FALSE
      )
  }
}

# Ajouter un marqueur pour la cellule avec la densité maximale
map <- map %>%
  addMarkers(
    lng = mean(maxcell$xrange),
    lat = mean(maxcell$yrange),
    popup = sprintf("%.2f tornades/km²", maxdensity)
  )

# Ajouter une échelle de couleur à la carte
map <- addLegend(
  map = map,
  position = "bottomright",
  pal = pal,
  values = unlist(densities),
  title = "Densité de tornades (tornades/km²)"
)

# Afficher la carte
map


#############################


library(sp)
library(leaflet)

# Définir la matrice de caractéristiques pour chaque cellule
cell.features <- cbind(counts, densities)

# Définir la matrice de distance entre les cellules
cell.distances <- dist(cell.features)

# Regrouper les cellules en clusters hiérarchiques
cell.hclust <- hclust(cell.distances, method = "complete")

# Sélectionner le niveau de granularité souhaité
cell.cutree <- cutree(cell.hclust, h = 3)

# Colorier les cellules en fonction de leur cluster
colors <- colorFactor(palette = "Set1", domain = cell.cutree)
leaflet() %>%
  addTiles() %>%
  addPolygons(
    lng = c(cell$xrange[1], cell$xrange[2], cell$xrange[2], cell$xrange[1]),
    lat = c(cell$yrange[1], cell$yrange[1], cell$yrange[2], cell$yrange[2]),
    fillColor = colors(cell.cutree),
    fillOpacity = 0.5,
    stroke = FALSE
  ) %>%
 # addPolygons(data = cells, stroke = FALSE, fillOpacity = 0.7,
 #             fillColor = colors(cell.cutree), group = "Clusters") %>%
  addLegend(pal = colors, values = cell.cutree, title = "Clusters")



##############



