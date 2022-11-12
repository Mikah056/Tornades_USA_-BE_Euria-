###########  Traitement de la base  ###########


###########################################################################
## Exportation et importation des bases obtenues
###########################################################################

## Importation de la base Details
Data=read.csv(file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE.csv',header=TRUE)
View(Data)

#Exportation de la base Details obtenue
write.csv(Data,file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE.csv',row.names = FALSE)


#Importation de la base Locations
z=read.csv(file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE_L.csv',header=TRUE)
View(z)

#Exportation de la base Locations obtenue
write.csv(z,file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Base_BE_L.csv',row.names = FALSE)

########################################################################
########################################################################




###################### importation des 73 bases data-events-Details
####################################################################

#importation des 73 bases data-events-Details
Alldata=list(73)

for(i in 1950:1971){
  Alldata[[i]]=read.csv(paste0("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d",i,"_c20210803.csv"),header=T)
}
for(i in 1972:2013){
  Alldata[[i]]=read.csv(paste0("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d",i,"_c20220425.csv"),header=T)
}
Alldata[[2014]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d2014_c20220719.csv",header=T)
Alldata[[2015]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d2015_c20220425.csv",header=T)
Alldata[[2016]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d2016_c20220719.csv",header=T)
Alldata[[2017]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d2017_c20220719.csv",header=T)
Alldata[[2018]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d2018_c20220425.csv",header=T)
Alldata[[2019]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d2019_c20220425.csv",header=T)
Alldata[[2020]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d2020_c20220816.csv",header=T)
Alldata[[2021]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d2021_c20220921.csv",header=T)
Alldata[[2022]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Details/StormEvents_details-ftp_v1.0_d2022_c20220921.csv",header=T)

View(Alldata[[1972]])

#Suppression des variables inutiles
Alldata1=NULL
for (i in 1950:2022) {
  Alldata1[[i]]=Alldata[[i]][,-c(3,6,7,10,14,15,16,17,18,19,20,27,28,29,30,31,35,36,37,38,49,50,51)]
}

#Suppression des lignes des evevenements autres que les tornades
Alldata2=NULL
for (i in 1950:2022) {
  Alldata2[[i]]=Alldata1[[i]][-which(Alldata1[[i]]$EVENT_TYPE!='Tornado'),]
}

#Fusion des 73 bases
Data=Alldata2[[1950]]
for (i in 1951:2022) {
  Data=rbind(Data,Alldata2[[i]])
}

#Suppression de la colonne 'tornade'
Data=Data[,-9]

View(Data)

############################ Fin de la constitution de la base Details
########################################################################



######################################################################
###################### importation des 51 bases data-events-Locations

Alldata0=list(51)
for(i in 1972:2013){
  Alldata0[[i]]=read.csv(paste0("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d",i,"_c20220425.csv"),header=T)
}
Alldata0[[2014]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d2015_c20220425.csv",header=T)
Alldata0[[2015]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d2015_c20220425.csv",header=T)
Alldata0[[2016]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d2016_c20220719.csv",header=T)
Alldata0[[2017]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d2017_c20220719.csv",header=T)
Alldata0[[2018]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d2018_c20220425.csv",header=T)
Alldata0[[2019]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d2019_c20220425.csv",header=T)
Alldata0[[2020]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d2020_c20220816.csv",header=T)
Alldata0[[2021]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d2021_c20220921.csv",header=T)
Alldata0[[2022]]=read.csv("C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/Storm Events _ Locations/StormEvents_Locations-ftp_v1.0_d2022_c20220921.csv",header=T)

#Ces bases sont vides en deca de 1996
View(Alldata0[[1996]])

#Fusion des 51 bases
DataL=Alldata0[[1996]]
for (i in 1997:2022) {
  DataL=rbind(DataL,Alldata0[[i]])
}

#Suppression des lignes des evevenements autres que les tornades a partir de EVENT_ID
z=NULL
for (i in 1:nrow(DataL)) {
  z=rbind(z,DataL[which(DataL$EVENT_ID[i]==Data$EVENT_ID),])
}
View(z)

#Nombre de valeurs manquantes a partir de 1996
dataL_na=apply(apply(z,2,is.na),2,sum)
dataL_na


str(Data)
View(Data)
############################## Fin de la constitution de la base Locations
###########################################################################


########################################################################
## Les variables de cout dans la base "Details" sont en chaine de caractere. 
## Nous les transformons ici en "integer"
########################################################################
library(stringr)

## Pour les domages aux propriétés
l = nchar(Data$DAMAGE_PROPERTY)
grandeur = str_sub(Data$DAMAGE_PROPERTY,l,l)
g = which((grandeur!="M") & (grandeur!="B") & (grandeur!="K"))
Data$DAMAGE_PROPERTY[g] = paste0(Data$DAMAGE_PROPERTY[g],"j")
l = nchar(Data$DAMAGE_PROPERTY)
grandeur = str_sub(Data$DAMAGE_PROPERTY,l,l)
Data$DAMAGE_PROPERTY = str_sub(Data$DAMAGE_PROPERTY,1,l-1)
Data$DAMAGE_PROPERTY = as.numeric(Data$DAMAGE_PROPERTY)
grandeur[which(grandeur=="j")]="1"
grandeur[which(grandeur=="K")]="1000"
grandeur[which(grandeur=="M")]="1000000"
grandeur[which(grandeur=="B")]="1000000000"
grangeur = as.numeric(grandeur)
Data$DAMAGE_PROPERTY = Data$DAMAGE_PROPERTY*grangeur

## Pour les domages aux cultures
l = nchar(Data$DAMAGE_CROPS)
grandeur = str_sub(Data$DAMAGE_CROPS,l,l)
g = which((grandeur!="M") & (grandeur!="B") & (grandeur!="K"))
Data$DAMAGE_CROPS[g] = paste0(Data$DAMAGE_CROPS[g],"j")
l = nchar(Data$DAMAGE_CROPS)
grandeur = str_sub(Data$DAMAGE_CROPS,l,l)
Data$DAMAGE_CROPS = str_sub(Data$DAMAGE_CROPS,1,l-1)
Data$DAMAGE_CROPS = as.numeric(Data$DAMAGE_CROPS)
grandeur[which(grandeur=="j")]="1"
grandeur[which(grandeur=="K")]="1000"
grandeur[which(grandeur=="M")]="1000000"
grandeur[which(grandeur=="B")]="1000000000"
grangeur = as.numeric(grandeur)
Data$DAMAGE_CROPS = Data$DAMAGE_CROPS*grangeur

########################################################################
########################################################################



########################################################################
### Analyse des valeurs manquantes
########################################################################

#Les bases Locations sont vides en deca de 1972; on peut supposer que les coordonnees
#n'etaient pas encore enregistrÃ©s de facon prÃ©cise, et ont ete estimees a partir des
#differentes sources d'information et des recits sur les evenements.

#On a vu qu'au dela de 1995, locations a plus de NA que Details
#Le croisement pour combler les vides des variables de localisation de Details
#n'a rien donne d'interessant

################################### Base Details
#Nombre de NA par variable
library(ggplot2)
data_na=apply(apply(Data,2,is.na),2,sum)
data_na=as.data.frame(data_na)
B=cbind(data_na,row.names(data_na))
B=B[13:27,]
colnames(B)=c("Nombre_de_valeurs_manquantes","Variables")
p = ggplot(data=B, aes(y=Nombre_de_valeurs_manquantes,x=Variables)) +
  geom_bar(stat='identity')
p + coord_flip()

#Pourcentage des observations manquantes apres 1985
datam=Data[,13:27]
(apply(apply(datam[which(Data$YEAR>1985),],2,is.na),2,sum)/apply(apply(datam,2,is.na),2,sum))*100
apply(apply(datam[which(Data$YEAR>2010),],2,is.na),2,sum)


## Repartition des NA par annee
Nombre_NA=c()
for (i in 1955:2022) { 
  Nombre_NA=c(Nombre_NA,sum(apply(apply(datam[which(Data$YEAR==i),],2,is.na),2,sum)))
}
Annees=1955:2022
v1=cbind(Annees,Nombre_NA)
v1=as.data.frame(v1)
p = ggplot(data=v1,mapping=aes(x=Annees,y=Nombre_NA)) +
  geom_line(color="blue") +
  geom_point() +
  xlab("Annees") + ylab("Nombre de valeurs manquantes")
p 



#Proportion de NA dans chaque variable
l=nrow(datam[which(Data$YEAR>=2000),])
na1=apply(apply(datam[which(Data$YEAR>=2000),],2,is.na),2,sum)
na2=100*na1/l
na2
#Exportation 
write.csv(na2,file='C:/Hakim/D1/Euria/M1/BE/Data/na2.csv',row.names = TRUE)


#Nombre de tornade n'ayant pas d'information d'arrivee sachant qu'elles n'ont pas
#d'information de depart
length(Data$END_LON[which(is.na(Data$BEGIN_LON))])

#Proportion des tornades dont nous n'avons aucune information geographique
(1191/length(Data$YEAR>=1980))*100
##On se propose de les supprimer et d'appliquer les mÃ©thodes de lissage aux autres valeurs manquantes


#Suppression des individus n'ayant pas d'information de localisation
length(which(is.na(Data$BEGIN_LAT) & is.na(Data$BEGIN_LON) & is.na(Data$END_LAT) & is.na(Data$END_LON)))
Data = Data[-which(is.na(Data$BEGIN_LON)),]

########################################################################
########################################################################


########################################################################
####### Traitement des valeurs manquantes






