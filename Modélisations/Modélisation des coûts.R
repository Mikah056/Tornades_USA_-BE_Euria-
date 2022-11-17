
###################################################################
#################    Staistiques descriptives

###################################
#Suppression des couts NA en attendant les traitements
z=Data[which(!is.na(Data$DAMAGE_PROPERTY)),]
###################################

##Statistiques descriptives des couts par annee
summary(z$DAMAGE_PROPERTY)         #Toute la distribution
tab=NULL
for (i in 1980:2022) {
  tab=rbind(tab,summary(z$DAMAGE_PROPERTY[which(z$YEAR==i)]))
}
tab=as.data.frame(cbind(1980:2022,tab))
colnames(tab)[1]="Annee"
tab
write.csv(tab,file='C:/Users/User/OneDrive - ENSEA/[L3]/Bureau/HAKIM_EURIA_M1/BE/Data/tabp.csv',row.names = TRUE)


#Représentation de la densité pour toutes les tornades
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)
p1=z %>%
  filter(DAMAGE_PROPERTY>1e5) %>%
  ggplot(aes(DAMAGE_PROPERTY)) +
  geom_density(color="blue")
p2=z %>%
  filter(DAMAGE_PROPERTY>1e6) %>%
  ggplot(aes(DAMAGE_PROPERTY)) +
  geom_density(color="blue")
p3=z %>%
  filter(DAMAGE_PROPERTY>1e8) %>%
  ggplot(aes(DAMAGE_PROPERTY)) +
  geom_density(color="blue")
p4=z %>%
  filter(DAMAGE_PROPERTY>1e9) %>%
  ggplot(aes(DAMAGE_PROPERTY)) +
  geom_density(color="blue")
plot_grid(p1, p2,p3 ,p4, labels=c("Couts > 10^5", "Couts > 10^6", "Couts > 10^8", "Couts > 10^9"), ncol = 2, nrow = 2)


#Verifions si cette structure est suivi par annee
za=z[which(z$YEAR==2022),]
p1=za %>%
  filter(DAMAGE_PROPERTY>1e5) %>%
  ggplot(aes(DAMAGE_PROPERTY)) +
  geom_density(color="blue")
p2=za %>%
  filter(DAMAGE_PROPERTY>1e6) %>%
  ggplot(aes(DAMAGE_PROPERTY)) +
  geom_density(color="blue")
p3=za %>%
  filter(DAMAGE_PROPERTY>1e8) %>%
  ggplot(aes(DAMAGE_PROPERTY)) +
  geom_density(color="blue")
p4=za %>%
  filter(DAMAGE_PROPERTY>1e9) %>%
  ggplot(aes(DAMAGE_PROPERTY)) +
  geom_density(color="blue")
plot_grid(p1, p2,p3 ,p4, labels=c("Couts > 10^5", "Couts > 10^6", "Couts > 10^8", "Couts > 10^9"), ncol = 2, nrow = 2)

#####La structure sur le jeu complet n'est pas la meme par annee
####La structure semble se reprter d'une annee a l'autre a l'interieur de certaines periodes









#Evolution des couts au fil des années
library(ggplot2)
library(dplyr)
z %>%
  filter(DAMAGE_PROPERTY>1e8) %>%
  ggplot(aes(DAMAGE_PROPERTY)) +
    geom_density(color="blue")



c1 = NULL
for (i in 1980:2022) {
  c1=c(c1,sum(z$DAMAGE_PROPERTY[which(z$YEAR==i)]))
}
c1=as.data.frame(cbind(1980:2022,c1))
c = ggplot(data=z,aes(group=YEAR,y=DAMAGE_PROPERTY)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=70, vjust=0.5)) +
  xlab("Annees") + ylab("Fréquence des tornades")
c 





































