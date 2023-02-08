
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


