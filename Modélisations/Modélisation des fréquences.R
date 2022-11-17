
###################################################################
#################    Statistiques descriptives

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







p <- ggplot(data = df, aes(x = dose, y = len, group = 1)) 
# Line plot basique avec des points
p + geom_line() + geom_point()












