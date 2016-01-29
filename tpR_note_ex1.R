rm(list=ls()) 

Table<-read.table("/home/acathignol/R/arbre.txt")

#print(Table)

T1<-Table$V1[Table$V2=="T1"]
T2<-Table$V1[Table$V2=="T2"]

par(mfrow=c(1,2))
hist(T1,ylab='Nombre d\'arbres',xlab='Hauteur',col='grey',ylim=c(0,6),xlim=c(22,29))
hist(T2,ylab='Nombre d\'arbres',xlab='Hauteur',col='grey',ylim=c(0,6),xlim=c(22,29))

x11()
boxplot(T1,T2,names=c("T1", "T2"),type='l',ylab='Hauteur',xlab='Terrain',col='grey')
title('Boxplots de le la hauteur d\'arbres issues de deux terrains différents')

ks.test(T1,T2)# warning: Approxiamation
#Il y a 69 chances pour 100 d'observer les distributions de tailles de T1 et T2 sous l'hypothèses qu'ils sont les même terrains

var.test(T1,T2)
#Il y a 36 chances pour 100 d'observer les variances de tailles de T1/T2 observées sous l'hypothèses qu'ils sont les même terrains NOT SURE!!!
#=> pas bcp!!!!!!!!!!!!!!!!!

t.test(T1,T2)
#Il y a 34 chances pour 100 d'observer les moy de tailles de T1 et T2 sous l'hypothèses qu'ils sont les même terrains
#=> pas bcp!!!!!!!!!!!!!!!!!

#Donc nous pouvons en conclure qu'il est propable que ces terrains soient bien similaires ou que ces arbres sont issus d'un même Terrain

