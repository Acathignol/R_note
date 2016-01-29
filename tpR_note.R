rm(list=ls()) 

Table<-read.table("/home/acathignol/R/arbre.txt")

#print(Table)

T1<-Table$V1[Table$V2=="T1"]
T2<-Table$V1[Table$V2=="T2"]

T1 
T2

"moyT1<-mean(T1)
moyT2<-mean(T2)

ETypT1=sqrt(var(T1))
ETypT2=sqrt(var(T2))"

par(mfrow=c(1,2))
hist(T1,ylab='Hauteur',xlab='Terrain',col='grey',ylim=c(0,7),xlim=c(22,29))
hist(T2,ylab='Hauteur',xlab='Terrain',col='grey',ylim=c(0,7),xlim=c(22,29))

x11()
boxplot(T1,T2,names=c("T1", "T2"),type='l',ylab='Hauteur',xlab='Terrain',col='grey')
title('Boxplots de le la hauteur des arbres issues de deux terrains différents')



