rm(list=ls()) 

Table<-read.table("/home/acathignol/R/arbre.txt")

#print(Table)

T1<-Table$V1[Table$V2=="T1"]
T2<-Table$V1[Table$V2=="T2"]

ST1=sum(T1)
#337.6
ST2=sum(T2)
#355.4
#peut pas savoir si significativement plus grande donc doit tester!

T12<-Table$V1[1:27]

SEch<-c()
for (i in 1:1000){
  test=sample(T12,13)
  Stest=sum(test)
  SEch[i]=Stest
}

hist(SEch,breaks=20,ylab='Nombre d\'échantillons',xlab='Somme de la hauteur des arbres',col='grey')
abline(v=ST1,lty=5)

#---------------------------------------UNILATERAL TEST-----------------------------------------------
pvalue=sum(SEch>=ST1)/1000
summary(SEch>=ST1)
pvalue
#187 true
#=> Semble plutot probable ??????????

#---------------------------------------BILATERAL TEST-----------------------------------------------

pvalue=2*sum(SEch>=ST1)/1000
pvalue
#372 true
#=> Semble plutot probable

#prefer XXXXX because XXXX
#What did we do not use or not blablabla
