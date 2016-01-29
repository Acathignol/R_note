rm(list=ls()) 

notes.m=matrix(nr=30,nc=20)
for (i in 1:20){notes.m[,i]=round(rnorm(30,10,abs(rnorm(1,mean=1,sd=0.3))), digit=2)}
colnames(notes.m)= c("E1","E2", "E3", "E4", "E5","E6","E7","E8","E9","E10","E11",
                     "E12","E13","E14","E15","E16","E17","E18","E19","E20")
notes=as.vector(t(notes.m))
etu=sort(rep(as.factor(1:30),20))
plot(notes~etu)
anova(lm(notes~etu))

#creer matrice 30par20
#rentre des notes selon loi normale
#nomme evaluation
#met les notes en vecteurs
#les trie dans l'ordre croissant
#boxplot notes pour les afficher
#ANOVA=??????????????????????????????????????????? analyse de variance notes 


pValues<-c()
count=0
for (i in 1:30){
  count=count+1
  for(j in 1:30){
    t.test(etu)
    pValues[count]<-pvalue
    count=count+1}
}#SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE how to do




#recup p value t test, ECRIT  !!!!!!!!!!!!!!!!!!! => sinon , "names(objet) => acceder dolar //// sinon chercher type objet(plu dur)






#selon la simul, ANOVA donne un summary de Df , sum, sq, mean, sq, F , value , Pr pour tout les étudiants
#y qttitatif mais x qualitatif => effet des notes entre les eleves (var notes entre eleve par rapport entre var des note pour un eleves
#=> inter vs intra => ANOVA (ou LM)) si p-value significative => les eleves ont pas la mm notes si non => tout les eleves ont globalement mm notes

#on trouvera pas significatif => tt eleve ont mm note (H0 vrai de vrai)

#tt les eleves 1 à 1 => test de t 
#rejet H0 alors que vraie (on trouve significative alors que faux)
# avec 435 test => prob de se tromper au moins une fois = 1
#COMMENT FAIRE SANS? => alpha=0.05/435 ATENTTION PB !!!!! PERTE DE TOUTE LA PUISSANCE DU TEST !!!!!!!!!!!!!!!!
#donc suite exo car plus cap de voir un meilleur qu'un autre

# donc controle de risque


#--------------------------------------------------------------------------------------------------------------

library(multcomp)
test.none <- pairwise.t.test(notes, etu,p.adj="none")
summary(as.vector(test.none$p.value<0.05))

#--------------------------------------------------------------------------------------------------------------

notes.m=matrix(nr=30,nc=20)
for (i in 1:20){notes.m[,i]=round(rnorm(30,10,abs(rnorm(1,mean=1,sd=0.3))), digit=2)}
colnames(notes.m)= c("E1","E2", "E3", "E4", "E5","E6","E7","E8","E9","E10","E11",
                     "E12","E13","E14","E15","E16","E17","E18","E19","E20")
notes.m[5,]=notes.m[5,]+1
notes=as.vector(t(notes.m))
etu=sort(rep(as.factor(1:30),20))
plot(notes~etu)
anova(lm(notes~etu))
test.none <- pairwise.t.test(notes, etu,p.adj="none")
summary(as.vector(test.none$p.value<0.05))
test.bonf <- pairwise.t.test(notes, etu,p.adj="bonferroni")
summary(as.vector(test.bonf$p.value<0.05))
((test.bonf$p.value/test.none$p.value)[test.bonf$p.value!=1])

#--------------------------------------------------------------------------------------------------------------
  
notes.m=matrix(nr=30,nc=20)
for (i in 1:20){notes.m[,i]=round(rnorm(30,10,abs(rnorm(1,mean=1,sd=0.3))), digit=2)}
colnames(notes.m)= c("E1","E2", "E3", "E4", "E5","E6","E7","E8","E9","E10","E11",
                     "E12","E13","E14","E15","E16","E17","E18","E19","E20")
notes.m[1:30,]=notes.m[1:30,]+seq(-1, +1, le=30)
notes.m=notes.m[sample(1:30,30, rep=F),]
notes=as.vector(t(notes.m))
etu=sort(rep(as.factor(1:30),20))
plot(notes~etu)
anova(lm(notes~etu))
test.none <- pairwise.t.test(notes, etu,p.adj="none")
summary(as.vector(test.none$p.value<0.05))
test.bonf <- pairwise.t.test(notes, etu,p.adj="bonferroni")
summary(as.vector(test.bonf$p.value<0.05))
test.holm <- pairwise.t.test(notes, etu,p.adj="holm")
summary(as.vector(test.holm$p.value<0.05))
test.BH <- pairwise.t.test(notes, etu,p.adj="BH")
summary(as.vector(test.BH$p.value<0.05))

#--------------------------------------------------------------------------------------------------------------