rm(list=ls()) 

library(grDevices)

s1<-rnorm(30,m=0,sd=1)
s2<-rnorm(30,m=2,sd=1)
a=qnorm(0.025)
b=qnorm(0.975)

x11()
curve(dnorm(x,m=0,sd=1),col='orange', lwd=3, lty=2,xlim=c(-4,6),ylab='Densités de probabilités',main='Loi normale',xlab='s1 et s2')
curve(dnorm(x,m=2,sd=1),col=rgb(0.05,0.3,1,0.5), lwd=3, lty=1,xlim=c(-4,6),ylab='Densités de probabilités',main='Loi normale',xlab='s1 et s2', add=TRUE)
legend("topright",legend=c("s1","s2"),lty=c(2,1),lwd=3,col=c('orange',rgb(0.05,0.3,1,0.5)))
abline(v=a,lty=5,col='orange')
abline(v=b,lty=5,col='orange')

abline(v=a+2,lty=5,col=rgb(0.05,0.3,1,0.5))
abline(v=b+2,lty=5,col=rgb(0.05,0.3,1,0.5))

x11()
#parmfrow=c(2,2)
h1=hist(s1,breaks=10,col=3)
h2=hist(s2,breaks=10,col=5)

#c=sort(c(h1$breaks,h2$breaks)) =>xlim=range(c)
#d=sort(c(h1$counts,h2$counts))#=>ylim=range(d)

hist(s1,col='orange',xlim=c(-4,6),ylim=c(0,0.7),breaks=10,main='Loi normale',ylab='Density',xlab='s1 et s2',freq=F)
hist(s2,col=rgb(0.05,0.3,1,0.5),breaks=10,add=TRUE,freq=F)
curve(dnorm(x,m=0,sd=1),col='orange', lwd=3, lty=2,add=TRUE)
curve(dnorm(x,m=2,sd=1),col=rgb(0.05,0.3,1,0.5), lwd=3, lty=1, add=TRUE)
abline(v=a,lty=5,col='orange')
abline(v=b,lty=5,col='orange')
abline(v=a+2,lty=5,col=rgb(0.05,0.3,1,0.5))
abline(v=b+2,lty=5,col=rgb(0.05,0.3,1,0.5))
legend("topright",legend=c("s1","s2"),lty=c(2,1),lwd=3,col=c('orange',rgb(0.05,0.3,1,0.5)))




