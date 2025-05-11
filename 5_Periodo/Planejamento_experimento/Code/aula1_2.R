### Aula 1 e 2 ###

#Exercicio 1

xb<-85
mpop<-100
desvio<-12
n<-16

tcalc<- (xb-mpop)/(desvio/sqrt(n))
tcalc

pt(tcalc,n-1) #p-valor
xcrit <- (qt(.1,(n-1))*sqrt(desvio^2/n)+mpop)
xcrit
#beta(100)
tbeta<-(xcrit-100)/(desvio/sqrt(n))
tbeta

beta<-pt(tbeta,n-1, lower.tail = FALSE)
beta
poder<- 1-beta
poder


#beta(90)
tbeta<-(xcrit-90)/(desvio/sqrt(n))

beta<-pt(tbeta,n-1, lower.tail = FALSE)
poder<- 1-beta



##Exercicio2

Aa<-c(13.6,13.6,14.7,12.1,12.3,13.2,11,12.4)
Bb<-c(11.4,12.5,14.6,13,11.7,10.3,9.8,10.4)
Dif<-Aa-Bb
Dif
ndif<-length(Dif)
ndif
mean(Dif)
sd(Dif)

tobs<-mean(Dif)/(sd(Dif)/sqrt(ndif))
tobs

pt(tobs,ndif-1,lower.tail = FALSE)

t.test(Aa,Bb, paired=TRUE, alternative="greater")
t.test(Dif, alternative="greater")

##Exercicio 3

variA<-c(1.3,1.4,1.1,1.4,1.5)
variB<-c(1.8,1.6,1.9,1.9,1.8)


mean(variA)
var(variA)
mean(variB)
var(variB)

#teste variancia
Fcalc<-var(variA)/var(variB)
Fcalc

valorp<-2*pf(Fcalc,length(variA)-1,length(variB)-1, lower.tail = FALSE)
valorp

var.test(variA,variB)

#teste t

variedade<-c(variA,variB)
variedade
grupo<-c(rep("A",length(variA)),rep("B",length(variB)))
grupo

t.test(variedade ~grupo,var.equal=TRUE)

2*pt(5.263,8,lower.tail = FALSE)
