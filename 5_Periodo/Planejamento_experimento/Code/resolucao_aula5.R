obs<-c(575,542,530,539,570,565,593,590,579,610,
       600,651,610,637,629,725,700,715,685,710)
potencia <- factor(rep(c(160,180,200,220),each=5))

boxplot(obs~potencia)




#Resolucao1
N<-length(obs)
N
media <- mean(obs)
media
SQT<-sum((obs-media)^2)
SQT

SQT2<-sum(obs^2) - ((sum(obs))^2/N)
SQT2


glT<-N-1


a<-length(unique(potencia))
n<-N/a

mediatrat <- tapply(obs,potencia,mean)
mediatrat

SQTrat<- n*(sum((mediatrat-media)^2))
SQTrat

glTrat<-a-1

SQRes <- sum((obs-rep(mediatrat,each=n))^2)
SQRes
glRes<-N-a

Fcalc<-(SQTrat/glTrat)/(SQRes/glRes)
Fcalc

qf(0.05,glTrat,glRes, lower.tail = FALSE)

valorp<-pf(Fcalc,glTrat,glRes, lower.tail = FALSE)
valorp

vajust<-rep(mediatrat,each=n)
residcalc<-obs -vajust


#Resolucao2
anovap<-aov(obs ~ potencia)
summary(anovap)


#Analise dos pressupostos
resid<-anovap$residuals

qmres<-summary(anovap)[[1]][2,3]
residpad<-resid/sqrt(qmres)

#normalidade
hist(resid)
hist(residpad)
boxplot(resid)
boxplot(residcalc)

qqnorm(resid)
qqline(resid)

shapiro.test(resid) 

#independencia

plot(resid)
plot(residpad)

#homogeneidade de variancia

plot(residpad,anovap$fitted.values)

vartrat <- tapply(obs,potencia,var)
vartrat

Fcalc<-max(vartrat)/min(vartrat)
Fcalc

valorp<-2*pf(Fcalc,n-1,n-1, lower.tail = FALSE)
valorp

pot200<-c(600,651,610,637,629)
pot220<-c(725,700,715,685,710)
var.test(pot200,pot220)


bartlett.test(obs ~ potencia)

library(car)

medianas <- tapply(obs,potencia,median)
desviosy <- abs(obs - rep(medianas,each=n))
summary(aov(desviosy~potencia))


leveneTest(obs ~ potencia)
###########################################

#############################################


