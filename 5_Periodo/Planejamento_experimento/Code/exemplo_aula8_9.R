obs<-c(90.3, 89.2,98.2,93.9,87.4,97.9,92.5,89.5,90.6,94.7,87,95.8,
       85.5,90.8,89.6,86.2,88,93.4,82.5,89.5,85.6,87.4,78.9,90.7)
       
trat<-factor(rep(c(8500,8700,8900,9100),each=6))
bloco<-factor(rep(c(1:6),4))

dados<-data.frame(obs,trat,bloco)




modeloBl<-aov(obs ~ trat + bloco, data=dados)
summary(modeloBl)


#################################################
modeloInt<-aov(obs ~ trat, data=dados)
summary(modeloInt)

################################################

#Analise dos pressupostos
resido<-modeloBl$residuals

qmres<-summary(modeloBl)[[1]][3,3]
residpad<-resido/sqrt(qmres)
#normalidade
hist(resido)
hist(residpad)
boxplot(resido)
boxplot(residpad)

qqnorm(resido)
qqline(resido)

shapiro.test(resido)

#independencia

plot(resido)
plot(residpad)

#homogeneidade de variancia
a<-length(unique(trat))
b<-length(unique(bloco))
N<-length(obs)

plot(residpad,modeloBl$fitted.values)

vartrat <- tapply(obs,trat,var)
vartrat

Fcalc<-max(vartrat)/min(vartrat)
Fcalc

valorp<-2*pf(Fcalc,b-1,b-1, lower.tail = FALSE)
valorp

tratmax<-c(90.3, 89.2,98.2,93.9,87.4,97.9)
tratmin<-c(85.5,90.8,89.6,86.2,88,93.4)
var.test(tratmax,tratmin)


bartlett.test(obs ~ trat)

library(car)

medianas <- tapply(obs,trat,median)
desviosy <- abs(obs - rep(medianas,each=b))
summary(aov(desviosy~trat))


leveneTest(obs ~ trat)
###########################################

#Aditividade

require(asbio)
tukey.add.test(obs,trat,bloco)  

mod<-lm(obs ~ trat + bloco)


ad<-(predict(mod))^2

modad<-lm(obs ~ trat + bloco + ad)

anova(mod,modad)

###########################################

#############################################
