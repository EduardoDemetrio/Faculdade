obs<-c(122,144,145,125,137,144,120,134,136,150,155,156,153,165,171)
trat<-factor(rep(c("A","B","C","D","E"),each=3))
bloco<-factor(rep(c(1:3),5))
dados<-data.frame(obs,trat,bloco)

boxplot(obs ~ trat)
boxplot(obs ~ bloco)

modelo<-aov(obs ~ trat + bloco)
summary(modelo)


#Analise dos pressupostos
resido<-modelo$residuals

qmres<-summary(modelo)[[1]][3,3]
residpad<-resido/sqrt(qmres)


##Normalidade

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

plot(residpad,modelo$fitted.values)

vartrat <- tapply(obs,trat,var)
vartrat

Fcalc<-max(vartrat)/min(vartrat)
Fcalc

valorp<-2*pf(Fcalc,b-1,b-1, lower.tail = FALSE)
valorp


bartlett.test(obs ~ trat)
bartlett.test(obs ~ bloco)

library(car)


leveneTest(obs ~ trat)
leveneTest(obs ~ bloco)

#Aditividade

mod<-lm(obs ~ trat + bloco)
anova(mod)

ad<-(predict(mod))^2

modad<-lm(obs ~ trat + bloco + ad)

anova(mod,modad)


require(asbio)
tukey.add.test(obs,trat,bloco) 
###########################################

#############################################
#item 1.4
sqtrat<-summary(modelo)[[1]][1,2]
sqres<-summary(modelo)[[1]][3,2]
sqtotal<-summary(modelo)[[1]][1,2]+summary(modelo)[[1]][2,2] + summary(modelo)[[1]][3,2]

R2<-1-(sqres/sqtotal)
R2

#Teste de Tuckey
mediatrat <- tapply(obs,trat,mean);mediatrat
TukeyHSD(modelo, ord=T)
TukeyHSD(modelo,which = 'trat')

