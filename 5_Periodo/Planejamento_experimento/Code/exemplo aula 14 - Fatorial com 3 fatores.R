
obs<-c(1.30,1.19,1.08,1.26,1.21,1.19,
       1.05,1.00,1.05,1.52,1.56,1.55)
       
n<-3
a<-2
b<-2

repet<-factor(rep(c(1:3),a*b))

fatA<-factor(rep(c("A0","A1"),each=b*n))

fatB<-factor(rep(rep(c("B0","B1"),each=n),a))

dados<-data.frame(obs,repet, fatA, fatB)
head(dados)

anovai<-aov(obs ~ fatA + fatB + fatA:fatB)
summary(anovai)

ano1<-aov(obs ~ fatA*fatB)
summary(ano1)

anovalm<-lm(obs ~ fatA*fatB)
summary(anovalm)
anova(anovalm)

###Verificar pressuposto###
resido<-anovai$residuals

qmres<-summary(anovai)[[1]][4,3]
residpad<-resido/sqrt(qmres)

#normalidade

qqnorm(resido)
qqline(resido)

shapiro.test(resido)

#independencia

plot(resido)
plot(residpad)

#homogeneidade de variancia

plot(anovai$residuals,anovai$fitted.values)

boxplot(anovai$residuals ~ fatA)
boxplot(anovai$residuals ~ fatB)


library(car)
leveneTest(obs ~ fatA)
leveneTest(obs ~ fatB)
leveneTest(obs ~ fatA:fatB)

library(lmtest)

#executar o teste de Breusch-Pagan
bptest(anovai)
bptest(anovalm)

#########

interaction.plot(fatB,fatA,obs)

interaction.plot(fatA,fatB,obs)

TukeyHSD(anovai)

media<-tapply(dados$obs, list(dados$fatA, dados$fatB), mean)
media

