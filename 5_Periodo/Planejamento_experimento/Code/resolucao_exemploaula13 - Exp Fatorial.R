

obs<-c(130,155,74,180,34,40,80,75,20,70,82,58,
       150,188,159,126,136,122,106,115,25,70,58,45,
       138,110,168,160,174,120,150,139,96,104,82,60)

n<-4
a<-3
b<-3

repet<-factor(rep(c(1:4),a*b))

fatA<-factor(rep(c("1","2","3"),each=b*n))

fatB<-factor(rep(rep(c("A","B","C"),each=n),a))

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
boxplot(anovai$residuals ~ fatA:fatB)

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
