obs<-c(90,86,96,84,100,92,92,81,102,87,106,90,105,97,
       96,80,114,93,112,91,108,95,98,83)

a<-3
b<-2
n<-4

fatA<-factor(rep(c("Al","Bm","Ch"),each=b*n))
fatB<-factor(rep(rep(c("1","2"),n),a))

bloco<-factor(rep(rep(c("1","2","3","4"),each=b),a))

dados<-data.frame(obs,fatA,fatB,bloco)
dados

anovab<-aov(obs ~ bloco + fatA + fatB + fatA:fatB)
summary(anovab)


interaction.plot(fatA,fatB,obs)


TukeyHSD(anovab)


###Verificar pressuposto###
resido<-anovab$residuals

qmres<-summary(anovab)[[1]][5,3]
residpad<-resido/sqrt(qmres)

#normalidade


qqnorm(resido)
qqline(resido)

shapiro.test(resido)

#independencia

plot(resido)
plot(residpad)

#homogeneidade de variancia

plot(anovab$residuals,anovab$fitted.values)

boxplot(anovab$residuals ~ fatA)
boxplot(anovab$residuals ~ fatB)


library(car)
leveneTest(obs ~ fatA)
leveneTest(obs ~ fatB)
leveneTest(obs ~ fatA:fatB)

leveneTest(obs ~ bloco)

library(lmtest)

#executar o teste de Breusch-Pagan
bptest(anovab)


##################################

### manual
modlct<-lm(obs ~ bloco + fatA + fatB + fatA:fatB)


adlct<-(predict(modlct))^2
modadlct<-lm(obs ~ bloco + fatA + fatB + fatA:fatB + adlct)
anova(modlct,modadlct)

### Ouuu usar o pacote dae:

require(dae)
tukey.1df(anovab , dados)

media<-tapply(dados$obs, list(dados$fatA, dados$fatB), mean)
media
TukeyHSD(anovab, ord=T)
