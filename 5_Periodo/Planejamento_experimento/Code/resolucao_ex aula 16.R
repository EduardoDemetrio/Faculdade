obs<-c(27,30,31,26,30,34,
       26,28,32,27,36,36,
       28,26,29,28,35,36,
       31,34,33,32,34,29,
       30,28,32,30,33,30,
       28,30,32,33,34,31,
       28,34,26,26,36,28,
       26,35,27,29,36,26,
       27,34,28,28,34,26)



n<-3
a<-3
b<-3
c<-2

#fator A - ciclo
fatA<-factor(rep(c("40","50","60"),each=b*c*n)) 

#fator B - operador
fatB<-factor(rep(rep(c("1","2","3"),c),a*n))

#fator C - temperatura
fatC<-factor(rep(rep(c("300","350"),each=b),a*n))

cbind(obs, fatA, fatB,fatC)

dados<-data.frame(obs, fatA, fatB,fatC)
dados

anovaf<-aov(obs ~ fatA*fatB*fatC)
summary(anovaf)



###Verificar pressuposto###
resido<-anovaf$residuals

qmres<-summary(anovaf)[[1]][8,3]
residpad<-resido/sqrt(qmres)

#normalidade


qqnorm(resido)
qqline(resido)

shapiro.test(resido)

#independencia

plot(resido)
plot(residpad)

#homogeneidade de variancia

plot(anovaf$residuals,anovaf$fitted.values)
#identify(anovaf$residuals,anovaf$fitted.values)

boxplot(anovaf$residuals ~ fatA)
boxplot(anovaf$residuals ~ fatB)
boxplot(anovaf$residuals ~ fatC)
boxplot(anovaf$residuals ~ fatA*fatB*fatC)

library(car)

leveneTest(obs ~ fatA)
leveneTest(obs ~ fatB)
leveneTest(obs ~ fatC)

leveneTest(obs ~ fatA*fatB)
leveneTest(obs ~ fatA*fatC)
leveneTest(obs ~ fatB*fatC)
leveneTest(obs ~ fatA*fatB*fatC)

library(lmtest)

#executar o teste de Breusch-Pagan
bptest(anovaf)

##################################

interaction.plot(fatA,fatB,obs)
interaction.plot(fatA,fatC,obs)
interaction.plot(fatB,fatC,obs)

TukeyHSD(anovaf)

media<-tapply(dados$obs, list(Ciclo = dados$fatA, Operador = dados$fatB, Temperatura = dados$fatC), mean)
media
