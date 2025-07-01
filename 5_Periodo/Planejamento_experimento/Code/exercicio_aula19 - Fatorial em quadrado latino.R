obs<-c(90,106,108,81,90,88,114,96,105,83,86,84,102,90,95,92,85,104,
       87,84,100,96,110,91,93,112,92,80,90,98,86,91,97,98,100,92)

linha<-factor(rep(c(1:6),each=6))
coluna<-factor(rep(c(1:6),6))

a<-2
b<-3

fatA<-factor(c("1","1","1","2","2","2",
               "1","1","1","2","2","2",
               "1","2","2","1","2","1",
               "2","2","1","1","1","2",
               "2","1","2","2","1","1",
               "2","2","2","1","1","1"))

fatB<-factor(c("1","2","3","1","3","2",
               "3","1","2","3","2","1",
               "2","2","3","1","1","3",
               "2","1","1","2","3","3",
               "3","3","1","2","1","2",
               "1","3","2","3","2","1"))

trat<-factor(c("A","B","C","D","F","E",
               "C","A","B","F","E","D",
               "B","E","G","A","D","C",
               "E","D","A","B","C","F",
               "F","C","D","E","A","B",
               "D","F","E","C","B","A"))

dados<-data.frame(obs,linha,fatA,fatB,coluna)
dados

anovaq<-aov(obs ~ linha + coluna + fatA + fatB + fatA:fatB )
summary(anovaq)



###Verificar pressuposto###
resido<-anovaq$residuals

qmres<-summary(anovaq)[[1]][6,3]
residpad<-resido/sqrt(qmres)

#normalidade


qqnorm(resido)
qqline(resido)

shapiro.test(resido)

#independencia

plot(resido)
plot(residpad)

#homogeneidade de variancia

plot(anovaq$residuals,anovaq$fitted.values)

boxplot(anovaq$residuals ~ fatA)
boxplot(anovaq$residuals ~ fatB)
boxplot(anovaq$residuals ~ fatA*fatB)

library(car)
leveneTest(obs ~ fatA)
leveneTest(obs ~ fatB)
leveneTest(obs ~ fatA:fatB)
leveneTest(obs ~ linha)
leveneTest(obs ~ coluna)

library(lmtest)

#executar o teste de Breusch-Pagan
bptest(anovaq)


##################################


modlct<-lm(obs ~ linha + coluna + fatA + fatB + fatA:fatB )
adlct<-(predict(modlct))^2
modadlct<-lm(obs ~ linha + coluna + fatA + fatB + fatA:fatB + adlct)
anova(modlct,modadlct)

require(dae)
tukey.1df(anovaq , dados)
#####################################
interaction.plot(fatA,fatB,obs)




media<-tapply(dados$obs, list(dados$fatA, dados$fatB), mean)
#media<-tapply(obs, trat, mean)
media
TukeyHSD(anovaq, ord=T)

