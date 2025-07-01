obs<-c(22,31,25,32,43,29,35,34,50,55,47,46,44,45,38,40,37,36,60,
       50,54,39,41,47)

length(obs)

a<-2
b<-2
c<-2
n<-3
N<-a*b*c*n

fata<-(rep(rep(c(-1,1),each=3),4))
fata

fatb<-(rep(c(rep(c(-1,-1),each=3),rep(c(1,1),each=3)),2))
fatb

fatc<-(c(rep(c(-1,-1,-1,-1),each=3),rep(c(1,1,1,1),each=3)))
fatc


X<-22+31+25
A<-32+43+29
B<-35+34+50
C<-44+45+38
AB<-55+47+46
AC<-40+37+36
BC<-60+50+54
ABC<-39+41+47

efeitoA<-(A+AB+AC+ ABC - X - B - C - BC)/(4*n)
efeitoA

modeloc<-lm(obs ~  fata*fatb*fatc)
summary(modeloc)

#modelosa<-lm(obs ~  fatb*fatc)
#summary(modelosa)

#anova(modeloc,modelosa)
fatA<-factor(rep(rep(c(-1,1),each=3),4))
fatA

fatB<-factor(rep(c(rep(c(-1,-1),each=3),rep(c(1,1),each=3)),2))
fatB

fatC<-factor(c(rep(c(-1,-1,-1,-1),each=3),rep(c(1,1,1,1),each=3)))
fatC


anovaf<-aov(obs ~ fatA*fatB*fatC)
summary(anovaf)

#################################
interaction.plot(fatA,fatC,obs)
interaction.plot(fatA,fatB,obs)
interaction.plot(fatB,fatC,obs)

TukeyHSD(anovaf)
media<-tapply(obs, list(fatA,fatB,fatC), mean);media
################################
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

library(lmtest)

#executar o teste de Breusch-Pagan
bptest(anovaf)


boxplot(anovaf$residuals ~ fatA)
boxplot(anovaf$residuals ~ fatB)
boxplot(anovaf$residuals ~ fatC)

library(car)

leveneTest(obs ~ fatA)
leveneTest(obs ~ fatB)
leveneTest(obs ~ fatC)
leveneTest(obs ~ fatA:fatB)
leveneTest(obs ~ fatA:fatC)
leveneTest(obs ~ fatB:fatC)
leveneTest(obs ~ fatA:fatB:fatC)

