obs<-c(575,542,530,539,570,565,593,590,579,610,
       600,651,610,637,629,725,700,715,685,710)
potencia <- factor(rep(c(160,180,200,220),each=5))

boxplot(obs~potencia)

#Resolucao1
N<-length(obs)
media <- mean(obs)
SQT<-sum((obs-media)^2)
SQT

glT<-N-1


a<-length(unique(potencia))
n<-N/a

mediatrat <- tapply(obs,potencia,mean)

SQTrat<- n*(sum((mediatrat-media)^2))
SQTrat
glTrat<-a-1

SQRes <- sum((obs-rep(mediatrat,each=n))^2)
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
cbind(resid,residcalc)

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

#item 1.5
sqtrat<-summary(anovap)[[1]][1,2]
sqtotal<-summary(anovap)[[1]][1,2]+summary(anovap)[[1]][2,2]

R2<-sqtrat/sqtotal
R2

#item 1.6
#Teste de Fisher
med12<-abs(mediatrat[1] - mediatrat[2])
med13<-abs(mediatrat[1] - mediatrat[3])
med14<-abs(mediatrat[1] - mediatrat[4])
med23<-abs(mediatrat[2] - mediatrat[3])
med24<-abs(mediatrat[2] - mediatrat[4])
med34<-abs(mediatrat[3] - mediatrat[4])

alpha<-0.05

LDS<-qt(alpha/2,N-a,lower.tail = FALSE)*(sqrt(2*qmres/n))

cbind(med12>LDS, med13>LDS,med14>LDS,med23>LDS,med24>LDS,med34>LDS)

#Teste de Tukey
f<-N-a

Talpha<-qtukey(alpha,a,f,lower.tail = FALSE)*(sqrt(qmres/n))
cbind(med12>Talpha, med13>Talpha,med14>Talpha,med23>Talpha,med24>Talpha,med34>Talpha)


TukeyHSD(anovap)
anovap.tukey = TukeyHSD( anovap, ordered = T )
anovap.tukey
plot(anovap.tukey)

#teste de Duncan
library(agricolae)
anovap.ducan<-duncan.test(anovap,"potencia")
anovap.ducan
plot(anovap.ducan)

require(ExpDes) 
#item 1.7

c1<-c(1,1,-1,-1)
c2<-c(1,-1,0,0)
c3<-c(0,0,1,-1)

estTc1<-abs(sum(c1*mediatrat)/(sqrt((qmres/n)*sum(c1^2))))
estTc1 > qt(alpha/2,N-a,lower.tail = FALSE)
2*pt(estTc1,N-a,lower.tail = FALSE)

SQc1<-(sum(c1*mediatrat))^2/((1/n)*sum(c1^2))

estTc2<-abs(sum(c2*mediatrat)/(sqrt((qmres/n)*sum(c2^2))))
estTc2 > qt(alpha/2,N-a,lower.tail = FALSE)
2*pt(estTc2,N-a,lower.tail = FALSE)

SQc2<-(sum(c2*mediatrat))^2/((1/n)*sum(c2^2))


estTc3<-abs(sum(c3*mediatrat)/(sqrt((qmres/n)*sum(c3^2))))
estTc3 > qt(alpha/2,N-a,lower.tail = FALSE)
2*pt(estTc3,N-a,lower.tail = FALSE)

SQc3<-(sum(c3*mediatrat))^2/((1/n)*sum(c3^2))

SQtratc<-SQc1 + SQc2 + SQc3
SQtratc

#ou

library(daewr)
#anovap<-aov(obs ~ potencia)
con = matrix(c(1, 1, -1, -1, 1, -1, 0, 0, 0, 0, 1, -1 ), 4, 3 )
L = t(con)
rownames(L) = c("baixas e altas", "baixas", "altas")
L
options(digits = 3)#muda a os decimais
library(gmodels)
fit.contrast( anovap, "potencia", L)

#teste F

Fc1<-(SQc1/1)/qmres
pvalor1<-pf(Fc1,1,N-a,lower.tail = FALSE)

Fc2<-(SQc2/1)/qmres
pvalor2<-pf(Fc2,1,N-a,lower.tail = FALSE)

Fc3<-(SQc3/1)/qmres
pvalor3<-pf(Fc3,1,N-a,lower.tail = FALSE)

#item 1.8

alpha<-0.05

mi<-c(575,600,650,675)
mimedia<-mean(mi)
taui<-mi - mimedia

sigma<-25

delta<-n*sum(taui^2)/sigma^2

fcrit <- qf(1-alpha,a-1,N-a)

beta <- pf(q = fcrit,df1 = a-1,df2 = a*n-a,ncp = delta)
beta
poder<-1-beta


#item 1.9

n<-6
n<-7
delta<-n*sum(taui^2)/sigma^2

fcrit <- qf(1-alpha,a-1,N-a)
beta <- pf(q = fcrit,df1 = a-1,df2 = a*n-a,ncp = delta)
beta
poder<-1-beta
poder
