obs<-c(432,518,458,583,331,724,478,524,550,400,489,384,556,297,420,
       494,500,313,486,501,515,660,438,394,318)

trat<-factor(c("D","A","B","C","E","C","E","A","B","D","E","B","C",
               "D","A","B","D","E","A","C","A","C","D","E","B"))

linha<-factor(rep(c(1,2,3,4,5),each=5))

coluna<-factor(rep(c(1:5),5))


dados<-data.frame(obs,linha,trat,coluna)

boxplot(obs ~ trat)

anovaq<-aov(obs ~ linha + trat + coluna)
summary(anovaq)

###Verificar pressuposto###
resido<-anovaq$residuals

qmres<-summary(anovaq)[[1]][4,3]
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

#bartlett.test(obs ~ trat)

library(car)
leveneTest(obs ~ trat)

leveneTest(obs ~ linha)
leveneTest(obs ~ coluna)

#Aditividade

### linha coluna trat
modlct<-lm(obs ~ linha + trat + coluna )


adlct<-(predict(modlct))^2
modadlct<-lm(obs ~ linha + trat + coluna + adlct)
anova(modlct,modadlct)

### Ouuu usar o pacote dae:

require(dae)
tukey.1df(anovaq, dados)
###########################################

######################################
#1.4
media<-mean(obs);media

eflinha<-tapply(obs,linha,mean)- media;eflinha

eftrat<-tapply(obs,trat,mean)- media;eftrat

efcoluna<-tapply(obs,coluna,mean)- media;efcoluna



#item 1.5

sqres<-summary(anovaq)[[1]][4,2]
sqtotal<-summary(anovaq)[[1]][1,2]+summary(anovaq)[[1]][2,2] +
  summary(anovaq)[[1]][3,2] + summary(anovaq)[[1]][4,2]


R2<-1-(sqres/sqtotal)
R2

sqmodel<-summary(anovaq)[[1]][1,2]+summary(anovaq)[[1]][2,2] +
  summary(anovaq)[[1]][3,2] 

R2<-sqmodel/sqtotal


###########################################

TukeyHSD(anovaq,which = 'trat', ord=T)


###########################################
