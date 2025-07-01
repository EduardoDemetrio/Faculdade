obs<-c(28,25,27,36,32,32,18,19,23,31,30,29)

n<-3

fatA<-factor(rep(rep(c("15","25"),each=n),2))
fatB<-factor(c(rep(c("1"),6),rep(c("2"),6)))

dados<-data.frame(obs,fatA,fatB)
dados

anova<-aov(obs ~ fatA + fatB + fatA:fatB )
summary(anova)


x1<-rep(rep(c(-1,1),each=3),2)
x1

x2<-rep(c(-1,1),each=6)
x2

modreg<-lm(obs ~ x1*x2)
summary(modreg)
