### Questão 1 - VARIAVEL ALEATÓRIA CONTINUA

amMontecarloBeta <- c() 
amMontecarloBetaAlte <- c()
amMontecarloPonde  <- c()
### Final 0,333333


for  (i in 1:10000) {
  am2 <- rbeta(1,5,3)
  amMontecarloBeta <- c(amMontecarloBeta,am2)
  am3 <- (am2*dbeta(am2,2,4))/ dbeta(am2,5,3)
  amMontecarloPonde <- c(amMontecarloPonde, am3)
}

for (i in 1:10000){
  ams1 <- rexp(1)
  ams2 <- (dexp(ams1,0.5))
}

## Teorico - Beta(a,b) --- E(X) = a/a+b 
a = 
b = 4
Teorico = a/(a+b)

mean(amMontecarloBeta)
mean(amMontecarloPonde)


### Questão 3 - VARIAVEL ALEATÓRIA CONTINUA

## modo teorico
pnorm(4.5, low=F)

## Modelo computacional
amostras <- c()
for (i in 1:1000){
  amostra <- rnorm(1,0,1)
  amostras <- c(amostras, amostra)
}
sum(amostras==4.5)

## modelo Importance Sampling
amostranorm1 <- c()
amostrapnderadas<- c()

for  (i in 1:10000) {
  amx <- rexp(1,rate=1)+4.5
  amy <- (amx*dnorm(amx,0,1))/ (dexp(amx,rate=1)+4.5)
  amostrapnderadas <- c(amostrapnderadas, amy)
}
mean(amostrapnderadas)
