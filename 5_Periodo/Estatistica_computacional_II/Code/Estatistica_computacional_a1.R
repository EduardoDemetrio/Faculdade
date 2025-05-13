# Criando amostras
#x <-sort(rnorm(10, 60, 3))
#y <-sort(rnorm(10, 54, 5))
source("http://www.leg.ufpr.br/~paulojus/CE312/dados/mandible.R")
y <- mandible$female
x <- mandible$male


# Total e média
total <- c(x-y)
media <- mean(total)
resultados <- c()

# Cria um loop de 1000
for (i  in 1:10000){
  total2 <- c(x,y)
  z <- sample(total2)
  x2 <- sort(z[1:10])
  y2 <- sort(z[11:20])
  dif2 <- (x2-y2)
  media2 <- mean(dif2)
  resultados <- c(resultados,media2)}
  

hist(resultados)
abline(v = media, col = "red", lwd = 2)  

resultado_ord <- sort(resultados)

quantil <-quantile(resultado_ord, 0.90)

if (quantil <= media) {
  valor <- "Evidências significativas para rejeitar H0"
} else {
  valor <- "Não rejeita H0"
}
print(valor)
print("HO médias iguais") 
print("H1 média de male > famale ")



### Exemplo de pegar números pares
valor2 <-c()
for (num in x) {
  if (num %% 4 == 0) {
    valor2 <- c(valor2, num)
  }
}


x <-c(1,2,3,4,6)
x
y <-c(1,57,8,9,0)
z <- x&y
w <- x|y
x || y
