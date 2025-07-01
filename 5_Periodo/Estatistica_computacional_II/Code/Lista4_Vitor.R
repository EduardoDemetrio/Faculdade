### lista 4

#1.1 estimação da area do circulo unitario (pi)
dentro <- 0
n <- 10000
for (i in 1:n) {
  x <- runif(1, -1, 1)
  y <- runif(1, -1, 1)
  
  #distancia a partir de pitagoras
  dist <- sqrt(x^2 + y^2)
  if(dist <= 1){
    dentro <- dentro + 1
  }
}

#P(ponto aleatorio dentro do circulo) = area circulo/area quadrado = pi/4
probabilidade <- dentro/n

probabilidade4

#versão vetorizada mais eficiente
n <- 10000
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)

distancias <- sqrt(x^2 + y^2)
piAprox <- 4 mean(distancias <= 1)
piAprox

#exercicio  agulha de buffon---------------------------------------------------

d <- 2 #largura da tábua
l <- 1 #comprimento da agulha

#razao <- runif(10000, 0, 1)

distancia <- runif(10000, 0, 1) #distancia entre o centro da agulha e a linha mais prox
theta <- runif(10000, 0, pi/2) #inclinação da agulha

#temos que para a agulha interceptar a linha a distancia(y) do ponto médio da 
#agulha até a divisão entre as tábuas deve ser maior que a distância vertical da 
#ponta da agulha até a linha mais próxima

p <- mean(distancia <= l/2sin(theta))
### Lista 4

num1 <- 2(l/(dp))
cat(num1)

#r = l/d 


set.seed(123)

buffon_sim <- function(l, d = 1, n = 10000) {
  distancia <- runif(n, 0, d/2)
  theta <- runif(n, 0, pi/2)
  p <- mean(distancia <= l/2 sin(theta))
  if (p == 0) return(NA)  # evitar divisão por zero
  pi_est <- 2 * l / (d * p)
  return(pi_est)
}

Testar diferentes valores de l/d,
l_values <- seq(0.1, 1, by = 0.1)
pi_estimates <- sapply(l_values, function(l) buffon_sim(l = l, d = 1))

Plotar,
plot(l_values, pi_estimates, type = "b", pch = 19,
     main = "Estimativa de π pela Agulha de Buffon",
     xlab = "Razão l/d (com d = 1)", ylab = "Estimativa de π")
abline(h = pi, col = "red", lty = 2)


mean(pi_estimates)