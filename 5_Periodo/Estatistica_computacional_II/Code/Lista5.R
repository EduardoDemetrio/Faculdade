### Lista 5

d <- 2
l <- 1

distancia <- runif(1000,0,1)
theta <- runif(1000,0,pi/2)

p <- mean(distancia<=l/2*sin(theta))

num <- 2*(l/(d*p))

## - Lista 5

## - Histograma

dados <- rnorm(100,mean =20, sd = 2)

hist(dados, probability = TRUE, main = 'Histograma de dados normal', xlab = 'Valores', col = 'orange')

lines(density(dados), col ='black',lwd = 2)

curve(dnorm(x, mean = 20, sd = 2), add = TRUE, lty = 2, col = "red")

## - Empirica Vs Teorica
plot(ecdf(dados), 
     main = "CDF Empírica vs CDF Teórica", 
     xlab = "Valores", 
     ylab = "Probabilidade acumulada", 
     col = "blue", 
     lwd = 2)

## - Simule dados a de uma distribuição normal de média 50 para quil espera-se encontrar 10% dos valores acima de 55;

# Nesse caso temos que P(Y>55) = 0.10 Faço a alteração de que P(Y<=55)=0.9
## E com isso vamos normalizar a probabilidade P(Z<=55-u/desvio) = 0.9
### Temos P(Z<=5/desvio) = 0.9

x <- qnorm(0.90)  ## Nesse caso com intuito de capturar o x que contem a probabilidade 0.9 da normal
desvio <-  5/x
mean(dados1 <- rnorm(100, mean = 50, sd = desvio)>55)


## - Simule 100 dados de uma distribuição normal
### CV = desvio/mean - desvio = mean * 10%
mean = 50
x10 <- rnorm(100, mean, sd = mean*0.10) ; x10
x50 <- rnorm(100, mean, sd = mean*0.50) ; x50
x100 <- rnorm(100, mean, sd = mean*1) ; x100

plot(density(x100), col = 'red', lwd = 2, main = "Densidades com Diferentes CVs",
     xlab = "Valor", ylim = c(0, max(density(x10)$y, density(x50)$y, density(x100)$y)))

lines(density(x10), col ='black',lwd = 2)
lines(density(x50), col ='orange',lwd = 2)


### - 2 Simulando de modelos

#1. simular um conjunto de dados do modelo
n <- 50
x <- runif(n, 0, 10)             # valores de x no intervalo [0, 10]
beta0 <- 2                      # intercepto
beta1 <- 1.5                    # coeficiente angular
sigma <- 2                     #desvio padrao do erro
erro <- rnorm(n, mean = 0, sd = sigma)
y <- beta0 + beta1 * x + erro   # modelo gerador

plot(x, y, main = "Dados simulados",
     xlab = "x", ylab = "y", pch = 19, col = "blue")

abline(a = beta0, b = beta1, col = "red", lwd = 2, lty = 2)

#4. Ajustar modelo aos dados
modelo <- lm(y ~ x)
abline(modelo, col = "red", lwd = 2)  # reta ajustada

coef(modelo)  # intercepto e inclinação estimados

confint(modelo, level = 0.95)
