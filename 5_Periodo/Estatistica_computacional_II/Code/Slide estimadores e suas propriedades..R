### Slides  -  Métodos de Monte Carlo para inferência estatística: estimadores e suas propriedades

## - Propriedade dos estimadores

#### - Estimadores bons, seguem:
- Não viesado
- Consistente
- Eficiente

### Viês 
## Geralmente temos um estimador não viesado quando a esperança do beta estimado é igual a 0
### E[beta estimado] = beta populacional

# Parâmetros verdadeiros
mu <- 5
sigma <- 2

# Configurações da simulação
n <- 30             # Tamanho da amostra
n_sim <- 10000      # Número de simulações

# Armazenar os estimadores
estimativas <- numeric(n_sim)

# Rodar a simulação de Monte Carlo
set.seed(123)  # Para reprodutibilidade
for (i in 1:n_sim) {
  amostra <- rnorm(n, mean = mu, sd = sigma)
  estimativas[i] <- mean(amostra)
}

# Calcular o valor esperado (média das estimativas)
media_estimada <- mean(estimativas)

# Calcular o viés estimado
viés_estimado <- media_estimada - mu

# Resultados
cat("Média estimada:", media_estimada, "\n")
cat("Viés estimado:", viés_estimado, "\n")


### Eficiência 
## Variancia do estimador1 / variancia do estimador2 


mu <- 5
sigma <- 2
n <- 30
n_sim <- 10000

# Armazenar estimativas
estimativas1 <- numeric(n_sim)  # var com (n - 1)
estimativas2 <- numeric(n_sim)  # var com n

for (i in 1:n_sim) {
  amostra <- rnorm(n, mean = mu, sd = sigma)
  
  # Estimador não-viesado (denominador n-1)
  estimativas1[i] <- var(amostra)
  
  # Estimador viesado (denominador n)
  media <- mean(amostra)
  estimativas2[i] <- mean((amostra - media)^2)  # denominador n
}

# Eficiência relativa: var2 vs var1
eficiencia_relativa <- var(estimativas2) / var(estimativas1)
eficiencia_relativa
sqrt(var(estimativas2))


###Erro padrão
sd(estimativas1)
sqrt(sum((tmean2 - m.tmean2)^2))/(N - 1)


### Variacia

n <- 20
N <- 1000
tmean2 <- numeric(N)
for (i in 1:N) {
  x <- sort(rnorm(n))
  tmean2[i] <- median(x)
}
(m.tmean2 <- mean(tmean2))
sum((tmean2 - m.tmean2)^2)/(N - 1)
var(tmean2)

sqrt(sum((tmean2 - m.tmean2)^2))/(N - 1)


sqrt(sum((tmean2 - m.tmean2)^2) / (N - 1))
sd(tmean2)
