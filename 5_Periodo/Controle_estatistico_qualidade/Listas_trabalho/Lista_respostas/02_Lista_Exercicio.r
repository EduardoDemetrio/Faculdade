#-- Lista 2 - Controle estatístico de qualidade

## questão 1
diametros <- c(50.001, 50.002, 49.998, 50.006, 50.005, 49.996, 50.003, 50.004)
media <- mean(diametros); media
desvio <- sd(diametros); desvio

## questão 2
tempos <- c(127, 125, 131, 124, 129, 121, 142, 151, 160, 125,
            124, 123, 120, 119, 128, 133, 137, 124, 142, 123,
            121, 136, 140, 137, 125, 124, 128, 129, 130, 122,
            118, 131, 125, 133, 141, 125, 140, 132, 129, 126)
dois.a <- mean(tempos); dois.a
dois.a.desvio <- sd(tempos); dois.a.desvio


hist(tempos, 
     xlab = "Tempo de vida (horas)",
     ylab = "Frequência",
     col = "#3f2ebb")

median(tempos)
quantile(tempos, 0.25)
quantile(tempos, 0.75)


## questão 3
rendimento <- c(94.1, 87.3, 94.1, 92.4, 84.6, 85.4, 93.2, 84.1, 92.1, 90.6,
                83.6, 86.6, 90.6, 90.1, 96.4, 89.1, 85.4, 91.7, 91.4, 95.2,
                88.2, 88.8, 89.7, 87.5, 88.2, 86.1, 86.4, 86.4, 87.6, 84.2,
                86.1, 94.3, 85.0, 85.1, 85.1, 85.1, 95.1, 93.2, 84.9, 84.0,
                89.6, 90.5, 90.0, 86.7, 87.3, 93.7, 90.0, 95.6, 92.4, 83.0,
                89.6, 87.7, 90.1, 88.3, 87.3, 95.3, 90.3, 90.6, 94.3, 84.1,
                86.6, 94.1, 93.1, 89.4, 97.3, 83.7, 91.2, 97.8, 94.6, 88.6,
                96.8, 82.9, 86.1, 93.1, 96.3, 84.1, 94.4, 87.3, 90.4, 86.4,
                94.7, 82.6, 96.1, 86.4, 89.1, 87.6, 91.1, 83.1, 98.0, 84.5)

# Histograma com ajuste de bins
hist(rendimento,
     breaks = 10,  # Número de intervalos
     main = "Distribuição do Rendimento do Processo Químico",
     xlab = "Rendimento (%)",
     ylab = "Frequência",
     col = "#3f2ebb",
     border = "black",
     xlim = c(80, 100))

curve(dnorm(x, mean = mean(rendimento), sd = sd(rendimento)) * length(rendimento) * diff(hist(rendimento)$breaks)[1],
add = TRUE, col = "red", lwd = 2)
## questão 4
mean(rendimento)
sd(rendimento)


## questão 7
p_critico <- 0.015
p_critico_maior <- 0.002
p_critico_menor <- 0.008
p_todos <- 0.001

p_apenas_critico <- p_critico - p_critico_maior - p_critico_menor + p_todos ; p_apenas_critico

p_descarte <- p_apenas_critico + p_critico_maior + p_critico_menor + p_todos ; p_descarte

p_maior <- 0.01
p_menor <- 0.02
p_maior_menor <- 0.005

p_apenas_maior <- p_maior - p_critico_maior - p_maior_menor + p_todos
p_apenas_menor <- p_menor - p_critico_menor - p_maior_menor + p_todos

p_retrabalho <- p_apenas_maior + p_apenas_menor + p_maior_menor - p_todos ; p_retrabalho


## Questão 9

# Parâmetros
k <- 0.05
prob <- c((1 + 3*k)/3, (1 + 2*k)/3, (0.5 + 5*k)/3) ; prob
valores_x <- 1:3 ; valores_x

# Média e variância teóricas
media <- sum(valores_x * prob)
variancia <- sum(valores_x^2 * prob) - media^2

# FDA
fda <- cumsum(prob)

## Questão 11
prob_falha <- 1 - exp(-0.125 * 1)

lucro_sem_falha <- 25
lucro_com_falha <- -25
lucro_esperado <- lucro_sem_falha * (1 - prob_falha) + lucro_com_falha * prob_falha
cat("Lucro esperado por unidade: $", round(lucro_esperado, 2), "\n")


## Questão 12
# Dados
volumes <- c(10.05, 10.03, 10.02, 10.04, 10.05, 10.01, 10.02, 10.02, 10.03, 10.01)

# Medidas resumo
cat("Média:", mean(volumes), "\nMediana:", median(volumes), "\nModa:", names(sort(table(volumes), decreasing = TRUE))[1])
cat("\nDesvio-padrão:", round(sd(volumes), 4), "\nVariância:", round(var(volumes), 4), "\nAmplitude:", diff(range(volumes)))

# Boxplot
boxplot(volumes, horizontal = TRUE, col = "lightblue", xlab = "Volume (unidades)")

# Histograma
hist(volumes, breaks = 5, col = "orange", xlab = "Volume (unidades)", ylab = "Frequência")


## Questão 13

# Probabilidade P(X = 0)
prob_0 <- choose(5, 0) * choose(95, 10) / choose(100, 10)

# Probabilidade P(X = 1)
prob_1 <- choose(5, 1) * choose(95, 9) / choose(100, 10)

# Probabilidade total P(X <= 1)
prob_total <- prob_0 + prob_1

cat("Probabilidade de 0 defeituosos:", round(prob_0, 4), "\n",
    "Probabilidade de 1 defeituoso:", round(prob_1, 4), "\n",
    "Probabilidade total (no máximo 1 defeituoso):", round(prob_total, 4))


## Questão 15

# Parâmetros
media <- 40
desvio <- 2
limite <- 35

# Cálculo da probabilidade
prob <- 1 - pnorm(limite, media, desvio)
cat("Probabilidade (X ≥ 35):", round(prob * 100, 2), "%")

# Gráfico
curve(dnorm(x, media, desvio), from = 30, to = 50,
      xlab = "Resistência (X)", ylab = "Densidade")
abline(v = limite, col = "red", lty = 2)
polygon(c(limite, seq(limite, 50, length = 100), 50),
        c(0, dnorm(seq(limite, 50, length = 100), media, desvio), 0),
        col = "lightblue")
text(limite, 0.02, "X ≥ 35", pos = 4, col = "red")