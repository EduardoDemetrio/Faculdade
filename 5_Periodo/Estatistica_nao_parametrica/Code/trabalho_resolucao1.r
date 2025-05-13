amostras <- expand.grid(rep(list(c(0, 1)), 4)); amostras
colnames(amostras) <- paste0("X", 1:n) # Renomear colunas para clareza

media <- apply(amostras, 1, mean) ; media
variancia <- apply(amostras, 1, function(x) var(x)); variancia

resultado <- data.frame(amostras, media, variancia)

tabela_media <- table(resultado$media) / nrow(resultado)
cat("\nFunção de probabilidade da média amostral (X̄):\n")
print(tabela_media)


df2 <- data.frame(
  Valor_Xn = names(tabela_media),
  Probabilidade = as.numeric(tabela_media)
); df2

tabela_variancia <- table(round(resultado$variancia, 3)) / nrow(resultado)
cat("\nFunção de probabilidade da variância amostral (S²):\n")
print(tabela_variancia)

variance_pmf_df <- data.frame(
  Valor_Sn2 = names(tabela_variancia),
  Probabilidade = as.numeric(tabela_variancia)
); variance_pmf_df




variance_pmf_df$Valor_Sn2_Frac <- ifelse(variance_pmf_df$Valor_Sn2 == "0.25", "1/4",
ifelse(variance_pmf_df$Valor_Sn2 == '0.333', "1/3",
variance_pmf_df$Valor_Sn2))


cat("Soma das probabilidades de Xn:", sum(df2$Probabilidade), "\n")
cat("Soma das probabilidades de Sn2:", sum(variance_pmf_df$Probabilidade), "\n")


### Questão 2
# Definindo a função phi
phi <- function(X1, X2, Y1, Y2) {
  if (max(X1, X2) < min(Y1, Y2) || max(Y1, Y2) < min(X1, X2)) {
    return(1)
  } else {
    return(0)
  }
}

# Função para estimar delta
delta_est <- function(X, Y) {
  m <- length(X)
  n <- length(Y)
  u_sum <- 0

  for (i1 in 1:(m - 1)) {
    for (i2 in (i1 + 1):m) {
      for (k1 in 1:(n - 1)) {
        for (k2 in (k1 + 1):n) {
          u_sum <- u_sum + phi(X[i1], X[i2], Y[k1], Y[k2])
        }
      }
    }
  }

  u_stat <- (1 / (choose(m, 2) * choose(n, 2))) * u_sum
  delta <- (1 / 2) * u_stat - 1 / 6
  return(delta)
}

# -------------------------------
# (b) Gerar amostras de referência F
set.seed(123)

tamanhos <- c(50, 100, 150)

# Lendo a amostra da distribuição G
G <- read.csv("C:/Users/edude/OneDrive/Área de Trabalho/Códigos/Faculdade/Estatistica_nao_parametrica/Code/Dados/dados.csv", header = FALSE)[[1]]

# Lista para armazenar resultados
resultados <- list()

for (n in tamanhos) {
  X_normal <- rnorm(n)
  X_cauchy <- rcauchy(n)
  X_t4 <- rt(n, df = 4)

  delta_normal <- delta_est(X_normal, G)
  delta_cauchy <- delta_est(X_cauchy, G)
  delta_t <- delta_est(X_t4, G)

  resultados[[paste0("n=", n)]] <- data.frame(
    Distribuicao = c("Normal", "Cauchy", "t-Student(4)"),
    Delta = c(delta_normal, delta_cauchy, delta_t)
  )
}

# (c) Mostrar os resultados
for (nome in names(resultados)) {
  cat("\n--- Resultados para", nome, "---\n")
  print(resultados[[nome]])
}

# (d) Análise final:
cat("\n--- Análise final ---\n")
cat("A distribuição F que apresentar o menor delta (mais próximo de 0) indica maior semelhança com a distribuição G.\n")


## Questão 3

n <- length(G)
set.seed(123)  # Reprodutibilidade
X <- rnorm(n)  # Referência F ~ Normal(0, 1)
Y <- G     

## A
est_esperanca_XY <- function(X, Y) {
  mean(X) * mean(Y)  # Porque são independentes!
}

## B
est_var_XmaisY <- function(X, Y) {
  var(X) + var(Y)  # var() em R já retorna o estimador não-viciado
}

# Aplicando
esperanca_xy <- est_esperanca_XY(X, Y) ; esperanca_xy 
variancia_xmaisy <- est_var_XmaisY(X, Y) ; variancia_xmaisy


# Lendo os dados
# Carrega os dados
phipsi <- read.csv("http://leg.ufpr.br/~lucambio/Nonparam/phipsi.csv", sep = ",", header = TRUE)
psi <- phipsi$psi

# Função para gerar novas amostras via KDE com kernel Gaussiano
gerar_amostra_kernel <- function(dados, N, h) {
  n <- length(dados)
  nova_amostra <- numeric(N)
  for (i in 1:N) {
    indice <- sample(1:n, 1)  # Escolhe aleatoriamente um ponto da amostra
    nova_amostra[i] <- rnorm(1, mean = dados[indice], sd = h)
  }
  return(nova_amostra)
}

# Calcula a largura de banda via regra de Silverman
h_silverman <- 1.06 * sd(psi) * length(psi)^(-1/5)

# Gera nova amostra com N = 600 usando o bandwidth da regra de Silverman
set.seed(42)  # Reprodutibilidade
N <- 600
nova_amostra_silverman <- gerar_amostra_kernel(psi, N, h_silverman)

# Gera nova amostra com N = 600 usando o bandwidth manual (h_custom)
h_custom <- 0.5  # Exemplo de valor manual para h
nova_amostra_custom <- gerar_amostra_kernel(psi, N, h_custom)

# Estima as densidades
dens_original <- density(psi)
dens_gerada_silverman <- density(nova_amostra_silverman)
dens_gerada_custom <- density(nova_amostra_custom)

# Plota a comparação
plot(dens_original,
     main = "Densidade Estimada: Original vs Gerada (Silverman e Custom)",
     col = "#0072B2", lwd = 2, xlab = "Valores de ψ", ylab = "Densidade", ylim = range(0, max(dens_original$y, dens_gerada_silverman$y, dens_gerada_custom$y)))

# Adiciona as linhas para a densidade gerada com Silverman e customizada
lines(dens_gerada_silverman, col = "darkgreen", lwd = 2, lty = 2)
lines(dens_gerada_custom, col = "orange", lwd = 2, lty = 3)

# Legenda para o gráfico
legend("topright",
       legend = c("Original (psi)", "Gerada via KDE (Silverman)", "Gerada via KDE (Custom)"),
       col = c("#0072B2", "darkgreen", "orange"),
       lwd = 2,
       lty = c(1, 2, 3))


------------- joao

phipsi <- read.csv("http://leg.ufpr.br/~lucambio/Nonparam/phipsi.csv", sep = ",", header = TRUE)
psi <- phipsi$psi

# Definir a função para gerar amostras a partir da densidade kernel estimada
gerar_amostra_kernel <- function(dados, N, h) {
  n <- length(dados)
  amostra <- numeric(N)
  for (i in 1:N) {
    # Escolher um índice aleatório
    indice <- sample(1:n, 1)
    # Gerar uma amostra da densidade gaussiana
    amostra[i] <- rnorm(1, mean = dados[indice], sd = h)
  }
  return(amostra)
}

# Regra de Silverman para escolher a largura de banda
h_silverman <- 1.06 * sd(psi) * length(psi)^(-1/5)

# Gerar uma amostra de tamanho N = 600
N <- 600
amostra_nova <- gerar_amostra_kernel(psi, N, h_silverman)
densidade_original <- density(psi)
densidade_nova <- density(amostra_nova)

# Plotar as densidades
plot(densidade_original, main = "Comparação das Densidades Estimadas", col = "blue", xlab = "Valores")
lines(densidade_nova, col = "red")
legend("topright", legend = c("Original (psi)", "Nova Amostra"), col = c("blue", "red"), lty = 1)