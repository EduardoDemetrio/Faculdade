'Questão 1 - A'

dados <- read.csv2("http://leg.ufpr.br/~lucambio/CE313/20241S/DietDepression.csv")
head(dados, 5)

shapiro.test(dados$CESDDiff)
shapiro.test(dados$DASSDiff)

wilcox.test(dados$CESDDiff, dados$DASSDiff, paired = TRUE)

with(subset(dados, Group == "Control"), wilcox.test(CESDDiff, DASSDiff, paired = TRUE))
with(subset(dados, Group == "Diet"), wilcox.test(CESDDiff, DASSDiff, paired = TRUE))


boxplot(CESDDiff ~ Group, data = dados, main = "CESD Diff por Grupo")
boxplot(DASSDiff ~ Group, data = dados, main = "DASS Diff por Grupo")

compare_by_group <- function(group_name) {
  grupo <- dados[dados$Group == group_name,]
  
  cat("========================================\n")
  cat(paste("Grupo:", group_name, "\n"))
  cat(">> Comparando CESD1 vs DASS1 (nível inicial):\n")
  print(wilcox.test(grupo$CESD1, grupo$DASS1, paired = TRUE))
  cat(">> Comparando CESDDiff vs DASSDiff (diferença ao final):\n")
  print(wilcox.test(grupo$CESDDiff, grupo$DASSDiff, paired = TRUE))
  
  cat("\n")
}
# Aplicando a função nos dois grupos
compare_by_group("Control")
compare_by_group("Diet")

'Questão 1 - B'
wilcox.test(dados$BMIDiff, mu = 0)

wilcox.test(BMIDiff ~ 1, data = subset(dados, Group == "Control"), mu = 0)
wilcox.test(BMIDiff ~ 1, data = subset(dados, Group == "Diet"), mu = 0)

wilcox.test(dados$BMI1 ~ dados$Group)
wilcox.test(dados$BMI21 ~ dados$Group)
wilcox.test(BMIDiff ~ Group, data = dados)


'Questão 2 - A'

beans <- read.csv("http://leg.ufpr.br/~lucambio/CE313/20241S/Beans_Dataset.csv")

head(beans,5)

# Histograma e QQ‐plot (global)
hist(beans$Perimeter, breaks=50,
     main="Histograma de Perimeter (global)",
     xlab="Perimeter")
qqnorm(beans$Perimeter, main="QQ‐Plot de Perimeter (global)")
qqline(beans$Perimeter)
 

'Questão 2 - B'

options(warn=-1) # Suprime todos os warnings
B <- 10000
sample_size <- 100
p_values <- numeric(B)

for (i in 1:B) {
  sub_sample <- sample(beans$Perimeter, sample_size, replace = TRUE)
  ks_test <- ks.test(sub_sample, "pnorm", mean = mean(sub_sample), sd = sd(sub_sample))
  p_values[i] <- ks_test$p.value
}

# Contar o número de amostras não conformes com a distribuição normal (p < 0.05)
non_conforming <- sum(p_values < 0.05)
cat(paste("Número de sub-amostras não conformes:", non_conforming, "\n"))

# Avaliar a proporção de amostras não conformes
proportion_non_conforming <- non_conforming / B
cat(paste("Proporção de sub-amostras não conformes:", proportion_non_conforming, "\n"))

# Função para padronizar os dados
scale_data <- function(x) {
  (x - mean(x)) / sd(x)
}

# Padronizar os dados
z <- scale_data(beans$Perimeter)

# Função para calcular a função característica empírica
Recf <- function(t, x) {
  n <- length(x)
  sum(exp(1i * t * x)) / n
}

# Avaliar a função característica empírica
phi_S <- Recf(1, z)

# Calcular a estatística de teste
nu_n <- log(abs(phi_S / exp(-1/2)))

# Calcular o valor do teste
test_statistic <- abs(4.8158 * sqrt(length(z)) * nu_n)

# Definir o nível de significância
alpha <- 0.05

# Calcular o valor crítico
z_critical <- qnorm(1 - alpha/2)

# Verificar se rejeitamos a normalidade
if (test_statistic > z_critical) {
  cat("Rejeitamos a normalidade com base no teste para amostras grandes.\n")
} else {
  cat("Não rejeitamos a normalidade com base no teste para amostras grandes.\n")
}


'Questão 3'
# 1. Carregar pacotes
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(mixtools)) install.packages("mixtools"); library(mixtools)
if (!require(dplyr))   install.packages("dplyr");   library(dplyr)

# 2. Leitura dos dados
altimetria <- read.csv(
  "http://leg.ufpr.br/~lucambio/CE313/20241S/Altimetria.csv",
  sep = ";", header = TRUE
)

# 3. Construir vetor de altitudes ponderado pela frequência (máx. 10 000 pontos)
set.seed(123)
total_freq <- sum(altimetria$Freq)
n_sample   <- min(total_freq, 10000)
altitudes  <- sample(
  altimetria$Altitude,
  size    = n_sample,
  replace = TRUE,
  prob    = altimetria$Freq
)

# 4. Análise exploratória: histograma + curva de densidade
df_alt <- data.frame(Altitude = altitudes)
ggplot(df_alt, aes(x = Altitude)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightgray") +
  geom_density() +
  labs(
    title = "Distribuição de Altitudes (amostragem ponderada)",
    x     = "Altitude (m)",
    y     = "Densidade"
  )

# 5. Ajuste de mistura de 2 distribuições normais
mix2 <- normalmixEM(
  altitudes,
  k       = 2,
  maxit   = 1000,
  epsilon = 1e-8,
  verb    = FALSE
)

# 6. Parâmetros estimados
params <- tibble::tibble(
  Componente = factor(1:2),
  Media      = mix2$mu,
  Desvio     = mix2$sigma,
  Proporcao  = mix2$lambda
)
print(params)

# 7. Gráfico com as curvas de densidade das componentes sobre o histograma
ggplot(df_alt, aes(x = Altitude)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightgray") +
  stat_function(fun = function(x) mix2$lambda[1] * dnorm(x, mix2$mu[1], mix2$sigma[1])) +
  stat_function(fun = function(x) mix2$lambda[2] * dnorm(x, mix2$mu[2], mix2$sigma[2])) +
  labs(
    title = "Mistura de 2 Gaussianas Ajustada",
    x     = "Altitude (m)",
    y     = "Densidade"
  )

# 8. Classificação por componente e resumo
df_alt <- df_alt %>%
  mutate(Cluster = factor(apply(mix2$posterior, 1, which.max)))

df_alt %>%
  group_by(Cluster) %>%
  summarise(
    Mediana = median(Altitude),
    IQR     = IQR(Altitude),
    N       = n()
  )


'Questão 4'
# 1. Parâmetros
n     <- 50       # tamanho da amostra
theta <- 1        # valor de θ
B     <- 20000    # número de réplicas bootstrap
set.seed(123)     # reprodutibilidade

# 2. Amostra original e pivot
X   <- runif(n, 0, theta)
X_n <- max(X)
xi_n <- function(X_max, theta, n) {
  n * (theta - X_max) / theta
}

# 3. Bootstrap vetorizado de ξₙ*
X_star_max <- replicate(
  B,
  max(sample(X, n, replace = TRUE))
)
xi_star <- xi_n(X_star_max, theta, n)

# 4. Teste de Kolmogorov–Smirnov contra Exp(1)
ks_res <- ks.test(xi_star, "pexp", rate = 1)
print(ks_res)

# 5. Visualização: histograma + densidade teórica
hist(xi_star,
     freq   = FALSE,
     breaks = 50,
     main   = expression("Distribuição Bootstrap de " * xi^"*"[n] * " vs Exp(1)"),
     xlab   = expression(xi^"*"[n]))
curve(dexp(x, rate = 1),
      from = 0, to = max(xi_star),
      col  = "red",
      lwd  = 2,
      add  = TRUE)
legend("topright",
       legend = c("Bootstrap", "Exp(1)"),
       col    = c("black", "red"),
       lty    = 1)


'Questão 5'# 1. Carregar dados e inspecionar estrutura
data.pizza <- read.csv(
  "http://leg.ufpr.br/~lucambio/CE313/20241S/Data-pizza.csv",
  sep = ",", header = TRUE
)
str(data.pizza)

# 2. Criar indicadores de NA
data.pizza$na_temp <- ifelse(is.na(data.pizza$temperature), 1, 0)
data.pizza$na_qual <- ifelse(is.na(data.pizza$quality),     1, 0)

# 3. Análise global de missingness
library(dplyr)
data.pizza %>% 
  summarize(
    prop_na_temp = mean(na_temp),
    prop_na_qual = mean(na_qual)
  ) %>% 
  print()

# teste de independência entre os dois indicadores (global)
chi_global <- chisq.test(
  table(data.pizza$na_temp, data.pizza$na_qual)
)
print(chi_global)

# 4. Missingness por região (area)
data.pizza %>% 
  group_by(area) %>% 
  summarize(
    prop_na_temp = mean(na_temp),
    prop_na_qual = mean(na_qual),
    n            = n()
  ) %>% 
  print()

# teste qui-quadrado para cada variável vs. area
for(var in c("na_temp","na_qual")){
  cat("\n--- Teste por area para", var, "---\n")
  tbl  <- table(data.pizza$area, data.pizza[[var]])
  print(chisq.test(tbl))
}

# 5. Missingness por operador
data.pizza %>% 
  group_by(operator) %>% 
  summarize(
    prop_na_temp = mean(na_temp),
    prop_na_qual = mean(na_qual),
    n            = n()
  ) %>% 
  print()

# teste qui-quadrado para cada variável vs. operator
for(var in c("na_temp","na_qual")){
  cat("\n--- Teste por operator para", var, "---\n")
  tbl  <- table(data.pizza$operator, data.pizza[[var]])
  print(chisq.test(tbl))
}
