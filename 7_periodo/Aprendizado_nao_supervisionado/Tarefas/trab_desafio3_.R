# 1. Carga de Pacotes e Dados
library(SVclust)
library(ggplot2)

data(iris)
# Normalização (essencial para SVC)
df_scaled <- scale(iris[, 1:4])

# 2. Execução do K-means (Referência para o relatório)
set.seed(123)
km_res <- kmeans(df_scaled, centers = 3)
iris$kmeans_cluster <- as.factor(km_res$cluster)

# 3. Execução do SVC (Ajustado com kernel "gaussian")
# gamma: controla a sensibilidade do kernel
# scan_n: define a precisão da varredura de contorno
svc_res <- SVC(data = df_scaled, 
               gamma = 0.5, 
               scan_n = 10, 
               kernel = "gaussian") 

# Adicionando o resultado ao dataframe
iris$svc_cluster <- as.factor(svc_res$cluster)

# 4. Visualização Comparativa
p1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = kmeans_cluster)) +
  geom_point() + labs(title = "Agrupamento K-means") + theme_minimal()

p2 <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = svc_cluster)) +
  geom_point() + labs(title = "Agrupamento SVC") + theme_minimal()

print(p1)
print(p2)

# 5. Avaliação (Tabelas para o Relatório)
cat("\n--- Tabela: Espécies Reais vs K-means ---\n")
print(table(iris$Species, iris$kmeans_cluster))

cat("\n--- Tabela: Espécies Reais vs SVC ---\n")
print(table(iris$Species, iris$svc_cluster))
