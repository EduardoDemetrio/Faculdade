
pacotes <- c("cluster", "ggplot2", "dplyr", "tidyr", "mclust", "dbscan", "patchwork")
instalar <- pacotes[!pacotes %in% installed.packages()[, "Package"]]
if(length(instalar) > 0) install.packages(instalar)

library(cluster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(mclust)
library(dbscan)
library(patchwork)

set.seed(123)

caminho <- "C:/Users/edude/OneDrive/Área de Trabalho/Códigos/Faculdade/7_periodo/Aprendizado_nao_supervisionado/Prova_Hands_on/hands_on2/seeds_dataset.txt"
seeds <- read.table(
  caminho,
  header = FALSE,
  sep = "",
  stringsAsFactors = FALSE
)

colnames(seeds) <- c(
  "area", "perimetro", "compacidade", "comprimento_grao", "largura_grao",
  "coef_assimetria", "comprimento_sulco", "classe"
)

# Conferência básica
print(dim(seeds))
print(table(seeds$classe))


X <- seeds[, 1:7]
classe_real <- factor(seeds$classe,
                      levels = c(1, 2, 3),
                      labels = c("Kama", "Rosa", "Canadian"))

# 2) Padronização --------------------------------------------
# Necessária porque as variáveis têm escalas diferentes.
X_pad <- scale(X)
dist_euc <- dist(X_pad, method = "euclidean")

# 3) Funções auxiliares de validação -------------------------
rand_index <- function(real, pred) {
  tab <- table(real, pred)
  n <- sum(tab)
  total_pares <- choose(n, 2)
  a <- sum(choose(tab, 2))
  linha <- sum(choose(rowSums(tab), 2))
  coluna <- sum(choose(colSums(tab), 2))
  b <- total_pares - linha - coluna + a
  (a + b) / total_pares
}

sil_score <- function(labels, dados, distancias = NULL) {
  labels <- as.factor(labels)
  if(length(unique(labels)) < 2 || length(unique(labels)) >= length(labels)) return(NA_real_)
  if(any(table(labels) == 1)) return(NA_real_)
  if(is.null(distancias)) distancias <- dist(dados)
  mean(cluster::silhouette(as.integer(labels), distancias)[, 3])
}

calinski_harabasz <- function(dados, labels) {
  labels <- as.factor(labels)
  k <- length(unique(labels))
  n <- nrow(dados)
  if(k < 2 || k >= n) return(NA_real_)

  centro_global <- colMeans(dados)
  W <- 0
  B <- 0

  for(g in levels(labels)) {
    Xg <- dados[labels == g, , drop = FALSE]
    centro_g <- colMeans(Xg)
    W <- W + sum(rowSums((sweep(Xg, 2, centro_g))^2))
    B <- B + nrow(Xg) * sum((centro_g - centro_global)^2)
  }

  (B / (k - 1)) / (W / (n - k))
}

davies_bouldin <- function(dados, labels) {
  labels <- as.factor(labels)
  grupos <- levels(labels)
  k <- length(grupos)
  if(k < 2) return(NA_real_)

  centroides <- sapply(grupos, function(g) colMeans(dados[labels == g, , drop = FALSE]))
  centroides <- t(centroides)

  S <- sapply(seq_along(grupos), function(i) {
    Xg <- dados[labels == grupos[i], , drop = FALSE]
    mean(sqrt(rowSums((sweep(Xg, 2, centroides[i, ]))^2)))
  })

  M <- as.matrix(dist(centroides))
  R <- matrix(NA_real_, nrow = k, ncol = k)
  for(i in 1:k) {
    for(j in 1:k) {
      if(i != j) R[i, j] <- (S[i] + S[j]) / M[i, j]
    }
  }
  mean(apply(R, 1, max, na.rm = TRUE))
}

calcular_metricas <- function(nome, labels, dados = X_pad, distancias = dist_euc, real = classe_real) {
  labels_original <- labels

  # Para DBSCAN, o pacote usa 0 para ruído. Para métricas internas, calculamos também sem ruído.
  sem_ruido <- labels_original != 0
  labels_sem_ruido <- labels_original[sem_ruido]
  dados_sem_ruido <- dados[sem_ruido, , drop = FALSE]

  data.frame(
    Metodo = nome,
    Clusters = length(setdiff(unique(labels_original), 0)),
    Ruidos = sum(labels_original == 0),
    Silhouette = sil_score(labels_original, dados, distancias),
    Silhouette_sem_ruido = sil_score(labels_sem_ruido, dados_sem_ruido),
    Calinski_Harabasz = calinski_harabasz(dados_sem_ruido, labels_sem_ruido),
    Davies_Bouldin = davies_bouldin(dados_sem_ruido, labels_sem_ruido),
    ARI = mclust::adjustedRandIndex(labels_original, real),
    Rand = rand_index(real, labels_original)
  )
}

# 4) Modelos de clusterização --------------------------------

# 4.1 K-médias
km <- kmeans(X_pad, centers = 3, nstart = 50)
cluster_km <- km$cluster

# 4.2 Agrupamento Hierárquico com Ward
hc <- hclust(dist_euc, method = "ward.D2")
cluster_ward <- cutree(hc, k = 3)
 
minPts_db <- 14
eps_db <- 1.00
db <- dbscan::dbscan(X_pad, eps = eps_db, minPts = minPts_db)
cluster_db <- db$cluster

 grade_db <- expand.grid(
  eps = seq(0.70, 1.40, by = 0.02),
  minPts = c(7, 10, 14)
)

grade_db$resultado <- apply(grade_db, 1, function(par) {
  ajuste <- dbscan::dbscan(X_pad, eps = as.numeric(par["eps"]), minPts = as.numeric(par["minPts"]))
  labels <- ajuste$cluster
  n_cl <- length(setdiff(unique(labels), 0))
  ruido <- mean(labels == 0)
  sil <- sil_score(labels, X_pad, dist_euc)
  paste(n_cl, round(ruido, 3), round(sil, 3), sep = ";")
})

write.csv(grade_db, "grade_parametros_dbscan.csv", row.names = FALSE)

# 4.4 Mistura de Gaussianas (GMM)
# G = 3 para comparar com as três variedades conhecidas do conjunto Seeds.
# O modelo EEE usa matriz de covariância comum entre os componentes.
gmm <- mclust::Mclust(X_pad, G = 3, modelNames = "EEE")
cluster_gmm <- gmm$classification

# Se quiser verificar o modelo selecionado automaticamente pelo BIC, rode também:
gmm_bic <- mclust::Mclust(X_pad, G = 1:6)
print(summary(gmm_bic))

# 5) Métricas -------------------------------------------------
metricas <- bind_rows(
  calcular_metricas("K-médias", cluster_km),
  calcular_metricas("Hierárquico Ward", cluster_ward),
  calcular_metricas("DBSCAN", cluster_db),
  calcular_metricas("GMM", cluster_gmm)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(metricas)
write.csv(metricas, "metricas_clusterizacao_seeds.csv", row.names = FALSE)

# Matrizes de contingência: classe real x cluster obtido
cat("\nK-médias\n")
print(table(Classe_real = classe_real, Cluster = cluster_km))

cat("\nWard\n")
print(table(Classe_real = classe_real, Cluster = cluster_ward))

cat("\nDBSCAN\n")
print(table(Classe_real = classe_real, Cluster = cluster_db))

cat("\nGMM\n")
print(table(Classe_real = classe_real, Cluster = cluster_gmm))

# 6) Visualização em 2D via PCA ------------------------------
pca <- prcomp(X_pad, center = TRUE, scale. = FALSE)
var_exp <- round(100 * summary(pca)$importance[2, 1:2], 2)

pca_df <- data.frame(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  Classe_real = classe_real,
  Kmedias = factor(cluster_km),
  Ward = factor(cluster_ward),
  DBSCAN = factor(cluster_db),
  GMM = factor(cluster_gmm)
)

plot_cluster <- function(df, coluna, titulo) {
  ggplot(df, aes(x = PC1, y = PC2, color = .data[[coluna]], shape = Classe_real)) +
    geom_point(size = 2.4, alpha = 0.85) +
    labs(
      title = titulo,
      x = paste0("PC1 (", var_exp[1], "% da variância)"),
      y = paste0("PC2 (", var_exp[2], "% da variância)"),
      color = "Cluster",
      shape = "Classe real"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
}

p1 <- plot_cluster(pca_df, "Kmedias", "K-médias")
p2 <- plot_cluster(pca_df, "Ward", "Hierárquico - Ward")
p3 <- plot_cluster(pca_df, "DBSCAN", paste0("DBSCAN (eps=", eps_db, ", minPts=", minPts_db, ")"))
p4 <- plot_cluster(pca_df, "GMM", "Mistura de Gaussianas")

figura_modelos <- (p1 | p2) / (p3 | p4)
print(figura_modelos)
ggsave("clusters_pca_seeds.png", figura_modelos, width = 12, height = 9, dpi = 300)

# Figura das classes reais no PCA
fig_classe <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Classe_real)) +
  geom_point(size = 2.4, alpha = 0.85) +
  labs(
    title = "Classes reais no plano dos dois primeiros componentes principais",
    x = paste0("PC1 (", var_exp[1], "% da variância)"),
    y = paste0("PC2 (", var_exp[2], "% da variância)"),
    color = "Variedade"
  ) +
  theme_minimal(base_size = 11)

print(fig_classe)
ggsave("classes_reais_pca_seeds.png", fig_classe, width = 8, height = 6, dpi = 300)

# Dendrograma do método Ward
png("dendrograma_ward_seeds.png", width = 1200, height = 800, res = 120)
plot(hc, labels = FALSE, hang = -1, main = "Dendrograma - Método de Ward", xlab = "Observações", ylab = "Altura")
rect.hclust(hc, k = 3, border = 2:4)
dev.off()

# Curva kNN para auxiliar escolha do eps no DBSCAN
png("knn_dbscan_seeds.png", width = 1000, height = 700, res = 120)
dbscan::kNNdistplot(X_pad, k = minPts_db)
abline(h = eps_db, col = "red", lty = 2)
title(main = paste0("kNN distance plot - minPts = ", minPts_db))
dev.off()

# 7) Informação extra para o relatório -----------------------
cat("\nVariância explicada pelos dois primeiros PCs:\n")
print(var_exp)

cat("\nArquivos gerados:\n")
cat("- metricas_clusterizacao_seeds.csv\n")
cat("- clusters_pca_seeds.png\n")
cat("- classes_reais_pca_seeds.png\n")
cat("- dendrograma_ward_seeds.png\n")
cat("- knn_dbscan_seeds.png\n")
cat("- grade_parametros_dbscan.csv\n")

