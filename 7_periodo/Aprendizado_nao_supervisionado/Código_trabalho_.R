
# 1. Criação da matriz de utilidade (usuários x filmes)
# Linhas: usuários (1 a 5)
# Colunas: filmes ("FilmeA" a "FilmeE")
# Notas de 1 a 5 (0 = não avaliado)
utilidade <- matrix(c(
  5, 3, 0, 1, 4,
  4, 0, 0, 1, 5,
  1, 1, 0, 5, 0,
  0, 2, 4, 0, 3,
  0, 4, 5, 0, 0
), nrow = 5, byrow = TRUE)
rownames(utilidade) <- paste0("Usuario", 1:5)
colnames(utilidade) <- c("FilmeA", "FilmeB", "FilmeC", "FilmeD", "FilmeE")

cat("Matriz de Utilidade (notas de 1 a 5, 0 = não avaliado):\n")
print(utilidade)

# 2. Função para calcular similaridade cosseno entre dois vetores (ignorando zeros)
# Fórmula: cos(A,B) = (A·B) / (||A|| * ||B||)
cosine_similarity <- function(a, b) {
  # Considera apenas índices onde ambos os vetores têm nota > 0
  common <- which(a > 0 & b > 0)
  if (length(common) == 0) return(0)  # sem itens em comum
  a_common <- a[common]
  b_common <- b[common]
  numerador <- sum(a_common * b_common)
  denominador <- sqrt(sum(a_common^2)) * sqrt(sum(b_common^2))
  if (denominador == 0) return(0)
  return(numerador / denominador)
}

# 3. Matriz de similaridade entre usuários (5x5)
n_usuarios <- nrow(utilidade)
sim_matrix <- matrix(0, n_usuarios, n_usuarios)
rownames(sim_matrix) <- rownames(utilidade)
colnames(sim_matrix) <- rownames(utilidade)

for (i in 1:n_usuarios) {
  for (j in 1:n_usuarios) {
    sim_matrix[i, j] <- cosine_similarity(utilidade[i, ], utilidade[j, ])
  }
}

cat("\nMatriz de Similaridade entre Usuários (cosseno):\n")
print(round(sim_matrix, 3))

# 4. Predição de nota para um usuário-alvo e um filme-alvo
# Exemplo: Usuario1 queremos predizer nota para FilmeC (índice 3)
target_user <- 1
target_item <- 3  # FilmeC (coluna 3)

# Obtém similaridades do usuário alvo com todos os outros
sim_target <- sim_matrix[target_user, ]

# Seleciona outros usuários que avaliaram o item alvo
# e têm similaridade positiva
avaliaram <- which(utilidade[, target_item] > 0)
# Remove o próprio usuário (se tivesse avaliado, mas aqui não)
avaliaram <- setdiff(avaliaram, target_user)

if (length(avaliaram) > 0) {
  # Pega as similaridades apenas desses usuários
  sim_relevantes <- sim_target[avaliaram]
  # Pega as notas desses usuários para o item alvo
  notas_relevantes <- utilidade[avaliaram, target_item]
  
  # Predição: média ponderada pelas similaridades
  # Só considera similaridades > 0
  pesos_positivos <- which(sim_relevantes > 0)
  if (length(pesos_positivos) > 0) {
    numerador <- sum(sim_relevantes[pesos_positivos] * notas_relevantes[pesos_positivos])
    denominador <- sum(sim_relevantes[pesos_positivos])
    nota_predita <- numerador / denominador
  } else {
    nota_predita <- mean(utilidade[target_user, which(utilidade[target_user,] > 0)])
  }
} else {
  # Se ninguém avaliou, usa a média do usuário
  nota_predita <- mean(utilidade[target_user, which(utilidade[target_user,] > 0)])
}

cat(sprintf("\nPredição de nota do %s para %s: %.2f\n", 
            rownames(utilidade)[target_user], 
            colnames(utilidade)[target_item], 
            nota_predita))

# 5. Recomendação: Top-N itens não avaliados para um usuário
recomendar <- function(usu_idx, n = 2) {
  # Itens que o usuário ainda não avaliou (nota == 0)
  itens_nao_avaliados <- which(utilidade[usu_idx, ] == 0)
  if (length(itens_nao_avaliados) == 0) {
    cat("Usuário já avaliou todos os itens.\n")
    return(NULL)
  }
  
  # Vetor para armazenar as predições
  predicoes <- numeric(length(itens_nao_avaliados))
  names(predicoes) <- colnames(utilidade)[itens_nao_avaliados]
  
  for (i in seq_along(itens_nao_avaliados)) {
    item <- itens_nao_avaliados[i]
    # Mesmo procedimento da predição
    sim_target <- sim_matrix[usu_idx, ]
    avaliaram <- which(utilidade[, item] > 0)
    avaliaram <- setdiff(avaliaram, usu_idx)
    if (length(avaliaram) > 0) {
      sim_relevantes <- sim_target[avaliaram]
      notas_relevantes <- utilidade[avaliaram, item]
      pesos_positivos <- which(sim_relevantes > 0)
      if (length(pesos_positivos) > 0) {
        nota <- sum(sim_relevantes[pesos_positivos] * notas_relevantes[pesos_positivos]) / sum(sim_relevantes[pesos_positivos])
      } else {
        nota <- mean(utilidade[usu_idx, which(utilidade[usu_idx,] > 0)])
      }
    } else {
      nota <- mean(utilidade[usu_idx, which(utilidade[usu_idx,] > 0)])
    }
    predicoes[i] <- nota
  }
  
  # Ordena as predições em ordem decrescente e seleciona top-n
  top_itens <- sort(predicoes, decreasing = TRUE)[1:min(n, length(predicoes))]
  return(top_itens)
}

cat("\nRecomendações para Usuario1 (Top-2 filmes não avaliados):\n")
recomendacoes <- recomendar(1, 2)
print(recomendacoes)
