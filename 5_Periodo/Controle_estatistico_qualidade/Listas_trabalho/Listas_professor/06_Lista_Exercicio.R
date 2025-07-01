
# Parâmetros
mu <- 600
sigma <- 12
n <- 9
alpha <- 0.01

# Cálculo do erro padrão da média
sigma_xbar <- sigma / sqrt(n)

# Valor crítico da normal padrão (para α = 0.01 → 0.005 em cada cauda)
z_alpha <- qnorm(1 - alpha / 2)

# Limites de controle
LIC <- mu - z_alpha * sigma_xbar
LSC <- mu + z_alpha * sigma_xbar

# Resultados
LIC
LSC



dados <- data.frame(
  amostra = 1:30,
  media = c(0.0143, 0.0183, -0.0278, 0.0112, -0.0040, -0.0078, 0.0042, -0.0077, 0.0003, 0.0050,
            0.0153, -0.0025, 0.0007, -0.0177, -0.0165, -0.0113, -0.0188, 0.0242, 0.0010,-0.0332,
            -0.0420, 0.0223, -0.0103, -0.0048, 0.0025, -0.0357, 0.0562, 0.0117, 0.0580, 0.0687),
  R = c(0.119, 0.114, 0.156, 0.167, 0.158, 0.128, 0.215, 0.102, 0.198, 0.148,
        0.070, 0.104, 0.090, 0.172, 0.067, 0.118, 0.119, 0.113, 0.159, 0.209,
        0.087, 0.168, 0.152, 0.157, 0.230, 0.215, 0.227, 0.215, 0.413, 0.110),
  S = c(0.0441, 0.0374, 0.0538, 0.0612, 0.0557, 0.0453, 0.0846, 0.0399, 0.0668, 0.0587,
        0.0257, 0.0354, 0.0382, 0.0597, 0.0265, 0.0469, 0.0526, 0.0394, 0.0537, 0.0762,
        0.0301, 0.0637, 0.0568, 0.0565, 0.0818, 0.0817, 0.0840, 0.0880, 0.1640, 0.0472)
)

dados

fase1 <- dados[1:20, ]
fase2 <- dados[21:30, ]
# Estimativas com base na Fase 1
xbar_barra <- mean(fase1$media)
R_barra <- mean(fase1$R)
S_barra <- mean(fase1$S)

# Constantes para n = 6 (Montgomery)
d2 <- 2.534
c4 <- 0.9213

# Estimativas de sigma
sigma_R <- R_barra / d2
sigma_S <- S_barra / c4

xbar_barra
sigma_R
sigma_S
# Constantes para n = 6
A2 <- 0.483
D3 <- 0.000
D4 <- 2.114
A3 <- 0.729
B3 <- 0.03
B4 <- 1.97

# Limites para gráfico x-R
LIC_x_R <- xbar_barra - A2 * R_barra
LC_x_R  <- xbar_barra
LSC_x_R <- xbar_barra + A2 * R_barra

LIC_R <- D3 * R_barra
LC_R  <- R_barra
LSC_R <- D4 * R_barra

# Limites para gráfico x-S
LIC_x_S <- xbar_barra - A3 * S_barra
LC_x_S  <- xbar_barra
LSC_x_S <- xbar_barra + A3 * S_barra

LIC_S <- B3 * S_barra
LC_S  <- S_barra
LSC_S <- B4 * S_barra

plot(fase1$media, type = "b", pch = 19, ylim = range(c(LIC_x_R, LSC_x_R)), main = "Gráfico x-barra (Fase 1)",
     xlab = "Amostra", ylab = "Média")
abline(h = c(LIC_x_R, LC_x_R, LSC_x_R), col = c("red", "blue", "red"), lty = c(2, 1, 2))
plot(fase1$R, type = "b", pch = 19, ylim = range(c(LIC_R, LSC_R)), main = "Gráfico R (Fase 1)",
     xlab = "Amostra", ylab = "Amplitude")
abline(h = c(LIC_R, LC_R, LSC_R), col = c("red", "blue", "red"), lty = c(2, 1, 2))
plot(dados$media, type = "b", pch = 19, ylim = range(c(LIC_x_R, LSC_x_R)), main = "Gráfico x-barra (Fase 1 + Fase 2)",
     xlab = "Amostra", ylab = "Média")
abline(h = c(LIC_x_R, LC_x_R, LSC_x_R), col = c("red", "blue", "red"), lty = c(2, 1, 2))
plot(dados$R, type = "b", pch = 19, ylim = range(c(LIC_R, LSC_R)), main = "Gráfico R (Fase 1 + Fase 2)",
     xlab = "Amostra", ylab = "Amplitude")
abline(h = c(LIC_R, LC_R, LSC_R), col = c("red", "blue", "red"), lty = c(2, 1, 2))
cat("**Relatório Final**\n\n")
cat("Foram utilizados dados de 30 amostras, sendo 20 na Fase 1 e 10 na Fase 2.\n\n")
cat("Os gráficos de controle \u0078-barra e R foram construídos com base nas estimativas:\n")
cat(sprintf("- Média do processo estimada: %.4f\n", xbar_barra))
cat(sprintf("- Desvio padrão estimado por R: %.4f\n", sigma_R))
cat("Os limites de controle foram calculados utilizando as constantes apropriadas de Montgomery (2016).\n\n")
cat("A análise gráfica da Fase 1 mostra um processo aparentemente sob controle.\n")
cat("Na Fase 2, foram identificados possíveis indícios de descontrole em algumas amostras, especialmente pela amplitude elevada na amostra 29.\n")
cat("Recomenda-se investigar as causas especiais associadas às amostras fora dos limites e aplicar ações corretivas conforme necessário.\n")




# Substitua pelos caminhos reais se estiver usando fora do ambiente da disciplina
caminho_f1 <- "C:/Users/edude/OneDrive/Área de Trabalho/Códigos/Faculdade/5_Periodo/Controle_estatistico_qualidade/Listas_trabalho/Lista_respostas/IsolanteF1.csv"
caminho_f2 <- "C:/Users/edude/OneDrive/Área de Trabalho/Códigos/Faculdade/5_Periodo/Controle_estatistico_qualidade/Listas_trabalho/Lista_respostas/IsolanteF2.csv"

# Leitura dos arquivos
fase1 <- read.csv2(caminho_f1, header = TRUE)
fase2 <- read.csv2(caminho_f2, header = TRUE)

head(fase1)
# Converter dados para matriz (cada linha = uma amostra)
matriz_f1 <- as.matrix(fase1)

# Criar gráfico de controle x-barra e R
controle_f1 <- qcc(matriz_f1, type = "xbar", plot = TRUE)
controle_r <- qcc(matriz_f1, type = "R", plot = TRUE)

# Converter para matriz
matriz_f2 <- as.matrix(fase2)

# Atualizar o gráfico com novos pontos
qcc_update <- qcc(matriz_f1, type = "xbar", newdata = matriz_f2)

# Estimar parâmetros do processo com base na Fase 1
media_proc <- mean(matriz_f1)
desvio_proc <- sd(as.vector(matriz_f1))  # com todos os dados amostrais

# Calcular PPFE
ppfe_inf <- pnorm(420, mean = media_proc, sd = desvio_proc)
ppfe_sup <- 1 - pnorm(480, mean = media_proc, sd = desvio_proc)
ppfe_total <- ppfe_inf + ppfe_sup

ppfe_total

cat("**Conclusão:**\n\n")
cat(sprintf("O processo apresenta uma média estimada de %.2f e desvio padrão de %.2f com base na Fase 1.\n", media_proc, desvio_proc))
cat("A construção dos gráficos de controle indica se há indícios de descontrole durante a Fase 2.\n")
cat(sprintf("A probabilidade estimada de produção fora das especificações (PPFE) é de aproximadamente %.2f%%.\n", 100 * ppfe_total))



# Ajuste seu caminho conforme sua máquina
base_dir <- "C:/Users/edude/OneDrive/Área de Trabalho/Códigos/Faculdade/5_Periodo/Controle_estatistico_qualidade/Listas_trabalho/Lista_respostas"

# Carregar arquivos
fase1 <- read.csv2(file.path(base_dir, "CompF1.csv"), header = TRUE)
fase2 <- read.csv2(file.path(base_dir, "CompF2.csv"), header = TRUE)

# Visualizar
head(fase1)

# Converter em matriz (cada linha = uma amostra)
matriz_f1 <- as.matrix(fase1)

# Gráfico x-barra com desvio padrão amostral ponderado
qcc_xs <- qcc(matriz_f1, type = "xbar", std.dev = "UWAVE-SD", plot = TRUE)

# Gráfico S (desvios padrão)
qcc_s <- qcc(matriz_f1, type = "S", plot = TRUE)

# Converter a fase 2 em matriz
matriz_f2 <- as.matrix(fase2)

# Atualizar gráfico x-barra com fase 2
qcc_xs_monitor <- qcc(matriz_f1, type = "xbar", std.dev = "UWAVE-SD", newdata = matriz_f2, plot = TRUE)


cat("**Conclusão:**\n\n")
cat("Os gráficos \u0078-barra e S foram construídos usando as 20 amostras da Fase 1.\n")
cat("A avaliação dos gráficos mostra se o processo estava sob controle inicialmente.\n")
cat("Após incluir as 15 amostras da Fase 2, observamos se algum ponto ultrapassa os limites de controle.\n")
cat("Se sim, isso indica uma possível causa especial e necessidade de investigação do processo.\n")




base_dir <- "C:/Users/edude/OneDrive/Área de Trabalho/Códigos/Faculdade/5_Periodo/Controle_estatistico_qualidade/Listas_trabalho/Lista_respostas"

# Leitura dos arquivos (dados individuais)
past1 <- read.csv2(file.path(base_dir, "past.csv"), header = TRUE)
past2 <- read.csv2(file.path(base_dir, "past2.csv"), header = TRUE)
past3 <- read.csv2(file.path(base_dir, "past3.csv"), header = TRUE)

# Extrair vetor com os valores (supondo 1 coluna com dados)
dados1 <- past1[[1]]

# Gráfico de controle para dados individuais
qcc_x <- qcc(dados1, type = "xbar.one", plot = TRUE)

dados2 <- past2[[1]]

# Adicionar ao gráfico
qcc_x_monitor <- qcc(dados1, type = "xbar.one", newdata = dados2, plot = TRUE)

dados3 <- past3[[1]]

# Adicionar dados da Parte 3 ao gráfico com os limites da Parte 1
qcc_x_final <- qcc(dados1, type = "xbar.one", newdata = dados3, plot = TRUE)

cat("**Conclusões da Questão 7:**\n\n")
cat("**Parte 1:** Os gráficos individuais mostraram os limites de controle com base nas 30 primeiras pastilhas.\n")
cat("**Parte 2:** Foram avaliadas 10 novas pastilhas. Caso alguma delas tenha ultrapassado os limites, há indício de descontrole.\n")
cat("**Parte 3:** Após a remoção da causa especial, o processo foi monitorado com 20 novas pastilhas.\n")
cat("Se todas estiverem dentro dos limites e sem padrão sistemático, o processo está sob controle.\n")
# Criando matriz (máximo tamanho amostral = 6)
fase1 <- data.frame(
  A1  = c(194, 200, 200, 197, 203, 186, 210, 206, 201, 202, 205, 188, 207, 200, 198),
  A2  = c(217, 203, 196, 209, 214, 212, 178, 204, 200, 214, 212, 199, 198, 195, 190),
  A3  = c(195, 204, 192, 216, 183, 205, 183, 186, 183, 184, 199, 197, 192, 208, 196),
  A4  = c(201, 193, 188, 211, 206, 195, 221, 193, 191, 199, 198, 209, 203, 199, 191),
  A5  = c(207, 205, 211, 183, 191, 194, 210, 205, 223, 192, 203, 204, 192, 203, 204),
  A6  = c(203, 216, 179, 198, NA, 198, 204, NA, 203, NA, 201, NA, 210, NA, 201)
)

# Fase 2 (10 amostras, n = 4)
fase2 <- data.frame(
  A1 = c(198, 177, 227, 197, 188, 177, 196, 167, 199, 190),
  A2 = c(191, 197, 211, 200, 186, 188, 191, 193, 202, 202),
  A3 = c(212, 212, 211, 202, 213, 183, 217, 193, 184, 208),
  A4 = c(177, 211, 203, 230, 212, 182, 186, 204, 181, 219),
  A5 = rep(NA, 10),
  A6 = rep(NA, 10)
)
qcc_xs <- qcc(fase1, type = "xbar", std.dev = "UWAVE-SD", plot = TRUE)
qcc_s <- qcc(fase1, type = "S", plot = TRUE)
qcc_xs_monitor <- qcc(fase1, type = "xbar", std.dev = "UWAVE-SD", newdata = fase2, plot = TRUE)
cat("**Conclusão:**\n\n")
cat("Os gráficos \u0078-barra e S foram construídos com base nas 15 amostras da Fase 1, com diferentes tamanhos amostrais (5 ou 6).\n")
cat("A Fase 2, com amostras de tamanho 4, foi adicionada para monitoramento.\n")
cat("Se houver pontos fora dos limites ou padrões suspeitos, isso indica descontrole estatístico.\n")
