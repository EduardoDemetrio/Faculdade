tratamento <- c("H2", "H3", "H1", "H1", "H4","H4", "H3", "H4", "H4", "H4","H2", "H2", "H2", "H1","H3", "H3", "H1", "H1", "H2", "H3")
tempos <- c(3.87, 3.40, 4.89, 3.98, 2.77,3.10, 3.07, 3.18, 2.65, 2.82,4.27, 4.22, 3.53, 3.68,3.46, 3.40, 4.42, 3.43, 3.60, 3.13)


dados <- data.frame(tratamento, tempos)



# ANOVA
anova_result <- aov(tempo ~ tratamento, data = dados)
# Obter resíduos
residuos <- residuals(anova_result)

hist(residuos, main="Histograma dos Resíduos", xlab="Resíduos")

qqnorm(residuos)
qqline(residuos, col="red")

shapiro.test(residuos)


plot(anova_result$residuals,
     type = "p",                   
     main = "Resíduos vs Ordem",
     
     ylab = "Resíduos",
     xlab = "Índice (Ordem dos dados)")
abline(h = 0, col = "red", lty = 2)

bartlett.test(tempos ~ tratamento, data = dados)