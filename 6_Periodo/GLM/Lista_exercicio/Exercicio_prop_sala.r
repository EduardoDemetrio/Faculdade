#%%
# '
# Exercicio proposto
# Você deverá usar modelos de regressão linear na análise dos dados disponíveis na base de dados lungcap, que pode ser acessada na biblioteca GLMsData do R. 
# A variável resposta deverá ser FEV, e as demais variáveis usadas como explicativas. 
# Dentre as etapas da análise sugere-se:
# Consulta à documentação da base para entendimento dos dados e das variáveis;
# Análise descritiva e exploratória;
# Ajuste de um modelo inicial e diagnóstico do ajuste;
# Aplicação de medidas corretivas, caso necessárias;
# Interpretação dos efeitos das variáveis explicativas e de suas significâncias;
# Eliminação das variáveis não significativas (se houver) e ajuste de um novo modelo;
# Obtenção de intervalos de confiança para os parâmetros do modelo;
# Obtenção de predições e ICs para duas novas observações (escolha valores das variáveis
# explicativas para essas observações).
# '


#%% Instalação dos pacotes necessários
install.packages("GLMsData")
install.packages("car")
install.packages("ggplot2")
install.packages("corrplot")
#%%Carregando os dados propostos e pacotes
library(GLMsData)
library(corrplot)
library(ggplot2)

data("lungcap")

#%% Vamos iniciar a analise descritiva/exploratória
print(lungcap)


### Dessa maneira temos todas as variaveis como númericas e já nos auxilia em muitos pontos

## Vamos agora entender como está nossa tabela
help(lungcap)
#### O que podemos extrair foi o contexto da base aonde temos que os dados são coletados de uma cidade (East Boston (EUA)) durante um periodo (1970 (meados até final da década)) e que temos 654 jovens que tem hábitos de fumo 
#### E nossa variavel resposta seria a capacidade pulmonar do individuo em volume então em litros (FEV)
summary(lungcap)

## Primeiro ponto vamos iniciar com uma transformação na tabela aonde teemos Gender (Gênero) -- F=0 e M=1

lungcap$Gender <- factor(lungcap$Gender,
                       labels = c(0,1))

lungcap$Smoke <- factor(lungcap$Smoke,
                       labels = c(0,1))
## Vamos entender gráficamente como elas se relacioam


ggplot(aes(x = FEV), data = lungcap)+
    geom_histogram(bins = 15, color="#e9ecef")+
    theme_classic(base_size = 14)

ggplot(aes(x = Age, y = FEV), data = lungcap)+
    geom_point()+
    theme_classic(base_size = 14)+
    geom_smooth(se = FALSE)

ggplot(aes(x = Ht, y = FEV), data = lungcap)+
    geom_point()+
    theme_classic(base_size = 14)+
    geom_smooth(se = FALSE)

ggplot(aes(x = Gender, y = FEV), data = lungcap)+
    geom_boxplot(fill = '#e9ecef')+
    theme_classic(base_size = 14)+
    geom_smooth(se = FALSE)

ggplot(aes(x = Smoke, y = FEV), data = lungcap)+
    geom_boxplot(fill = '#e9ecef')+
    theme_classic(base_size = 14)+
    geom_smooth(se = FALSE)

ggplot(lungcap, aes(x = Smoke, y = Age)) +
    geom_boxplot(fill = "#e9ecef") +
    theme_classic(base_size = 14) 


### Conseguimos tirar pequenas interpretações dos dados
#### Temos uma base com não fumantes mais novos e fumantes mais velhos, assim impactando na sua capacidadde pulmonar

#%% Vamos inicar ajustano um modelo de regressão linerar

ajuste <- lm(FEV ~ ., data=lungcap)
coef(ajuste)
summary(ajuste)

ajuste_2 <- lm(FEV ~ Age+Ht+Gender+Smoke, data=lungcap)
coef(ajuste_2)
summary(ajuste_2)