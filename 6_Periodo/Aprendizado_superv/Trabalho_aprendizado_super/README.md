# ☕ Predição de Resposta a Campanhas - Starbucks

**Disciplina:** Aprendizado Supervisionado  
**Professor:** Eduardo Vargas  

## 👥 Equipe
* **Líder:** Eduardo Demetrio 
---

## 📌 Sobre o Projeto

O objetivo deste projeto foi desenvolver um modelo preditivo capaz de estimar a probabilidade de um cliente responder positivamente a uma oferta de campanha do Starbucks. 

Como iniciamos com pouca experiência prática em *machine learning*, a proposta evoluiu gradualmente acompanhando o avanço das aulas. Ao final, conseguimos transformar uma ideia inicial simples em um pipeline completo e funcional de modelagem supervisionada.

---

## 📂 Estrutura do Repositório

```text
/
├── code/
│   ├── data_engineer.ipynb
│   ├── eda.ipynb
│   ├── model.ipynb 
│   └── relatorio_readme.ipynb
│
└── data/
    ├── bronze/   -> Dados brutos
    ├── silver/   -> Dados tratados e padronizados
    └── gold/     -> Dados prontos para modelagem




### Relatório:

#### Proposta do projeto ()
  O objetivo do grupo foi desenvolver um modelo preditivo capaz de estimar a probabilidade de um cliente responder positivamente a uma oferta de campanha do Starbucks.

  Como iniciamos com pouca experiência prática em machine learning, a proposta evoluiu gradualmente, acompanhando o avanço das aulas. Ao final, conseguimos transformar uma ideia inicial simples em um pipeline completo e funcional de modelagem supervisionada.

#### Estrutura/engenharia de dados (data_engineer.ipynb)
  - Iniciamos entendendo as tabelas, fazendo pequenas limpezas e reestruturações nas bases.
    - Aonde finalizamos a etapa de construção da tabela silver, que pega todos os nossos registros dos clientes, campanhas e verifica se tivemos o retorno ou não dessa campanha. Unificando numa granularidade de apenas registros únicos o que nos ajudou muito no desenvolvimento das próximas etapas.

  - Dificuldades/desafios: Geralmente estamos acostumados com bases "perfeitinhas" e esse desafio de pegar uma base crua e desenvolver essa engenharia de dados, foi desafiador e gratificante ao chegar numa tabela final.

 #### Analise exploratória (eda.ipynb)
  - Essa etapa ela é muito importante, pois ela é o que realmente vai nos guiar para um modelo de sucesso ou uma falha geral, pois é aonde podemos entender tudo da nossa base de dados e suas caracteristicas.

  - Pontos importantes conseguimos extrair a informação de quais ofertas "são as melhores", que mais convertem os clientes.  
  Também observamos como os registros foram evoluindo mensalmente, trazendo fato histórico da marca, em 2016 a companhia teve seus olhos voltados a população, com campanhas sociais e sustentabilidade, facilidades em pagamentos (pagamentos mobile), isso mostra esse impacto de ações sociais e melhorias no seu produto tem retorno positivo nos números de clientes.

  #### Modelo (model.ipynb)
  - Essa etapa tivemos uma atenção grande nos detalhes e no foco da construção de cada etapa. Iniciamos com uma separação de treino e teste, separando e validando nossos dados, entendendo se essa separação foi devidamente feita ou temos um problema de desbalancemanto no treino e teste.
  - Após essa etapa criamos nossa pipeline de transformação e criação das nossas variáveis, aonde criamos a nossa classe "classes.py - FeatureBuilder()" aonde fizemos as seguintes alterações:
    - Remoção de colunas
    - Transformação de tipos de dados
    - Transformação de colunas de texto para binários (sexo)
    - Criação de nova variável
      - Binning
  - Feito essa criação de pipeline, fizemos a etapa dos nossos modelos, aonde testamos 3 tipos de modelos Random Forest, Regressão Logistica e LightGBM, avaliando os modelos, conseguimos concluir com base em gráficos e métricas que o modelo que melhor se ajusta aos dados é o modelo boosting LightGBM.
  - E porque desses modelos, inicialmente para nosso problema são modelos que se encaixam perfeitamente, e focando em decisões com  o que ajuda na interpretabilidade.  
  - Fizemos um estudo das nossas variaveis importantes e de qual melhor threshold e essas foram as conclusões:
    - Notamos que em todos os modelos as váriaveis númericas difficulty, gender, reward_offer, são variáveis que tem um grande efeito positivo no nosso modelo, porém o que chama atenção é os casos das variáveis categóricas "time_total_bin_600+" que demonstra pra gente que quanto maior o tempo da oferta menor a chance de conversão. 
    - Pensando no threshold do nosso modelo, concluimos ele com 3 possibilidades de faixas de cortes (faixa conservadora, faixa média e faixa escolhida-risco)
    - Finalizando assim nosso modelo com essa matriz de confusão:

    | Real \ Predito | **0** | **1** |
    |----------------|-------|-------|
    | **0**          | 4488  | 966   |
    | **1**          | 2168  | 3419  | 

   - **Acurácia:** 0.72  
  - **Macro Avg (Precision/Recall/F1):** 0.73 / 0.72 / 0.71  
  - **Weighted Avg:** 0.73 / 0.72 / 0.71
  
    - Conclusão e insights: as campanhas precisam focar em alguns tipos de perfis, campanhas para tipos de genêros especificados como cliente alvo, clientes com uma renda baixa e que possam criar um vinculo com a empresa, criando clube de vantagem ou soma de pontos no aplicativo, assim tendo um publico mais presente e mais ativo com as campanhas.

  - Dificuldade/desafios: Acredito que em nome de todos do grupos, foi um trabalho desafiador e que ajudou a todos amadurecer e entender mais sobre o assunto identificamos que temos pontos de melhorias no modelo, pontos que serão aplicados futuramente, mas concluímos que o modelo atual apresenta desempenho sólido para o problema proposto..