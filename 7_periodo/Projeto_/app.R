library(shiny)
library(tidyverse)

# ============================================================
# 1. CARGA E PREPARAÇÃO DOS DADOS
# ============================================================

data_raw <- read_csv("amostra_268.csv", show_col_types = FALSE)

min_idade <- min(data_raw$P34, na.rm = TRUE)
max_idade <- max(data_raw$P34, na.rm = TRUE)

generos      <- c("Todos", sort(unique(na.omit(data_raw$P35))))
escolaridades <- c("Todos", sort(unique(na.omit(data_raw$P36))))
rendas       <- c("Todos", sort(unique(na.omit(data_raw$P43))))
religioes    <- c("Todos", sort(unique(na.omit(data_raw$P44))))

data_long <- data_raw %>%
  pivot_longer(
    cols = matches("^(P01_|P03_|P10$|P11$|P15_|P19_|P20$|P21$|P25$|P27_)"),
    names_to = "pergunta",
    values_to = "resposta"
  ) %>%
  filter(!is.na(resposta), resposta != "")

# ============================================================
# 2. MAPAS DE LABELS E CORES
# ============================================================

labels_map <- c(
  "P01_Politica" = "Interesse do brasileiro por Política",
  "P01_Medicina_e_Saude" = "Interesse do brasileiro por Medicina e Saúde",
  "P01_Arte_e_Cultura" = "Interesse do brasileiro por Arte e Cultura",
  "P01_Meio_Ambiente" = "Interesse do brasileiro por Meio Ambiente",
  "P01_Ciencia_e_Tecnologia" = "Interesse do brasileiro por Ciência e Tecnologia",
  "P01_Esportes" = "Interesse do brasileiro por Esportes",
  "P01_Economia" = "Interesse do brasileiro por Economia",
  "P01_Religiao" = "Interesse do brasileiro por Religião",
  "P01_Educacao" = "Interesse do brasileiro por Educação",
  "P01_Descobertas_e_Invencoes" = "Interesse do brasileiro por Descobertas e Invenções",
  "P03_Biblioteca" = "Visitou Biblioteca nos últimos 12 meses?",
  "P03_Museu_Arte_Historia" = "Visitou Museu de Arte ou História?",
  "P03_Zoologico_Aquario" = "Visitou Zoológico ou Aquário?",
  "P03_Parque_Reserva" = "Visitou Parque ou Reserva?",
  "P03_Laboratorio_Ict" = "Visitou Laboratório de ICT?",
  "P03_Planetario" = "Visitou Planetário?",
  "P03_Feira_Olimpiada" = "Participou de Feira ou Olimpíada?",
  "P03_Evento_Snct" = "Participou de evento SNCT?",
  "P03_Atividades_Exposicoes" = "Participou de Atividades/Exposições?",
  "P03_Parana_Faz_Ciencia" = "Participou do Paraná Faz Ciência?",
  "P03_Museu_Ciencia_Tecnologia" = "Visitou Museu de Ciência e Tecnologia?",
  "P10" = "Frequência de compartilhamento de informações",
  "P11" = "Já compartilhou informações falsas?",
  "P15_Primeiro_Lugar" = "Primeira profissão de maior confiança",
  "P15_Segundo_Lugar" = "Segunda profissão de maior confiança",
  "P19_Afirmacao_1" = "Afirmação 1 - Ciência traz benefícios",
  "P19_Afirmacao_2" = "Afirmação 2 - Ciência é necessária",
  "P19_Afirmacao_3" = "Afirmação 3 - Ciência gera empregos",
  "P20" = "Mudanças climáticas: causas",
  "P21" = "Nível de preocupação ambiental",
  "P25" = "Participação em decisões de CT"
)

# Escala 1-4 das perguntas P27 (áreas de destaque do estado)
labels_p27 <- c(
  "1" = "Nenhum Destaque",
  "2" = "Pouco Destaque",
  "3" = "Destaque",
  "4" = "Muito Destaque",
  "NS/NR" = "NS/NR"
)

# Escala 1-4 das perguntas P01 (interesse por temas)
labels_p01 <- c(
  "1" = "Nada interessado",
  "2" = "Pouco interessado",
  "3" = "Interessado",
  "4" = "Muito interessado",
  "NS/NR" = "NS/NR"
)

# Faixas etárias
faixas_idade <- function(idade) {
  case_when(
    idade < 25 ~ "16-24 anos",
    idade < 35 ~ "25-34 anos",
    idade < 45 ~ "35-44 anos",
    idade < 60 ~ "45-59 anos",
    TRUE       ~ "60+ anos"
  )
}
niveis_faixa <- c("16-24 anos", "25-34 anos", "35-44 anos", "45-59 anos", "60+ anos")

# Labels curtos para escolaridade
labels_escol <- c(
  "00) Não Estudou"                                                    = "Não estudou",
  "01) Ensino Fundamental Incompleto"                                  = "Fund. Incomp.",
  "02) Ensino Fundamental Completo"                                    = "Fund. Comp.",
  "03) Ensino Médio ou Antigo 2º Grau Incompleto"                     = "Médio Incomp.",
  "04) Ensino Médio ou Antigo 2º Grau Completo"                       = "Médio Comp.",
  "05) Graduação ou Ensino Tecnológico Incompleto"                    = "Superior Incomp.",
  "06) Graduação ou Ensino Tecnológico Completo"                      = "Superior Comp.",
  "07) Pós-Graduação (Especialização, Mestrado ou Doutorado)"         = "Pós-Graduação",
  "08) Não Sei Informar"                                               = "NS/NR"
)

# Labels curtos para renda
labels_renda <- c(
  "1. De R$ 0,00 a R$3.000,00 (0 a 2 salários-mínimos)"     = "Até 2 SM",
  "2. De R$ 3.001 a R$ 7.500 (2 a 5 salários-mínimos)"      = "2 a 5 SM",
  "3. De R$ 7.501 a R$ 15.000 (5 a 10 salários-mínimos)"    = "5 a 10 SM",
  "4. De R$ 15.001 a R$ 30.000 (10 a 20 salários-mínimos)"  = "10 a 20 SM",
  "5. Acima de R$ 30.000 (20 salários-mínimos)"              = "Acima de 20 SM"
)

# Ordem fixa da escala de interesse (para o gráfico empilhado)
niveis_interesse <- c("Nada interessado", "Pouco interessado", 
                      "Interessado", "Muito interessado", "NS/NR")

# Paleta CGEE para gráficos simples
cores_cgee <- c("#779861", "#EB8891", "#343A8C", "#683B81", "#D4A76A", "#5BA3A6")

# Paleta da escala de interesse (estilo CGEE - imagem 1)
cores_interesse <- c(
  "Nada interessado"   = "#E05C4B",
  "Pouco interessado"  = "#EB8891",
  "Interessado"        = "#5BA3A6",
  "Muito interessado"  = "#343A8C",
  "NS/NR"              = "#AAAAAA"
)

# ============================================================
# 3. FUNÇÃO AUXILIAR PARA APLICAR FILTROS DEMOGRÁFICOS
# ============================================================

aplicar_filtros <- function(df, idade, genero, escolaridade, renda, religiao) {
  df <- df %>% filter(P34 >= idade[1], P34 <= idade[2])
  if (genero       != "Todos") df <- df %>% filter(P35 == genero)
  if (escolaridade != "Todos") df <- df %>% filter(P36 == escolaridade)
  if (renda        != "Todos") df <- df %>% filter(P43 == renda)
  if (religiao     != "Todos") df <- df %>% filter(P44 == religiao)
  df
}

# ============================================================
# 4. UI
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      * { font-family: 'Inter', sans-serif; }
      body { background-color: #F5F5F5; }
      .filter-panel { background: #FFFFFF; border: 1px solid #E0E0E0; border-radius: 8px; padding: 24px 24px 16px 24px; margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.04); }
      .chart-panel { background: #FFFFFF; border: 1px solid #E0E0E0; border-radius: 8px; padding: 24px; box-shadow: 0 1px 3px rgba(0,0,0,0.04); }
      .filter-label { font-size: 12px; font-weight: 500; color: #555555; margin-bottom: 4px; }
      .selectize-input, .selectize-dropdown, .irs-single, .irs-from, .irs-to { font-family: 'Inter', sans-serif !important; }
      .selectize-input { border-radius: 6px !important; border-color: #D0D0D0 !important; }
      .irs-bar { background: #343A8C !important; border-color: #343A8C !important; }
      .irs-single, .irs-from, .irs-to { background: #343A8C !important; }
      .chart-title { font-size: 18px; font-weight: 600; color: #222222; text-align: left; margin-bottom: 4px; }
      .chart-subtitle { font-size: 13px; color: #888888; text-align: left; margin-bottom: 20px; }
      .legend-container { display: flex; flex-wrap: wrap; justify-content: center; gap: 20px; margin-top: 16px; padding-top: 16px; border-top: 1px solid #F0F0F0; }
      .legend-item { display: flex; align-items: center; gap: 6px; font-size: 13px; color: #555555; }
      .legend-color { width: 14px; height: 14px; border-radius: 3px; display: inline-block; }
    "))
  ),
  
  div(style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
      div(style = "text-align: left; margin-bottom: 24px;",
          h1("Dashboard - Percepção Pública da CT", style = "font-weight: 700; color: #222; font-size: 26px; margin: 0;"),
          p("Explore as respostas da pesquisa", style = "color: #888; font-size: 14px; margin: 4px 0 0 0;")
      ),
      div(class = "filter-panel",
          div(style = "margin-bottom: 16px;",
              div(class = "filter-label", "ESCOLHA A PERGUNTA:"),
              selectInput("pergunta", NULL, choices = NULL, width = "100%")
          ),
          div(style = "display: flex; gap: 16px; margin-bottom: 16px;",
              div(style = "flex: 1;",
                  div(class = "filter-label", "ESCOLHA O SEXO:"),
                  selectInput("genero", NULL, choices = generos, width = "100%")
              ),
              div(style = "flex: 1.5;",
                  div(class = "filter-label", "ESCOLHA A FAIXA DE IDADE:"),
                  sliderInput("idade", NULL, min = min_idade, max = max_idade,
                              value = c(min_idade, max_idade), step = 1, width = "100%")
              ),
              div(style = "flex: 1.5;",
                  div(class = "filter-label", "ESCOLHA A ESCOLARIDADE:"),
                  selectInput("escolaridade", NULL, choices = escolaridades, width = "100%")
              )
          ),
          div(style = "display: flex; gap: 16px;",
              div(style = "flex: 1;",
                  div(class = "filter-label", "ESCOLHA A RENDA:"),
                  selectInput("renda", NULL, choices = rendas, width = "100%")
              ),
              div(style = "flex: 1;",
                  div(class = "filter-label", "ESCOLHA A RELIGIÃO:"),
                  selectInput("religiao", NULL, choices = religioes, width = "100%")
              )
          )
      ),
      
      
      # PERFIL DA AMOSTRA  <-- NOVO BLOCO
      div(style = "margin-bottom: 20px;",
          div(style = "font-size: 13px; font-weight: 600; color: #555; margin-bottom: 12px; text-transform: uppercase; letter-spacing: 0.05em;",
              "Perfil da amostra selecionada"),
          div(style = "display: flex; gap: 16px;",
              div(class = "chart-panel", style = "flex: 1; padding: 16px;",
                  div(style = "font-size: 12px; font-weight: 600; color: #888; margin-bottom: 8px;", "SEXO"),
                  plotOutput("perfil_sexo", height = "140px")
              ),
              div(class = "chart-panel", style = "flex: 1.5; padding: 16px;",
                  div(style = "font-size: 12px; font-weight: 600; color: #888; margin-bottom: 8px;", "FAIXA ETÁRIA"),
                  plotOutput("perfil_idade", height = "140px")
              ),
              div(class = "chart-panel", style = "flex: 2; padding: 16px;",
                  div(style = "font-size: 12px; font-weight: 600; color: #888; margin-bottom: 8px;", "ESCOLARIDADE"),
                  plotOutput("perfil_escol", height = "140px")
              ),
              div(class = "chart-panel", style = "flex: 1.5; padding: 16px;",
                  div(style = "font-size: 12px; font-weight: 600; color: #888; margin-bottom: 8px;", "RENDA"),
                  plotOutput("perfil_renda", height = "140px")
              )
          )
      ),
      
      div(class = "chart-panel",
          uiOutput("chart_header"),
          plotOutput("grafico", height = "450px"),
          uiOutput("legenda")
      )
  )
)

# ============================================================
# 5. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ---- Popular o select da pergunta ----
  observe({
    perguntas <- sort(unique(data_long$pergunta))
    nice_labels <- recode(perguntas, !!!labels_map, .default = str_replace_all(perguntas, "_", " "))
    choices <- setNames(perguntas, nice_labels)
    choices <- c("Interesse total temas" = "P01_TODOS", choices)
    updateSelectInput(session, "pergunta", choices = choices, selected = "P01_TODOS")
  })
  
  is_todos <- reactive({ input$pergunta == "P01_TODOS" })
  
  # Dados da amostra com filtros aplicados (para o perfil)
  data_perfil <- reactive({
    data_raw %>%
      aplicar_filtros(input$idade, input$genero, input$escolaridade, input$renda, input$religiao)
  })
  
  # Função auxiliar para gráfico de perfil horizontal
  plot_perfil <- function(df_contagem, fill_cor = "#343A8C") {
    ggplot(df_contagem, aes(x = categoria, y = prop)) +
      geom_bar(stat = "identity", fill = fill_cor, width = 0.7) +
      geom_text(aes(label = paste0(round(prop), "%")),
                hjust = -0.1, size = 3.2, color = "#444444") +
      scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
      coord_flip() +
      theme_minimal(base_family = "sans") +
      theme(
        axis.text.x  = element_blank(),
        axis.text.y  = element_text(size = 10, color = "#444"),
        axis.title   = element_blank(),
        panel.grid   = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin  = margin(4, 20, 4, 4)
      )
  }
  
  output$perfil_sexo <- renderPlot({
    df <- data_perfil() %>%
      filter(!is.na(P35)) %>%
      count(categoria = P35) %>%
      mutate(prop = n / sum(n) * 100) %>%
      arrange(desc(prop))
    plot_perfil(df, "#343A8C")
  })
  
  output$perfil_idade <- renderPlot({
    df <- data_perfil() %>%
      filter(!is.na(P34)) %>%
      mutate(categoria = factor(faixas_idade(P34), levels = niveis_faixa)) %>%
      count(categoria) %>%
      mutate(prop = n / sum(n) * 100) %>%
      arrange(categoria)
    plot_perfil(df, "#5BA3A6")
  })
  
  output$perfil_escol <- renderPlot({
    df <- data_perfil() %>%
      filter(!is.na(P36)) %>%
      mutate(categoria = factor(
        recode(P36, !!!labels_escol, .default = P36),
        levels = labels_escol
      )) %>%
      count(categoria) %>%
      mutate(prop = n / sum(n) * 100) %>%
      arrange(categoria)
    plot_perfil(df, "#683B81")
  })
  
  output$perfil_renda <- renderPlot({
    df <- data_perfil() %>%
      filter(!is.na(P43), P43 != "NA") %>%
      mutate(categoria = factor(
        recode(P43, !!!labels_renda, .default = P43),
        levels = labels_renda
      )) %>%
      count(categoria) %>%
      mutate(prop = n / sum(n) * 100) %>%
      arrange(categoria)
    plot_perfil(df, "#D4A76A")
  })
  
  # ---- Dados filtrados (pergunta única) ----
  data_filt <- reactive({
    req(input$pergunta, !is_todos())
    data_long %>%
      filter(pergunta == input$pergunta) %>%
      aplicar_filtros(input$idade, input$genero, input$escolaridade, input$renda, input$religiao)
  })
  
  # ---- df para gráfico simples (pergunta única) ----
  df_plot <- reactive({
    df <- data_filt() %>%
      count(resposta) %>%
      mutate(prop = n / sum(n) * 100) %>%
      arrange(desc(n))
    
    if (grepl("^P27_", input$pergunta)) {
      df <- df %>% mutate(resposta = recode(resposta, !!!labels_p27, .default = resposta))
    }
    
    if (nrow(df) > 6) {
      top5 <- slice_head(df, n = 5)
      outros <- df %>% anti_join(top5, by = "resposta") %>%
        summarise(resposta = "Outros", n = sum(n), prop = sum(prop))
      df <- bind_rows(top5, outros)
    }
    df
  })
  
  # ---- df para gráfico de TODOS os P01 ----
  df_plot_todos <- reactive({
    req(is_todos())
    
    df <- data_long %>%
      filter(grepl("^P01_", pergunta)) %>%
      aplicar_filtros(input$idade, input$genero, input$escolaridade, input$renda, input$religiao) %>%
      mutate(resposta = recode(resposta, !!!labels_p01, .default = NA_character_)) %>%
      filter(!is.na(resposta)) %>%
      count(pergunta, resposta) %>%
      group_by(pergunta) %>%
      mutate(prop = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(
        tema = recode(pergunta, !!!labels_map, .default = pergunta),
        tema = str_remove(tema, "Interesse do brasileiro por ")
      )
    
    # ordenar temas por % de "Muito interessado"
    ordem <- df %>%
      filter(resposta == "Muito interessado") %>%
      arrange(prop) %>%
      pull(tema)
    
    df %>% mutate(tema = factor(tema, levels = ordem))
  })
  
  # ---- Header do gráfico ----
  output$chart_header <- renderUI({
    if (is_todos()) {
      n_total <- data_raw %>%
        aplicar_filtros(input$idade, input$genero, input$escolaridade, input$renda, input$religiao) %>%
        nrow()
      div(
        div(class = "chart-title", "Interesse do brasileiro por todos os temas (%)"),
        div(class = "chart-subtitle", paste0("2025: ", n_total, " entrevistados"))
      )
    } else {
      nice_name <- recode(input$pergunta, !!!labels_map, .default = str_replace_all(input$pergunta, "_", " "))
      div(
        div(class = "chart-title", paste0(nice_name, " (%)")),
        div(class = "chart-subtitle", paste0("2025: ", nrow(data_filt()), " entrevistados"))
      )
    }
  })
  
  # ---- Gráfico ----
  output$grafico <- renderPlot({
    if (is_todos()) {
      df <- df_plot_todos()
      
      ggplot(df, aes(x = tema, y = prop, 
                     fill = factor(resposta, levels = niveis_interesse))) +
        geom_bar(stat = "identity", width = 0.7) +
        geom_text(aes(label = ifelse(prop >= 5, paste0(round(prop), "%"), "")),
                  position = position_stack(vjust = 0.5),
                  color = "white", fontface = "bold", size = 4) +
        scale_fill_manual(values = cores_interesse, name = NULL, drop = FALSE) +
        scale_y_continuous(labels = scales::percent_format(scale = 1),
                           breaks = seq(0, 100, 25), expand = c(0, 0)) +
        labs(x = NULL, y = NULL) +
        coord_flip() +
        theme_minimal(base_family = "sans") +
        theme(
          plot.margin = margin(10, 20, 10, 20),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.5),
          axis.text.y = element_text(size = 11, color = "#444444"),
          axis.text.x = element_text(size = 11, color = "#666666"),
          legend.position = "bottom",
          legend.text = element_text(size = 11),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
    } else {
      df <- df_plot()
      cores_usadas <- setNames(cores_cgee[seq_len(nrow(df))], df$resposta)
      
      ggplot(df, aes(x = "", y = prop, 
                     fill = factor(resposta, levels = df$resposta))) +
        geom_bar(stat = "identity", width = 0.6) +
        geom_text(aes(label = ifelse(prop >= 3, paste0(round(prop, 1), "%"), "")),
                  position = position_stack(vjust = 0.5),
                  color = "white", fontface = "bold", size = 5) +
        scale_fill_manual(values = cores_usadas) +
        scale_y_continuous(labels = scales::percent_format(scale = 1),
                           breaks = seq(0, 100, 20), expand = c(0, 0)) +
        labs(x = NULL, y = NULL) +
        coord_flip() +
        theme_minimal(base_family = "sans") +
        theme(
          plot.margin = margin(10, 20, 10, 20),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "#E8E8E8", linewidth = 0.5),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12, color = "#666666"),
          legend.position = "none",
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
    }
  })
  
  # ---- Legenda ----
  output$legenda <- renderUI({
    if (is_todos()) return(NULL)
    
    df <- df_plot()
    cores_usadas <- cores_cgee[seq_len(nrow(df))]
    
    div(class = "legend-container",
        tagList(
          lapply(seq_len(nrow(df)), function(i) {
            div(class = "legend-item",
                span(class = "legend-color", 
                     style = paste0("background-color: ", cores_usadas[i], ";")),
                span(paste0(df$resposta[i], " (", round(df$prop[i], 1), "%)"))
            )
          })
        )
    )
  })
}

shinyApp(ui = ui, server = server)