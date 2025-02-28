# ============================================================================
# Pacotes necessários ---------------------------------------------------------
# Instale, se não tiver:
# install.packages(c("shiny", "shinythemes", "tidyverse", "plotly"))
# ============================================================================
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)

# 1) CARREGAR E PREPARAR DADOS -----------------------------------------------

# Carrega os dados principais
dados <- read.csv("planilha_acrilamida.csv", sep = ",", stringsAsFactors = FALSE)

# Carrega o arquivo descritivo
descricoes <- read.csv("descri.csv", sep = ",", stringsAsFactors = FALSE)

# Ajusta nomes das colunas de 'descricoes'
colnames(descricoes) <- tolower(trimws(colnames(descricoes)))
descricoes <- descricoes |> mutate(cnae = trimws(as.character(descricoes$cnae)))

# Ajusta nomes das colunas de 'dados'
colnames(dados) <- trimws(colnames(dados))  
dados$Total_Detectados <- as.numeric(dados$Total_Detectados)

# Criação de colunas "num_empresa_*" a partir de cada "cnae_..."
# + Criação de 'deteccao' (1 se Total_Detectados > 0, senão 0)
dados <- dados |> 
  mutate(
    across(
      starts_with("cnae_"),
      ~ ifelse(. > 0, 1, 0),
      .names = "num_empresa_{.col}"
    )
  ) |> 
  mutate(deteccao = ifelse(Total_Detectados > 0, 1, 0))

# 2) UI -----------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Dashboard CNAE - Parâmetro Acrilamida"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cnae_var", 
        "Escolha um CNAE:", 
        choices = names(dados)[grepl("^cnae_", names(dados))],
        selected = names(dados)[grepl("^cnae_", names(dados))][1]
      ),
      br(),
      textOutput("descricao_geral"),
      textOutput("descricao_especifica")
    ),
    
    mainPanel(
      h3("Tabela Completa"),
      tableOutput("tabela_freq"),
      htmlOutput("chi_square_completa"),
      
      h3("Tabela Apenas Consistentes"),
      tableOutput("tabela_consistentes"),
      htmlOutput("chi_square_consistentes"),
      
      h3("Gráfico de Dispersão"),
      plotlyOutput("grafico_dispersao_interativo")
    )
  )
)

# 3) SERVER -------------------------------------------------------------------
server <- function(input, output) {
  
  # 3.1) Descrições (geral e específica) a partir de 'descricoes'
  output$descricao_geral <- renderText({
    cnae_col <- as.character(input$cnae_var)
    descricao <- descricoes  |> 
      filter(trimws(cnae) == trimws(cnae_col)) |> 
      pull(denominacao_geral)
    
    if (length(descricao) > 0 && !is.na(descricao[1])) {
      paste("Descrição Geral: ", descricao[1])
    } else {
      "Descrição não encontrada"
    }
  })
  
  output$descricao_especifica <- renderText({
    cnae_col <- as.character(input$cnae_var)
    descricao <- descricoes |>
      filter(trimws(cnae) == trimws(cnae_col)) |>
      pull(denominacao_especifica)
    
    if (length(descricao) > 0 && !is.na(descricao[1])) {
      paste("Descrição Específica: ", descricao[1])
    } else {
      "Descrição não encontrada"
    }
  })
  
  # 3.2) Tabela Frequência (dados completos)
  output$tabela_freq <- renderTable({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      tabela <- table(dados$deteccao, dados[[paste0("num_empresa_", cnae_col)]])
      proporcoes <- prop.table(tabela, margin = 1)
      tabela_final <- cbind(
        as.data.frame.matrix(tabela),
        Proporção = round(proporcoes[, 2], 2)
      )
      colnames(tabela_final) <- c("sem detecção", "com detecção", "Proporção")
      rownames(tabela_final) <- c("sem empresas no CNAE", "com empresas no CNAE")
      tabela_final
    }
  }, rownames = TRUE)
  
  # 3.3) Qui-Quadrado (dados completos)
  output$chi_square_completa <- renderPrint({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      tabela <- table(dados$deteccao, dados[[paste0("num_empresa_", cnae_col)]])
      resultado <- chisq.test(tabela)
      
      if (resultado$p.value < 0.05) {
        HTML(
          paste0(
            "Pelo teste qui-quadrado de independência (Tabela Completa), ",
            "com nível de significância de 5%,<br>",
            "pode-se concluir que <strong>existe associação</strong> ",
            "entre a presença de acrilamida na água e ",
            "a existência de empresas com o CNAE selecionado."
          )
        )
      } else {
        HTML(
          paste0(
            "Pelo teste qui-quadrado de independência (Tabela Completa), ",
            "com nível de significância de 5%,<br>",
            "pode-se concluir que <strong>não existe associação</strong> ",
            "entre a presença de acrilamida na água e ",
            "a existência de empresas com o CNAE selecionado."
          )
        )
      }
    }
  })
  
  # 3.4) Tabela Frequência (apenas dados consistentes)
  output$tabela_consistentes <- renderTable({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      dados_consistentes <- dados  |>  filter(consistente == 1)
      
      tabela <- table(dados_consistentes$deteccao, 
                      dados_consistentes[[paste0("num_empresa_", cnae_col)]])
      proporcoes <- prop.table(tabela, margin = 1)
      tabela_final <- cbind(
        as.data.frame.matrix(tabela),
        Proporção = round(proporcoes[, 2], 2)
      )
      colnames(tabela_final) <- c("sem detecção", "com detecção", "Proporção")
      rownames(tabela_final) <- c("sem empresas no CNAE", "com empresas no CNAE")
      tabela_final
    }
  }, rownames = TRUE)
  
  # 3.5) Qui-Quadrado (dados consistentes)
  output$chi_square_consistentes <- renderPrint({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      dados_consistentes <- dados |> filter(consistente == 1)
      tabela <- table(dados_consistentes$deteccao, 
                      dados_consistentes[[paste0("num_empresa_", cnae_col)]])
      resultado <- chisq.test(tabela)
      
      if (resultado$p.value < 0.05) {
        HTML(
          paste0(
            "Pelo teste qui-quadrado de independência (Tabela Completa), ",
            "com nível de significância de 5%,<br>",
            "pode-se concluir que <strong>existe associação</strong> ",
            "entre a presença de acrilamida na água e ",
            "a existência de empresas com o CNAE selecionado."
          )
        )
      } else {
        HTML(
          paste0(
            "Pelo teste qui-quadrado de independência (Tabela Completa), ",
            "com nível de significância de 5%,<br>",
            "pode-se concluir que <strong>não existe associação</strong> ",
            "entre a presença de acrilamida na água e ",
            "a existência de empresas com o CNAE selecionado."
          )
        )
      }
    }
  })
  
 
  output$grafico_dispersao_interativo <- renderPlotly({
    cnae_col <- input$cnae_var
    
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      
   
      dados_consistentes <- dados |>
        filter(consistente == 1) |>
        rowwise() |>
        mutate(num_cnaes = sum(c_across(starts_with("cnae_")))) |>
        ungroup() |>
        mutate(
          regiao = case_when(
            uf %in% c("AC","AM","AP","PA","RO","RR","TO") ~ "Norte",
            uf %in% c("AL","BA","CE","MA","PB","PE","PI","RN","SE") ~ "Nordeste",
            uf %in% c("DF","GO","MT","MS") ~ "Centro-Oeste",
            uf %in% c("ES","MG","RJ","SP") ~ "Sudeste",
            uf %in% c("PR","RS","SC") ~ "Sul",
            TRUE ~ "Outra"  
          )
        )
      
      # Gera o gráfico
      plot_ly(
        data = dados_consistentes,
        x = ~.data[[cnae_col]],
        y = ~Total_Detectados,
        type = "scatter",
        mode = "markers",
        color = ~regiao,
        colors = c("Norte" = "#458B00", "Nordeste" = "#EE7600","Centro-Oeste" = "#E31A1C",
          "Sudeste" = "#912CEE", "Sul" = "#1C86EE", "Outra" = "#B15928"
        ),
        
        # Tooltip
        text = ~paste(
          "Município:", municipio,
          "<br>Código IBGE:", codigo_ibge,
          "<br>UF:", uf,
          "<br>Empresas com CNAEs:", num_cnaes,
          "<br>Total Detectados:", Total_Detectados
        ),
        hoverinfo = "text",
        
        # Ajuste de aparência dos marcadores
        marker = list(size = 8)
      ) |>
        layout(
          title = "Gráfico de Dispersão (Consistentes)",
          xaxis = list(title = cnae_col),
          yaxis = list(title = "Total Detectados"),
          
          # Ajuste do tamanho da fonte na legenda
          legend = list(
            font = list(size = 10)
          )
        )
    }
  })
}

# 4) RODAR O APP -------------------------------------------------------------
shinyApp(ui = ui, server = server)
