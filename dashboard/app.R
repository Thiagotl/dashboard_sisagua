library(shiny)
library(shinythemes)
library(tidyverse)

# Carregar os dados corretamente
dados <- read.csv("planilha_acrilamida.csv", sep=",", stringsAsFactors = FALSE)
descricoes <- read.csv("descri.csv", sep=",", stringsAsFactors = FALSE)

# Ajustar nomes das colunas e converter para string corretamente
colnames(descricoes) <- tolower(trimws(colnames(descricoes)))
descricoes <- descricoes %>% mutate(cnae = trimws(as.character(descricoes$cnae)))

# Ajustar nomes das colunas e converter valores
colnames(dados) <- trimws(colnames(dados))  # Remove espaços extras
dados$Total_Detectados <- as.numeric(dados$Total_Detectados)

# Função para generalizar a criação de num_empresa
dados <- dados |> 
  mutate(across(starts_with("cnae_"), ~ ifelse(. > 0, 1, 0), .names = "num_empresa_{.col}")) |> 
  mutate(deteccao = ifelse(Total_Detectados > 0, 1, 0))

# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Dashboard CNAE - Parâmetro Acrilamida"),
  sidebarLayout(
    sidebarPanel(
      selectInput("cnae_var", "Escolha um CNAE:", 
                  choices = names(dados)[grepl("^cnae_", names(dados))], selected = names(dados)[grepl("^cnae_", names(dados))][1]),
      br(),
      textOutput("descricao_geral"),
      textOutput("descricao_especifica")
    ),
    mainPanel(
      h3("Tabela Completa"),
      tableOutput("tabela_freq"),
      verbatimTextOutput("chi_square_completa"),
      h3("Tabela Apenas Consistentes"),
      tableOutput("tabela_consistentes"),
      verbatimTextOutput("chi_square_consistentes")
    )
  )
)

# Server
server <- function(input, output) {
  output$descricao_geral <- renderText({
    cnae_col <- as.character(input$cnae_var)
    descricao <- descricoes %>% filter(trimws(cnae) == trimws(cnae_col)) %>% pull(denominacao_geral)
    if (length(descricao) > 0 && !is.na(descricao[1])) {
      paste("Descrição Geral: ", descricao[1])
    } else {
      "Descrição não encontrada"
    }
  })
  
  output$descricao_especifica <- renderText({
    cnae_col <- as.character(input$cnae_var)
    descricao <- descricoes %>% filter(trimws(cnae) == trimws(cnae_col)) %>% pull(denominacao_especifica)
    if (length(descricao) > 0 && !is.na(descricao[1])) {
      paste("Descrição Específica: ", descricao[1])
    } else {
      "Descrição não encontrada"
    }
  })
  
  output$tabela_freq <- renderTable({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      tabela <- table(dados$deteccao, dados[[paste0("num_empresa_", cnae_col)]])
      proporcoes <- prop.table(tabela, margin = 1)
      tabela_final <- cbind(as.data.frame.matrix(tabela), Proporção = round(proporcoes[, 2], 2))
      colnames(tabela_final) <- c("sem detecção", "com detecção", "Proporção")
      rownames(tabela_final) <- c("sem empresas no CNAE", "com empresas no CNAE")
      tabela_final
    }
  }, rownames = TRUE)
  
  output$chi_square_completa <- renderPrint({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      tabela <- table(dados$deteccao, dados[[paste0("num_empresa_", cnae_col)]])
      resultado <- chisq.test(tabela)
      
      if (resultado$p.value < 0.05) {
        cat("Pelo teste qui-quadrado de independência (Tabela Completa), com nível de significância de 5%,\n",
            "pode-se concluir que **existe associação** entre a presença de acrilamida na água e, \n", 
            "a existência de empresas com o CNAE selecionado. \n")
      } else {
        cat("Pelo teste qui-quadrado de independência (Tabela Completa), com nível de significância de 5%, \n",
            "pode-se concluir que **não existe associação** entre a presença de acrilamida na água e,\n",
            "a existência de empresas com o CNAE selecionado.\n")
      }
    }
  })
  
  output$chi_square_consistentes <- renderPrint({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      dados_consistentes <- dados %>% filter(consistente == 1)
      tabela <- table(dados_consistentes$deteccao, dados_consistentes[[paste0("num_empresa_", cnae_col)]])
      resultado <- chisq.test(tabela)
      
      if (resultado$p.value < 0.05) {
        cat("Pelo teste qui-quadrado de independência (Tabela Apenas Consistentes), com nível de significância de 5%,\n",
            "pode-se concluir que **existe associação** entre a presença de acrilamida na água e, \n", 
            "a existência de empresas com o CNAE selecionado. \n")
      } else {
        cat("Pelo teste qui-quadrado de independência (Tabela Apenas Consistentes), com nível de significância de 5%, \n",
            "pode-se concluir que **não existe associação** entre a presença de acrilamida na água e,\n",
            "a existência de empresas com o CNAE selecionado.\n")
      }
    }
  })
  
  output$tabela_consistentes <- renderTable({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      dados_consistentes <- dados %>% filter(consistente == 1)
      tabela <- table(dados_consistentes$deteccao, dados_consistentes[[paste0("num_empresa_", cnae_col)]])
      proporcoes <- prop.table(tabela, margin = 1)
      tabela_final <- cbind(as.data.frame.matrix(tabela), Proporção = round(proporcoes[, 2], 2))
      colnames(tabela_final) <- c("sem detecção", "com detecção", "Proporção")
      rownames(tabela_final) <- c("sem empresas no CNAE", "com empresas no CNAE")
      tabela_final
    }
  }, rownames = TRUE)
}

# Rodar app
shinyApp(ui = ui, server = server)