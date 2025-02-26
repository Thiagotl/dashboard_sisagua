library(shiny)
library(shinythemes)
library(dplyr)

# Carregar os dados corretamente
dados <- read.csv("planilha_acrilamida.csv", sep=";", stringsAsFactors = FALSE)
descricoes <- read.csv("descri.csv", sep=",", stringsAsFactors = FALSE)

# Ajustar nomes das colunas e converter para string corretamente
colnames(descricoes) <- tolower(trimws(colnames(descricoes)))
descricoes <- descricoes %>% mutate(cnae = trimws(as.character(descricoes$cnae)))


# Função para generalizar a criação de num_empresa
criar_num_empresa <- function(df) {
  cnae_cols <- names(df)[grepl("^cnae_", names(df))]
  df <- df |> 
    mutate(across(all_of(cnae_cols), ~ ifelse(. > 0, 1, 0), .names = "num_empresa_{.col}")) |> 
    mutate(deteccao = ifelse(Total_Detectados > 0, 1, 0))
  return(df)
}

# Aplicar a função aos dados
dados <- criar_num_empresa(dados)

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
      tableOutput("tabela_freq"),
      verbatimTextOutput("chi_square")
    )
  )
)

# Server
server <- function(input, output) {
  output$tabela_freq <- renderTable({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      tabela <- table(dados$deteccao, dados[[paste0("num_empresa_", cnae_col)]])
      proporcoes <- prop.table(tabela, margin = 1)
      tabela_final <- cbind(as.data.frame.matrix(tabela), Proporção = round(proporcoes[, 2], 2))
      colnames(tabela_final) <- c("com detecção", "sem detecção", "Proporção")
      rownames(tabela_final) <- c("sem empresas no CNAE", "com empresas no CNAE")
      tabela_final
    }
  }, rownames = TRUE)
  
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
  
  output$chi_square <- renderPrint({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col) && cnae_col %in% names(dados)) {
      tabela <- table(dados$deteccao, dados[[paste0("num_empresa_", cnae_col)]])
      dimnames(tabela) <- list(
        "CNAE" = c("sem empresas no CNAE", "com empresas no CNAE"),
        "Detecção" = c("com detecção", "sem detecção")
      )
      resultado <- chisq.test(tabela)
      
      if (resultado$p.value < 0.05) {
        cat("Pelo teste qui-quadrado de independência, com nível de significância de 5%,\n",
            "pode-se concluir que existe associação entre a presença de acrimilada na água e, \n", 
            "a existência de empresas com o CNAE selecionado. \n")
      } else {
        cat("Pelo teste qui-quadrado de independência, com nível de significância de 5%, \n",
            "não há presença de associação entre as variáveis.\n")
      }
    }
  })
}

# Rodar app
shinyApp(ui = ui, server = server)



#rsconnect::deployApp("/home/thiago/Documentos/dashboard_sisagua/dashboard", appFiles = c("app.R", "planilha_acrilamida.csv","descri.csv"))
