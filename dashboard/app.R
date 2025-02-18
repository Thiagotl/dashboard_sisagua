library(shiny)
library(shinythemes)
library(dplyr)

# Carregar os dados corretamente
dados <- read.csv("planilha_acrilamida.csv", sep=";", stringsAsFactors = FALSE)

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
                  choices = names(dados)[grepl("^cnae_", names(dados))])
    ),
    mainPanel(
      tableOutput("tabela_prop"),
      verbatimTextOutput("chi_square")
    )
  )
)

# Server
server <- function(input, output) {
  output$tabela_prop <- renderTable({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col)) {
      tabela <- prop.table(table(dados[[paste0("num_empresa_", cnae_col)]], dados$deteccao), 1)
      colnames(tabela) <- c("sem empresas no CNAE", "com empresas no CNAE")
      rownames(tabela) <- c("com detecção", "sem detecção") # arrumar aqui
      as.data.frame.matrix(tabela)
    }
  }, rownames = TRUE)
  
  output$chi_square <- renderPrint({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col)) {
      tabela <- table(dados[[paste0("num_empresa_", cnae_col)]], dados$deteccao)
      dimnames(tabela) <- list(
        "CNAE" = c("com detecção", "sem detecção"),
        "Detecção" = c("sem empresas no CNAE", "com empresas no CNAE")
      )
      resultado <- chisq.test(tabela)
      
      if (resultado$p.value < 0.05) {
        cat("Pelo teste qui-quadrado de independência, com nível de significância de 5%,\n",
        "há presença de associação entre as variáveis. \n")
      } else {
        cat("Pelo teste qui-quadrado de independência, com nível de significância de 5%, \n",
        "não há presença de associação entre as variáveis.\n")
      }
    }
  })
}

# Rodar app
shinyApp(ui = ui, server = server)

# rsconnect::deployApp("/home/thiago/Documentos/dashboard_sisagua/dashboard", appFiles = c("app.R", "planilha_acrilamida.csv"))
