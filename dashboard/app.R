library(shiny)
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
  titlePanel("Dashboard CNAE"),
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
      as.data.frame.matrix(tabela)
    }
  }, rownames = TRUE)
  
  output$chi_square <- renderPrint({
    cnae_col <- input$cnae_var
    if (!is.null(cnae_col)) {
      tabela <- table(dados[[paste0("num_empresa_", cnae_col)]], dados$deteccao)
      chisq.test(tabela)
    }
  })
}

# Rodar app
shinyApp(ui = ui, server = server)
