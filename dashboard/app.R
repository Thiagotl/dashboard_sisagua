library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Carregar os dados
df <- read.csv("planilha_acrilamida.csv", stringsAsFactors = FALSE)

# Ajustar os nomes das colunas para remover espaços invisíveis
colnames(df) <- trimws(colnames(df))

# Obter opções únicas para os select inputs
municipios <- unique(df$municipio)
cnae_colunas <- grep("cnae_", names(df), value = TRUE)
cnaes <- c("Todos", cnae_colunas)  # Opção para selecionar todos os CNAEs

# Função para calcular a tabela de proporção
calcular_tabela_proporcao <- function(df, cnae) {
  df <- df |> 
    mutate(
      num_empresa = ifelse(.data[[cnae]] > 0, 1, 0),  # Verifica se há empresas no CNAE
      deteccao = ifelse(`Total de Consistentes detectados Acima do VMP` > 0, 1, 0)  # Detecção de acrilamida
    )
  
  prop_table <- prop.table(table(df$num_empresa, df$deteccao), 1)
  
  # Converter para um data frame de forma correta
  df_prop <- as.data.frame.matrix(prop_table)
  df_prop <- tibble::rownames_to_column(df_prop, var = "Empresa CNAE")
  
  colnames(df_prop) <- c("Empresa CNAE", "Sem Detecção", "Com Detecção")
  
  return(df_prop)
}

# UI - Interface do usuário
ui <- fluidPage(
  titlePanel("Dashboard - Análise de Acrilamida"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("municipio", "Selecionar Município:", choices = c("Todos", municipios), selected = "Todos", multiple = FALSE, options = list(maxOptions = 10)),
      selectInput("cnae", "Selecionar CNAE:", choices = cnaes, selected = "Todos"),
      actionButton("filtrar", "Aplicar Filtros")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico", plotOutput("grafico")),
        tabPanel("Tabela de Proporção", DTOutput("tabela_proporcao"))
      )
    )
  )
)

# Server - Lógica do servidor
server <- function(input, output, session) {
  # Filtrar dados com base nos inputs
  dados_filtrados <- reactive({
    req(input$filtrar)  # Só filtra quando o botão é pressionado
    df_filtrado <- df
    
    if (input$municipio != "Todos") {
      df_filtrado <- df_filtrado %>%
        filter(municipio == input$municipio)
    }
    
    if (input$cnae != "Todos") {
      df_filtrado <- df_filtrado %>%
        filter(.data[[input$cnae]] > 0)
    }
    
    df_filtrado
  })
  
  # Renderizar gráfico de testes por município
  output$grafico <- renderPlot({
    df_plot <- dados_filtrados() %>%
      group_by(municipio) %>%
      summarise(total_testes = sum(`Total de testes substâncias em geral para cada linha - incluindo MENOR_LQ`, na.rm = TRUE)) %>%
      arrange(desc(total_testes)) %>%
      head(10)
    
    ggplot(df_plot, aes(x = reorder(municipio, total_testes), y = total_testes)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Municípios com Mais Testes", x = "Município", y = "Total de Testes") +
      theme_minimal()
  })
  
  # Renderizar a tabela de proporção para o CNAE selecionado
  output$tabela_proporcao <- renderDT({
    req(input$cnae != "Todos")  # Só processa se um CNAE específico for escolhido
    
    tabela <- calcular_tabela_proporcao(dados_filtrados(), input$cnae)
    datatable(as.data.frame(tabela), options = list(pageLength = 5))  # Convertendo explicitamente para DataFrame
  })
}

# Rodar o aplicativo
shinyApp(ui, server)
