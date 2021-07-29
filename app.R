#
#   TEST FILE
#


library(shiny)
library(plotly)
library(tidyverse)
library(scales)
library(lemon)
library(magrittr)




# IBGE
# "3512001", "4128104", "4203808", "4210001", "4212809",  "4213500"
IBGE <- c("4212809")




# saude bucal escovacoes faixa etaria
saude_bucal_escovacoes_faixa_etaria <- read.csv("vw_saude_bucal_escovacoes_faixa_etaria.csv", header = T, sep = ";", row.names = NULL)

saude_bucal_escovacoes_faixa_etaria <- cbind(index = rownames(saude_bucal_escovacoes_faixa_etaria), saude_bucal_escovacoes_faixa_etaria)

saude_bucal_escovacoes_faixa_etaria <- saude_bucal_escovacoes_faixa_etaria %>%
  filter(ibge %in% IBGE)

saude_bucal_escovacoes_faixa_etaria <- saude_bucal_escovacoes_faixa_etaria %>%
  separate(data_registro, c('data', 'hora'), " ") 


# saude bucal escovaceos
saude_bucal_escovacoes <- read.csv("vw_saude_bucal_escovacoes.csv", header = T, sep = ";", row.names = NULL)

## separa data
saude_bucal_escovacoes <- saude_bucal_escovacoes %>%
  separate(data_registro, c('data', 'hora'), " ") 

## filtro ibge
saude_bucal_escovacoes <- saude_bucal_escovacoes %>%
  filter(ibge %in% IBGE)


## formatacao data
saude_bucal_escovacoes %<>%
  mutate(data = as.Date(data, format = "%Y-%m-%d"))

## nome unidade fator
saude_bucal_escovacoes <- within(saude_bucal_escovacoes, 
                                 nome_unidade_saude <- factor(nome_unidade_saude, 
                                                              levels=names(sort(table(nome_unidade_saude), 
                                                                                decreasing=TRUE))))
# lista de UBSs
lista_UBS <- unique(saude_bucal_escovacoes$nome_unidade_saude)







ui <- fluidPage(
  
  # painel de filtro
  wellPanel(tags$h4("Filtros"),
            fluidRow(
              column(4, dateRangeInput("SELECTED_DATE", "Selecione o período:",
                                       start = 
                                         as.character(format(as.Date(min(saude_bucal_escovacoes$data))),"dd/mm/yyyy"), # Start 
                                       end = 
                                         as.character(format(as.Date(max(saude_bucal_escovacoes$data))),"dd/mm/yyyy"), # End 
                                       min = 
                                         as.character(format(as.Date(min(saude_bucal_escovacoes$data))),"dd/mm/yyyy"),
                                       max = 
                                         as.character(format(as.Date(max(saude_bucal_escovacoes$data))),"dd/mm/yyyy"),
                                       format = "dd/mm/yyyy",
                                       language = "pt-BR",
                                       separator = " até ")),
              column(8, selectInput(inputId = "UBS", label = "Selecione UBS", choices = lista_UBS, multiple = T, selected = lista_UBS))
                    )
            ),
  
  
  # bloco de totais/médias
  # tags$img(height = 100, width = 100, src = "1.png"),
  fluidRow(
    column(4, wellPanel(style = "background: white",
                        "Escovações Supervisionadas",
                        fluidRow(column(6 , align="center",tags$img(height = 100, width = 100, src = "1.png")),
                                 column(6, align="center", tags$h1(textOutput("ESCSUP"), tags$h5("Total de atividades coletivas")))
                        ))),
    
    column(4, wellPanel(style = "background: white", 
                        "Média de pessoas", 
                        fluidRow(column(6 , align="center",tags$img(height = 100, width = 100, src = "2.png")),
                                 column(6, align="center", tags$h1(textOutput("MED"), tags$h5("Média de pessoas por escovação")))
                        ))),
    
    column(4, wellPanel(style = "background: white", 
                        "Equipes realizando escovações", 
                        fluidRow(column(6 , align="center",tags$img(height = 100, width = 100, src = "3.png")),
                                 column(6, align="center", tags$h1(textOutput("EQ"), tags$h5("Total de equipes participantes")))
                        )))
  ),
  
  
  # bloco de gráficos
  fluidRow(
    column(8, wellPanel(style = "background: white", "Escovações Supervisionadas", plotOutput("BAR"))),
    column(4, wellPanel(style = "background: white", "Distribuição por sexo e faixa etária", plotlyOutput("PYR")))
  )
)








server <- function(input, output){
  
  
  ### FILTROS DE TABELA
  
  # filtra a tabela saude_bucal_escovacoes a partir de um IBGE selecionado
  sb_escovacoes_filtrada <- reactive({
    saude_bucal_escovacoes %>%
      filter((nome_unidade_saude %in% input$UBS) & (data >= input$SELECTED_DATE[1]) & (data <= input$SELECTED_DATE[2])) 
  })
  
  
  # filtra a tabela saude_bucal_escovacoes_faixa_etaria a partir de um IBGE selecionado  
  sb_esc_faixa_etaria_filtrada <- reactive({
    saude_bucal_escovacoes_faixa_etaria %>%
      filter((nome_unidade_saude %in% input$UBS) & (data >= input$SELECTED_DATE[1]) & (data <= input$SELECTED_DATE[2]))
  })
  
  
  
  
  ### BLOCOS
  
  # calcula o número de linhas da tabela filtrada saude_bucal_escovacoes
  # total de at. coletivas de escovação
  output$ESCSUP <- renderText({
    esc_sup <- nrow(sb_escovacoes_filtrada())
    esc_sup
  })
  
  
  # calcula a média de participantes de atividades coletivas a partir da tabela filtrada saude_bucal_escovacoes
  output$MED <- renderText({
    media_part <- sb_escovacoes_filtrada() %>%
      summarise_at("numero_participantes", mean, na.rm = T)
    media_part <- as.integer(media_part)
    media_part
  })
  
  
  # calcula o número de equipes participantes de atividades coletivas a partir da tabela filtrada saude_bucal_escovacoes  
  output$EQ <- renderText({
    n_equipes <- sb_escovacoes_filtrada() %>%
      distinct(ine) %>%
      nrow()
    n_equipes
  })
  
  
  
  
  ### GRÁFICOS
  
  # gráfico de barras 
  output$BAR <- renderPlot({
    
    ggplot(sb_escovacoes_filtrada() ,aes(x = nome_unidade_saude))  +
      geom_bar(fill = "#01B8AA") +
      scale_y_log10() +
      geom_text(aes(label = ..count..), stat = "count", colour = "white", vjust= 1.7, size = 6) +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.major.y = element_line(colour = "gray"),
            text = element_text(size = 15),
            axis.text.x = element_text(size = 15),
            #axis.line = element_line(colour = "gray")
      ) +
      scale_x_discrete(labels = wrap_format(20)) +
      labs(x = "", y ="Número de atividades coletivas\n", colour = "gray")
    
#    font = list(
#      color = "black"
#    )
    
#    label = list(
#      bgcolor = "white",
#      bordercolor = "gray",
#      font = font
#    )
    
   # ggplotly(bp, tooltip = c("text")) %>%
  #    style(hoverlabel = label)
  })
  
  
  # gráfico de faixa etária
  output$PYR <- renderPlotly({
    pp <- ggplot(sb_esc_faixa_etaria_filtrada(),
                 aes(nome_faixa_etaria, 
                     fill = nome_sexo)) +
      geom_bar(data = subset(sb_esc_faixa_etaria_filtrada(), 
                             nome_sexo == "Masculino"), 
               aes(y=..count..*(1),
                   text = paste0("Contagem: ", ..count..*(1))),
               fill = "#73A9C2") +
      geom_bar(data = subset(sb_esc_faixa_etaria_filtrada(), 
                             nome_sexo == "Feminino"),
               aes(y=..count..*(-1), 
                   text = paste0("Contagem: ", -..count..*(-1))),
               fill = "#FBA0E3") +
      scale_y_symmetric(labels = abs) +
      coord_flip() +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            text = element_text(size = 12),
            #axis.line = element_line(colour = "gray"),
            legend.position = "bottom", legend.title = element_blank()) +
      labs(x = "", y ="", colour = "nome_sexo")
    
    font = list(
      color = "black"
    )
    
    label = list(
      bgcolor = "white",
      bordercolor = "gray",
      font = font
    )
    
    ggplotly(pp, tooltip = c("text")) %>%
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2)) %>%
      style(hoverlabel = label) %>%
      config(displayModeBar = FALSE)
    
  })
  
} 


shinyApp(ui = ui, server = server)