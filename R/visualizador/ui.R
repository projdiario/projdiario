# ui.R  visualizador
library(shiny) 

shinyUI(fluidPage( 
  tags$style('table, td { 
             border: 1px solid black; 
             } 
             tr:hover { 
             background-color: #f5f5f5 
             }'), 
  
  img(src = "BarraSuperior2000.png", height = "100%", width = "100%"), 
  
  tags$div(align = "center", 
           titlePanel("Ministério da Agricultura, Pecuária e Abastecimento"), 
           br(), 
           h4("Informe os parâmetros para a pesquisa:")), 
  
  fluidRow( 
    column(4, 
           textInput(inputId = "numero_busca", 
                     label = "Número:"), 
           br(), 
           dateRangeInput(inputId = "data_busca", 
                          label = "Período de publicação no D.O.U.:", 
                          start = Sys.Date() - 3650, end = Sys.Date(), 
                          format = "dd/M/yyyy", language = "pt-BR",  
                          separator = " até ") 
    ), 
    column(4, 
           textInput(inputId = "orgao_busca", 
                     label = "Órgão:"), 
           br(), 
           textInput(inputId = "termo_busca", 
                     label = "Palavra-chave:"), 
           br(), 
           br() 
    ), 
    column(4, 
           checkboxGroupInput(inputId = "tipo_ato",  
                              label = "Atos normativos mais pesquisados:", 
                              choices = list("LEI" = 1, "DECRETO" = 2, "ATO" = 3, 
                                             "ATA" = 3, "DECISÃO" = 4, "MEDIDA PROVISÓRIA" =5, 
                                             "DECRETO-LEI" = 6, "LEI COMPLEMENTAR" = 7, 
                                             "PORTARIA" = 17, "INSTRUÇÃO NORMATIVA" = 8, 
                                             "RESOLUÇÃO" = 9)) 
    ) 
  ), 
  
  tags$div(align = "center", 
           fluidRow( 
             column(1), 
             column(2, 
                    br(), 
                    actionButton(inputId = "buscar", label = "Buscar") 
             ), 
             column(4,  
                    selectInput(inputId = "numero_portaria", 
                                label = "Escolha a portaria que deseja visualizar:", 
                                choices = "", selected = "", width = "100%")), 
             column(2, 
                    br(), 
                    span(style = "color:darkgreen", 
                         textOutput("n_resultados"))), 
             column(2, 
                    br()#, 
                    # downloadButton("download", "Download")
                    ), 
             br()) 
  ), 
  
  fluidRow( 
    column(1), 
    column(10, 
           tags$div(style = "background-color:#e6eeff",  
                    br(), 
                    tags$div(align = "justify", 
                             style = "border:20px solid #e6eeff", 
                             htmlOutput("texto_portaria", inline = TRUE)  
                    ) 
           ), 
           br()
    ) 
  ),
  tags$div(align = "center", 
              p("Esta solução foi construída pelo", 
                tags$a(href = "http://www.agricultura.gov.br", 
                       "Ministério da Agricultura, Pecuária e Abastecimento",
                       target = 'blank'), "."), 
              img(src = 'marca.png', width = "276px", height = "73px"))
)) 