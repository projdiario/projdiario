# server.R visualizador
portarias <- readRDS('../validador/normas2_teste.RDS') 
lidos <- readRDS('../validador/log.RDS') 
lidos <- lidos[!duplicated(lidos$id), ]
id_lidos <- lidos$id[lidos$validacao == 'Aceito']
# id_lidos <- id_lidos[!duplicated(id_lidos)]
portarias <- portarias[portarias$ID_LEGISLACAO %in% id_lidos & !duplicated(portarias$ID_LEGISLACAO), ]
portarias <- merge(portarias, lidos[lidos$id %in% id_lidos, ],
                   by.x = 'ID_LEGISLACAO', by.y = 'id', all.x = TRUE)
# portarias$DS_RESUMO <- portarias$texto

# mockingInput
# input <- list()
# input$numero_busca = ''
# input$data_busca = c(as.Date('2010-01-01'), as.Date('2022-12-31'))
# input$orgao_busca <- ''
# input$termo_busca = ''
# input$tipo_ato = 1
# input$numero_portaria = ''

shinyServer(function(session, input, output) { 
  
  lista <- reactive({ 
    logi_tipo <- if (is.null(input$tipo_ato)) { 
      portarias$ID_TIPO_LEGISLACAO %in% c(1, 6, 17) 
    } else { 
      portarias$ID_TIPO_LEGISLACAO %in% input$tipo_ato 
    } 
    
    logi_data <- portarias$DT_PUBLICACAO >= input$data_busca[1] & 
      portarias$DT_PUBLICACAO <= input$data_busca[2] 
    
    logi_termo <- grepl(pattern = tolower(input$termo_busca), 
                        x = tolower(portarias$DS_CONTEUDO)) 
    
    # logi_orgao <- grepl(pattern = tolower(input$orgao_busca), 
    #                     x = tolower(portarias$orgao)) 
    logi_orgao <- TRUE 
    
    logi_num <- if (input$numero_busca == '') { 
      TRUE 
    } else { 
      portarias$NU_LEGISLACAO %in% input$numero_busca
    } 
    
    logico <- logi_termo & logi_data & logi_tipo & logi_orgao & logi_num 
    !is.na(logico) & logico
  }) 
  
  botao <- eventReactive(eventExpr = input$buscar, { 
    ids <- lista()   
    
    escolhas <- paste('Norma Nº', portarias$NU_LEGISLACAO[ids], 'de', 
                      format(portarias$DT_LEI[ids], format = "%d de %B de %Y")) 
    
    updateSelectInput(session = session, 
                      inputId = "numero_portaria", 
                      choices = escolhas) 
  } 
  ) 
  
  quantidade_resultados <- eventReactive(eventExpr = input$buscar, { 
    
    n_resultado <- sum(lista()) 
    
    if (n_resultado == 0) "Nenhum resultado foi encontrado. Tente outra busca." 
    else if (n_resultado == 1) "Apenas um resultado foi encontrado." 
    else paste("A busca atual encontrou", n_resultado, "resultados.") 
  }) 
  
  output$n_resultados <- renderText({ 
    quantidade_resultados() 
  }) 
  
  output$texto_portaria <- renderUI({ 
    botao() 
    escolhas <- paste('Norma Nº', portarias$NU_LEGISLACAO, 'de',
                      format(portarias$DT_LEI, format = "%d de %B de %Y")) 
    HTML(portarias$texto[escolhas == input$numero_portaria]) 
  }) 
}) 
