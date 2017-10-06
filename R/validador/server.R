function(input, output, session) {
  valores <- reactiveValues()
  valores$num <- 1
  valores$usuario <- c(Sys.info()[["nodename"]], Sys.info()[["effective_user"]])
  valores$validacao <- ''
  valores$html <- ''
  valores$nova <- ifelse(length(lidas) == 0, 1000000, max(as.numeric(lidas)) + 1)
  
  observeEvent(input$aceita, {
    if (is.na(input$num_norma) | !is.numeric(input$num_norma)) {
      shinyjs::alert("Por favor inclua um número para a norma.\nSiga as regras do sistema")
    } else {
      valores$validacao <- "Aceito"
      js$pegaTexto()
    }
  })
  
  observeEvent(input$pula, {
    valores$validacao <- "Indefinido"
    registrar_log(valores, normas$ID[valores$num])
    valores$num <- valores$num + 1
  })
  
  observeEvent(input$recusa, {
    valores$validacao <- "Recusado"
    registrar_log(valores, normas$ID[valores$num])
    valores$num <- valores$num + 1
  })
  
  observeEvent(input$entradaPrincipal, {
    if (input$entradaPrincipal != '') {
      # valores$html <- gsub(pattern = '&.+{1,5};', replacement = '', input$entradaPrincipal)
      escrever_na_base(input, driver, configs)
      registrar_log(valores, normas$ID[valores$num])
      valores$num <- valores$num + 1
    }
  })
  
  observeEvent(valores$num, {
    if (is.na(normas$NUM_PAGINA[valores$num])) {
      updateNumericInput(session, 'pag_pdf', value = 1)
    } else {
      updateNumericInput(session, 'pag_pdf', value = normas$NUM_PAGINA[valores$num])
    }
  })
  
  output$pag <- renderText({
    paste("Norma", valores$num, "de", nrow(normas))
  })
  
  output$info <- renderText(
    paste("Você está logado em", valores$usuario[1], "como", valores$usuario[2])
  )
  
  output$pdf <- renderUI({
    observacao <- normas[valores$num, ]
    base_url <- 'http://pesquisa.in.gov.br/imprensa/servlet/INPDFViewer?'
    jornal <- paste0('jornal=', observacao$ID_TIPO_SECAO)
    pagina <- paste0('&pagina=', input$pag_pdf)
    data_url <- paste0('&data=', format(observacao$DTA_PROMULGACAO, format = '%d/%m/%Y') )
    fim <- '&captchafield=firistAccess'
    url <- paste0(base_url, jornal, pagina, data_url, fim)
    destino <- './www/pdfjs/web/pagina.pdf'
    httr::RETRY(verb = "GET", url = url, httr::write_disk(destino, overwrite = TRUE))    
    HTML('<iframe style="height:600px; width:100%" src="./pdfjs/web/viewer.html#zoom=125"></iframe>')

  })
  
  output$editor <- renderUI({
    input$nova
    valores$nova
    div(
      div(
        tags$script(src = "./js/tinymce/tinymce.min.js"),
        tags$script(src = "./js/editor.js"),
        tags$script('editor("#textoPrincipal")')
      ),
      div(
        HTML('<form id="formulario" method="post">',
             '<textarea id="textoPrincipal" style = "resize:vertical;">',
             gsub('<p>SECRETARIA</p>', paste0('<p>', normas$SGL_ORGAO[valores$num], '</p>'),
                  normas$TXT_TEXTO[valores$num]),
             '</textarea>',
             '</form>')
      )
    )
  })
  
  output$meta_info <- renderUI({
    observacao <- normas[valores$num, ]
    fluidRow(id = "norm-form",
      fluidRow(
        column(4, numericInput('num_norma', 'Número: ', as.numeric(observacao$NUM_ATO), 1)),
        column(4, dateInput('data_dou', 'Data de publicação: ', observacao$DTA_PROMULGACAO,
                            format = 'dd/mm/yyyy')),
        column(4, selectInput('sgl_tipo', 'Tipo: ', width = '100%',
                              choices =  unique(normas$SGL_TIPO), selected = observacao$SGL_TIPO))
        ),
      fluidRow(
        column(6, selectInput('sgl_orgao', 'Sigla do órgão: ',
                              siglas_possiveis, observacao$SGL_ORGAO)),
        column(6, textAreaInput('des_titulo', 'Título da norma: ', observacao$DES_TITULO))
      ),
      fluidRow(
        column(12, textAreaInput('txt_ementa', 'Ementa: ', gsub(" ?</?p> ?", "",observacao$TXT_EMENTA),
                                 cols = 200, rows = 2))
      )
    )
  })
  
  observeEvent(input$nova, {
    showModal(
      modalDialog(size = 'l', easyClose = TRUE,
        uiOutput('editorModal'),
        actionButton('btn_nova', 'Incluir'))
    )
  })
  
  output$editorModal <- renderUI({
    valores$nova
    div(
      div(
        fluidRow(
          column(4, numericInput('num_norma_nova', 'Número: ', 1, min =  1)),
          column(4, dateInput('data_dou_nova', 'Data de publicação: ', Sys.Date(), format = 'dd/mm/yyyy')),
          column(4, selectInput('sgl_tipo_nova', 'Tipo: ', width = '100%',
                                choices =  unique(normas$SGL_TIPO)))
        ),
        fluidRow(
          column(6, selectInput('sgl_orgao_nova', 'Sigla do órgão: ', siglas_possiveis)),
          column(6, textAreaInput('des_titulo_nova', 'Título da norma: '))
        ),
        fluidRow(
          column(12, textAreaInput('txt_ementa_nova', 'Ementa: ', cols = 200, rows = 2))
        )
      ),
      div(
        tags$script(src = "./js/editor.js"),
        tags$script('editor("#textoModal")'),
        HTML('<form id="formularioModal" method="post">',
             '<textarea id="textoModal" style = "resize:vertical;">',
             '</textarea>',
             '</form>'
        )
      )
    )
  })
  
  observeEvent(input$btn_nova, {
    valores$validacao <- 'Aceito'
    js$novaNorma()
  })
  
  observeEvent(input$entradaNova, {
    if (input$entradaNova != '' &  valores$validacao == 'Aceito') {
      valores$html <- input$entradaNova
      lista <- list(
        num_norma  = input$num_norma_nova,
        sgl_tipo = input$sgl_tipo_nova,
        data_dou = input$data_dou_nova,
        sgl_orgao = input$sgl_orgao_nova,
        entradaPrincipal = input$entradaNova,
        txt_ementa = input$txt_ementa_nova,
        des_titulo = input$des_titulo_nova
      )
      escrever_na_base(lista, driver, configs)
      registrar_log(valores, valores$nova)
      valores$nova <- valores$nova + 1
      removeModal(session)
      shinyjs::alert('Norma incluída!')
    }
    
  })
  
  observeEvent(input$alteracao, {
    showModal(
      modalDialog(size = 'l', easyClose = TRUE,
                  uiOutput('editorAlteracao'),
                  actionButton('btn_alteracao', 'Alterar'))
    )
  })
  
  output$editorAlteracao <- renderUI({
    conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
    base <- RJDBC::dbGetQuery(conexao, 'SELECT * FROM ADMLEGIS.ATO_AGRICULTURA')
    valores$ato <<- RJDBC::dbGetQuery(conexao, 'SELECT  NUM_ATO, SEQ_ATO, SGL_TIPO, VLR_ANO, SGL_ORGAO, DES_TITULO FROM ADMLEGIS.ATO')
    RJDBC::dbDisconnect(conexao)

    valores$nova
    div(
      div(
        fluidRow(
          column(4, numericInput('num_norma_alteracao', 'Número: ', 0, 1, 31122099)),
          shinyjs::hidden(column(4, id = 'col_data',
            selectInput('data_dou_alteracao', 'Ano de publicação: ', unique(base$VLR_ANO), ''))
            ),
          shinyjs::hidden(column(4, id = 'col_tipo',
            selectInput('sgl_tipo_alteracao', 'Tipo: ', unique(base$SGL_TIPO), ''))
          )
        ),
        fluidRow(
          shinyjs::hidden(column(4, id = 'col_orgao',
            selectInput('sgl_orgao_alteracao', 'Sigla do órgão: ', unique(base$SGL_ORGAO), ''))
          ),
          shinyjs::hidden(column(8, id = 'col_titulo',
                                 textAreaInput('des_titulo_alteracao', 'Título do órgão: ', ''))
          )
        )
      ),
      shinyjs::hidden(
        div(id = 'textos_ato',
          tags$script(src = "./js/editor.js"),
          tags$script('editor("#textoAlteracao")'),
          fluidRow(
            column(12, textAreaInput('txt_ementa_alteracao', 'Ementa: ', cols = 200, rows = 2))
          ),
          HTML('<form id="formularioAlteracao" method="post">',
               '<textarea id="textoAlteracao" style = "resize:vertical;">',
               '</textarea>',
               '</form>'
          )
        )
      )
    )
  })
  
  observeEvent(input$num_norma_alteracao, {
    if (!is.na(input$num_norma_alteracao) && !is.null(input$num_norma_alteracao) && input$num_norma_alteracao != 0) {
      valores$possiveis1 <- valores$ato[as.numeric(valores$ato$NUM_ATO) == input$num_norma_alteracao , ]
      updateSelectInput(session, 'data_dou_alteracao', choices = na.omit(valores$possiveis1$VLR_ANO))
      shinyjs::show('col_data')
    } else {
      shinyjs::hide('col_data')
      updateSelectInput(session, 'data_dou_alteracao', selected = '')
    }
  })
  
  observeEvent(input$data_dou_alteracao, {
    if (input$data_dou_alteracao != '') {
      valores$possiveis2 <- valores$possiveis1[valores$possiveis1$VLR_ANO == input$data_dou_alteracao , ]
      updateSelectInput(session, 'sgl_tipo_alteracao', choices = na.omit(valores$possiveis2$SGL_TIPO))
      shinyjs::show('col_tipo')
    } else {
      shinyjs::hide('col_tipo')
      updateSelectInput(session, 'sgl_tipo_alteracao', selected = '')
    }
  })
  
  observeEvent(input$sgl_tipo_alteracao, {
    if (input$sgl_tipo_alteracao != '') {
      valores$possiveis3 <- valores$possiveis2[valores$possiveis2$SGL_TIPO == input$sgl_tipo_alteracao , ]
      updateSelectInput(session, 'sgl_orgao_alteracao', choices = na.omit(valores$possiveis3$SGL_ORGAO))
      shinyjs::show('col_orgao')
    } else {
      shinyjs::hide('col_orgao')
      updateSelectInput(session, 'sgl_orgao_alteracao', selected = '')
      
    }
  })
  
  observeEvent(input$sgl_orgao_alteracao, {
    if (input$sgl_orgao_alteracao != '') {
      valores$possiveis4 <- valores$possiveis3[valores$possiveis3$SGL_ORGAO == input$sgl_orgao_alteracao , ]
      updateTextAreaInput(session, 'des_titulo_alteracao', value = na.omit(valores$possiveis4$DES_TITULO))
      updateTextAreaInput(session, 'textoAlteracao', value = na.omit(valores$possiveis4$TXT_TEXT))
      shinyjs::show('col_titulo')
      shinyjs::show('textos_ato')
    } else {
      shinyjs::hide('col_titulo')
      shinyjs::hide('textos_ato')
      updateTextAreaInput(session, 'des_titulo_alteracao', value = '')
      updateTextAreaInput(session, 'textoAlteracao', value = '')
    }
  })
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}