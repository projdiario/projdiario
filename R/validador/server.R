function(input, output, session) {
  valores <- reactiveValues()
  valores$num <- 1
  valores$usuario <- c(Sys.info()[["nodename"]], Sys.info()[["effective_user"]])
  valores$validacao <- ''
  valores$html <- ''
  valores$nova <- ifelse(length(lidas) == 0, 1000000, max(as.numeric(lidas)) + 1)
  
  shinyjs::onclick("toggleMeta",
                   shinyjs::toggle(id = "norm-form", anim = TRUE))  
  
  observeEvent(input$aceita, {
    if (is.na(input$num_norma) | !is.numeric(input$num_norma)) {
      shinyjs::alert("Por favor inclua um número para a norma.\nSiga as regras do sistema")
    } else {
      valores$validacao <- "Aceito"
      js$pegaTexto()
    }
  })
  
  observeEvent(input$pula, {
    conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
    valores$validacao <- "Indefinido"
    registrar_log(valores, normas$ID[valores$num], conexao)
    valores$num <- prox_num(valores$num, conexao, valores$usuario[[2]])
    RJDBC::dbDisconnect(conexao)
  })
  
  observeEvent(input$recusa, {
    conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
    valores$validacao <- "Recusado"
    registrar_log(valores, normas$ID[valores$num], conexao)
    valores$num <- prox_num(valores$num, conexao, valores$usuario[[2]])
    RJDBC::dbDisconnect(conexao)
  })
  
  observeEvent(input$entradaPrincipal, {
    if (input$entradaPrincipal != '') {
      conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
      escrever_na_base(input, conexao)
      registrar_log(valores, normas$ID[valores$num], conexao)
      valores$num <- prox_num(valores$num, conexao, valores$usuario[[2]])
      RJDBC::dbDisconnect(conexao)
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
    fim <- '&captchafield=firstAccess'
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
        column(6, textAreaInput('des_titulo', 'Título da norma: ', observacao$DES_TITULO,
                                resize = 'vertical'))
      ),
      fluidRow(
        column(12, textAreaInput('txt_ementa', 'Ementa: ', gsub(" ?</?p> ?", "",observacao$TXT_EMENTA),
                                 resize = 'vertical'))
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
          column(6, textAreaInput('des_titulo_nova', 'Título da norma: ', resize = 'vertical'))
        ),
        fluidRow(
          column(12, textAreaInput('txt_ementa_nova', 'Ementa: ', resize = 'vertical'))
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
      conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
      escrever_na_base(lista, conexao)
      registrar_log(valores, valores$nova, conexao)
      RJDBC::dbDisconnect(conexao)
      valores$nova <- valores$nova + 1
      removeModal(session)
      shinyjs::alert('Norma incluída!')
    }
    
  })
  
  observeEvent(input$alteracao, {
    showModal(
      modalDialog(size = 'l', easyClose = TRUE, uiOutput('editorAlteracao'))
    )
  })
  
  output$editorAlteracao <- renderUI({
    conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
    valores$ato_agricultura <- RJDBC::dbGetQuery(conexao, 'SELECT * FROM ADMLEGIS.ATO_AGRICULTURA')
    RJDBC::dbDisconnect(conexao)

    valores$nova
    div(
      fluidRow(
        column(4, numericInput('num_norma_alteracao', 'Número: ', 0, 1, 31122099)),
        column(4, id = 'col_data', selectInput('data_dou_alteracao', 'Ano de publicação: ', unique(valores$ato_agricultura$VLR_ANO), '')),
        column(4, id = 'col_tipo', selectInput('sgl_tipo_alteracao', 'Tipo: ', unique(valores$ato_agricultura$SGL_TIPO), ''))
      ),
      fluidRow(
        column(8, id = 'col_orgao', selectInput('sgl_orgao_alteracao', 'Sigla do órgão: ', unique(valores$ato_agricultura$SGL_ORGAO), '')),
        column(4, actionButton('btn_busca_alteracao', 'Buscar', icon = icon('search')))
      ),
      shinyjs::hidden(
        fluidRow(id = 'linha_titulo',
                 column(8, selectInput('seq_ato_alteracao', 'Sigla do órgão: ', unique(valores$ato_agricultura$SGL_ORGAO), '')),
                 column(4, actionButton('btn_textos_alteracao', 'Selectionar esta norma'))
        )
      ),
      shinyjs::hidden(
        div(id = 'textos_ato',
            uiOutput('textos_alteracao')
        )
      )
    )
  })
  
  observeEvent(input$btn_busca_alteracao, {
    conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
    query <- "SELECT  SEQ_ATO, DES_TITULO, TXT_EMENTA FROM ADMLEGIS.ATO
                      WHERE NUM_ATO = :1 AND VLR_ANO = :2 AND SGL_ORGAO = :3 AND SGL_TIPO = :4"
    valores$ato <<- RJDBC::dbGetQuery(conexao, query, meu_formato(input$num_norma_alteracao, 8),
                              input$data_dou_alteracao, input$sgl_orgao_alteracao,
                              input$sgl_tipo_alteracao)
    RJDBC::dbDisconnect(conexao)
    opcoes <- valores$ato$SEQ_ATO
    names(opcoes) <- valores$ato$DES_TITULO
    updateSelectInput(session, 'seq_ato_alteracao', choices = opcoes)
    
    shinyjs::show('linha_titulo')
    
    # if (!is.na(input$num_norma_alteracao) && !is.null(input$num_norma_alteracao) && input$num_norma_alteracao != 0) {
    #   valores$possiveis1 <- valores$ato[as.numeric(valores$ato$NUM_ATO) == input$num_norma_alteracao , ]
    #   updateSelectInput(session, 'data_dou_alteracao', choices = na.omit(valores$possiveis1$VLR_ANO))
    #   shinyjs::show('col_data')
    # } else {
    #   shinyjs::hide('col_data')
    #   updateSelectInput(session, 'data_dou_alteracao', selected = '')
    # }
  })
  
  observeEvent(input$btn_textos_alteracao, shinyjs::show('textos_ato'))
  
  output$textos_alteracao <- renderUI({
    conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
    query <- "SELECT  TXT_TEXTO FROM ADMLEGIS.ITEM_ATO
                      WHERE NUM_ATO = :1 AND VLR_ANO = :2 AND SGL_ORGAO = :3 AND
                          SGL_TIPO = :4 AND SEQ_ATO = :5"
    valores$item_ato <<- RJDBC::dbGetQuery(conexao, query, meu_formato(input$num_norma_alteracao, 8),
                                      input$data_dou_alteracao, input$sgl_orgao_alteracao,
                                      input$sgl_tipo_alteracao, input$seq_ato_alteracao)
    RJDBC::dbDisconnect(conexao)
    indice <- which(valores$ato$SEQ_ATO == input$seq_ato_alteracao)[[1]]
    div(
      tags$script(src = "./js/editor.js"),
      tags$script('editor("#textoAlteracao")'),
      fluidRow(
        column(6, textAreaInput('txt_ementa_alteracao', 'Ementa: ', resize = 'vertical',
                                 enc2native(valores$ato$TXT_EMENTA[[indice]]))),
        column(6, textInput('des_titulo_alteracao', 'Título do órgão: ',
                                enc2native(valores$ato$DES_TITULO[[indice]])))
      ),
      HTML('<form id="formularioAlteracao" method="post">',
           '<textarea id="textoAlteracao" style = "resize:vertical;">',
           valores$item_ato$TXT_TEXTO[[1]],
           '</textarea>',
           '</form>'
      ),
      actionButton('btn_alteracao', 'Enviar alterações')
    )
  })
  
  observeEvent(input$btn_alteracao, {
    valores$validacao <- 'Aceito'
    js$alteraNorma()
  })
  
  observeEvent(input$entradaAlteracao, {
    if (input$entradaAlteracao != '' &  valores$validacao == 'Aceito') {
      conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
      RJDBC::dbSendUpdate(conexao, 'UPDATE ADMLEGIS.ITEM_ATO SET TXT_TEXTO = :1
                                        WHERE NUM_ATO = :2 AND VLR_ANO = :3 AND SGL_ORGAO = :4 AND
                                            SGL_TIPO = :5 AND SEQ_ATO = :6',
                          input$entradaAlteracao, meu_formato(input$num_norma_alteracao, 8),
                          input$data_dou_alteracao, input$sgl_orgao_alteracao,
                          input$sgl_tipo_alteracao, input$seq_ato_alteracao)
      
      RJDBC::dbSendUpdate(conexao, 'UPDATE ADMLEGIS.ATO
                                        SET TXT_EMENTA = :1,  DES_TITULO = :2
                                        WHERE NUM_ATO = :3 AND VLR_ANO = :4 AND SGL_ORGAO = :5 AND
                                            SGL_TIPO = :6 AND SEQ_ATO = :7',
                          input$txt_ementa_alteracao, input$des_titulo_alteracao,
                          meu_formato(input$num_norma_alteracao, 8),
                          input$data_dou_alteracao, input$sgl_orgao_alteracao,
                          input$sgl_tipo_alteracao, input$seq_ato_alteracao)
      registrar_log(valores, valores$nova, conexao)
      
      RJDBC::dbCommit(conexao)
      RJDBC::dbDisconnect(conexao)
      
      valores$nova <- valores$nova + 1
      removeModal(session)
      shinyjs::alert('Norma alterada!')
    }
    
  })
  
  session$onSessionEnded(function() {
    RJDBC::dbDisconnect(conexao)
    stopApp()
  })
}