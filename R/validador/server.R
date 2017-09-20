function(input, output, session) {
  valores <- reactiveValues()
  valores$num <- 1
  valores$usuario <- c(Sys.info()[["nodename"]], Sys.info()[["effective_user"]])
  valores$validacao <- ''
  valores$html <- ''
  # valores$aprovados <- data.frame() # readRDS('base_final.RDS')
  valores$nova <- 88000
  
  observeEvent(input$aceita, {
    valores$validacao <- "Aceito"
    registrar_log(valores, normas$ID[valores$num])
    js$pegaTexto()
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
      valores$html <- gsub(pattern = '&.+{1,5};', replacement = '', input$entradaPrincipal)
      escrever_na_base(input, driver, configs)
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
    valores$aval <- 'Aceito'
    js$novaNorma()
  })
  
  observeEvent(input$entradaNova, {
    shinyjs::alert('Norma incluída!')
    if (input$entradaNova != '' &  valores$aval == 'Aceito') {
      valores$html <- input$entradaNova
      # valores$aprovados <- rbind(valores$aprovados, incluir_dado(nova = TRUE))
      # saveRDS(object = valores$aprovados, 'base_final.RDS')
    }
    valores$nova <- valores$nova + 1
    removeModal(session = session)
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}