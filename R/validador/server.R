function(input, output, session) {
  valores <- reactiveValues()
  valores$num <- 1
  valores$usuario <- c(Sys.info()[["nodename"]], Sys.info()[["effective_user"]])
  valores$aval <- ''
  valores$html <- ''
  valores$aprovados <- readRDS('log.RDS')
  valores$nova <- 40000
  
  # reactive({
  #   valores$nova <- if(max(valores$aprovados$id, na.rm = TRUE) < '40000')
  #     '40000' else max(valores$aprovados$id, na.rm = TRUE) + 1
  # })
  
  observeEvent(input$aceita, {
    valores$aval <- "Aceito"
    js$pegaTexto()
  })
  
  observeEvent(input$pula, {
    valores$aval <- "Indefinido"
    js$pegaTexto()
  })
  
  observeEvent(input$recusa, {
    valores$aval <- "Recusado"
    js$pegaTexto()
  })
  
  observeEvent(input$entradaPrincipal, {
    if (input$entradaPrincipal != '') {
      valores$html <- gsub(pattern = '&.+{1,5};', replacement = '', input$entradaPrincipal)
      valores$aprovados <- rbind(valores$aprovados, inclui_dado(valores))
      saveRDS(object = valores$aprovados, 'log.RDS')
      valores$num <- valores$num + 1
    }
  })
  
  observeEvent(valores$num, {
    if (is.na(normas$NU_PAGINA[valores$num])) {
      updateNumericInput(session, 'pag_pdf', value = 1)
    } else {
      updateNumericInput(session, 'pag_pdf', value = normas$NU_PAGINA[valores$num])
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
    data_url <- paste0('&data=', format(observacao$DT_PUBLICACAO, format = '%d/%m/%Y') )
    fim <- '&captchafield=firistAccess'
    url <- paste0(base_url, jornal, pagina, data_url, fim)
    destino <- './www/pdfjs/web/pagina.pdf'
    httr::RETRY(verb = "GET", url = url, httr::write_disk(destino, overwrite = TRUE))    

    HTML(paste0('<iframe style="height:600px; width:100%" src="./pdfjs/web/viewer.html#zoom=125"></iframe>'))
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
             texto_em_html(normas$DS_CONTEUDO[valores$num]),
             '</textarea>',
             '</form>')
      )
    )
  })
  
  output$meta_info <- renderUI({
    observacao <- normas[valores$num, ]
    fluidRow(id = "norm-form",
      fluidRow(
        column(4, numericInput('num_norma', 'Número: ', observacao$NU_LEGISLACAO, 1)),
        column(4, numericInput('num_pag', 'Página: ', observacao$NU_PAGINA, 1)),
        column(4, numericInput('num_secao', 'SEÇÃO: ', observacao$ID_TIPO_SECAO, 1, 3))
        ),
      fluidRow(
        column(6, dateInput('data_dou', 'Data de publicação: ', observacao$DT_PUBLICACAO,
                  format = 'dd/mm/yyyy')),
        column(6, dateInput('data_norma', 'Data da norma: ', observacao$DT_LEI,
        format = 'dd/mm/yyyy'))
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
    shinyjs::alert('Norma incluída')
    if (input$entradaNova != '' &  valores$aval == 'Aceito') {
      valores$html <- input$entradaNova
      valores$aprovados <- rbind(valores$aprovados, inclui_dado(valores, valores$nova))
      saveRDS(object = valores$aprovados, 'log.RDS')
    }
    valores$nova <- valores$nova + 1
    removeModal(session = session)
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}