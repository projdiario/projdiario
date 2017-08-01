fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode),
  tags$style(
    "
    table {
    border: 1px solid black;
    border-collapse: collapse;
    width: 100%;
    }
    th, td {
    border: 1px solid black;
    padding: 8px;
    text-align: left;
    }
    tr:hover{background-color:#f5f5f5}"
  ),
  tags$div(
    style = "position: fixed; top: 0; width: 100%; background: #c2cff6; padding: 5px; z-index: 10;",
    align = "center",
    actionButton("nova", "Inserir nova norma", style = "float: left;"),
    actionButton("aceita", "Aceitar",
                 style = "background: lightgreen; border-color: lightgreen; margin-right: 10px;"),
    actionButton("pula", "Decidir depois", 
                 style = "margin-right: 10px;"),
    actionButton("recusa", "Recusar", 
                 style = "background: lightcoral; border-color: lightcoral;")
  ),
  tags$div(
    class = "main", style = "margin-top: 50px",
    fluidRow(style = "margin-top: 50px;", 
             HTML('<hr style="margin-top: 0px;margin-bottom: 5px;">'),
             column(4, textOutput('pag'), align = "left"),
             column(8, textOutput('info'), align = "right", style = "float: right")
    ),
    HTML('<hr style="margin-top: 5px; margin-bottom: 5px;">'),
    fluidRow(
      column(6,
             uiOutput('meta_info'),
             uiOutput('editor')
      ),
      column(6, uiOutput('pdf'),
             fluidRow(style = 'display: -webkit-box; margin-top: -570px; margin-left: 15px;',
                      div(style = 'display: inherit; ',
                          numericInput('pag_pdf', 'Página:  ', value = 1, min = 1, width = '50%')
                      )
             )
      )
    )
  ),
  fluidRow(textInput('entrada', 'Teste de texto', ''), style = 'display:none;')
)