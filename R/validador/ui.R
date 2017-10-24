positionNavbar <- 'window.onscroll = function() {moveNavbar()};
  function moveNavbar() {
var elmnt = document.body;
var hh = document.getElementById("navbar");
var y = elmnt.scrollTop;
var m = hh.scrollHeight;
if (y > m+15) {
document.getElementById("navbar").className = "nav-fix";
} else {
document.getElementById("navbar").className = "nav-rel";
}
};'

fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode),
  tags$link(href = 'validador.css', type = "text/css", rel = 'stylesheet', media = 'all'),
  tags$ul(id = "navbar", class = "nav-rel",
          tags$div(id = "nav-left",
                   tags$li(id = "webname",
                           tags$h3("Validador", style = "font-weight: bold; font-style: oblique;"),
                           tags$h6("Versão 1.0.0", style = "padding: 0px;")),
                   tags$li(actionButton("nova", "Inserir nova norma", icon = icon('file', lib = 'glyphicon')),
                           actionButton("alteracao", "Alterar norma", icon = icon('file', lib = 'glyphicon')))),
          tags$li(class = "right", textOutput('info'))
  ),
  tags$div(
    class = "main",
    fluidRow(
      column(6,
             textOutput('pag'),
             uiOutput('meta_info'),
             tags$div(id = "norm-btns",
                      actionButton("aceita", "Aceitar", class = "btn-custom1 green",
                                   icon = icon("ok", lib = "glyphicon")),
                      actionButton("pula", "Decidir depois", class = "btn-custom1",
                                   icon = icon("hourglass", lib = "glyphicon")),
                      actionButton("recusa", "Recusar", class = "btn-custom1 red",
                                   icon = icon("remove", lib = "glyphicon"))),
             uiOutput('editor')
      ),
      column(6, 
             fluidRow(id = "pag-pdf",
                      numericInput('pag_pdf', 'Página:', value = 1, min = 1)),
             uiOutput('pdf')
      )
    )
  ),
  fluidRow(textInput('entrada', 'Teste de texto', ''), style = 'display:none;'),
  tags$script(HTML(positionNavbar))
)