rm(list=ls())
# shell('start firefox http://127.0.0.1:6312/')
cat('\014')

# Define funções
incluir_dado <- function(valores, id_norma) {
  if (missing(id_norma)) {
    id_norma <-  normas$ID_LEGISLACAO[valores$num]
  }
  
  df <- data.frame(
    id = id_norma, #texto = valores$html,
    validacao = valores$aval,
    maquina = valores$usuario[1], usuario = valores$usuario[2],
    data = format(Sys.time(), format = "%d/%m/%Y %H:%M:%S"),
    stringsAsFactors = FALSE)
  df
}

incluir_registro <- function() {
  df <- data.frame(ID_LEGISLACAO = max(base_final$ID_LEGISLACAO) + 1, DS_RESUMO = input$resumo,
                   DT_PUBLICACAO = input$data_dou, NU_LEGISLACAO = input$num_norma,
                   DS_CONTEUDO = valores$html, DT_LEI = input$data_norma, NU_PAGINA = input$num_pag,
                   DS_INDEXACAO = NA, ID_TIPO_LEGISLACAO = input$tipo, ID_TIPO_SITUACAO = 0,
                   CD_TIPO_LIBERACAO = 0, ID_MODO_PUBLICACAO = 0, ID_USUARIO_CADASTRO = 0,
                   ID_USUARIO_LIBERACAO = 0, ID_TIPO_SECAO = input$num_secao, DT_CADASTRO = Sys.Date(),
                   NU_PUBLICACAO = 0, NU_VOLUME = 0, stringsAsFactors = FALSE)
  df
}

texto_em_html <- function(texto) {
  html <- gsub("\\n<" , "\r<", texto)
  html <- gsub("\\n\\s" , "\n", html)
  html <- gsub("\\n" , "</p><p>\n", html)
  html <- paste("<p>", "<p>MINISTÉRIO DA AGRICULTURA, PECUÁRIA E ABASTECIMENTO</p>",
                 "<p>SECRETARIA</p>", gsub("\\r" , "", html), "</p>")
  html
}

jsCode <<- "shinyjs.pegaTexto = function() {
             elem = document.getElementById('textoPrincipal_ifr').contentDocument.getElementById('tinymce');
             Shiny.onInputChange('entradaPrincipal', elem.innerHTML);
            };
            shinyjs.novaNorma = function() {
              elem = document.getElementById('textoModal_ifr').contentDocument.getElementById('tinymce');
              Shiny.onInputChange('entradaNova', elem.innerHTML);

            };"

# Começa a brincadeira
normas <- readRDS('normas2_teste.RDS')

if (file.exists('log.RDS')) {
  lidos <- readRDS('log.RDS')
  resolvidos <- lidos$id[lidos$validacao == 'Aceito' | lidos$validacao == 'Recusado']
  normas <- normas[! normas$ID_LEGISLACAO %in% resolvidos, ]
} else {
  df_inicial <- data.frame(id = "", #texto = "",
                           validacao = "", maquina = "",
                           usuario = "", data = "", stringsAsFactors = FALSE)
  saveRDS(df_inicial[FALSE,], 'log.RDS')
}

if (file.exists('base_final.RDS')) {
  base_final <- readRDS('base_final.RDS')
} else {
  base_final <- data.frame(ID_LEGISLACAO = '', DS_RESUMO = '', DT_PUBLICACAO = Sys.Date(),
                           NU_LEGISLACAO = 0, DS_CONTEUDO = '', DT_LEI = Sys.Date(),
                           NU_PAGINA = 0, DS_INDEXACAO = NA, ID_TIPO_LEGISLACAO = 0,
                           ID_TIPO_SITUACAO = 0, CD_TIPO_LIBERACAO = 0, ID_MODO_PUBLICACAO = 0,
                           ID_USUARIO_CADASTRO = 0, ID_USUARIO_LIBERACAO = 0, ID_TIPO_SECAO = 0,
                           DT_CADASTRO = Sys.Date(), NU_PUBLICACAO = 0, NU_VOLUME = 0,
                           stringsAsFactors = FALSE)[FALSE, ]
}
