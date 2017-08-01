rm(list=ls())
shell('start firefox http://127.0.0.1:6312/')
cat('\014')

normas <- readRDS('normas2_teste.RDS')

if (file.exists('log.RDS')) {
  lidos <- readRDS('log.RDS')
  resolvidos <- lidos$id[lidos$validacao == 'Aceito' | lidos$validacao == 'Recusado']
  normas <- normas[! normas$ID_LEGISLACAO %in% resolvidos, ]
} else {
  df_inicial <- data.frame(id = "", texto = "", validacao = "", maquina = "",
                           usuario = "", data = "", stringsAsFactors = FALSE)
  saveRDS(df_inicial[FALSE,], 'log.RDS')
}

inclui_dado <- function(valores, id_norma) {
  if (missing(id_norma)) {
    id_norma <-  normas$ID_LEGISLACAO[valores$num]
  }
  
  df <- data.frame(
    id = id_norma, texto = valores$html, validacao = valores$aval,
    maquina = valores$usuario[1], usuario = valores$usuario[2],
    data = format(Sys.time(), format = "%d/%m/%Y %H:%M"),
    stringsAsFactors = FALSE)
  df
}

texto_em_html <- function(texto) {
  html1 <- gsub("\\n<" , "\r<", texto)
  html1 <- gsub("\\n\\s" , "\n", html1)
  html1 <- gsub("\\n" , "</p><p>\n", html1)
  html1 <- paste("<p>", "<p>MINISTÉRIO DA AGRICULTURA, PECUÁRIA E ABASTECIMENTO</p>",
                 "<p>SECRETARIA</p>", gsub("\\r" , "", html1), "</p>")
  html1
}

jsCode <<- "shinyjs.pegaTexto = function() {
             elem = document.getElementById('textoPrincipal_ifr').contentDocument.getElementById('tinymce');
             Shiny.onInputChange('entradaPrincipal', elem.innerHTML);
            };
            shinyjs.novaNorma = function() {
              elem = document.getElementById('textoModal_ifr').contentDocument.getElementById('tinymce');
              Shiny.onInputChange('entradaNova', elem.innerHTML);

            };"

sapply(c('shiny', 'shinyjs'), function(pacote) {
  if (!require(pacote, character.only = TRUE)) {
    install.packages(pacote, dependencies = TRUE, verbose = FALSE,
                     repos = 'http://cran.fiocruz.br', quiet = TRUE)
    invisible(require(pacote, character.only = TRUE))
  }
})
