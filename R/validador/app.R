rm(list=ls())
cat('\014')

# Criar driver de conexão e pegar siglas possíveis
driver <- RJDBC::JDBC("oracle.jdbc.OracleDriver", "www/ojdbc6.jar")
configs <<- readLines('www/config_oracle')
conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
siglas_possiveis <- RJDBC::dbGetQuery(conexao, 'SELECT DISTINCT SGL_ORGAO FROM ADMLEGIS.ATO_AGRICULTURA')[[1]]
lidas <<- RJDBC::dbGetQuery(conexao, "SELECT ID FROM VALID_LOG WHERE VALIDACAO <> 'Indefinido' ")[[1]]
RJDBC::dbDisconnect(conexao)

# Define funções
registrar_log <- function(valores, id_norma) {
  
  conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
  
  RJDBC::dbSendUpdate(conexao, 'INSERT INTO VALID_LOG VALUES (:1, :2, :3, :4, :5)',
                      formatC(id_norma, width = 10, flag = 0), valores$validacao,
                      valores$usuario[1], valores$usuario[2],
                      format(Sys.time(), format = "%d/%m/%Y %H:%M:%S"))
  
  RJDBC::dbCommit(conexao)
  
  RJDBC::dbDisconnect(conexao)
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
normas <- readRDS('www/aplicacao.RDS')
normas <- normas[! normas$ID %in% lidas, ]



escrever_na_base <- function(input, driver, configs) {
  conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
  
  NUM_ATO <- formatC(input$num_norma, width = 8, flag = '0')
  SGL_TIPO <- input$sgl_tipo
  VLR_ANO <- as.character(lubridate::year(input$data_dou))
  SGL_ORGAO <- input$sgl_orgao
  COD_TIPO <- 'A' # Precisa determinar
  DES_ITEM <- '0' # Precisa determinar
  NUM_LINHA <- '0' # Precisa determinar
  TXT_TEXTO <- input$entradaPrincipal
  DTA_PROMULGACAO <- input$data_dou
  TXT_EMENTA <- paste0("<p>", input$txt_ementa, "</p>")
  COD_LOCAL <- 'BR' # Precisa determinar
  DES_TITULO <- input$des_titulo
  
  query_seq <- paste0("SELECT * FROM ADMLEGIS.ATO_AGRICULTURA WHERE NUM_ATO='",
                      NUM_ATO, "' AND SGL_TIPO='", SGL_TIPO, "' AND VLR_ANO='",
                      VLR_ANO, "' AND SGL_ORGAO='", SGL_ORGAO, "'")
  
  registros_seq <- RJDBC::dbGetQuery(conexao, query_seq)
  
  SEQ_ATO <- max(as.numeric(registros_seq$SEQ_ATO))
  
  if (is.infinite(SEQ_ATO) | is.na(SEQ_ATO)) {
    SEQ_ATO <- '000'
  } else {
    SEQ_ATO <- formatC(SEQ_ATO + 1, width = 3, flag = '0')
  }
  
  query1 <- "INSERT INTO ADMLEGIS.ITEM_ATO (NUM_ATO, SEQ_ATO, SGL_TIPO, VLR_ANO, SGL_ORGAO, COD_TIPO, DES_ITEM, NUM_LINHA, TXT_TEXTO)
               VALUES (:1, :2, :3, :4, :5, :6, :7, :8, :9)"
  query2 <- "UPDATE ADMLEGIS.ATO SET 
               DTA_PROMULGACAO = TO_DATE(:1, 'yyyy-mm-dd'),
               TXT_EMENTA = :2, COD_LOCAL = :3, DES_TITULO = :4
                 WHERE NUM_ATO = :5 AND SEQ_ATO = :6 AND SGL_TIPO = :7 AND VLR_ANO = :8 AND SGL_ORGAO = :9"
  query3 <- "INSERT INTO ADMLEGIS.ATO_AGRICULTURA (NUM_ATO, SEQ_ATO, SGL_TIPO, VLR_ANO, SGL_ORGAO)
               VALUES (:1, :2, :3, :4, :5)"
  
  RJDBC::dbSendUpdate(conexao, query1, NUM_ATO, SEQ_ATO, SGL_TIPO, VLR_ANO, SGL_ORGAO,
                      COD_TIPO, DES_ITEM, NUM_LINHA, TXT_TEXTO)
  
  RJDBC::dbSendUpdate(conexao, query2, DTA_PROMULGACAO, TXT_EMENTA, COD_LOCAL, 
                      DES_TITULO, NUM_ATO, SEQ_ATO, SGL_TIPO, VLR_ANO, SGL_ORGAO)
  
  RJDBC::dbSendUpdate(conexao, query3, NUM_ATO, SEQ_ATO, SGL_TIPO, VLR_ANO, SGL_ORGAO)
  
  RJDBC::dbCommit(conexao)
  
  RJDBC::dbDisconnect(conexao)
}

sapply(c('shiny', 'shinyjs', 'httr', 'rJava', 'RJDBC', 'lubridate'), function(pacote) {
  if (!require(pacote, character.only = TRUE)) {
    install.packages(pacote, dependencies = TRUE, verbose = FALSE,
                     repos = 'http://cran.fiocruz.br', quiet = TRUE)
    invisible(require(pacote, character.only = TRUE))
  }
})
runApp('.', launch.browser = FALSE, port = 6312)
