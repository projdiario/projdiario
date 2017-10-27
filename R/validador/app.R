sapply(c('shiny', 'shinyjs', 'httr', 'rJava', 'RJDBC', 'lubridate'), function(pacote) {
  if (!require(pacote, character.only = TRUE, quietly = TRUE)) {
    install.packages(pacote, dependencies = TRUE, verbose = FALSE,
                     repos = 'http://cran.fiocruz.br', quiet = TRUE)
    require(pacote, character.only = TRUE, quietly = TRUE)
  }
})

options(scipen = 999)

# Criar driver de conexão e pegar siglas possíveis
driver <<- RJDBC::JDBC("oracle.jdbc.OracleDriver", "www/ojdbc6.jar")
configs <<- readLines('www/config_oracle')
conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
siglas_possiveis <<- RJDBC::dbGetQuery(conexao, 'SELECT DISTINCT SGL_ORGAO FROM ADMLEGIS.ATO_AGRICULTURA')[[1]]

usuarios_cadastrados <- RJDBC::dbGetQuery(conexao, 'SELECT USUARIO FROM FAZENDO')[[1]]
usuario <- Sys.info()[['effective_user']]
if (usuario %in% usuarios_cadastrados) {
  RJDBC::dbSendUpdate(conexao, "UPDATE FAZENDO SET ID = '0000000000' WHERE USUARIO = :1",
                      usuario)
} else {
  RJDBC::dbSendUpdate(conexao, "INSERT INTO FAZENDO VALUES (:1, '0000000000')", usuario)
}

lidas <<- c(RJDBC::dbGetQuery(conexao,
              "SELECT ID FROM VALID_LOG WHERE VALIDACAO <> 'Indefinido'")[[1]],
            RJDBC::dbGetQuery(conexao, 'SELECT ID FROM FAZENDO')[[1]])
normas <- RJDBC::dbGetQuery(conexao, "SELECT * FROM ATO_PARSE")
normas$DTA_PROMULGACAO <- as.Date(substr(normas$DTA_PROMULGACAO, 1, 10), format = "%Y-%m-%d")
normas <<- normas[! normas$ID %in% lidas, ]

RJDBC::dbSendUpdate(conexao, "UPDATE FAZENDO SET ID = :1 WHERE USUARIO = :2",
                    normas$ID[[1]], usuario)

RJDBC::dbDisconnect(conexao)

# Define funções
meu_formato <- function(numero, tamanho) {
  res <- format(numero, width = tamanho)
  gsub(' ', '0', res)
}

registrar_log <- function(valores, id_norma, conexao) {
  RJDBC::dbSendUpdate(conexao, 'INSERT INTO VALID_LOG VALUES (:1, :2, :3, :4, :5)',
                      meu_formato(id_norma, 10), valores$validacao,
                      valores$usuario[1], valores$usuario[2],
                      format(Sys.time(), format = "%d/%m/%Y %H:%M:%S"))
  
  RJDBC::dbCommit(conexao)
}

jsCode <<- "shinyjs.pegaTexto = function() {
elem = document.getElementById('textoPrincipal_ifr').contentDocument.getElementById('tinymce');
Shiny.onInputChange('entradaPrincipal', elem.innerHTML);
};
shinyjs.novaNorma = function() {
elem = document.getElementById('textoModal_ifr').contentDocument.getElementById('tinymce');
Shiny.onInputChange('entradaNova', elem.innerHTML);
};
shinyjs.alteraNorma = function() {
elem = document.getElementById('textoAlteracao_ifr').contentDocument.getElementById('tinymce');
Shiny.onInputChange('entradaAlteracao', elem.innerHTML);
};"

escrever_na_base <- function(input, conexao) {
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
  
  SEQ_ATO <- suppressWarnings(max(as.numeric(registros_seq$SEQ_ATO)))
  
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
}

prox_num <- function(num, conexao, usuario) {
  trabalhando <- RJDBC::dbGetQuery(conexao, "SELECT ID FROM FAZENDO")[[1]]
  # RETORNA UM NUM CUJO ID NENHUM USUÁRIO ESTEJA TRABALHANDO
  if (normas$ID[[num + 1]] %in% trabalhando) {
    cat("Este estava sendo trabalhado. Vou buscar o próximo")
    prox_num(num + 1, conexao, usuario)
  } else {
    RJDBC::dbSendUpdate(conexao, paste0("UPDATE FAZENDO SET ID = '", normas$ID[[num + 1]],
                                        "' WHERE USUARIO = '", usuario, "'"))
    cat(paste0("UPDATE FAZENDO SET ID = '", normas$ID[[num + 1]],
               "' WHERE USUARIO = '", usuario,"'\n"))
    RJDBC::dbCommit(conexao)
    cat('valor alterado para ', normas$ID[[num + 1]])
    num + 1
  }
}

runApp('.', launch.browser = FALSE, port = 6312)
