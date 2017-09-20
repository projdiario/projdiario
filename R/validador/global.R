rm(list=ls())
# shell('start firefox http://127.0.0.1:6312/')
cat('\014')

# Criar driver de conexão e pegar siglas possíveis
driver <- RJDBC::JDBC("oracle.jdbc.OracleDriver", "www/ojdbc6.jar")
configs <<- readLines('www/config_oracle')
conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
siglas_possiveis <- RJDBC::dbGetQuery(conexao, 'SELECT DISTINCT SGL_ORGAO FROM ADMLEGIS.ITEM_ATO')[[1]]
RJDBC::dbDisconnect(conexao)

# Define funções
# registrar_log <- function(valores, id_norma) {
#   if (missing(id_norma)) {
#     id_norma <-  normas$ID_LEGISLACAO[valores$num]
#   }
#   
#   df <- data.frame(
#     id = id_norma, #texto = valores$html,
#     validacao = valores$aval,
#     maquina = valores$usuario[1], usuario = valores$usuario[2],
#     data = format(Sys.time(), format = "%d/%m/%Y %H:%M:%S"),
#     stringsAsFactors = FALSE)
#   df
# }

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
normas <- readRDS('aplicacao.RDS')

if (file.exists('log.RDS')) {
  lidos <- readRDS('log.RDS')
  resolvidos <- lidos$id[lidos$validacao == 'Aceito' | lidos$validacao == 'Recusado']
  # normas <- normas[! normas$ID_LEGISLACAO %in% resolvidos, ]
} else {
  df_inicial <- data.frame(id = "", #texto = "",
                           validacao = "", maquina = "",
                           usuario = "", data = "", stringsAsFactors = FALSE)
  saveRDS(df_inicial[FALSE, ], 'log.RDS')
}

escrever_na_base <- function(input, driver, configs) {
  conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])
  NUM_ATO <- formatC(input$num_norma, width = 8, flag = '0')
  SGL_TIPO <- input$sgl_tipo
  VLR_ANO <- as.character(lubridate::year(input$data_dou))
  SGL_ORGAO <- input$sgl_orgao
  # COD_TIPO <- ''
  # DES_ITEM <- ''
  # NUM_LINHA <- 0
  TXT_TEXTO <- input$entradaPrincipal
  DTA_PROMULGACAO <- input$data_dou
  TXT_EMENTA <- paste0("<p>", input$txt_ementa, "</p>")
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
  
  query1 <- paste0("DECLARE TEXTO CLOB := '", TXT_TEXTO,"';\n
                    BEGIN
                      INSERT INTO ADMLEGIS.ITEM_ATO (NUM_ATO, SEQ_ATO, SGL_TIPO, VLR_ANO, SGL_ORGAO, COD_TIPO, DES_ITEM, NUM_LINHA, TXT_TEXTO)
                          VALUES ('", NUM_ATO,"', '", SEQ_ATO, "', '", SGL_TIPO,
                              "', '", VLR_ANO, "', '", SGL_ORGAO, "', 'A', 0, 0, TEXTO);
                    END;")
  query2 <- paste0("UPDATE ADMLEGIS.ATO SET
    DTA_PROMULGACAO = TO_DATE('", DTA_PROMULGACAO,"', 'yyyy-mm-dd'),
    TXT_EMENTA = '", TXT_EMENTA, "',
    COD_LOCAL = 'BR',
    DES_TITULO = '", DES_TITULO,"'
      WHERE NUM_ATO = '", NUM_ATO,"' AND SEQ_ATO = '", SEQ_ATO,
      "' AND SGL_TIPO = '", SGL_TIPO,"' AND VLR_ANO = '", VLR_ANO,
      "' AND SGL_ORGAO = '", SGL_ORGAO,"'")
  query3 <- paste0("INSERT INTO ADMLEGIS.ATO_AGRICULTURA (NUM_ATO, SEQ_ATO, SGL_TIPO, VLR_ANO, SGL_ORGAO)
    VALUES ('", NUM_ATO, "', '", SEQ_ATO, "', '", SGL_TIPO, "', '", VLR_ANO,"', '", SGL_ORGAO, "')")
  
  RJDBC::dbSendUpdate(conexao, query1)
  RJDBC::dbSendUpdate(conexao, query2)
  RJDBC::dbSendUpdate(conexao, query3)
  RJDBC::dbCommit(conexao)
  RJDBC::dbDisconnect(conexao)
}
