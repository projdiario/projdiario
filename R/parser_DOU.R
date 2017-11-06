devtools::document('R/rDOU')
devtools::install('R/rDOU')
library(rDOU)
library(RJDBC)

driver <- RJDBC::JDBC("oracle.jdbc.OracleDriver", "R/validador/www/ojdbc6.jar")
configs <- readLines('R/validador/www/config_oracle')
conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])

if (!RJDBC::dbExistsTable(conexao, 'ATO_PARSE')) {
  RJDBC::dbSendUpdate(conexao, 
    'CREATE TABLE ATO_PARSE (
    ID VARCHAR2(10),
    NUM_ATO VARCHAR2(8),
    SGL_TIPO VARCHAR2(3),
    VLR_ANO VARCHAR2(4),
    SGL_ORGAO VARCHAR2(30),
    COD_TIPO VARCHAR2(3),
    TXT_TEXTO CLOB,
    DTA_PROMULGACAO DATE,
    TXT_EMENTA CLOB,
    DES_TITULO VARCHAR2(400),
    NUM_PAGINA NUMBER(3),
    ID_TIPO_SECAO NUMBER(1)
    )')
} else {
  # nada
}

pastas <- list.dirs('\\\\hp64957/Base SISLEGIS/SISLEGIS/dados/txt') %>% 
  grep(pattern = '/[0-9]{2}$', value = TRUE)

conexao %>% parsear_e_escrever(pastas[111:120])

tabela <- conexao %>% RJDBC::dbGetQuery('SELECT * FROM ATO_PARSE ORDER BY ID')
tail(tabela)

RJDBC::dbDisconnect(conexao)

