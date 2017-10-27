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

parseadas <- RJDBC::dbGetQuery(conexao, 'SELECT ID, DTA_PROMULGACAO, ID_TIPO_SECAO FROM ATO_PARSE')
if (nrow(parseadas) > 0) {
  maior_id <- max(as.numeric(parseadas$ID))
  
  datas <- parseadas$DTA_PROMULGACAO %>% substr(1, 10) %>%
    as.Date(format = '%Y-%m-%d') %>% format(format = "%Y/%B/%d") %>%
    paste0('DOU', parseadas$ID_TIPO_SECAO, '/',.) %>%
    unique()
  
  pastas_lidas <- purrr::map(datas, grep, pastas) %>% Reduce(f = c)
  pastas_ler <- pastas[-pastas_lidas]
} else {
  maior_id <- 0
  pastas_ler <- pastas
}

lista_arquivos <- lapply(pastas_ler, function(x) dir(x, full.names = T))

exemplo <- purrr::map(lista_arquivos[1:10], pegar_normas_dou, debug = TRUE)

exemplo_df <- purrr::map_df(exemplo, criar_tabela_app) %>% gerar_id(anterior = maior_id)

for (linha in seq_len(nrow(exemplo_df))) {
  linha_atual <- exemplo_df[linha, ]
  RJDBC::dbSendUpdate(conexao,
                      "INSERT INTO ATO_PARSE
    VALUES (:1, :2, :3, :4, :5, :6, :7, TO_DATE(:8, 'yyyy-mm-dd'), :9, :10, :11, :12)",
                      linha_atual$ID, formatC(linha_atual$NUM_ATO, width = 8, flag = 0), linha_atual$SGL_TIPO,
                      linha_atual$VLR_ANO, substr(linha_atual$SGL_ORGAO, 1, 30), linha_atual$COD_TIPO, 
                      linha_atual$TXT_TEXTO, linha_atual$DTA_PROMULGACAO, linha_atual$TXT_EMENTA, 
                      linha_atual$DES_TITULO, linha_atual$NUM_PAGINA, linha_atual$ID_TIPO_SECAO
  )
}

# teste
RJDBC::dbCommit(conexao)
RJDBC::dbDisconnect(conexao)

