#' Parsear normas em uma sessao do DOU e escrever na base
#'
#' @param conexao Uma conexao
#' @param pastas Vetor com pastas em que se encontram as paginas txt de uma unidade do DOU
#' @param quais ID do ultimo ato escrito na base. Se nao existe ID = 0 
#' @param debug Debugando?
#' 
#' @return
#' @export
#'
parsear_e_escrever <- function(conexao, pastas, quais = seq_len(10), debug = FALSE) {
  
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
  
  lista_de_normas <- purrr::map(lista_arquivos[quais], pegar_normas_dou, debug = debug)
  normas <- purrr::map_df(lista_de_normas, criar_tabela_app) %>% gerar_id(anterior = maior_id)
  
  for (linha in seq_len(nrow(normas))) {
    linha_atual <- normas[linha, ]
    RJDBC::dbSendUpdate(conexao,
                        "INSERT INTO ATO_PARSE
                        VALUES (:1, :2, :3, :4, :5, :6, :7, TO_DATE(:8, 'yyyy-mm-dd'), :9, :10, :11, :12)",
                        linha_atual$ID, formatC(linha_atual$NUM_ATO, width = 8, flag = 0), linha_atual$SGL_TIPO,
                        linha_atual$VLR_ANO, substr(linha_atual$SGL_ORGAO, 1, 30), linha_atual$COD_TIPO, 
                        linha_atual$TXT_TEXTO, linha_atual$DTA_PROMULGACAO, linha_atual$TXT_EMENTA, 
                        linha_atual$DES_TITULO, linha_atual$NUM_PAGINA, linha_atual$ID_TIPO_SECAO
    )
  }
  cat(nrow(normas), 'normas foram inseridas na base.\n')
  RJDBC::dbCommit(conexao)
}