#' Tabela Ato
#' 
#'
#' @param lista_de_atos Lista de atos retirados de um dia do DOU com a função `pega_normas_dou()`
#'
#' @return Uma tibble com os fa tabela Ato no banco de dados
#' @export
#'
#' @examples
criar_tabela_ato <- function(lista_de_atos) {
  # lista_de_atos = lista
  data_ato <- sapply(lista_de_atos, pega_data)
  if (is.numeric(data_ato)) {
    data_ato <- as.Date(data_ato, '1970-01-01')
  }
  # Não vai funcionar porque arquivos não esta mais aqui
  # a resposta ideia talvez seja criar uma classe com os atributos:
  # Seção, Data, limites orgaos
  data_dou <- stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
    as.Date(format = "%Y_%m_%d") %>% unique()
  
  # tem que entrar como atributo. Não tem jeito
  limites_orgaos <- sapply(lista_de_atos, pega_limites_orgaos)
  orgao <- lista_de_atos[atos[-length(atos)] %em% limites_orgaos]
  
  ######
  
  tibble::tibble(
    NUM_ATO = sapply(lista_de_atos, pega_numero),
    SEQ_ATO = NA_integer_, # na maior parte dos casos é um numero
    # possui muitas ocorencias de 0 a 80. Tem casos com letras ('0GQ')
    SGL_TIPO = sapply(lista_de_atos, pega_tipo), # precisa melhorar
    VLR_ANO = lubridate::year(data_ato),
    SGL_ORGAO = sapply(lista_de_atos, pega_orgao), # não está feita ####
    DTA_PROMULGACAO = data_ato,
    TXT_EMENTA = sapply(lista_de_atos, pega_resumo),
    COL_DUMMY = NA, # ?
    IND_HISTORICO = NA, # é binária S ou N
    DES_NUM_ATO = NA, # como texto?
    TXT_EMENTA_ORI = NA, # ? Só tem valor 'TESTE'
    IND_EDICAO = NA, # é binária S ou N
    COD_LOCAL = 'BR', # BR ou CF ou TO
    IND_REVOGADO = NA, # é binária S ou N
    IND_REDACAO = NA, # é binária S ou N
    IND_REGULAMENTO = NA, # é binária S ou N
    DES_TITULO = sapply(lista_de_atos, pega_titulo), # não está feita ####
    IND_INTEIRO_TEOR = NA, # é binária S ou N
    COD_ATO = NA, # numerica
    COD_ORIGEM = NA # 1 ou 2 ??
  )
}
