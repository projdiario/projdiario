#' Tabela Ato
#' 
#'
#' @param lista_de_atos Lista de atos retirados de um dia do DOU com a função `pega_normas_dou()`
#'
#' @return Uma tibble com os fa tabela Ato no banco de dados
#' @examples
#' # Sem exemplo
#' 
#' @export

criar_tabela_ato <- function(lista_de_atos) {
  # lista_de_atos = lista
  
  tibble::tibble(
    NUM_ATO = sapply(lista_de_atos, pega_numero),
    SEQ_ATO = NA_character_,
    SGL_TIPO = sapply(lista_de_atos, pega_tipo), # precisa melhorar
    VLR_ANO = attr(lista_de_atos, 'data_dou') %>% lubridate::year() %>% as.character(),
    SGL_ORGAO = attr(lista_de_atos, 'orgao'),
    DTA_PROMULGACAO = attr(lista_de_atos, 'data_dou'),
    TXT_EMENTA = sapply(lista_de_atos, pega_resumo),
    COL_DUMMY = NA, # é vazia na base
    IND_HISTORICO = 'N', # é binária S ou N
    DES_NUM_ATO = NA_character_, # como texto?
    TXT_EMENTA_ORI = NA_character_, # ? Só tem valor 'TESTE'
    IND_EDICAO = 'N', # é binária S ou N
    COD_LOCAL = 'BR', # BR, veio de DOU
    IND_REVOGADO = 'N', # é binária S ou N
    IND_REDACAO = NA_character_, # é binária S ou N
    IND_REGULAMENTO = NA_character_, # é binária S ou N
    DES_TITULO = sapply(lista_de_atos, pega_titulo),
    IND_INTEIRO_TEOR = NA_character_, # é binária S ou N
    COD_ATO = NA_integer_, # numerica
    COD_ORIGEM = NA_integer_ # 1 ou 2 ??
  )
}
