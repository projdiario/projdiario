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
  
  tibble::tibble(
    NUM_ATO = sapply(lista_de_atos, pega_numero),
    SEQ_ATO = NA_integer_, # na maior parte dos casos é um numero
    # possui muitas ocorencias de 0 a 80. Tem casos com letras ('0GQ')
    SGL_TIPO = sapply(lista_de_atos, pega_tipo), # precisa melhorar
    VLR_ANO = lubridate::year(data_ato),
    SGL_ORGAO = attr(lista_de_atos, 'orgao'),
    DTA_PROMULGACAO = data_ato,
    TXT_EMENTA = sapply(lista_de_atos, pega_resumo),
    COL_DUMMY = NA, # é vazia na base
    IND_HISTORICO = NA, # é binária S ou N
    DES_NUM_ATO = NA_character_, # como texto?
    TXT_EMENTA_ORI = NA_character_, # ? Só tem valor 'TESTE'
    IND_EDICAO = NA, # é binária S ou N
    COD_LOCAL = 'BR', # BR ou CF ou TO
    IND_REVOGADO = NA, # é binária S ou N
    IND_REDACAO = NA, # é binária S ou N
    IND_REGULAMENTO = NA, # é binária S ou N
    DES_TITULO = sapply(lista_de_atos, pega_titulo),
    IND_INTEIRO_TEOR = NA, # é binária S ou N
    COD_ATO = NA_integer_, # numerica
    COD_ORIGEM = NA_integer_ # 1 ou 2 ??
  )
}
