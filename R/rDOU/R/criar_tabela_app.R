#' Tabela para Validação na Aplicação
#'
#' @param lista_de_atos 
#'
#' @return tabela com informações que precisam ser validadas na aplicação
#' @export
#'
#' @examples
#' # Sem exemplos
criar_tabela_app <- function(lista_de_atos) {
  
  tibble::tibble(
    NUM_ATO = sapply(lista_de_atos, pegar_numero), # Ok
    SGL_TIPO = sapply(lista_de_atos, pegar_tipo), # Ok
    VLR_ANO = attr(lista_de_atos, 'data_dou') %>% lubridate::year() %>% as.character(), # Deriva de DTA_PROMULGACAO
    SGL_ORGAO = attr(lista_de_atos, 'orgao'), # Ok
    COD_TIPO = sapply(lista_de_atos, pegar_tipo, 'cod'), # tem que derivar do tipo
    TXT_TEXTO = sapply(lista_de_atos, paste, collapse = "\n") %>% sapply(texto_para_html), # Ok
    DTA_PROMULGACAO = attr(lista_de_atos, 'data_dou'), # Ok
    TXT_EMENTA = sapply(lista_de_atos, pegar_resumo), # A principio fora
    DES_TITULO = sapply(lista_de_atos, pegar_titulo), 
    NUM_PAGINA = sapply(lista_de_atos, pegar_pagina, attr(lista_de_atos, 'arquivos')),
    ID_TIPO_SECAO = attr(lista_de_atos, 'secao')
  )
}