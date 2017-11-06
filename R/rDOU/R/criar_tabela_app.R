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

  normas <- tibble::tibble(
    NUM_ATO = sapply(lista_de_atos, pegar_numero), # Ok
    SGL_TIPO = sapply(lista_de_atos, pegar_tipo), # Ok
    VLR_ANO = attr(lista_de_atos, 'data_dou') %>% lubridate::year() %>% as.character(), # Deriva de DTA_PROMULGACAO
    SGL_ORGAO = attr(lista_de_atos, 'orgao'), # Ok
    COD_TIPO = sapply(lista_de_atos, pegar_tipo, 'cod'), # tem que derivar do tipo
    TXT_TEXTO = sapply(lista_de_atos, paste, collapse = "\n"), # Ok
    DTA_PROMULGACAO = attr(lista_de_atos, 'data_dou'), # Ok
    TXT_EMENTA = sapply(lista_de_atos, pegar_resumo), # A principio fora
    DES_TITULO = sapply(lista_de_atos, pegar_titulo),
    NUM_PAGINA = sapply(lista_de_atos, pegar_pagina, attr(lista_de_atos, 'arquivos')),
    ID_TIPO_SECAO = attr(lista_de_atos, 'secao')
  )

  remover <- c(grep("PORTARIAS", normas$DES_TITULO), grep("DECISÕES", normas$DES_TITULO))

  multiplas <- lapply(normas$TXT_TEXTO[remover], function(x) {
    stringr::str_split(x, "\n")[[1]]
  })

  novas <- lapply(multiplas, multipla_para_individualizada)

  tam_novas <- sapply(novas, length)

  remover <- remover[tam_novas >= 1]

  novas <- novas[tam_novas >= 1]

  novas_vetor <- unlist(novas)

  numero <- stringr::str_extract(novas_vetor, "Nº [0-9]{1,3}\\.?[0-9]{0,3}") %>%
    stringr::str_replace_all("Nº ", "") %>% stringr::str_replace_all("\\.", "") %>%
    as.numeric()

  tamanhos <- sapply(novas, length)

  repete_dado <- function(variavel) {
    res <- mapply(rep, normas[[variavel]][remover], each = tamanhos, USE.NAMES = FALSE)
    res[, 1]
  }

  novas_obs <- tibble::tibble(
    NUM_ATO = repete_dado("NUM_ATO"),
    SGL_TIPO = repete_dado("SGL_TIPO"),
    VLR_ANO = repete_dado("VLR_ANO"),
    SGL_ORGAO = repete_dado("SGL_ORGAO"),
    COD_TIPO = repete_dado("COD_TIPO"),
    TXT_TEXTO = novas_vetor,
    DTA_PROMULGACAO = as.Date(repete_dado("DTA_PROMULGACAO"), origin = "1970-01-01"),
    TXT_EMENTA = sapply(novas_vetor, pegar_resumo),
    DES_TITULO = sapply(novas_vetor, pegar_titulo),
    NUM_PAGINA = repete_dado("NUM_PAGINA"),
    ID_TIPO_SECAO = repete_dado("ID_TIPO_SECAO")
  )

  res <- dplyr::bind_rows(normas[-remover, ], novas_obs)
  res$TXT_TEXTO <- res$TXT_TEXTO %>% sapply(texto_para_html)
  res
}
