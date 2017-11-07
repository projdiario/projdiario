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
    NUM_ATO = sapply(lista_de_atos, pegar_numero, USE.NAMES = FALSE), # Ok
    SGL_TIPO = sapply(lista_de_atos, pegar_tipo, USE.NAMES = FALSE), # Ok
    VLR_ANO = attr(lista_de_atos, 'data_dou') %>% lubridate::year() %>% as.character(), # Deriva de DTA_PROMULGACAO
    SGL_ORGAO = attr(lista_de_atos, 'orgao'), # Ok
    COD_TIPO = sapply(lista_de_atos, pegar_tipo, 'cod', USE.NAMES = FALSE), # tem que derivar do tipo
    TXT_TEXTO = sapply(lista_de_atos, paste, collapse = "\n", USE.NAMES = FALSE), # Ok
    DTA_PROMULGACAO = attr(lista_de_atos, 'data_dou'), # Ok
    TXT_EMENTA = sapply(lista_de_atos, pegar_resumo, USE.NAMES = FALSE), # A principio fora
    DES_TITULO = sapply(lista_de_atos, pegar_titulo, USE.NAMES = FALSE),
    NUM_PAGINA = sapply(lista_de_atos, pegar_pagina, attr(lista_de_atos, 'arquivos'), USE.NAMES = FALSE),
    ID_TIPO_SECAO = attr(lista_de_atos, 'secao')
  )

  remover <- c(grep("PORTARIAS", normas$DES_TITULO),
               grep("DECISÕES", normas$DES_TITULO),
               grep("RETIFICAÇÕES", normas$DES_TITULO))

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
    res <- purrr::map2(normas[[variavel]][remover], tamanhos, ~ rep(.x, each = .y))
    unlist(res)
  }

  novas_obs <- tibble::tibble(
    NUM_ATO = sapply(novas_vetor, pegar_numero, USE.NAMES = FALSE),
    SGL_TIPO = sapply(novas_vetor, pegar_tipo, USE.NAMES = FALSE),
    VLR_ANO = repete_dado("VLR_ANO"),
    SGL_ORGAO = repete_dado("SGL_ORGAO"),
    COD_TIPO = sapply(novas_vetor, pegar_tipo, 'cod', USE.NAMES = FALSE),
    TXT_TEXTO = novas_vetor,
    DTA_PROMULGACAO = as.Date(repete_dado("DTA_PROMULGACAO"), origin = "1970-01-01"),
    TXT_EMENTA = sapply(novas_vetor, pegar_resumo, USE.NAMES = FALSE),
    DES_TITULO = sapply(novas_vetor, pegar_titulo, USE.NAMES = FALSE),
    NUM_PAGINA = repete_dado("NUM_PAGINA"),
    ID_TIPO_SECAO = repete_dado("ID_TIPO_SECAO")
  )

  res <- dplyr::bind_rows(normas[-remover, ], novas_obs)
  res$TXT_TEXTO <- res$TXT_TEXTO %>% sapply(texto_para_html)
  res
}
