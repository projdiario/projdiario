#' Remover erros de interpretacao (parse) do texto
#'
#' @param texto O texto que sera limpo
#'
#' @return O mesmo texto limpo
#' @export
limpar_texto <- function(texto) {
  regex_n_ordinal <- desescapar("N(o|o-|\\u00b0-|\\u00b0)? ?(?=\\\\d)")
  n_ordinal <- desescapar("N\\u00ba ")
  regex_o_ordinal <- desescapar("(?<=\\\\d)(o-|o|\\u00b0-|\\u00b0) ?")

  resp <- texto %>% stringr::str_replace_all(regex_n_ordinal, n_ordinal) %>%
    stringr::str_replace_all(regex_o_ordinal, desescapar("\\u00ba ")) %>%
    stringr::str_replace_all("(?<=\\d)(a-|a) ?", desescapar("\\u00aa ")) %>%
    stringr::str_trim("both") %>%
    # extract(!stringr::str_detect(., "Este documento pode ser verificado no endereço")) %>%
    eliminar_quebras() %>%
    stringr::str_trim("both")

  resp[resp != ""]
}

#' Eliminar quebras indesejadas
#'
#' @param texto Texto que tera as quebras de linhas erradas eliminadas
#'
#' @return O mesmo texto sem as quebras indesejadas
#'
#' @examples
#' eliminar_quebras('Um texto com \n2 quebras de linha\n')
#'
#' @export
eliminar_quebras <- function(texto) {
  entrou_colapsado <- length(texto) == 1
  texto <- paste0(texto, collapse = "\n") %>%
    stringr::str_replace_all('-\\n', '') %>%
    stringr::str_replace_all('(?<=,)\\n', ' ') %>%
    stringr::str_replace_all('(?<=\\s(DO|NO))\\n', ' ') %>%
    stringr::str_replace_all('\\n ?(?=[a-z][^\\)-])', ' ') %>%
    stringr::str_replace_all('(?<![;:\\.]) ?\\n ?(?=\\d{2,})', ' ') %>%
    stringr::str_replace_all('(?<=\\d[\\.,\\/]) ?\\n ?(?=\\d)', '') %>%
    stringr::str_replace_all('[ ]{2,}', ' ')

  if (entrou_colapsado) {
    res <- texto # devolver com tamanho 1
  } else {
    res <- strsplit(texto, "\\n")[[1]] # devolver como vetor
  }
  res
}

#' Qual faixa de b (limite) contem a (conteudo)?
#'
#' @rdname em
#'
#' @param conteudo vetor a ser analisado
#' @param limite vetor com limites
#'
#' @return Em qual intervalo de \code{limite} encontra-se cada elemento de \code{conteudo}.
#'
#' @export
`%em%` <- function(conteudo, limite) {
  resposta <- numeric(length(conteudo))
  for (i in seq_along(conteudo)) {
    if (conteudo[i] %in% limite) {
      resposta[i] <- NA_integer_
    } else {
      resposta[i] <- suppressWarnings(max(limite[limite < conteudo[i]]))
    }
  }

  resposta[is.infinite(resposta)] <- NA_integer_
  resposta
}

#' Transformar Texto em Parágrafos de HTML
#'
#' @param texto Um vetor com texto
#' @param orgao Nome do orgao que sera inserido no texto
#' @param cabecalho Cabecalho que sera incluido no inicio do texto
#'
#' @return O mesmo texto com as quebras de linha substituídas por tags de parágrafos \code{<p>}.
#'         Adiciona um cabeçalho.
#'
#' @export
texto_para_html <- function(texto, orgao, cabecalho) {
  resp <- gsub("\\n<" , "\r<", texto) %>%
    stringr::str_replace_all("\\n\\s" , "\n") %>%
    stringr::str_replace_all("\\n" , "</p>\n<p>") %>%
    stringr::str_replace_all("\\r" , "")

  paste0("<p>", cabecalho, "</p><p>", orgao,"</p>", resp)
}

#' Criar indices
#'
#' @param vetor_inicios Vetor de indices de inicios
#'
#' @return uma lista com os indices entre cada elemento de \code{vetor_inicios}
criar_indices <- function(vetor_inicios) {
  lapply(seq_len(length(vetor_inicios) - 1), function (i) {
    vetor_inicios[i]:(vetor_inicios[i+1] - 1)
  })
}

#' Cria um objeto para cada norma em objeto de normas multiplas
#'
#' @param norma um vetor o texto da norma multipla
#'
#' @return um vetor com uma norma em cada elemento
#'
#' @export
multipla_para_individualizada <- function(norma) {
  # Função para verificar existencia de "chunks"
  casos <- function(texto){
    resolve <- stringr::str_which(texto, "resolve[::punct::]")
    cessao <- stringr::str_which(texto, desescapar("cess[\\u00e3a]o[\\\\.:]"))
    sort(unique(c(resolve, cessao)))
  }

  padrao <- desescapar("N\\u00ba ?\\\\d+\\\\.?\\\\d*")
  numeros <- stringr::str_extract(norma, padrao)
  numeros <- numeros[!is.na(numeros)]
  tipo <- stringr::str_extract(norma[1], '^.+?(?=DE)') %>%
    stringr::str_replace(desescapar("\\u00d5ES\\\\b"), desescapar("\\u00c3O")) %>%
    stringr::str_replace("AS\\b", "A") %>%
    stringr::str_replace("OS\\b", "O")

  dia_norma <- stringr::str_extract(norma[1], 'DE .+ \\d{4}')
  nomes <- paste0(tipo, numeros, ', ', dia_norma)
  inicios <- c(grep(padrao, norma), length(norma))
  cabeca <- paste0(norma[2:( inicios[1] - 1 )], collapse = " ")
  rodape <- norma[length(norma)] # por def tamanho 1

  # Regra para retificações
  if (grepl('RETIFIC', norma[1])) {
    padrao <- desescapar("onde se l[\\u00eae]:?")
    inicios <- c(grep(padrao, tolower(norma)), length(norma) + 1)
    indices <- criar_indices(inicios)

    retif <- desescapar("RETIFICA\\u00c7\\u00c3O\\n")
    lista_normas <- purrr::map(
      indices, ~ paste0(retif, paste0(norma[.x], collapse = "\n"))
    )

    individualizadas <- gsub(paste0(padrao, ' ?-'), "", unlist(lista_normas))
    return(individualizadas)
  }

  # Regra para tipos diferentes publicados juntos
  if (length(casos(norma)) > 1) {
    novas_ind <- c(casos(norma), length(norma))
    nome <- norma[1]
    lista_ind <- criar_indices(novas_ind)

    novas <- purrr::map(lista_ind, ~ c(nome, norma[.x], rodape))

    if (any(purrr::map_lgl(novas, ~ (length(casos(.x)) != 1) ))) {
      individualizadas <- "AJUSTE"
    } else {
      individualizadas <- unlist(lapply(novas, multipla_para_individualizada))
    }
  # Caso padrão: 1 resolve ou cessão
  } else {
    indices <- criar_indices(inicios)
    form <- ~ paste0(.x, "\n", cabeca, "\n",
                     paste0(norma[ .y ], collapse = "\n"), "\n", rodape
    )

    lista_normas <- purrr::map2(nomes, indices, form)

    individualizadas <- gsub(paste0(padrao, ' ?-'), "", unlist(lista_normas))
  }
  individualizadas
}

novas_observacoes <- function(lista_de_indices, df, arquivos, encodificacao) {
  multiplas <- lapply(df$TX_TEXTO[lista_de_indices], function(x) {
    stringr::str_split(x, "\n")[[1]]
  })

  novas <- lapply(multiplas, multipla_para_individualizada)

  tam_novas <- sapply(novas, length)

  remover <- lista_de_indices[tam_novas >= 1]

  novas <- novas[tam_novas >= 1]

  novas_vetor <- unlist(novas)

  tamanhos <- sapply(novas, length)

  repete_dado <- function(variavel) {
    res <- purrr::map2(df[[variavel]][remover], tamanhos, ~ rep(.x, each = .y))
    unlist(res)
  }

  novas_obs <- tibble::tibble(
    NR_ATO = sapply(novas_vetor, pegar_numero, USE.NAMES = FALSE),
    SG_TIPO = sapply(novas_vetor, pegar_tipo, USE.NAMES = FALSE),
    AN_ATO = repete_dado("AN_ATO"),
    SG_ORGAO = repete_dado("SG_ORGAO"),
    CD_TIPO_ATO = 'A', # codigo é verificado na aplicação
    TX_TEXTO = novas_vetor,
    DT_PROMULGACAO = as.Date(repete_dado("DT_PROMULGACAO"), origin = "1970-01-01"),
    TX_EMENTA = purrr::map(novas_vetor, ~ strsplit(.x, '\n')[[1]]) %>%
      sapply(pegar_resumo, USE.NAMES = FALSE),
    DS_TITULO = sapply(novas_vetor, pegar_titulo, USE.NAMES = FALSE),
    NM_PAGINA = sapply(novas_vetor, pegar_pagina, arquivos,
                       encodificacao, USE.NAMES = FALSE),
    ID_TIPO_SECAO = repete_dado("ID_TIPO_SECAO")
  )

  novas_obs
}

#' @importFrom magrittr %>%
magrittr::`%>%`

#' @importFrom magrittr extract
magrittr::extract

#' @importFrom magrittr extract2
magrittr::extract2

#' @importFrom stringi stri_unescape_unicode
desescapar <- stringi::stri_unescape_unicode
