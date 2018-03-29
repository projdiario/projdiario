#' Procura um termo no inicio de uma string
#'
#' @param vetor um vetor de texto
#' @param termo termo a ser buscado
#'
#' @return O índice do \code{vetor} em que o \code{termo} se encontra no inicio da string
#'
#' @export
procurar_inicio <- function(vetor, termo) {
  stringr::str_which(vetor, paste0('^', termo))
}

#' Remover erros de interpretacao (parse) do texto
#'
#' @param texto O texto que sera limpo
#'
#' @return O mesmo texto limpo
#' @export
limpar_texto <- function(texto) {
  resp <- texto %>% stringr::str_replace_all("N(o|o-|°-|°)? ?(?=[0-9])", "Nº ") %>%
    stringr::str_replace_all("(?<=[0-9])(o-|o|°-|°) ?", "º ") %>%
    stringr::str_replace_all("(?<=[0-9])(a-|a) ?", "ª ") %>%
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
    stringr::str_replace_all('(?<![;:\\.]) ?\\n ?(?=[0-9]{2,})', ' ') %>%
    stringr::str_replace_all('(?<=[0-9][\\.,\\/]) ?\\n ?(?=[0-9])', '') %>%
    stringr::str_replace_all('[ ]{2,}', ' ')

  # Casos ruins
  # X Quebra depois de ',', '-', 'DO' ou 'NO'.
  # X Quebra antes de '[a-z]' (frase quebrada no meio),
  ## X '([0-9](.|-/\)[0-9]+)' (número de processo ou RG ou CPF),
  ##

  # Casos bons
  # Quebra depois de '.', ';', ':',
  # '\nDetermina que' (Quebra antes de letra maiúscula),
  # 'II)', 'ii)', 'a)', 'A)',
  # '1. asdklajsdkla.\n# 2. asiodasjlkda.'


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

#' Adiciona IDs ao data.frame
#'
#' @param df Um data.frame
#' @param anterior Caminho do RDS com a tabela anterior de cujos ID devem se seguir
#'
#' @return O mesmo df precedido por novos IDs
#'
#' @export
gerar_id <- function(df, anterior) {
  # le atos já gravados
  if (missing(anterior)) {
    base_id <- 0
  } else if (is.character(anterior)) {
    base_id <- readRDS(anterior) %>%
      `[[`('ID') %>% as.integer() %>% max()
  } else if (is.numeric(anterior)) {
    base_id <- anterior
  } else {
    stop('Argumento "anterior" foi passado com tipo inesperado.')
  }

  # cria sequências de novos IDs
  ID_ATO <- seq(base_id + 1, by = 1, length.out = nrow(df)) %>%
    format(width = 10) %>%
    gsub(pattern = ' ', replacement = '0')

  tibble::add_column(df, ID_ATO, .before = TRUE)
}

#' Transformar Texto em Parágrafos de HTML
#'
#' @param texto Um vetor com texto
#' @param orgao Nome do orgao que sera inserido no texto
#'
#' @return O mesmo texto com as quebras de linha substituídas por tags de parágrafos \code{<p>}.
#'         Adiciona um cabeçalho.
#'
#' @export
texto_para_html <- function(texto, orgao) {
  resp <- gsub("\\n<" , "\r<", texto) %>%
    stringr::str_replace_all("\\n\\s" , "\n") %>%
    stringr::str_replace_all("\\n" , "</p>\n<p>") %>%
    stringr::str_replace_all("\\r" , "")

  paste0("<p>MINISTÉRIO DA AGRICULTURA, PECUÁRIA E ABASTECIMENTO</p>",
          "<p>", orgao,"</p>", resp)
}

#' Cria um objeto para cada portaria em objeto de portarias multiplas
#'
#' @param portaria um vetor o texto da portaria multipla
#'
#' @return um vetor com uma portaria em cada elemento
#'
#' @export
multipla_para_individualizada <- function(portaria) {
  # Função para verificar existencia de "chunks"
  casos <- function(texto){
    resolve <- grep("resolve[::punct::]", texto)
    cessao <- grep("cess[ãa]o[\\.:]", texto)
    sort(unique(c(resolve, cessao)))
  }

  if (length(procurar_inicio(portaria[length(portaria)], "<table><tr><td>")) == 1) {
    portaria <- portaria[-length(portaria)]
  }
  # Regra para retificações
  if (grepl('RETIFIC', portaria[1])) {
    padrao <- 'onde se l[êe]:?'
    inicios <- c(grep(padrao, tolower(portaria)), length(portaria) + 1)

    indices <- vector("list", length(inicios) - 1)

    for (i in seq_along(indices)) {
      fim <- inicios[i+1] - 1
      indices[[i]] <- inicios[i]:fim
      rm(fim)
    }

    lista_portarias <- vector("list", length(indices))

    for (i in seq_along(lista_portarias)) {
      lista_portarias[[i]] <- paste0("RETIFICAÇÃO", "\n",
                                     paste0(portaria[ indices[[i]] ], collapse = "\n"))
    }

    individualizadas <- gsub(paste0(padrao, ' ?-'), "", unlist(lista_portarias))
    return(individualizadas)
  }
  # Regra para caso de cultivares
  if (length(casos(portaria)) == 0) {
    # 'proteção:'
    padrao <- 'Nº ?[0-9]+\\.?[0-9]*'
    inicios <- c(grep(padrao, portaria), length(portaria))
    numeros <- stringr::str_extract(portaria, padrao)
    numeros <- numeros[!is.na(numeros)]

    nomes <- paste0("DECISÃO ", numeros, ', ',
                    stringr::str_extract(portaria[1], 'DE .+ [0-9]{4}'))

    cabeca <- paste0(portaria[2:( inicios[1]-1 )], collapse = " ")

    rodape <- portaria[length(portaria)] # por def tamanho 1

    indices <- vector("list", length(inicios) - 1)

    for (i in seq_along(indices)) {
      indices[[i]] <- inicios[i]:eval(inicios[i+1] - 1)
    }

    lista_portarias <- vector("list", length(indices))

    for (i in seq_along(lista_portarias)) {
      lista_portarias[[i]] <- paste0(nomes[i], "\n", cabeca, "\n",
                                     paste0(portaria[ indices[[i]] ], collapse = "\n"),
                                     "\n", rodape)
    }

    individualizadas <- gsub(paste0(padrao, ' ?-'), "", unlist(lista_portarias))
  } else if (length(casos(portaria)) != 1) {
    novas_ind <- c(casos(portaria), length(portaria))
    nome <- portaria[1]
    rodape <- portaria[length(portaria)]

    lista_ind <- vector("list", length(novas_ind) - 1 )

    for (i in seq_along(lista_ind)) {
      lista_ind[[i]] <- novas_ind[i]:(novas_ind[i + 1] - 1)
    }

    novas <- vector("list", length(lista_ind))

    for (i in seq_along(novas)) {
      novas[[i]] <- c(nome, portaria[ lista_ind[[i]] ], rodape)
    }

    if (any(sapply(novas, function(x) (length(casos(x)) != 1) ))) {
      individualizadas <- "AJUSTE"
    } else {
      individualizadas <- unlist(lapply(novas, multipla_para_individualizada))
    }
  # Caso padrão: 1 resolve ou cessão
  } else {
    padrao <- 'Nº ?[0-9]+\\.?[0-9]*'
    inicios <- c(grep(padrao, portaria), length(portaria))
    numeros <- stringr::str_extract(portaria, padrao)
    numeros <- numeros[!is.na(numeros)]

    nomes <- paste0("PORTARIA ", numeros, ', ',
                    stringr::str_extract(portaria[1], 'DE .+ [0-9]{4}'))

    cabeca <- paste0(portaria[2:( inicios[1]-1 )], collapse = " ")

    rodape <- portaria[length(portaria)] # por def tamanho 1

    indices <- vector("list", length(inicios) - 1)

    for (i in seq_along(indices)) {
      indices[[i]] <- inicios[i]:(inicios[i+1] - 1)
    }

    lista_portarias <- vector("list", length(indices))

    for (i in seq_along(lista_portarias)) {
      lista_portarias[[i]] <- paste0(nomes[i], "\n", cabeca, "\n",
                                     paste0(portaria[ indices[[i]] ], collapse = "\n"),
                                     "\n", rodape)
    }

    individualizadas <- gsub(paste0(padrao, ' ?-'), "", unlist(lista_portarias))
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
    TX_EMENTA = purrr::map(novas_vetor, ~strsplit(.x, '\n')[[1]]) %>%
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

