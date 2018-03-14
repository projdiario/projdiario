#' Procura um termo no inicio de uma string
#'
#' @param vetor um vetor de texto
#' @param termo termo a ser buscado
#'
#' @return O índice do \code{vetor} em que o \code{termo} se encontra no inicio da string
#'
#' @export
procurar_inicio <- function(vetor, termo) {
  termo_sem_regex <- stringr::str_replace_all(termo, "\\[.+?\\]", "") %>%
    stringr::str_replace_all("\\{.+?\\}", "") %>%
    stringr::str_replace_all("[+^?*]", "")
  indice <- stringr::str_which(stringr::str_sub(vetor, 1,
                                                stringr::str_length(termo_sem_regex)),
                               termo)

  # retorna ínidice em que letras iniciais batem com o termo
  indice
}

#' Qual faixa de b (limite) contem a (conteudo)?
#'
#' @rdname em
#'
#' @param conteudo
#' @param limite
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
  ID <- seq(base_id + 1, by = 1, length.out = nrow(df)) %>%
    formatC(width = 10, flag = '0')

  tibble::add_column(df, ID, .before = TRUE)
}

#' Transformar Texto em Parágrafos de HTML
#'
#' @param texto Um vetor com texto
#'
#' @return O mesmo texto com as quebras de linha substituídas por tags de parágrafos \code{<p>}.
#'         Adiciona um cabeçalho.
#'
#' @export
texto_para_html <- function(texto, orgao) {
  gsub("\\n<" , "\r<", texto) %>%
    gsub("\\n\\s" , "\n", .) %>%
    gsub("\\n" , "</p><p>\n", .) %>% # Aqui estou incluindo um '\n',
    # não sei bem porque fiz isso. Vou deixar até testar
    gsub("\\r" , "", .) %>%
    paste0("<p>MINISTÉRIO DA AGRICULTURA, PECUÁRIA E ABASTECIMENTO</p>",
          "<p>", orgao,"</p>", .)
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
  # texto = 'Um monte de quebras Nº \n21000....'
  # texto = 'Multiplas\n quebras de linhas\n na mesma \nstring'
  # texto = string7
  regex_boas <- '\\n[a-zA-Z]{1,3} ?[\\)\\.-]|\\n[IVXCDLivxcld]+ ?[\\)\\.-]|\\n[A-Z]|\\n[0-9]{1,2} ?[\\)\\.-]'
  quebras_boas <- stringr::str_locate_all(texto, regex_boas)[[1]]

  for (i in sort(seq_len(nrow(quebras_boas)), TRUE)) {
    stringr::str_sub(texto, quebras_boas[i, , drop = FALSE]) <- stringr::str_sub(texto, quebras_boas[i, , drop = FALSE]) %>%
      stringr::str_replace_all('\\n', '{quebra}')
  } # texto

  quebras <- stringr::str_locate_all(texto, '\n ?[[:alnum:]]')[[1]]
  while (length(quebras) > 0) {
    encontrado <- stringr::str_sub(texto, quebras)
    if (stringr::str_detect(encontrado[1], ' ')) {
      trecho <- quebras[1, ] - c(0, 2)
    } else {
      trecho <- quebras[1, ] - c(0, 1)
    }
    stringr::str_sub(texto, trecho[1], trecho[2]) <- ''
    quebras <- stringr::str_locate_all(texto, '\n ?[[:alnum:]]')[[1]]
  }

  texto %>% stringr::str_replace_all('\\{quebra\\}', '\n')
}

#' Cria um objeto para cada portaria em objeto de portarias multiplas
#'
#' @param portaria um vetor o texto da portaria multipla
#'
#' @return um vetor com uma portaria em cada elemento
#'
#' @export
multipla_para_individualizada <- function(portaria) {
  # portaria <- normas$DS_CONTEUDO[remover[8]] %>% stringr::str_split("\n") %>% .[[1]]
  # Função para verificar existencia de "chunks"
  casos <- function(texto){
    resolve <- grep("resolve[::punct::]", texto)
    cessao <- grep("cess[ãa]o[\\.:]", texto)
    # if (length(cessao) > 0)
    #   warning(paste(texto[cessao], collapse = '|\n'), call. = FALSE)
    sort(unique(c(resolve, cessao)))
    #resolve
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
  # Regra para caso de cultivadores
  # if (length(grep("resolve:", portaria)) == 0) {
  if (length(casos(portaria)) == 0) {
    # 'proteção:'
    padrao <- 'Nº ?[0-9]+\\.?[0-9]*'
    inicios <- c(grep(padrao, portaria), length(portaria))

    nomes <- paste("DECISÃO", stringr::str_extract(portaria, padrao) %>%
                     extract(!is.na(.)),
                   stringr::str_sub(portaria[1], 11))

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

    nomes <- paste("PORTARIA", stringr::str_extract(portaria, padrao) %>%
                     extract(!is.na(.)),
                   stringr::str_sub(portaria[1], 11))

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

novas_observacoes <- function(lista_de_indices, df) {
  multiplas <- lapply(df$TXT_TEXTO[lista_de_indices], function(x) {
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
    NUM_ATO = sapply(novas_vetor, pegar_numero, USE.NAMES = FALSE),
    SGL_TIPO = sapply(novas_vetor, pegar_tipo, USE.NAMES = FALSE),
    VLR_ANO = repete_dado("VLR_ANO"),
    SGL_ORGAO = repete_dado("SGL_ORGAO"),
    COD_TIPO = sapply(novas_vetor, pegar_tipo, 'cod', USE.NAMES = FALSE),
    TXT_TEXTO = novas_vetor,
    DTA_PROMULGACAO = as.Date(repete_dado("DTA_PROMULGACAO"), origin = "1970-01-01"),
    TXT_EMENTA = purrr::map(novas_vetor, ~strsplit(.x, '\n')[[1]]) %>%
      sapply(pegar_resumo, USE.NAMES = FALSE),
    DES_TITULO = sapply(novas_vetor, pegar_titulo, USE.NAMES = FALSE),
    NUM_PAGINA = repete_dado("NUM_PAGINA"),
    ID_TIPO_SECAO = repete_dado("ID_TIPO_SECAO")
  )

  novas_obs
}

