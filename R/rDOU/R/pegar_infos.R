#' Pegar data do nome de um ato
#'
#' @param ato um vetor com o conteudo de um ato
#' @return A data do \code{ato}
#'
#' @export
pegar_data <- function(ato) {
  if (grepl('win', Sys.info()["sysname"], ignore.case = TRUE) ) {
    res <- stringr::str_extract(stringr::str_to_lower(ato)[1],
                                "\\d{1,2} de [a-z]+ de \\d{4}") %>%
      as.Date('%d de %B de %Y')

  } else {
    original <- Sys.getlocale("LC_TIME")
    invisible(Sys.setlocale("LC_TIME", "pt_BR.UTF-8"))
    res <- stringr::str_extract(stringr::str_to_lower(ato)[1],
                                "\\d{1,2} de [a-z]+ de \\d{4}") %>%
      as.Date('%d de %B de %Y')

    invisible(Sys.setlocale("LC_TIME", original))
  }
  res
}

#' Pegar o limite dos orgaos de um ministerio
#'
#' @param pagina um vetor com todo o conteudo de um dia do DOU
#'
#' @return Em qual linha (elemento) de \code{pagina} estao os orgaos de um ministerio
#'
#' @export
pegar_limites_orgaos <- function(pagina) {
  # # Procurar termos na página:
  res <- c("^GABINETE", "^SECRETARIA", "^INSTITUTO", "^SUPERINTEND\\u00caNCIA",
           "^SUBSECRETARIA", "^COMISS\\u00c3O", "^EMPRESA", "^COMPANHIA") %>%
    desescapar() %>%
    purrr::map(~ stringr::str_which(pagina, .x)) %>% unlist()

  names(res) <- pagina[res]

  # retorna um vetor numérico nomeado com o inicio dos orgaos
  res
}

#' Pegar numero dos atos
#'
#' @param ato um vetor com o conteudo de um ato
#'
#' @return O numero de \code{ato} com 8 digitos (completados com 0)
#' @export
pegar_numero <- function(ato) {
  # Se for retificaçao voltar NA
  if (grepl('RETIF', ato[1], ignore.case =  TRUE)) {
    return(NA_character_)
  }

  # Caso contrário retornar sem numero
  numero <- stringr::str_extract(ato[1], "[Nn].{2,3}\\d{1,3}(\\.?\\d{3})*") %>%
    gsub(pattern = "\\.", replacement = "") %>%
    stringr::str_extract("\\d+") %>%
    as.numeric()
  if (is.na(numero)) {
    return(NA_character_)
  }
  formatC(numero, width = 8, flag = 0)
}

#' Pegar resumo do ato
#'
#' @param ato um vetor com o conteudo de um ato
#'
#' @return A data de \code{vetor} no formvetor dia DE MES_POR_EXTENSO DE ANO
#'
#' @export
pegar_resumo <- function (ato) {
  padrao <- desescapar("Art\\\\. ?(I|1\\u00ba?) ?-?")
  art1 <- stringr::str_which(ato, paste0("^", padrao))[1]
  if (!is.na(art1)) {
    texto <- ato[art1]
  } else {
    dois_pontos <- stringr::str_which(ato, ":")[1]
    muitos <- stringr::str_locate_all(ato[dois_pontos[1]], ':')[[1]]
    if (length(dois_pontos) > 1 | nrow(muitos) > 1) {
      texto <- ato[ dois_pontos[1] ]
    } else {
      texto <- ato[dois_pontos + 1]
    }
  }
  regex_num <- desescapar("N\\u00ba ?\\\\d{1,3}(\\\\.?\\\\d{3})* ?-")
  resp <- texto %>%
    stringr::str_replace(padrao, "") %>%
    stringr::str_replace(regex_num, "") %>%
    stringr::str_replace("^I -", "") %>%
    stringr::str_replace("[rR]\\b,?", "") %>%
    stringr::str_replace_all("\\s\\s", " ") %>%
    stringr::str_trim()
  resp
}

#' Pegar tipo dos ato
#'
#' @param ato um vetor com o conteudo de um ato
#'
#' @return O tipo do ato de \code{vetor}.
#'
#' @export
pegar_tipo <- function(ato) {
  primeira_linha <- ato[[1]]
  regex_tipo <- paste0("([A-Z\\u00c3\\u00d5\\u00c1\\u00c9\\u00cd\\u00d3\\u00da",
                       "\\u00c2\\u00ca\\u00ce\\u00d4\\u00db\\u00c7]+( )?)+") %>%
    desescapar()
  termo_busca <- primeira_linha %>% stringr::str_to_upper() %>%
    stringr::str_extract(regex_tipo) %>%
    stringr::str_replace_all('MINIST.*', '') %>%
    stringr::str_replace_all(desescapar('SECRET\\u00c1R.*'), '') %>%
    stringr::str_replace_all('DIRETOR.*', '') %>%
    stringr::str_replace_all('COORDENADOR.*', '') %>%
    stringr::str_replace_all(' D[AEO]S? ?$', '') %>%
    stringr::str_trim('both')

  # Ajusta termo encontrado (RETIFICAÇaO) ao termo
  # usado no banco de dados (AVISO DE RETIFICAÇaO)
  if (termo_busca == desescapar("RETIFICA\\u00c7\\u00c3O")) {
    termo_busca <- desescapar("AVISO DE RETIFICA\\u00c7\\u00c3O")
  }

  dic_tipos <- rDOU::dic_tipos

  distancia <- levsim(termo_busca, dic_tipos$DES_TIPO)
  maior <- which(distancia == max(distancia))[[1]]

  dic_tipos$SGL_TIPO[maior]
}

#' Título do Ato
#'
#' @param ato Vetor de texto com conteudo da norma legal
#'
#' @return Título de um ato de seu corpo de texto
#'
#' @export
pegar_titulo <- function(ato) {
  # primeira linha de todo ato e seu titulo
  titulo <- stringr::str_split(ato, '\n')[[1]][1]
  # eventualmente limita tamanho do titulo
  stringr::str_sub(titulo, 1, 400)
}

#' Numero da Pagina do ato
#'
#' @param ato um vetor com o conteudo de um ato
#' @param arquivos vetor com caminho dos arquivos em que o ato pode estar
#' @param encodificacao encodificação dos arquivos
#'
#' @return Página de cada ato
#'
#' @export
pegar_pagina <- function(ato, arquivos, encodificacao = 'latin1') {
  if (length(ato) == 1) {
    ato <- stringr::str_split(ato, '\n')[[1]]
  }

  exrtair_pagina <- function(x) {
    stringr::str_extract(x, "pg[0-9]{3}") %>%
      sub(pattern = "pg", replacement = "") %>%
      as.numeric()
  }

  titulo_buscado <- pegar_titulo(ato)

  paginas <- integer(length(arquivos))

  for (i in seq_along(arquivos)) {
    texto <- readLines(arquivos[i], encoding = encodificacao, warn = FALSE) %>%
      limpar_texto()

    if (all(ato %in% texto)) {
      return(exrtair_pagina(arquivos[i]))
    }

    retif <- desescapar("RETIFICA\\u00c7\\u00c3O")
    if (any(texto == titulo_buscado) && titulo_buscado != retif) {
      return(exrtair_pagina(arquivos[i]))
    }

    paginas[i] <- sum(unique(ato) %in% unique(texto))
  }

  resp <- exrtair_pagina(arquivos[which.max(paginas)])
  if (!is.na(resp)) {
    return(resp)
  }
  else {
    desescapar("Este ato n\\u00e3o foi encontrado em nenhuma p\\u00e1gina") %>%
    warning(call. = FALSE)
    return(NA_real_)
  }
}

#' Conteudo de um orgao
#'
#' @param orgao_alvo nome do orgao procurado
#' @param conteudo vetor de texto com conteudo do DOU
#' @param orgaos vetor de texto com orgaos publicados naquele dia e secao
#' @param arquivo arquivo com informacoes de secao e data do DOU
#' @param consulta Se TRUE retorna o nome do orgao_alvo como esta no DOU
#'
#' @return texto com as publicacoes do orgao alvo
conteudo_orgao <- function(orgao_alvo, conteudo, orgaos, arquivo, consulta = FALSE) {
  SECAO <- stringr::str_extract(arquivo, "DOU[1-3]")
  DATA <- stringr::str_extract(arquivo, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
    stringr::str_replace_all('_', '/')

  inicios <- "(Atos|Minist\\u00e9rio|Presid\\u00eancia|Tribunal|Entidades)"
  padrao <- paste0("^", desescapar(inicios)," *d..? *", orgao_alvo)
  conteudo_orig <- stringr::str_replace_all(conteudo, "</?(table|tr|td)>", "")

  if (any(grepl(padrao, orgaos))) {
    alvo <- stringr::str_which(conteudo_orig, padrao)
    alvo <- alvo[grep(pattern = "\\D+", conteudo[alvo + 1])]
    if (length(alvo) > 1) {
      alvo <- alvo[alvo > 30][1]
    }

    if (consulta) return(alvo)

    nome_prox_alvo <- orgaos[grep(orgao_alvo, orgaos) + 1] %>%
      stringr::str_extract(paste0(desescapar(inicios), "( ?[[:alpha:]])+"))

    suppressWarnings({
      prox_alvo <- stringr::str_which(conteudo_orig, paste0("^", nome_prox_alvo))
      prox_alvo <- prox_alvo[prox_alvo > alvo] %>% min()
    })

    if (is.infinite(prox_alvo)) {
      suppressWarnings({
        prox_alvo <- stringr::str_which(conteudo_orig, paste0("^\t", nome_prox_alvo))
        prox_alvo <- prox_alvo[prox_alvo > alvo] %>% min()
      })
    }
    # Se falhou até entao erro na conversao é muito provável
    if (is.infinite(prox_alvo)) {
      paste0("O in\\u00edcio do \\u00f3rgao subsequente ao \\u00f3rgao ",
            "alvo n\\u00e3o foi encontrado no ", SECAO, " de ", DATA,".\n",
            "Verifique se todas as p\\u00e1ginas do \\u00f3rg\\u00e3o alvo ",
            "foram convertidas para '.txt' corretamente") %>%
        desescapar() %>%
      warning(call. = FALSE)
      res <- ""
    } else {
      # linhas que correspondem aos elementos
      res <- conteudo[alvo: (prox_alvo - 1)]
    }
  } else {
    desescapar(c("Padr\\u00e3o n\\u00e3o est\\u00e1 entre os \\u00f3rg\\u00e3os ",
               "identificados no ", SECAO, ' de ', DATA)) %>%
      warning(call. = FALSE)
    res <- ""
  }
  res
}

#' Encontrar inicio dos atos no texto
#'
#' @param texto Texto em que as normas serao buscadas
#'
#' @return vetor  numerico com inicio dos atos
inicios_atos <- function(texto) {
  tipos <- c("LEI", "DECRETO", "DECIS", "PORTARIA", "DESPACHO", "ATO ",
             "RETIFICA\\u00c7", "INSTRU\\u00c7", "RESOLU\\u00c7", "ATA ") %>%
    desescapar()
  atos <- paste0("^", tipos) %>%
    purrr::map(~ stringr::str_which(texto, .x )) %>%
    unlist() %>% c(length(texto))

  sort(atos)
}

#' Pega todos os dados de todos os atos de um dia do DOU (txt)
#'
#' @param arquivos um vetor com os caminhos dos arquivos (.txt) de um dia do DOU
#' @param orgao_alvo orgão cujas normas seram buscadas
#' @param encodificacao encodificação dos arquivos
#'
#' @return Uma lista com todas as normas extraidas do DOU e algumas mata-informações
#'
#' @export
pegar_normas_dou <- function(arquivos, orgao_alvo, encodificacao = 'latin1') {
  conteudo <- lapply(arquivos, readLines, warn = FALSE,
                     encoding = encodificacao) %>% unlist()
  pag1 <- readLines(arquivos[1], encoding = encodificacao, warn = FALSE)
  lim_orgaos <- grep("[\\.]{2,} *?[0-9]+", pag1) %>% range()
  orgaos <- conteudo[lim_orgaos[1]:lim_orgaos[2]] %>% paste(collapse = "") %>%
    stringr::str_split("\\.") %>% extract2(1)
  orgaos <- orgaos[orgaos != ""] %>%
    stringr::str_replace_all("[0-9]+", "") %>% stringr::str_trim()

  # 2 - Delimitar atos dos Ministérios
  conteudo_limpo <- purrr::map(orgao_alvo, conteudo_orgao, conteudo,
                               orgaos, arquivos[1]) %>%
    unlist() %>%
    limpar_texto() %>% c("") # elemento que sera eliminado pela forma do loop

  # 3 - fazer busca pelo inicio dos atos
  # padrao TIPO DE ATO ao inicio da linha
  atos <- inicios_atos(conteudo_limpo)

  indices <- criar_indices(atos)

  limites_orgaos <- pegar_limites_orgaos(conteudo_limpo)

  indices_limpos <- purrr::map(indices, ~ .x[!(.x %in% limites_orgaos)])

  lista_atos <- purrr::map(indices_limpos, ~ conteudo_limpo[.x])

  data_dou <- stringr::str_extract(arquivos, "\\d{4}_\\d{2}_\\d{2}") %>%
    as.Date(format = "%Y_%m_%d") %>% unique()

  meio <- arquivos[1] %>%
    stringr::str_split('/') %>%
    extract2(1)
  meio <- meio %>%
    extract(length(meio)) %>%
    stringr::str_sub(1, 4)

  tipo_secao <- switch(meio, "DOU1" = 1, "DOU2" = 2, "DOU3" = 3,
                       "DOUE" = 4, # Edição extra
                       "DOUS" = 5,  NA) # Suplemento e caso padrão

  lista_atos <- lapply(lista_atos, eliminar_quebras)

  structure(
    lista_atos,
    class = 'norma',
    orgao = conteudo_limpo[atos[-length(atos)] %em% limites_orgaos],
    arquivos = arquivos,
    data_dou = data_dou,
    secao = tipo_secao,
    encodificacao = encodificacao,
    orgao_alvo = conteudo_orgao(orgao_alvo, conteudo_limpo, orgaos, arquivos[1],
                                TRUE)
  )

}

#' Tabela para Validação na Aplicação
#'
#' @param lista_de_normas Lista de normas retornada pela função \code{pegar_normas_dou}.
#'
#' @return tabela com informações que precisam ser validadas na aplicação
#' @export
estruturar_normas <- function(lista_de_normas) {
  data_dou <- attr(lista_de_normas, 'data_dou')
  arquivos <- attr(lista_de_normas, 'arquivos')
  encodificacao <- attr(lista_de_normas, 'encodificacao')
  cabecalho <- attr(lista_de_normas, 'orgao_alvo')

  normas <- tibble::tibble(
    NR_ATO = purrr::map_chr(lista_de_normas, pegar_numero),
    SG_TIPO = purrr::map_chr(lista_de_normas, pegar_tipo),
    AN_ATO = lubridate::year(data_dou) %>% as.character(),
    SG_ORGAO = attr(lista_de_normas, 'orgao'),
    CD_TIPO_ATO = 'A', # codigo é verificado na aplicação
    TX_TEXTO = purrr::map_chr(lista_de_normas, paste, collapse = "\n"),
    DT_PROMULGACAO = attr(lista_de_normas, 'data_dou'),
    TX_EMENTA = purrr::map_chr(lista_de_normas, pegar_resumo),
    DS_TITULO = purrr::map_chr(lista_de_normas, pegar_titulo),
    NM_PAGINA = purrr::map_dbl(lista_de_normas, pegar_pagina, arquivos,
                               encodificacao),
    ID_TIPO_SECAO = attr(lista_de_normas, 'secao')
  )

  remover <- c(
    stringr::str_which(normas$DS_TITULO, "AS\\b"),
    stringr::str_which(normas$DS_TITULO,"OS\\b"),
    stringr::str_which(normas$DS_TITULO, desescapar("\\u00d5ES\\\\b"))
  ) %>% sort()

  if (length(remover) > 0) {
    novas_obs <- purrr::map(remover, novas_observacoes, normas, arquivos,
                            encodificacao)

    # Garante que a ordem seja a mesma da publicação
    for (i in seq_along(remover)) {
      if (!'atual' %in% ls()) {
        antes <- seq_len(remover[i])
        atual <- dplyr::bind_rows(normas[antes[-length(antes)], ],
                                  novas_obs[[i]])
      } else {
        anteriores <- seq_len(remover[i-1])
        antes <- seq_len(remover[i])[-anteriores]
        atual <- dplyr::bind_rows(atual, normas[antes[-length(antes)], ],
                                  novas_obs[[i]])
      }
    }

    if (remover[i] != nrow(normas)) {
      ultimas <- seq(remover[i] + 1, nrow(normas))
      res <- dplyr::bind_rows(atual, normas[ultimas, ])
    } else {
      res <- atual
    }

  } else {
    res <- normas
  }

  res$TX_TEXTO <- res$TX_TEXTO %>%
    purrr::map2_chr(res$SG_ORGAO, texto_para_html, cabecalho = cabecalho)
  res$SG_ORGAO <- pegar_sigla_orgao(res$SG_ORGAO)
  res
}

#' Aplicacao de algoritmo de levenshtein entre vetor e valor escalar
#'
#' @param str1 primeira string
#' @param str2 segunda string
#'
#' @return vetor de distancias
#'
# Esta funcao foi tirada deste link
# https://stackoverflow.com/questions/11535625/#11535768
levsim <- function (str1, str2) {
  indice <- RecordLinkage::levenshteinDist(str1, str2)
  tamanho_maior <- max(nchar(str1), nchar(str2))
  return(1 - (indice / tamanho_maior))
}

#' Pegar Sigla do Orgao
#'
#' @param nome_orgao Nome do orgao cuja sigla sera buscada
#'
#' @return A sigla do orgao passado como argumento
#'
pegar_sigla_orgao <- function(nome_orgao) {
  if (length(nome_orgao) > 1) {
    return(purrr::map_chr(nome_orgao, pegar_sigla_orgao))
  }

  if (length(nome_orgao) == 0) {
    return(character(0))
  }

  dic_orgaos <- rDOU::dic_orgaos

  distancia <- levsim(nome_orgao, dic_orgaos$DES_ORGAO)
  maximo <- max(distancia)
  dic_orgaos$SGL_ORGAO[distancia == maximo][[1]]
}

