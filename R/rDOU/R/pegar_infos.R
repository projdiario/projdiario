#' Pegar data do nome de um ato
#'
#' @param ato um vetor com o conteudo de um ato
#' @return A data do \code{ato}
#'
#' @export
pegar_data <- function(ato) {
  if (grepl('win', Sys.info()["sysname"], ignore.case = TRUE) ) {
    res <- stringr::str_extract(stringr::str_to_lower(ato)[1],
                                "[0-9]{1,2} de [a-z]+ de [0-9]{4}") %>%
      as.Date('%d de %B de %Y')

  } else {
    original <- Sys.getlocale("LC_TIME")
    invisible(Sys.setlocale("LC_TIME", "pt_BR.UTF-8"))
    res <- stringr::str_extract(stringr::str_to_lower(ato)[1],
                                "[0-9]{1,2} de [a-z]+ de [0-9]{4}") %>%
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
  # Procurar termos na página:
  gab <- procurar_inicio(pagina, "GABINETE") # GM
  sec <- procurar_inicio(pagina, "SECRETARIA") # Secretarias
  inst <- procurar_inicio(pagina, "INSTITUTO") # INMET
  sfa <- procurar_inicio(pagina, "SUPERINTENDÊNCIA") # SFAs
  sub <- procurar_inicio(pagina, "SUBSECRETARIA") # SPOA
  comissao <- procurar_inicio(pagina, "COMISSÃO") # CEPLAC
  empresa <- procurar_inicio(pagina, "EMPRESA") # Embrapa
  companhia <- procurar_inicio(pagina, "COMPANHIA") # Conab

  res <- c(gab, sec, inst, sfa, sub, comissao, empresa, companhia)
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
  # Se for retificação voltar NA
  if (grepl('RETIF', ato[1], ignore.case =  TRUE)) {
    return(NA_character_)
  }

  # Caso contrário retornar sem numero
  numero <- stringr::str_extract(ato[1], "(N|n).{2,3}[0-9]+\\.?[0-9]*") %>%
    gsub(pattern = "\\.", replacement = "") %>%
    stringr::str_extract("[0-9]+") %>%
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
  padrao <- "Art\\. ?[I1]º? ?-?"
  art1 <- procurar_inicio(ato, padrao)[1]
  if (!is.na(art1)) {
    texto <- ato[art1]
  } else {
    dois_pontos <- grep(pattern = ":", ato)[1]
    muitos <- stringr::str_locate_all(ato[dois_pontos[1]], ':')[[1]]
    if (length(dois_pontos) > 1 | nrow(muitos) > 1) {
      texto <- ato[dois_pontos[1] ]
    } else {
      texto <- ato[dois_pontos + 1]
    }
  }
  texto %>%
    sub(pattern = padrao, replacement = "") %>%
    sub(pattern = "Nº ?[0-9]+\\.?[0-9]* ?-", replacement = "") %>%
    sub(pattern = "^I -", replacement = "") %>%
    sub(pattern = "[rR][ ,]", replacement = " ") %>%
    gsub(pattern = "\\s\\s", replacement = " ") %>%
    stringr::str_trim() %>%
    paste0('<p>', ., '</p>')
}

#' Pegar tipo dos ato
#'
#' @param ato um vetor com o conteudo de um ato
#' @param retorno Deve retornar 'txt' ou 'cod'?
#'
#' @return O tipo do ato de \code{vetor}.
#'
#' @export
pegar_tipo <- function(ato, retorno = 'txt') {
  primeira_linha <- ato[[1]]
  regex_tipo <- '([A-ZÃÕÁÉÍÓÚÂÊÎÔÛÇ]+( )?)+'
  termo_busca <- primeira_linha %>% stringr::str_to_upper() %>%
    stringr::str_extract(regex_tipo) %>%
    stringr::str_replace_all('MINIST.*', '') %>%
    stringr::str_replace_all('SECRETÁR.*', '') %>%
    stringr::str_replace_all('DIRETOR.*', '') %>%
    stringr::str_replace_all('COORDENADOR.*', '') %>%
    stringr::str_replace_all(' D[AEO]S? ?$', '') %>%
    stringr::str_trim('both')

  # Ajusta termo encontrado (RETIFICAÇÃO) ao termo
  # usado no banco de dados (AVISO DE RETIFICAÇÃO)
  if (termo_busca == 'RETIFICAÇÃO') {
    termo_busca <- 'AVISO DE RETIFICAÇÃO'
  }

  distancia <- rDOU:::levsim(termo_busca, dic_tipos$DES_TIPO)
  maior <- which(distancia == max(distancia))[[1]]
  res <- dic_tipos$DES_TIPO[maior]

  retorno <- match.arg(retorno, c('txt', 'cod'))
  if (retorno == 'txt') {
    res <- dic_tipos$SGL_TIPO[maior]
  } else {
    res <- switch (res,
                   "LEI" = 'A',"PORTARIA" = 'A', "DESPACHO" = 'A',
                   "RETIFI" = 'A', "DECRETO-LEI" = 'A',
                   "DECRETO" = 'A', "ATO" = 'E', "ATA" = 'P',
                   "INSTRUÇÃO NORMATIVA" = 'R', "RESOLU" = 'X',
                   NA_character_
    )
  }
  res
}

#' Título do Ato
#'
#' @param ato
#'
#' @return Título de um ato de seu corpo de texto
#'
#' @export
pegar_titulo <- function(ato) {
  # primeira linha de todo ato é seu título
  titulo <- stringr::str_split(ato, '\n')[[1]][1]
  if (stringr::str_length(titulo) > 400) {
    stringr::str_sub(titulo, 1, 400)
  } else {
    titulo
  }
}

#' Número da Página do ato
#'
#' @param ato um vetor com o conteudo de um ato
#' @param arquivos vetor com caminho dos arquivos em que o ato pode estar
#'
#' @return Página de cada ato
#'
#' @export
pegar_pagina <- function(ato, arquivos) {
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
    texto <- readLines(arquivos[i], encoding = 'latin1') %>% limpar_texto()

    if (all(ato %in% texto)) {
      return(exrtair_pagina(arquivos[i]))
    }

    if (any(texto == titulo_buscado) && titulo_buscado != 'RETIFICAÇÃO') {
      return(exrtair_pagina(arquivos[i]))
    }

    paginas[i] <- sum(unique(ato) %in% unique(texto))
  }

  resp <- exrtair_pagina(arquivos[which.max(paginas)])
  if (!is.na(resp)) {
    return(resp)
  }
  else {
    warning("Este ato não foi encontrado em nenhuma página", call. = FALSE)
    return(NA_real_)
  }
}

#' Pega todos os dados de todos os atos de um dia do DOU (txt)
#'
#' @param debug A função está sendo debugada?
#' @param arquivos um vetor com os caminhos dos arquivos (.txt) de um dia do DOU
#'
#' @return Uma lista com todas as normas extraidas do DOU e algumas mata-informações
#'
#' @export
pegar_normas_dou <- function(arquivos, debug = FALSE) {
  SECAO <- unique(stringr::str_extract(arquivos, "DOU[1-3]"))
  DATA <- stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
    unique() %>% stringr::str_replace_all('_', '/')

  if (debug) cat(DATA,'\n')

  encodificacao <- 'latin1'

  conteudo <- lapply(arquivos, readLines, encoding = encodificacao) %>% unlist()
  pag1 <- readLines(arquivos[1], encoding = encodificacao)
  lim_orgaos <- grep("[\\.]{2,} *?[0-9]+", pag1) %>% range()
  orgaos <- conteudo[lim_orgaos[1]:lim_orgaos[2]] %>% paste(collapse = "") %>%
    stringr::str_split("\\.") %>%
    extract2(1) %>% extract(. != "") %>%
    stringr::str_replace_all("[0-9]+", "") %>% stringr::str_trim()

  # 2 - Delimitar atos dos Ministérios
  ###
  conteudo_orgao <- function(nome) {
    padrao <- paste0("Ministério *d..? *", nome)
    conteudo_orig <- conteudo %>%
      gsub(pattern = "</?table>", replacement = "") %>%
      gsub(pattern = "</?tr>", replacement = "") %>%
      gsub(pattern = "</?td>", replacement = "")
    if (any(grepl(padrao, orgaos))) {
      alvo <- procurar_inicio(conteudo_orig, padrao) %>%
        extract(which(!grepl(pattern = "[0-9]+", conteudo[. + 1])))
      if (length(alvo) > 1) {
        alvo <- alvo[alvo > 30][1]
      }
      nome_prox_alvo <- orgaos[grep(nome, orgaos) + 1] %>%
        stringr::str_extract("Ministério [[:alpha:]]+ [[:alpha:]]+")

      suppressWarnings({
        prox_alvo <- procurar_inicio(conteudo_orig, nome_prox_alvo) %>%
          extract(. > alvo) %>% min()
      })

      if (is.infinite(prox_alvo)) {
        suppressWarnings({
          prox_alvo <- procurar_inicio(conteudo_orig, paste0("\t", nome_prox_alvo)) %>%
            extract(. > alvo) %>% min()
        })
      }
      # Se falhou até então erro na conversão é muito provável
      if (is.infinite(prox_alvo)) {
        message('O início do Ministério subsequente ao Ministério alvo não foi encontrado no ',
                SECAO, ' de ', DATA,'.\n',
                'Verifique se todas as páginas do Ministério alvo foram convertidas para \'.txt\' corretamente')
        res <- ""
      } else {
        # linhas que correspondem aos elementos
        res <- conteudo[alvo: (prox_alvo - 1)]
      }
    } else {
      message('Padrão não está entre os órgãos identificados no ', SECAO, ' de ', DATA)
      res <- ""
    }
    res
  }

  # ministerios <- c("Agricultura", "Meio", "Saúde")
  ministerios <- c("Agricultura")

  conteudo_limpo <- lapply(ministerios, conteudo_orgao) %>% unlist() %>%
    rDOU:::limpar_texto() %>% c("") # linha que não aparece pela forma do loop

  # 3 - fazer busca pelo inicio dos atos
  # padrão TIPO DE ATO ao inicio da linha

  # tipo de ATO
  # Lei
  # Decreto
  # DECISÃO
  # PORTARIAS DE XX
  # PORTARIA Nº XXX
  # DESPACHO
  # RETIFICAÇÃO[ÕES]
  # INSTRUÇÃO
  # RESOLUÇÃO
  # ATO
  # ATA

  leis <- procurar_inicio(conteudo_limpo, "LEI")

  decretos <- procurar_inicio(conteudo_limpo, "DECRETO")

  decisao <- procurar_inicio(conteudo_limpo, "DECIS")

  portarias <- procurar_inicio(conteudo_limpo, "PORTARIA")

  despachos <- procurar_inicio(conteudo_limpo, "DESPACHO")

  retificacao <- procurar_inicio(conteudo_limpo, "RETIFICAÇ")

  instrucao <- procurar_inicio(conteudo_limpo, "INSTRUÇ")

  resolucao <- procurar_inicio(conteudo_limpo, "RESOLUÇ")

  ato <- procurar_inicio(conteudo_limpo, "ATO ")

  ata <- procurar_inicio(conteudo_limpo, "ATA ")

  atos <- c(leis, decretos, decisao, portarias, despachos, retificacao,
            instrucao, resolucao, ato, ata, length(conteudo_limpo)) %>% sort()

  indices <- vector("list", length(atos) - 1)

  for (i in seq_along(indices)) {
    indices[[i]] <- atos[i]:(atos[i+1] - 1)
  }

  limites_orgaos <- pegar_limites_orgaos(conteudo_limpo)

  indices_limpos <- lapply(indices, function(x) x[!(x %in% limites_orgaos)])

  lista_atos <- vector("list", length(atos) - 1)

  for (i in seq_along(lista_atos)) {
    lista_atos[[i]] <- conteudo_limpo[indices_limpos[[i]]]
  }

  data_dou <- stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
    as.Date(format = "%Y_%m_%d") %>% unique()

  meio <- arquivos[1] %>% stringr::str_split('/') %>%
    extract2(1) %>% extract(length(.)) %>% stringr::str_sub(1, 4)
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
    secao = tipo_secao
  )

}

#' Tabela para Validação na Aplicação
#'
#' @param lista_de_normas
#'
#' @return tabela com informações que precisam ser validadas na aplicação
#' @export
criar_tabela_app <- function(lista_de_normas) {
  arquivos <- attr(lista_de_normas, 'arquivos')

  normas <- tibble::tibble(
    NUM_ATO = sapply(lista_de_normas, pegar_numero, USE.NAMES = FALSE), # Ok
    SGL_TIPO = sapply(lista_de_normas, pegar_tipo, USE.NAMES = FALSE), # Ok
    VLR_ANO = attr(lista_de_normas, 'data_dou') %>% lubridate::year() %>% as.character(), # Deriva de DTA_PROMULGACAO
    SGL_ORGAO = attr(lista_de_normas, 'orgao'), # Ok
    COD_TIPO = sapply(lista_de_normas, pegar_tipo, 'cod', USE.NAMES = FALSE), # tem que derivar do tipo
    TXT_TEXTO = sapply(lista_de_normas, paste, collapse = "\n", USE.NAMES = FALSE), # Ok
    DTA_PROMULGACAO = attr(lista_de_normas, 'data_dou'), # Ok
    TXT_EMENTA = sapply(lista_de_normas, pegar_resumo, USE.NAMES = FALSE), # A principio fora
    DES_TITULO = sapply(lista_de_normas, pegar_titulo, USE.NAMES = FALSE),
    NUM_PAGINA = sapply(lista_de_normas, pegar_pagina, arquivos, USE.NAMES = FALSE),
    ID_TIPO_SECAO = attr(lista_de_normas, 'secao')
  )

  remover <- sort(c(grep("PORTARIAS", normas$DES_TITULO),
                    grep("DECISÕES", normas$DES_TITULO),
                    grep("RETIFICAÇÕES", normas$DES_TITULO)))

  if (length(remover) > 0) {
    novas_obs <- purrr::map(remover, novas_observacoes, normas, arquivos)

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

  res$TXT_TEXTO <- res$TXT_TEXTO %>% purrr::map2_chr(res$SGL_ORGAO, texto_para_html)
  res$SGL_ORGAO <- pegar_sigla_orgao(res$SGL_ORGAO)
  res
}

#' Parsear normas em uma sessao do DOU e escrever na base
#'
#' @param conexao Uma conexao
#' @param pastas Vetor com pastas em que se encontram as paginas txt de uma unidade do DOU
#' @param debug Debugando?
#'
#' @return
#' @export
#'
parsear_e_escrever <- function(conexao, pastas, debug = FALSE) {

  parseadas <- RJDBC::dbGetQuery(conexao, 'SELECT ID, DTA_PROMULGACAO, ID_TIPO_SECAO FROM ATO_PARSE')

  if (nrow(parseadas) > 0) {
    maior_id <- max(as.numeric(parseadas$ID))

    datas <- parseadas$DTA_PROMULGACAO %>% substr(1, 10) %>%
      as.Date(format = '%Y-%m-%d') %>% format(format = "%Y/%B/%d") %>%
      paste0('DOU', parseadas$ID_TIPO_SECAO, '/',.) %>%
      unique()

    pastas_lidas <- purrr::map(datas, grep, pastas) %>% Reduce(f = c)
    if (length(pastas_lidas) != 0) {
      pastas_ler <- pastas[-pastas_lidas]
      if (length(pastas_ler) == 0) {
        message('Estas pastas já foram incluídas na base')
        return(TRUE)
      }
    } else {
      pastas_ler <- pastas
    }

  } else {
    maior_id <- 0
    pastas_ler <- pastas
  }

  lista_arquivos <- lapply(pastas_ler, function(x) dir(x, full.names = T))

  lista_de_normas <- purrr::map(lista_arquivos, pegar_normas_dou, debug = debug)
  normas <- purrr::map_df(lista_de_normas, criar_tabela_app) %>% gerar_id(anterior = maior_id)

  for (linha in seq_len(nrow(normas))) {
    linha_atual <- normas[linha, ]
    RJDBC::dbSendUpdate(conexao,
                        "INSERT INTO ATO_PARSE
                        VALUES (:1, :2, :3, :4, :5, :6, :7, TO_DATE(:8, 'yyyy-mm-dd'), :9, :10, :11, :12)",
                        linha_atual$ID, formatC(linha_atual$NUM_ATO, width = 8, flag = 0), linha_atual$SGL_TIPO,
                        linha_atual$VLR_ANO, substr(linha_atual$SGL_ORGAO, 1, 30), linha_atual$COD_TIPO,
                        linha_atual$TXT_TEXTO, linha_atual$DTA_PROMULGACAO, linha_atual$TXT_EMENTA,
                        linha_atual$DES_TITULO, linha_atual$NUM_PAGINA, linha_atual$ID_TIPO_SECAO
    )
  }
  cat(nrow(normas), 'normas foram inseridas na base.\n')
  RJDBC::dbCommit(conexao)
}

#' Aplicacao de algoritmo de levenshtein entre vetor e valor escalar
#'
#' @param str1 primeira string
#' @param str2 segunda string
#'
#' @return vetor de distancias
#'
# Esta funcao foi tirada deste link
# https://stackoverflow.com/questions/11535625/similarity-scores-based-on-string-comparison-in-r-edit-distance#11535768
levsim <- function (str1, str2) {
  indice <- RecordLinkage::levenshteinDist(str1, str2)
  tamanho_maior <- max(nchar(str1), nchar(str2))
  return(1 - (indice / tamanho_maior))
}

#' Pegar Sigla do Orgao
#'
#' @param nome_orgao
#'
#' @return A sigla do orgao passado como argumento
#'
#' @examples
pegar_sigla_orgao <- function(nome_orgao) {
  if (length(nome_orgao) > 1) {
    return(purrr::map_chr(nome_orgao, pegar_sigla_orgao))
  }
  distancia <- levsim(nome_orgao, dic_orgaos$DES_ORGAO)
  dic_orgaos$SGL_ORGAO[distancia == max(distancia)][[1]]
}

