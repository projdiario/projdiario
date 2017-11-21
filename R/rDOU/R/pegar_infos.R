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
  gab <- procurar_inicio(pagina, "GABINETE") # GABINETE
  sec <- procurar_inicio(pagina, "SECRETARIA") # SECRETARIA
  inst <- procurar_inicio(pagina, "INSTITUTO") # INSTITUTO
  sfa <- procurar_inicio(pagina, "SUPERINTENDÊNCIA") # SUPERINTENDÊNCIA

  res <- c(gab, sec, inst, sfa)
  names(res) <- pagina[res]

  # retorna um vetor numérico nomeado com o inicio dos orgaos
  res
}

#' Pegar numero dos atos
#'
#' @param ato um vetor com o conteudo de um ato
#'
#' @return A data de \code{vetor} no formato dia DE MES_POR_EXTENSO DE ANO
#' @export
pegar_numero <- function(ato) {
  stringr::str_extract(ato[1], "(N|n).{2,3}[0-9]+\\.?[0-9]*") %>%
    gsub(pattern = "\\.", replacement = "") %>%
    stringr::str_extract("[0-9]+") %>%
    as.numeric() %>% formatC(width = 8, flag = 0)
}

#' Pegar numero dos vetors
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

  para_buscar <- stringr::str_to_upper(ato[[1]]) %>% stringr::str_extract('[A-Z]+ ?[A-Z]* ?[A-Z]*')

  distancia <- RecordLinkage::levenshteinSim(para_buscar, dic_tipos$DES_TIPO)

  res <- dic_tipos$DES_TIPO[distancia == max(distancia)]

  retorno <- match.arg(retorno, c('txt', 'cod'))

  if (retorno == 'txt') {
    res <- dic_tipos$SGL_TIPO[dic_tipos$DES_TIPO == res]
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
  for (arq in arquivos) {
    arq2 <- readLines(arq) %>% stringr::str_replace_all("No-",
                                                        "Nº") %>% stringr::str_trim("both") %>% extract(!stringr::str_detect(.,
                                                                                                                             "Este documento pode ser verificado no endereço")) %>%
      extract(. != "") %>% c("") %>% paste0(collapse = "\\n") %>%
      gsub(pattern = "o-", replacement = "º") %>% gsub(pattern = "°-",
                                                       replacement = "º") %>% gsub(pattern = "°", replacement = "º") %>%
      gsub(pattern = "-\\\\n", replacement = "") %>%
      gsub(pattern = ",\\\\n", replacement = ", ") %>%
      strsplit("\\\\n") %>% extract2(1)
    if (all(ato %in% arq2)) {
      return(stringr::str_extract(arq, "pg[0-9]{3}") %>%
               sub(pattern = "pg", replacement = "") %>% as.numeric())
    }
  }
  cont <- 1
  paginas <- integer(length(arquivos))
  for (arq in arquivos) {
    arq2 <- readLines(arq) %>% stringr::str_replace_all("No-",
                                                        "Nº") %>% stringr::str_trim("both") %>% extract(!stringr::str_detect(.,
                                                                                                                             "Este documento pode ser verificado no endereço")) %>%
      extract(. != "") %>% c("") %>% paste0(collapse = "\\n") %>%
      gsub(pattern = "o-", replacement = "º") %>% gsub(pattern = "°-",
                                                       replacement = "º") %>% gsub(pattern = "°", replacement = "º") %>%
      gsub(pattern = "-\\\\n", replacement = "") %>%
      gsub(pattern = ",\\\\n", replacement = ", ") %>%
      strsplit("\\\\n") %>% extract2(1)
    paginas[cont] <- sum(ato %in% arq2)
    cont <- cont + 1
  }
  resp <- stringr::str_extract(arquivos[which.max(paginas)],
                               "pg[0-9]{3}") %>% sub(pattern = "pg", replacement = "") %>%
    as.numeric()
  if (!is.na(resp)) {
    return(resp)
  }
  else {
    warning("Este ato não foi encontrado em nenhuma página",
            call. = FALSE)
    NA
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

  lim_orgaos <- grep("\\.\\.\\.*? *?[0-9]+", readLines(arquivos[1], encoding = encodificacao)) %>% range()
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

  ###
  conteudo_limpo <- lapply(ministerios, conteudo_orgao) %>% unlist() %>%
    stringr::str_replace_all("No-", "Nº") %>%
    stringr::str_trim("both") %>%
    extract(!stringr::str_detect(., "Este documento pode ser verificado no endereço")) %>%
    extract(. != "") %>% c("") # linha que não aparece pela forma do loop

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
    lista_atos[[i]] <- conteudo_limpo[indices_limpos[[i]]] %>%
      paste0(collapse = "\n")%>%
      gsub(pattern = "o-", replacement = "º") %>%
      gsub(pattern = "°-", replacement = "º") %>%
      gsub(pattern = "°", replacement = "º") %>%
      gsub(pattern = "-\\n", replacement = "") %>%
      gsub(pattern = ",\\n", replacement = ", ") %>%
      strsplit("\\n") %>% extract2(1)
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
#' @param lista_de_atos
#'
#' @return tabela com informações que precisam ser validadas na aplicação
#' @export
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

  if (length(remover) > 0) {
    multiplas <- lapply(normas$TXT_TEXTO[remover], function(x) {
      stringr::str_split(x, "\n")[[1]]
    })

    novas <- lapply(multiplas, multipla_para_individualizada)

    tam_novas <- sapply(novas, length)

    remover <- remover[tam_novas >= 1]

    novas <- novas[tam_novas >= 1]

    novas_vetor <- unlist(novas)

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
      TXT_EMENTA = lapply(novas_vetor, function(x) strsplit(x, '\n')[[1]]) %>%
        sapply(pegar_resumo, USE.NAMES = FALSE),
      DES_TITULO = sapply(novas_vetor, pegar_titulo, USE.NAMES = FALSE),
      NUM_PAGINA = repete_dado("NUM_PAGINA"),
      ID_TIPO_SECAO = repete_dado("ID_TIPO_SECAO")
    )

    res <- dplyr::bind_rows(normas[-remover, ], novas_obs)
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

pegar_sigla_orgao <- function(nome_orgao) {
  distancia <- RecordLinkage::levenshteinSim(nome_orgao, dic_orgaos$DES_ORGAO)
  dic_orgaos$SGL_ORGAO[distancia == max(distancia)][[1]]
}

