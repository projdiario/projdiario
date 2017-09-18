#' Pega todos os dados de todos os atos de um dia do DOU (txt)
#'
#' @export
#' @param arquivos um vetor com os caminhos dos arquivos (.txt) de um dia do DOU
#' @return Uma tabela com todos os dados extraidos do DOU.
#' @examples
#' #Sem exemplo


pega_dados_dou <- function(arquivos, debug = FALSE) {
  if (debug) cat(unique(stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}")),'\n')
  # arquivos de teste
  # arquivos <- dir(path = "../../dados/txt", pattern = "DOU.+.txt$", full.names = TRUE)
  # arquivos <- dir(path = "dados/txt", pattern = "DOU.+.txt$", full.names = TRUE)
  # lista_arquivos <- split(arquivos, stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>% as.factor())
  # arquivos <- lista_arquivos[[58]] ; rm(lista_arquivos)
  if (grepl('win', Sys.info()["sysname"], ignore.case = TRUE) ) {
    encodificacao <- 'latin1'
  } else {
    encodificacao <- 'latin1'
  }
  
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
      alvo <- procura_inicio(conteudo_orig, padrao) %>%
        extract(which(!grepl(pattern = "[0-9]+", conteudo[. + 1])))
      if (length(alvo) > 1) {
        alvo <- alvo[alvo > 30][1]
      }
      nome_prox_alvo <- orgaos[grep(nome, orgaos) + 1] %>%
        stringr::str_extract("Ministério [[:alpha:]]+ [[:alpha:]]+")

      suppressWarnings({
        prox_alvo <- procura_inicio(conteudo_orig, nome_prox_alvo) %>%
          extract(. > alvo) %>% min()
      })

      if (is.infinite(prox_alvo)) {
        suppressWarnings({
          prox_alvo <- procura_inicio(conteudo_orig, paste0("\t", nome_prox_alvo)) %>%
        extract(. > alvo) %>% min()
        })

      }
      # linhas que correspondem aos elementos
      res <- conteudo[alvo: (prox_alvo - 1)]
    } else {
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
  # RESULUÇÃO
  # ATO
  # ATA

  leis <- procura_inicio(conteudo_limpo, "LEI")

  decretos <- procura_inicio(conteudo_limpo, "DECRETO")

  decisao <- procura_inicio(conteudo_limpo, "DECIS")

  portarias <- procura_inicio(conteudo_limpo, "PORTARIA")

  despachos <- procura_inicio(conteudo_limpo, "DESPACHO")

  retificacao <- procura_inicio(conteudo_limpo, "RETIFICAÇ")

  instrucao <- procura_inicio(conteudo_limpo, "INSTRUÇ")

  resolucao <- procura_inicio(conteudo_limpo, "RESOLUÇ")

  ato <- procura_inicio(conteudo_limpo, "ATO ")

  ata <- procura_inicio(conteudo_limpo, "ATA ")

  atos <- c(leis, decretos, decisao, portarias, despachos, retificacao,
            instrucao, resolucao, ato, ata, length(conteudo_limpo)) %>% sort()

  indices <- vector("list", length(atos) - 1)

  for (i in seq_along(indices)) {
    indices[[i]] <- atos[i]:(atos[i+1] - 1)
  }

  limites_orgaos <- pega_limites_orgaos(conteudo_limpo)

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

  # 5 - Retirar os 10 tipos de dados listados
  numero <- sapply(lista_atos, pega_numero)
  tipo <- sapply(lista_atos, pega_tipo)
  data_ato <- sapply(lista_atos, pega_data)
  if (is.numeric(data_ato)) {
    data_ato <- as.Date(data_ato, '1970-01-01')
  }
  data_dou <- stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
    as.Date(format = "%Y_%m_%d") %>% unique()
  meio <- stringr::str_sub(arquivos, 10, 13) %>% unique()
  # autoridade <- sapply(lista_atos, function(x) x[length(x)]) # tem caso ruim de string mal cortada
  # cargo <- sapply(lista_atos, pega_cargo)
  texto_ato <- sapply(lista_atos, paste, collapse = "\n")
  id <- paste0(data_dou, meio,"-", formatC(seq_along(lista_atos), width = 2, flag = 0))
  resumo <- sapply(lista_atos, pega_resumo)

  n_pag <- sapply(lista_atos, function (ato) {
    # procura igualdade exata
    for (arq in arquivos) {
      arq2 <- readLines(arq) %>% # repete limpeza feita no ato
        stringr::str_replace_all("No-", "Nº") %>%
        stringr::str_trim("both") %>%
        extract(!stringr::str_detect(., "Este documento pode ser verificado no endereço")) %>%
        extract(. != "") %>% c("") %>%
        paste0(collapse = "\n")%>%
        gsub(pattern = "o-", replacement = "º") %>%
        gsub(pattern = "°-", replacement = "º") %>%
        gsub(pattern = "°", replacement = "º") %>%
        gsub(pattern = "-\\n", replacement = "") %>%
        gsub(pattern = ",\\n", replacement = ", ") %>%
        strsplit("\\n") %>% extract2(1)
      if (all(ato %in% arq2)) {
        return(stringr::str_extract(arq, "pg[0-9]{3}") %>%
                 sub(pattern = "pg", replacement = "") %>% as.numeric())
      }
    }
    
    # Ou se contenta com igualdade parcial
    cont <- 1
    paginas <- integer(length(arquivos))

    for (arq in arquivos) {
      arq2 <- readLines(arq) %>% # repete limpeza feita no ato
        stringr::str_replace_all("No-", "Nº") %>%
        stringr::str_trim("both") %>%
        extract(!stringr::str_detect(., "Este documento pode ser verificado no endereço")) %>%
        extract(. != "") %>% c("") %>%
        paste0(collapse = "\n")%>%
        gsub(pattern = "o-", replacement = "º") %>%
        gsub(pattern = "°-", replacement = "º") %>%
        gsub(pattern = "°", replacement = "º") %>%
        gsub(pattern = "-\\n", replacement = "") %>%
        gsub(pattern = ",\\n", replacement = ", ") %>%
        strsplit("\\n") %>% extract2(1)
      paginas[cont] <- sum(ato %in% arq2)
      cont <- cont + 1
    }

    resp <- stringr::str_extract(arquivos[which.max(paginas)], "pg[0-9]{3}") %>%
      sub(pattern = "pg", replacement = "") %>% as.numeric()
    if (!is.na(resp)) {
      return(resp)
    } else {
      warning("Este ato não foi encontrado em nenhuma página", call. = FALSE)
      NA
    }
  })

  # dic_tipos <- readxl::read_xlsx('dados/outros/S_TIPO_LEGISLACAO.xlsx')
  opt_tipo <- paste("switch(stringr::str_to_title(ato),",
                    paste0('"', dic_tipos$DS_TIPO_LEGISLACAO, '"', " = ",
                    dic_tipos$ID_TIPO_LEGISLACAO, collapse = ","),
                    ")")
  tipo_cod <- sapply(tipo, function(ato) eval(parse(text = opt_tipo))[1])

  tipo_secao <- switch(meio, "DOU1" = 1, "DOU2" = 2, "DOU3" = 3,
                       "DOUE" = 4, # Edição extra
                       "DOUS" = 5,  NA) # Suplemento e caso padrão

  modo_pub <- switch(substr(meio, 1, 3), "DOU" = 1, "CLS" = 2, "BP_" = 3, NA)

  tibble::tibble(ID_LEGISLACAO = id, DS_RESUMO = resumo, DT_PUBLICACAO = data_dou,
                 NU_LEGISLACAO = numero, DS_CONTEUDO = texto_ato, DT_LEI = data_ato,
                 NU_PAGINA = n_pag, DS_INDEXACAO = NA_character_,
                 ID_TIPO_LEGISLACAO = tipo_cod, ID_TIPO_SITUACAO = 99, # 99 =  não verificado
                 CD_TIPO_LIBERACAO = 1, # 1 = público, já que vem do DOU
                 ID_MODO_PUBLICACAO = modo_pub,
                 ID_USUARIO_CADASTRO = 0, ID_USUARIO_LIBERACAO = 0,
                 ID_TIPO_SECAO = tipo_secao, DT_CADASTRO = Sys.Date(),
                 NU_PUBLICACAO = 0, NU_VOLUME = 0)
}
