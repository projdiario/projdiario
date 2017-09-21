#' Pega cargo da autoridade que assintou o ato
#'
#' @export
#' @param ato um vetor com o conteudo de um ato
#' @return O cargo da autoridade que assintou o \code{ato}.
#' @examples
#' #Sem exemplo

pegar_cargo <- function(ato) {
  ato <- ato[-1] %>% # remove linha em que nome do ato "Portaria XXX de ...."
    paste(collapse = "")
  
  virgulas <- stringr::str_locate_all(ato, ",")[[1]][, 2]
  
  # há uma virgula que não é a que buscamos
  agri <- stringr::str_locate(toupper(ato), "AGRICULTURA,")[, 2]
  
  fim <- virgulas[!(virgulas %in% agri)]
  
  stringr::str_sub(ato, 3, # começa do terceiro elemento porque as primeiras
                   # letras sempre são "O/A " (com espaço)
                   (fim[1] - 1)) # tira ultima letra (",")
}

#' Pega data do nome de um ato
#'
#' @export
#' @param ato um vetor com o conteudo de um ato
#' @return A data do \code{ato}
#' @examples
#' #Sem exemplo


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

#' Pega todos os dados de todos os atos de uma edicao do Boletim de Pessoal (pdf)
#'
#' @param arquivo O caminho do arquivo (.pdf) de uma edicao do Boletim de Pessoal
#' @return Uma tabela com todos os dados extraidos do DOU.
#' @examples
#' #Sem exemplo

# pega_dados_BP <- function(arquivo, debug = FALSE) {
#   if (debug) cat(unique(stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}")),'\n')
#
#   # arquivos de teste
#   # arquivo = 'C:/Users/tomas.barcellos/Downloads/BP_2017_06_13.pdf'
#
#   conteudo <- pdftools::pdf_text(arquivo) %>%
#     lapply(function(x) strsplit(x, '\r\n') %>% unlist() %>% str_trim('both') )
#
#   # lim_orgaos <- grep("\\.\\.\\.*? *?[0-9]+", conteudo) %>% range()
#   # orgaos <- conteudo[lim_orgaos[1]:lim_orgaos[2]] %>% paste(collapse = "") %>%
#   #   stringr::str_split("\\.") %>%
#   #   extract2(1) %>% extract(. != "") %>%
#   #   stringr::str_replace_all("[0-9]+", "") %>% stringr::str_trim()
#
#   conteudo_limpo <- conteudo %>% unlist() %>%
#     stringr::str_replace_all("No-", "Nº") %>%
#     stringr::str_trim("both") %>%
#     extract(!stringr::str_detect(., "Este documento pode ser verificado no endereço")) %>%
#     extract(. != "") %>% c("") # linha que não aparece pela forma do loop
#
#   # 3 - fazer busca pelo inicio dos atos
#   # padrão TIPO DE ATO ao inicio da linha
#
#   # tipo de ATO
#   # DECISÃO
#   # PORTARIAS DE XX
#   # PORTARIA Nº XXX
#   # DESPACHO
#   # RETIFICAÇÃO[ÕES]
#   # INSTRUÇÃO
#   # RESULUÇÃO
#   # ATO
#   # ATA
#
#   decisao <- procura_inicio(conteudo_limpo, "DECISÃO")
#
#   portarias <- procura_inicio(conteudo_limpo, "PORTARIA")
#
#   despachos <- procura_inicio(conteudo_limpo, "DESPACHO")
#
#   retificacao <- procura_inicio(conteudo_limpo, "RETIFICAÇ")
#
#   instrucao <- procura_inicio(conteudo_limpo, "INSTRUÇ")
#
#   resolucao <- procura_inicio(conteudo_limpo, "RESOLUÇ")
#
#   ato <- procura_inicio(conteudo_limpo, "ATO ")
#
#   ata <- procura_inicio(conteudo_limpo, "ATA ")
#
#   atos <- c(decisao, portarias, despachos, retificacao,
#             resolucao, ato, ata, length(conteudo_limpo)) %>% sort()
#
#   indices <- vector("list", length(atos) - 1)
#
#   for (i in seq_along(indices)) {
#     indices[[i]] <- atos[i]:(atos[i+1] - 1)
#   }
#
#   indices_limpos <- indices
#
#   lista_atos <- vector("list", length(atos) - 1)
#
#   for (i in seq_along(lista_atos)) {
#     lista_atos[[i]] <- conteudo_limpo[indices_limpos[[i]]] %>%
#       paste0(collapse = "\n")%>%
#       gsub(pattern = "o-", replacement = "º") %>%
#       gsub(pattern = "°-", replacement = "º") %>%
#       gsub(pattern = "°", replacement = "º") %>%
#       gsub(pattern = "-\\n", replacement = "") %>%
#       gsub(pattern = ",\\n", replacement = ", ") %>%
#       strsplit("\\n") %>% extract2(1)
#   }
#
#   # 5 - Retirar os 10 tipos de dados listados
#   numero <- sapply(lista_atos, pega_numero)
#   tipo <- sapply(lista_atos, pega_tipo)
#   data_ato <- as.Date(sapply(lista_atos, pega_data), origin = "1970-01-01")
#   data_dou <- stringr::str_extract(arquivo, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
#     as.Date(format = "%Y_%m_%d")
#   meio <- stringr::str_extract(arquivo, "BP_[0-9]?") %>% unique() # resolver caso do boletim de pessoal
#   autoridade <- sapply(lista_atos, function(x) x[length(x)]) # tem caso ruim de string mal cortada
#   cargo <- sapply(lista_atos, pega_cargo)
#   texto_ato <- sapply(lista_atos, paste, collapse = "\n")
#   id <- paste0(data_dou, "-", formatC(seq_along(lista_atos), width = 2, flag = 0))
#   resumo <- sapply(lista_atos, pega_resumo)
#
#   n_pag <- sapply(lista_atos, function (ato) {
#     for (i in seq_along(conteudo)) {
#       arq2 <- conteudo[[i]] %>% # repete limpeza feita no ato
#         stringr::str_replace_all("No-", "Nº") %>%
#         stringr::str_trim("both") %>%
#         extract(!stringr::str_detect(., "Este documento pode ser verificado no endereço")) %>%
#         extract(. != "") %>% c("") %>%
#         paste0(collapse = "\n")%>%
#         gsub(pattern = "o-", replacement = "º") %>%
#         gsub(pattern = "°-", replacement = "º") %>%
#         gsub(pattern = "°", replacement = "º") %>%
#         gsub(pattern = "-\\n", replacement = "") %>%
#         gsub(pattern = ",\\n", replacement = ", ") %>%
#         strsplit("\\n") %>% extract2(1)
#       if (all(ato %in% arq2)) {
#         return(i)
#       }
#     }
#     warning("Este ato não foi encontrado em nenhuma página", call. = FALSE)
#     NA
#   })
#   # n_pag <- NA
#
#   dic_tipos <- readxl::read_xlsx('dados/outros/S_TIPO_LEGISLACAO.xlsx')
#   opt_tipo <- paste("switch(stringr::str_to_title(ato),",
#                     paste0('"', dic_tipos$DS_TIPO_LEGISLACAO, '"', " = ",
#                            dic_tipos$ID_TIPO_LEGISLACAO, collapse = ","),
#                     ")")
#   tipo_cod <- sapply(tipo, function(ato) eval(parse(text = opt_tipo))[1])
#
#   tipo_secao <- switch(meio, "DOU1" = 1, "DOU2" = 2, "DOU3" = 3,
#                        "DOUE" = 4, # Edição extra
#                        "DOUS" = 5,  NA) # Suplemento e caso padrão
#
#   modo_pub <- switch(substr(meio, 1, 3), "DOU" = 1, "CLS" = 2, "BP_" = 3, NA)
#
#   tibble::tibble(ID_LEGISLACAO = id, DS_RESUMO = resumo, DT_PUBLICACAO = data_dou,
#                  NU_LEGISLACAO = numero, DS_CONTEUDO = texto_ato, DT_LEI = data_ato,
#                  NU_PAGINA = n_pag, DS_INDEXACAO = NA,
#                  ID_TIPO_LEGISLACAO = tipo_cod, ID_TIPO_SITUACAO = 99, # 99 =  não verificado
#                  CD_TIPO_LIBERACAO = 1, # 1 = público, já que vem do DOU
#                  ID_MODO_PUBLICACAO = modo_pub,
#                  ID_USUARIO_CADASTRO = 0, ID_USUARIO_LIBERACAO = 0,
#                  ID_TIPO_SECAO = tipo_secao, DT_CADASTRO = Sys.Date(),
#                  NU_PUBLICACAO = 0, NU_VOLUME = 0)
# }


#' Pega todos os dados de todos os atos de um dia do DOU (txt)
#'
#' @export
#'
#' @param debug A função está sendo debugada?
#' @param arquivos um vetor com os caminhos dos arquivos (.txt) de um dia do DOU
#'
#' @return Uma tabela com todos os dados extraidos do DOU.
#' @examples
#' #Sem exemplo

pegar_dados_dou <- function(arquivos, debug = FALSE) {
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
                    ", NA_real_)")
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

#' Pega o limite dos orgaos de um ministerio
#'
#' @export
#'
#' @param pagina um vetor com todo o conteudo de um dia do DOU
#'
#' @return Em qual linha (elemento) de \code{pagina} estao os orgaos de um ministerio
#' @examples
#' #Sem exemplo

pegar_limites_orgaos <- function(pagina) {
  # Procurar termos na página:
  gab <- procura_inicio(pagina, "GABINETE") # GABINETE
  sec <- procura_inicio(pagina, "SECRETARIA") # SECRETARIA
  inst <- procura_inicio(pagina, "INSTITUTO") # INSTITUTO
  sfa <- procura_inicio(pagina, "SUPERINTENDÊNCIA") # SUPERINTENDÊNCIA
  
  res <- c(gab, sec, inst, sfa)
  names(res) <- pagina[res]
  
  # retorna um vetor numérico nomeado com o inicio dos orgaos
  res
}

#' Pega todos os dados de todos os atos de um dia do DOU (txt)
#'
#' @export
#'
#' @param debug A função está sendo debugada?
#' @param arquivos um vetor com os caminhos dos arquivos (.txt) de um dia do DOU
#'
#' @return Uma lista com todas as normas extraidas do DOU e algumas mata-informações
#' @examples
#' #Sem exemplo

pegar_normas_dou <- function(arquivos, debug = FALSE) {
  if (debug) cat(unique(stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}")),'\n')
  
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
  # RESOLUÇÃO
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
  
  data_dou <- stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
    as.Date(format = "%Y_%m_%d") %>% unique()
  
  meio <- arquivos[1] %>% stringr::str_split('/') %>%
    extract2(1) %>% extract(length(.)) %>% stringr::str_sub(1, 4)
  tipo_secao <- switch(meio, "DOU1" = 1, "DOU2" = 2, "DOU3" = 3,
                       "DOUE" = 4, # Edição extra
                       "DOUS" = 5,  NA) # Suplemento e caso padrão
  
  structure(
    lista_atos,
    class = 'norma',
    orgao = conteudo_limpo[atos[-length(atos)] %em% limites_orgaos],
    arquivos = arquivos,
    data_dou = data_dou,
    secao = tipo_secao
  )
  
}

#' Pega numero dos atos
#'
#' @param ato um vetor com o conteudo de um ato
#'
#' @export
#'
#' @return A data de \code{vetor} no formato dia DE MES_POR_EXTENSO DE ANO
#' @examples
#' #Sem exemplo

pegar_numero <- function(ato) {
  stringr::str_extract(ato[1], "(N|n).{2,3}[0-9]+\\.?[0-9]*") %>%
    gsub(pattern = "\\.", replacement = "") %>%
    stringr::str_extract("[0-9]+") %>%
    as.numeric() %>% formatC(width = 8, flag = 0)
}

#' Pega numero dos vetors
#'
#' @param ato um vetor com o conteudo de um ato
#'
#' @export
#' @return A data de \code{vetor} no formvetor dia DE MES_POR_EXTENSO DE ANO
#' @examples
#' #Sem exemplo

pegar_resumo <- function (ato) {
  padrao <- "Art\\. ?[I1]º? ?-?"
  art1 <- procura_inicio(ato, padrao)[1]
  if (!is.na(art1)) {
    texto <- ato[art1]
  } else {
    dois_pontos <- grep(pattern = ":", ato)[1]
    texto <- ato[dois_pontos + 1]
  }
  texto %>%
    sub(pattern = padrao, replacement = "") %>%
    sub(pattern = "Nº ?[0-9]+\\.?[0-9]* ?-", replacement = "") %>%
    sub(pattern = "^I -", replacement = "") %>%
    sub(pattern = "[rR][ ,]", replacement = " ") %>%
    gsub(pattern = "\\s\\s", replacement = " ") %>%
    stringr::str_trim() %>% 
    paste('<p>', ., '</p>')
}

#' Pega tipo dos ato
#'
#' @param ato um vetor com o conteudo de um ato
#' @param retorno Deve retornar 'txt' ou 'cod'?
#'
#' @export
#' @return O tipo do ato de \code{vetor}.
#' @examples
#' #Sem exemplo

pegar_tipo <- function(ato, retorno = 'txt') {
  # Lei
  # Decreto
  # PORTARIAS DE XX
  # PORTARIA Nº XXX
  # DESPACHO
  # RETIFICAÇÃO[ÕES]
  # INSTRUÇÃO
  # RESULUÇÃO
  # ATO
  # ATA
  atos_possiveis <- c("LEI","PORTARIA", "DESPACHO", "RETIFI",
                      "DECRETO-LEI", "DECRETO", "ATO", "ATA",
                      "INSTRUÇÃO NORMATIVA", "RESOLU")
  
  for (i in atos_possiveis) {
    if (stringr::str_detect(stringr::str_to_upper(ato[1], "pt"), i)) {
      res <- i
    }
  }
  
  if (!exists('res')) {
    res <- "Sem tipo"
  }
  
  retorno <- match.arg(retorno, c('txt', 'cod'))
  
  if (retorno == 'txt') {
    res <- switch (res,
                   "LEI" = 'LEI',"PORTARIA" = 'POR', "DESPACHO" = 'DPS',
                   "RETIFI" = NA_character_, "DECRETO-LEI" = 'DEL',
                   "DECRETO" = 'DEC', "ATO" = 'ATO', "ATA" = 'ATA',
                   "INSTRUÇÃO NORMATIVA" = 'INM', "RESOLU" = 'RES',
                   NA_character_
                   
                   # 'ADC', 'ADE', 'AHO', 'ALV', 'ATA', 'ATO',
                   # 'AVD', 'AVL', 'BPM', 'CIR', 'COV', 'CPB',
                   # 'DCS', 'DEC', 'DEL', 'DEP', 'DLB', 'DLG',
                   # 'DO1', 'DO2', 'DO3', 'DPS', 'DSN', 'EDL',
                   # 'EIC', 'EPT', 'ETA', 'ETD', 'EXC', 'EXI',
                   # 'EXM', 'INM', 'LEI', 'MPV', 'MSG', 'NOT',
                   # 'OFC', 'PAR', 'PIM', 'POR', 'REN', 'RES',
                   # 'RHO', 'SCT', 'SUP'
    )
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
#' @export
#'
#' @examples
pegar_titulo <- function(ato) {
  # primeira linha de todo ato é seu título
  stringr::str_split(ato, '\n')[[1]][1]
}

#' Número da Página do ato
#'
#' @param ato um vetor com o conteudo de um ato
#' @param arquivos vetor com caminho dos arquivos em que o ato pode estar
#'
#' @return Página de cada ato
#' @export
#'
#' @examples
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
