#' Pega todos os dados de todos os atos de um dia do DOU (txt)
#'
#' @export
#' @param arquivos um vetor com os caminhos dos arquivos (.txt) de um dia do DOU
#' @return Uma lista com todas as normas extraidas do DOU.
#' @examples
#' #Sem exemplo


pega_normas_dou <- function(arquivos, debug = FALSE) {
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
    data_dou = data_dou,
    secao = tipo_secao
  )
  
}
