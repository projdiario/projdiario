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

