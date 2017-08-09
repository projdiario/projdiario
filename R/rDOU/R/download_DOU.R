#' Faz download de todas as páginas do DOU da seção e dia escolhidos
#'
#' @param data Data do diário que será baixada. Deve ser fornecida no formato "%d/%m/%Y"
#' @param diario Número da seção do diário (1, 2 ou 3)
#' @return nada. Seu objetivo são efeitos colaterais (donwload)
#' @examples
#' #Sem exemplo

# library(rDOU)
# download_DOU <- function(data, diario) {
#   arquivos <- dir(path = "dados/pdf", pattern = paste0("DOU", diario, ".+pdf$"),
#                   recursive = TRUE, full.names = TRUE)
#
#   baixados <- stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
#     unique()
#
#   if (as.Date(data, format = '%d/%m/%Y') %in% as.Date(baixados, format = '%Y_%m_%d')) {
#     return()
#   }
#
#   url_busca <- paste0('pesquisa.in.gov.br/imprensa/jsp/visualiza/index.jsp?jornal=',
#                       diario, '&pagina=1&data=', data)
#
#   num_pag <- try(httr::GET(url_busca) %>%
#     httr::content() %>% rvest::html_nodes('frame') %>% rvest::html_attr('src') %>%
#     stringr::str_extract('[0-9]+$') %>% magrittr::extract(1) %>% as.numeric())
#
#   if (is.na(num_pag) || 'try-error' %in% class(num_pag)) {
#     Sys.sleep(5)
#     return()
#   } else {
#     dia <- format(as.Date(data, format = "%d/%m/%Y"), format = "%Y_%m_%d")
#     ano <- format(as.Date(data, format = "%d/%m/%Y"), format = "%Y")
#
#     base_url <- 'http://pesquisa.in.gov.br/imprensa/servlet/INPDFViewer?'
#
#     jornal <- paste0('jornal=', diario)
#
#     pagina <- paste0('&pagina=', seq_len(num_pag))
#
#     data_url <- paste0('&data=', data)
#
#     fim <- '&captchafield=firistAccess'
#
#     url <- paste0(base_url, jornal, pagina, data_url, fim)
#
#     destino <- paste0('dados/pdf/DOU', diario, '_', dia, '_pg',
#                       formatC(seq_len(num_pag), width = 3, flag = 0),".pdf")
#
#     baixa_pdf <- function(url, destino) {
#       httr::RETRY(verb = "GET", url = url, httr::write_disk(destino, overwrite = TRUE))
#     }
#
#     resp <- vector("list", length(url))
#
#     for (i in seq_along(url)) {
#       resp[[i]] <- baixa_pdf(url[i], destino[i])
#       if (i %% 50 == 0) Sys.sleep(1)
#     }
#
#     # confirma que não houve erro em nenhum downlaod
#     erros <- which('try-error' %in% sapply(resp, class) || sapply(destino, file.size) == 0)
#
#     while (length(erros) > 0) {
#       for (i in erros) {
#         resp[[i]] <- baixa_pdf(url[i], destino[i])
#       }
#
#       erros <- which('try-error' %in% sapply(resp, class) || sapply(destino, file.size) == 0)
#     }
#     Sys.sleep(2)
#     invisible(c("Sucesso!!"))
#   }
# }

# dias <- seq.Date(from = as.Date('2017-07-20'),
#                         to = as.Date('2017-05-01'), by = -1)
#
# dias_semana <- format(dias, format = '%A')
# dias <- dias[dias_semana != 'sábado' & dias_semana != 'domingo']
#
# sucesso1 <- lapply(format(dias, format = "%d/%m/%Y"), download_DOU, diario = 1)
# Sys.sleep(120)
# sucesso2 <- lapply(format(dias, format = "%d/%m/%Y"), download_DOU, diario = 2)
# Sys.sleep(120)
# sucesso3 <- lapply(format(dias, format = "%d/%m/%Y"), download_DOU, diario = 3)
#
# arquivos <- dir(path = "dados/pdf", pattern = paste0("DOU", 1, ".+pdf$"),
#                 recursive = TRUE, full.names = TRUE)
#
# baixados <- stringr::str_extract(arquivos, "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
#   unique() %>% as.Date(format = "%Y_%m_%d")
#
# sum(!dias %in% baixados)
# sum(!(dias - length(dias)) %in% baixados)
# sum(!(dias - (2*length(dias))) %in% baixados)

# inteiro <- integer(1)
#
# while (inteiro != 200) {
#   inteiro <- GET('http://download.in.gov.br/do/secao2/2017/2017_03_15/DO2_2017_03_15.pdf?arg1=xYjI5pRykOmAHonYGB_S1w&arg2=1489636728',
#                  write_disk('teste.pdf', overwrite = TRUE))
#   inteiro <- status_code(inteiro)
# }
