#' Sugere atos que merecem atenção
#'
#' @param conferir um vetor com o texto a ser conferido
#' @param nomes o vetor com os nomes ou ids dos textos a serem conferidos
#' @param gabarito vetor com o qual as respostas devem ser próximas
#' @return Um data.frame com um vetor dizendo se a observação merece atenção
#'         e outro vetor com o nome da observação sugerida
#' @examples
#' #Sem exemplo

atencao <- function(conferir, nomes, gabarito) {
#   requireNamespace('quanteda', quietly = TRUE)
#   names(conferir) <- nomes
#
#   resp <- logical(length(gabarito))
#
#   for (i in seq_along(resp)) {
#     docs <- quanteda::dfm(c(txt = gabarito[i], conferir), stem = TRUE,
#                 remove = stopwords("portuguese"))
#
#     # compute some document similarities
#     tmp <- quanteda::similarity(docs, margin = "documents")$txt
#     # output as a matrix
#     resp[i] <- names(tmp)[which.max(tmp)]
#   }
#
#   indice <- integer(length(resp))
#
#   for (j in seq_along(indice)) {
#     indice[j] <- which(resp[j] == nomes)
#   }
#
#   data.frame(atencao = nomes != resp,
#              sugerido = indice,
#              stringsAsFactors = FALSE)
}

#' Elimina as tags de estilo um documento html
#'
#' @export
#' @param html um vetor com html
#' @return O mesmo \code{html} sem as tags de estilo \code{<style>}, 
#'         da Office \code{<o:*>} ou \code{<v:*>}
#' @examples
#' #Sem exemplo

tirar_estilo <- function(html) {
  colapsado <- paste(html, collapse = "\\n")
  limpo <- gsub(pattern = "<style>.*?</style>", replacement = "",
                x = colapsado)
  limpo <- gsub(pattern = "</?o:.*?>", replacement = "",
                x = limpo)
  limpo <- gsub(pattern = "</?v:.*?>", replacement = "",
                x = limpo)
  
  strsplit(limpo, '\\\\n')[[1]]
}

#' Elimina as tags de um documento html
#'
#' @export
#' @param html um vetor com html
#' @return O mesmo \code{html} sem as tags
#' @examples
#' #Sem exemplo

remover_tags <- function(html) {
  colapsado <- paste(html, collapse = " ")
  gsub(pattern = "<.*?>", replacement = "", x = colapsado) %>%
    gsub(pattern = "  +", replacement = " ")
}

#' Procura um termo no inicio de uma string
#'
#' @export
#' @param vetor um vetor de texto
#' @param termo termo a ser buscado
#' @return O índice do \code{vetor} em que o \code{termo} se encontra no inicio da string
#' @examples
#' #Sem exemplo

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
#' @param conteudo 
#' @param limite 
#'
#' @return Em qual intervalo de \code{limite} encontra-se cada elemento de \code{conteudo}.
#' @examples
#' # Sem exemplo
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
#' @examples
#' # Sem exemplo
#' 
#' @export

gerar_id <- function(df, anterior) {
  # le atos já gravados
  if (missing(anterior)) {
    base_id <- 0
  } else {
    base_id <- readRDS(anterior) %>% 
      `[[`('ID') %>% as.integer() %>% max()
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
#' @examples
#' # Sem exemplo
#' 
#' @export

texto_para_html <- function(texto) {
  gsub("\\n<" , "\r<", texto) %>% 
    gsub("\\n\\s" , "\n", .) %>% 
    gsub("\\n" , "</p><p>\n", .) %>% # Aqui estou incluindo um '\n',
    # não sei bem porque fiz isso. Vou deixar até testar
    gsub("\\r" , "", .) %>% 
    paste("<p>MINISTÉRIO DA AGRICULTURA, PECUÁRIA E ABASTECIMENTO</p>",
          "<p>SECRETARIA</p>", .)
}

#' Faz download de todas as páginas do DOU da seção e dia escolhidos
#' 
#' @param data Data do diário que será baixada. Deve ser fornecida no formato "%d/%m/%Y"
#' @param diario Número da seção do diário (1, 2 ou 3)
#' @return nada. Seu objetivo são efeitos colaterais (donwload)
#' @examples
#' # Sem exemplo

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


#' Limpar atributos de texto HTML
#' @param html um vetor com html
#' @return O mesmo \code{html} sem os atributos em todas as tags
#' @examples
#' # Sem exemplo
#' 
#' @export

limpar_atributos <- function(html) {
  texto <- paste0(html, collapse = "\\n")
  
  texto <- gsub(pattern = "<!--\\[if.*?<!\\[endif\\]-->",
                replacement = "",
                x = texto)
  
  tags_docto <- unique(stringr::str_extract_all(texto, "</.*? ?>")[[1]]) %>%
    gsub(x = ., pattern = "(<|>|/| )", replacement = "")
  
  for (tag in tags_docto) {
    texto <- gsub(pattern = paste0('<', tag, " .*?>"),
                  replacement = paste0('<', tag,">"),
                  x = texto)
  }
  
  texto <- gsub(pattern = "<tr .*?>",
                replacement = "<tr>",
                x = texto)
  
  texto <- gsub(pattern = "<td .*?>",
                replacement = "<td>",
                x = texto)
  
  texto <- gsub(pattern = "<table .*?>",
                replacement = "<table>",
                x = texto)
  
  texto <- gsub(pattern = "<br\\s.*?>",
                replacement = "<br>",
                x = texto)
  
  texto <- gsub(pattern = "<link .*?>",
                replacement = "",
                x = texto)
  
  texto <- gsub(pattern = "<meta .*?>",
                replacement = "",
                x = texto)
  
  texto <- gsub(pattern = "<img .*?>",
                replacement = "",
                x = texto)
  
  texto <- gsub(pattern = "Este documento pode ser verificado no endereço eletrônico http://www.in.gov.br/autenticidade.html,\\spelo código [0-9]+",
                replacement = "",
                x = texto)
  
  texto <- gsub(pattern = "Documento assinado digitalmente conforme MP no- 2.200-2 de 24/08/2001, que institui a\\sInfraestrutura de Chaves Públicas Brasileira - ICP-Brasil.",
                replacement = "",
                x = texto)
  
  
  strsplit(texto, "\\\\n")[[1]]
}

