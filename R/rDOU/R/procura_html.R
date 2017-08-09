#' Procura um termo em uma tag de um html
#'
#' @export
#' @param hmtl um vetor com o nome de um ato
#' @param tag um vetor com o nome de um ato
#' @param termo um vetor com o nome de um ato
#' @return O enésimo item do texto com correspondência do \code{termo} na \code{tag} do \code{html}
#' @examples
#' #Sem exemplo

procura_html <- function(html, tag, termo, letra = FALSE, value = FALSE) {
  # library(rDOU)
  # html = dir(path = "../../dados/html", pattern = "DOU.+html$", recursive = TRUE, full.names = TRUE)[26:75] %>%
  #   lapply(readLines, encoding = 'utf-8') %>% unlist() %>%
  #   gsub(pattern = 'span', replacement = 'span ')
  # tag = "span"
  # termo = "\\SMinistério *da *Agricultura"
  # letra = TRUE
  # value = FALSE

  # if (letra) {
  #   texto <- paste0(html, collapse = "\n")
  #
  #   enesimo <- stringr::str_locate_all(texto, termo)[[1]][,1]
  #
  #   tag_loc <- stringr::str_locate_all(texto, paste0('<', tag))[[1]][,1]
  #
  #   return(max(tag_loc[tag_loc < enesimo]))
  # } else {
    texto <- html_pra_texto(html, tag)

    enesimo <- grep(termo, texto, value = value)

    if (value) return(enesimo)

    tag_loc <- grep(paste0("<", tag), html)

    # retorna o enésimo item do texto com correspondência do termo na tag
    # isto é, a linha em que começa o tag (p, span, h1, ...) com o termo
    tag_loc[enesimo]
  # }
}
