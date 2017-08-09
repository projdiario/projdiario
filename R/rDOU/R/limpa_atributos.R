#' Limpa atributos de um html
#'
#' @export
#' @param html um vetor com html
#' @return O mesmo \code{html} sem os atributos em todas as tags
#' @examples
#' #Sem exemplo

limpa_atributos <- function(html) {
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
