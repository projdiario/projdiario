#' Elimina as tags de estilo um documento html
#'
#' @export
#' @param html um vetor com html
#' @return O mesmo \code{html} sem as tags de estilo
#' @examples
#' #Sem exemplo

tira_estilo <- function(html) {
  colapsado <- paste(html, collapse = "\\n")
  limpo <- gsub(pattern = "<style>.*?</style>", replacement = "",
                x = colapsado)
  limpo <- gsub(pattern = "</?o:.*?>", replacement = "",
                x = limpo)
  limpo <- gsub(pattern = "</?v:.*?>", replacement = "",
                x = limpo)

  strsplit(limpo, '\\\\n')[[1]]
}
