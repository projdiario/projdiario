#' Título do Ato
#'
#' @param ato 
#'
#' @return Título de um ato de seu corpo de texto
#' @export
#'
#' @examples
pega_titulo <- function(ato) {
  # primeira linha de todo ato é seu título
  stringr::str_split(ato, '\n')[[1]][1]
}