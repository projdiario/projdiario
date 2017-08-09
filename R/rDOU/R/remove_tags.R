#' Elimina as tags de um documento html
#'
#' @export
#' @param html um vetor com html
#' @return O mesmo \code{html} sem as tags
#' @examples
#' #Sem exemplo

remove_tags <- function(html) {
  colapsado <- paste(html, collapse = " ")
  gsub(pattern = "<.*?>", replacement = "", x = colapsado) %>%
    gsub(pattern = "  +", replacement = " ")
}
