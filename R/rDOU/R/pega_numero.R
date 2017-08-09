#' Pega numero dos atos
#'
#' @export
#' @param vetor um vetor com o nome de um ato
#' @return A data de \code{vetor} no formato dia DE MES_POR_EXTENSO DE ANO
#' @examples
#' #Sem exemplo

pega_numero <- function(vetor) {
  res <- stringr::str_extract(vetor[1], "(N|n).{2,3}[0-9]+\\.?[0-9]*")
  # if (is.na(res)) {
  #   res <- "Sem número"
  # }
  res %>% gsub(pattern = "\\.", replacement = "") %>%
    stringr::str_extract("[0-9]+") %>% as.numeric()
}
