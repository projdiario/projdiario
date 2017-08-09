#' Qual faixa de b contem a?
#'
#' @export
#' @param a um vetor
#' @param b um vetor
#' @return Em qual \code{b} encontra-se cada elemento de \code{a}.
#' @examples
#' #Sem exemplo

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
