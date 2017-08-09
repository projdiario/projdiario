#' Procura um termo no inicio de uma string
#'
#' @export
#' @param vetor um vetor de um dia do DOU
#' @param termo termo a ser buscado
#' @return O indice do \code{vetor} em que o termo se encontra no inicio da string
#' @examples
#' #Sem exemplo

procura_inicio <- function(vetor, termo) {
  termo_sem_regex <- stringr::str_replace_all(termo, "\\[.+?\\]", "") %>%
    stringr::str_replace_all("\\{.+?\\}", "") %>%
    stringr::str_replace_all("[+^?*]", "")
  indice <- stringr::str_which(stringr::str_sub(vetor, 1,
                                                stringr::str_length(termo_sem_regex)),
                               termo)

  # retorna ínidice em que letras iniciais batem com o termo
  indice
}
