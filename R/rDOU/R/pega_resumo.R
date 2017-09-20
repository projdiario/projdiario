#' Pega numero dos vetors
#'
#' @export
#' @param vetor um vetor com o nome de um vetor
#' @return A data de \code{vetor} no formvetor dia DE MES_POR_EXTENSO DE ANO
#' @examples
#' #Sem exemplo

pega_resumo <- function (vetor) {
  padrao <- "Art\\. ?[I1]º? ?-?"
  art1 <- procura_inicio(vetor, padrao)[1]
  if (!is.na(art1)) {
    texto <- vetor[art1]
  } else {
    dois_pontos <- grep(pattern = ":", vetor)[1]
    texto <- vetor[dois_pontos + 1]
  }
  texto %>%
    sub(pattern = padrao, replacement = "") %>%
    sub(pattern = "Nº ?[0-9]+\\.?[0-9]* ?-", replacement = "") %>%
    sub(pattern = "^I -", replacement = "") %>%
    sub(pattern = "[rR][ ,]", replacement = " ") %>%
    gsub(pattern = "\\s\\s", replacement = " ") %>%
    stringr::str_trim() %>% 
    paste('<p>', ., '</p>')
}
