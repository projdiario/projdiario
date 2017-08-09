#' Pega tipo dos ato
#'
#' @export
#' @param vetor um vetor com o conteudo de um ato
#' @return O tipo do ato de \code{vetor}.
#' @examples
#' #Sem exemplo

pega_tipo <- function(vetor) {
  # Lei
  # Decreto
  # PORTARIAS DE XX
  # PORTARIA Nº XXX
  # DESPACHO
  # RETIFICAÇÃO[ÕES]
  # INSTRUÇÃO
  # RESULUÇÃO
  # ATO
  # ATA
  atos_possiveis <- c("Lei","Portaria", "Despacho", "Lei", "Retificação",
                      "Retificações", "Decreto", "Ato", "Ata",
                      "Instruçao Normativa", "Resolução", "Resoluções")

  for (i in atos_possiveis) {
    if (stringr::str_detect(stringr::str_to_title(vetor[1], "pt"), i)) {
      return(i)
    }
  }

  "Sem tipo"
}
