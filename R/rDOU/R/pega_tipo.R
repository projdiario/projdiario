#' Pega tipo dos ato
#'
#' @export
#' @param vetor um vetor com o conteudo de um ato
#' @return O tipo do ato de \code{vetor}.
#' @examples
#' #Sem exemplo

pega_tipo <- function(vetor, retorno = 'txt') {
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
      res <- i
    }
  }
  
  if (!exists(res)) {
    res <- "Sem tipo"
  }
  
  retorno <- match.arg(retorno, c('txt', 'cod'))

  if (retorno == 'txt') {
    # não muda data
  } else {
    res <- switch (res,
                   "Lei" = 1,
                   "Portaria" = 2,
                   "Despacho" = 3,
                   "Lei" = 4,
                   "Retificação" = 5,
                   "Retificações" = 6,
                   "Decreto" = 7,
                   "Ato" = 8, "Ata" = 9,
                   "Instruçao Normativa" = 10,
                   "Resolução" = 11,
                   "Resoluções" = 12
    )
  }
  
  res
}
