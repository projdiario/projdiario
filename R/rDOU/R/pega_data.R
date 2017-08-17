#' Pega data do nome de um ato
#'
#' @export
#' @param vetor um vetor com o nome de um ato
#' @return A data de \code{vetor} no formato dia DE MES_POR_EXTENSO DE ANO
#' @examples
#' #Sem exemplo


pega_data <- function(vetor) {
  if (grepl('win', sessionInfo()[['platform']]) ) {
    res <- stringr::str_extract(stringr::str_to_lower(vetor)[1],
                                "[0-9]{1,2} de [a-z]+ de [0-9]{4}") %>%
      as.Date('%d de %B de %Y')
    
  } else {
    original <- Sys.getlocale("LC_TIME")
    invisible(Sys.setlocale("LC_TIME", "pt_BR.UTF-8"))
    res <- stringr::str_extract(stringr::str_to_lower(vetor)[1],
                                "[0-9]{1,2} de [a-z]+ de [0-9]{4}") %>%
      as.Date('%d de %B de %Y')
    
    invisible(Sys.setlocale("LC_TIME", original))
  }
  res
}
