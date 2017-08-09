#' Pega cargo da autoridade que assintou o ato
#'
#' @export
#' @param ato um vetor com o conteudo de um ato
#' @return O cargo da autoridade que assintou um \code{ato}.
#' @examples
#' #Sem exemplo

pega_cargo <- function(ato) {
  ato <- ato[-1] %>% # remove linha em que nome do ato "Portaria XXX de ...."
    paste(collapse = "")

  virgulas <- stringr::str_locate_all(ato, ",")[[1]][, 2]

  agri <- stringr::str_locate(toupper(ato), "AGRICULTURA,")[, 2] # há uma virgula que não é a que buscamos

  fim <- virgulas[!(virgulas %in% agri)]

  stringr::str_sub(ato, 3, # começa do terceiro elemento porque as primeiras
                           # letras sempre são "O/A " (com espaço)
                   (fim[1] - 1)) # tira ultima letra (",")
}
