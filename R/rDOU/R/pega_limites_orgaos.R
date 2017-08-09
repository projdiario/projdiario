#' Pega o limite dos orgaos de um ministerio
#'
#' @export
#' @param pagina um vetor com todo o conteudo de um dia do DOU
#' @return Em qual linha (elemento) de \code{pagina} estao os orgaos de um ministerio
#' @examples
#' #Sem exemplo


pega_limites_orgaos <- function(pagina) {
  # Procurar termos na página:
  gab <- procura_inicio(pagina, "GABINETE") # GABINETE
  sec <- procura_inicio(pagina, "SECRETARIA") # SECRETARIA
  inst <- procura_inicio(pagina, "INSTITUTO") # INSTITUTO
  sfa <- procura_inicio(pagina, "SUPERINTENDÊNCIA") # SUPERINTENDÊNCIA

  res <- c(gab, sec, inst, sfa)
  names(res) <- pagina[res]

  # retorna um vetor numérico nomeado com o inicio dos orgaos
  res
}
