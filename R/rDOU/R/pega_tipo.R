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
  atos_possiveis <- c("LEI","PORTARIA", "DESPACHO", "RETIFI",
                      "DECRETO-LEI", "DECRETO", "ATO", "ATA",
                      "INSTRUÇÃO NORMATIVA", "RESOLU")

  for (i in atos_possiveis) {
    if (stringr::str_detect(stringr::str_to_upper(vetor[1], "pt"), i)) {
      res <- i
    }
  }
  
  if (!exists('res')) {
    res <- "Sem tipo"
  }
  
  retorno <- match.arg(retorno, c('txt', 'cod'))

  if (retorno == 'txt') {
    res <- switch (res,
            "LEI" = 'LEI',"PORTARIA" = 'POR', "DESPACHO" = 'DPS',
            "RETIFI" = NA_character_, "DECRETO-LEI" = 'DEL',
            "DECRETO" = 'DEC', "ATO" = 'ATO', "ATA" = 'ATA',
            "INSTRUÇÃO NORMATIVA" = 'INM', "RESOLU" = 'RES',
            NA_character_
            
            # 'ADC', 'ADE', 'AHO', 'ALV', 'ATA', 'ATO',
            # 'AVD', 'AVL', 'BPM', 'CIR', 'COV', 'CPB',
            # 'DCS', 'DEC', 'DEL', 'DEP', 'DLB', 'DLG',
            # 'DO1', 'DO2', 'DO3', 'DPS', 'DSN', 'EDL',
            # 'EIC', 'EPT', 'ETA', 'ETD', 'EXC', 'EXI',
            # 'EXM', 'INM', 'LEI', 'MPV', 'MSG', 'NOT',
            # 'OFC', 'PAR', 'PIM', 'POR', 'REN', 'RES',
            # 'RHO', 'SCT', 'SUP'
    )
  } else {
    res <- switch (res,
                   "LEI" = 'A',"PORTARIA" = 'A', "DESPACHO" = 'A',
                   "RETIFI" = 'A', "DECRETO-LEI" = 'A',
                   "DECRETO" = 'A', "ATO" = 'E', "ATA" = 'P',
                   "INSTRUÇÃO NORMATIVA" = 'R', "RESOLU" = 'X',
                   NA_character_
    )
  }
  
  res
}
