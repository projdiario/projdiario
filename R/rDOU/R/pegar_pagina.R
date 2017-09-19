#' Número da Página do ato
#'
#' @param ato 
#' @param arquivos 
#'
#' @return Página de cada ato
#' @export
#'
#' @examples
pegar_pagina <- function(ato, arquivos) {
  for (arq in arquivos) {
    arq2 <- readLines(arq) %>% stringr::str_replace_all("No-", 
                                                        "Nº") %>% stringr::str_trim("both") %>% extract(!stringr::str_detect(., 
                                                                                                                             "Este documento pode ser verificado no endereço")) %>% 
      extract(. != "") %>% c("") %>% paste0(collapse = "\\n") %>% 
      gsub(pattern = "o-", replacement = "º") %>% gsub(pattern = "°-", 
                                                       replacement = "º") %>% gsub(pattern = "°", replacement = "º") %>% 
      gsub(pattern = "-\\\\n", replacement = "") %>% 
      gsub(pattern = ",\\\\n", replacement = ", ") %>% 
      strsplit("\\\\n") %>% extract2(1)
    if (all(ato %in% arq2)) {
      return(stringr::str_extract(arq, "pg[0-9]{3}") %>% 
               sub(pattern = "pg", replacement = "") %>% as.numeric())
    }
  }
  cont <- 1
  paginas <- integer(length(arquivos))
  for (arq in arquivos) {
    arq2 <- readLines(arq) %>% stringr::str_replace_all("No-", 
                                                        "Nº") %>% stringr::str_trim("both") %>% extract(!stringr::str_detect(., 
                                                                                                                             "Este documento pode ser verificado no endereço")) %>% 
      extract(. != "") %>% c("") %>% paste0(collapse = "\\n") %>% 
      gsub(pattern = "o-", replacement = "º") %>% gsub(pattern = "°-", 
                                                       replacement = "º") %>% gsub(pattern = "°", replacement = "º") %>% 
      gsub(pattern = "-\\\\n", replacement = "") %>% 
      gsub(pattern = ",\\\\n", replacement = ", ") %>% 
      strsplit("\\\\n") %>% extract2(1)
    paginas[cont] <- sum(ato %in% arq2)
    cont <- cont + 1
  }
  resp <- stringr::str_extract(arquivos[which.max(paginas)], 
                               "pg[0-9]{3}") %>% sub(pattern = "pg", replacement = "") %>% 
    as.numeric()
  if (!is.na(resp)) {
    return(resp)
  }
  else {
    warning("Este ato não foi encontrado em nenhuma página", 
            call. = FALSE)
    NA
  }
}