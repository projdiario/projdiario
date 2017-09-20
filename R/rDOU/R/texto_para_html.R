#' Transformar Texto em Parágrafos de HTML
#'
#' @param texto 
#'
#' @return
#' @export
#'
#' @examples
texto_para_html <- function(texto) {
  gsub("\\n<" , "\r<", texto) %>% 
    gsub("\\n\\s" , "\n", .) %>% 
    gsub("\\n" , "</p><p>\n", .) %>% # Aqui estou incluindo um '\n',
    # não sei bem porque fiz isso. Vou deixar até testar
    gsub("\\r" , "", .) %>% 
    paste("<p>", "<p>MINISTÉRIO DA AGRICULTURA, PECUÁRIA E ABASTECIMENTO</p>",
          "<p>SECRETARIA</p>", ., "</p>")
}