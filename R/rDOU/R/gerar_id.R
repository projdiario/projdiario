#' Adiciona IDs ao data.frame
#'
#' @param lista_de_atos 
#'
#' @return O mesmo df precedido por novos IDs
#' @export
#'
#' @examples
gerar_id <- function(df, anterior) {
  # le atos já gravados
  if (missing(anterior)) {
    base_id <- 0
  } else {
    base_id <- readRDS(anterior) %>% 
      `[[`('ID') %>% as.integer() %>% max()
  }
  # cria sequências de novos IDs
  ID <- seq(base_id + 1, by = 1, length.out = nrow(df)) %>% 
    formatC(width = 10, flag = '0')
  
  tibble::add_column(df, ID, .before = TRUE)
}