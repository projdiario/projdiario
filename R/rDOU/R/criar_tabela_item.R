#' Tabela Item
#'
#' @param lista_de_atos 
#'
#' @return Uma tabela com os mesmos campos da tabela Item no Banco de Dados
#' @export
#'
#' @examples
criar_tabela_item <- function(lista_de_atos) {
  
  tibble::tibble(
    NUM_ATO = sapply(lista_de_atos, pega_numero),
    SEQ_ATO = NA_integer_, # na maior parte dos casos é um numero
    # possui muitas ocorencias de 0 a 80. Tem casos com letras ('0GQ')
    SGL_TIPO = sapply(lista_de_atos, pega_tipo), # precisa melhorar
    VLR_ANO = lubridate::year(data_ato),
    SGL_ORGAO = attr(lista_de_atos, 'orgao'),
    COD_TIPO = sapply(lista_de_atos, pega_tipo, 'cod'), # precisa melhorar,
    # DES_ITEM - Nem ideia do que seja este campo.
    # Há casos de números romanos, codigos alphanuméricos ou mesmo números
    DES_ITEM = NA_character_, # ?
    # NUM_LINHA - Este campo é uma loucura. O decreto 1541/1995 tem 9 artigos
    # E muito mais do que 10 linhas. Há muitos atos sem valor neste campo (ex. 8.112)
    NUM_LINHA = NA_integer_, # ?
    TXT_TEXTO = sapply(lista_de_atos, paste, collapse = "\n") %>% 
      sapply(texto_para_html)
    
  )
}