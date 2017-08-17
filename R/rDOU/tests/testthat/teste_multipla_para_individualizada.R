context("Portarias multiplas")

entrada <- c("PORTARIAS DE 25 DE FEVEREIRO DE 2017",
              "O FULANO resolve:",
              "Nº 1 - BLABLA",
              "Nº 2 - TAL COISA:",
              "Art. 1 - FAZ ISSO",
              "Art. 2 - FAZ AQUILO",
              "Nº 3 - LOREM IPSUM",
              "Estas portarias entram em vigor a partir desta data")

esperado <- c( "PORTARIA Nº 1 DE 25 DE FEVEREIRO DE 2017\nO FULANO resolve:\n BLABLA\nEstas portarias entram em vigor a partir desta data",
               "PORTARIA Nº 2 DE 25 DE FEVEREIRO DE 2017\nO FULANO resolve:\n TAL COISA:\nArt. 1 - FAZ ISSO\nArt. 2 - FAZ AQUILO\nEstas portarias entram em vigor a partir desta data",
               "PORTARIA Nº 3 DE 25 DE FEVEREIRO DE 2017\nO FULANO resolve:\n LOREM IPSUM\nEstas portarias entram em vigor a partir desta data")

arquivos <- lapply(list(1:4, 5:10, 11:18, 19:20, 21:23, 24:28),
                   function(indice) dir('exemplos', full.names = TRUE)[indice])

decisoes <- pega_dados_dou(arquivos[[3]])$DS_CONTEUDO[8] %>%
  stringr::str_split("\n") %>% .[[1]]
portarias <- pega_dados_dou(arquivos[[5]])$DS_CONTEUDO[5] %>%
  stringr::str_split("\n") %>% .[[1]]

espera_multipla <- function(objeto, expectativa, FUN = NULL) {
  if (is.null(FUN)) {
    obj <- multipla_para_individualizada(objeto)
  } else {
    obj <- FUN(multipla_para_individualizada(objeto))
  }

  expect_equal(obj, expectativa)
}


test_that("Quebra portarias multiplas", {
  espera_multipla(entrada, esperado)
  espera_multipla(decisoes, 2, length)
  espera_multipla(portarias, 2, length)

})
