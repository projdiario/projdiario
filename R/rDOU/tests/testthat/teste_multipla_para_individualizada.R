context("Portarias multiplas")

entrada <- c("PORTARIAS DE 25 DE FEVEREIRO DE 2017", "O FULANO resolve:",
             "Nº 1 - BLABLA", "Nº 2 - TAL COISA:", "Art. 1 - FAZ ISSO",
             "Art. 2 - FAZ AQUILO", "Nº 3 - LOREM IPSUM",
             "Estas portarias entram em vigor a partir desta data")

esperado <- c( "PORTARIA Nº 1, DE 25 DE FEVEREIRO DE 2017\nO FULANO resolve:\n BLABLA\nEstas portarias entram em vigor a partir desta data",
               "PORTARIA Nº 2, DE 25 DE FEVEREIRO DE 2017\nO FULANO resolve:\n TAL COISA:\nArt. 1 - FAZ ISSO\nArt. 2 - FAZ AQUILO\nEstas portarias entram em vigor a partir desta data",
               "PORTARIA Nº 3, DE 25 DE FEVEREIRO DE 2017\nO FULANO resolve:\n LOREM IPSUM\nEstas portarias entram em vigor a partir desta data")

arquivos <- lapply(list(1:4, 5:10, 11:18, 19:20, 21:23, 24:28), function(indice) {
  dir('exemplos', full.names = TRUE, include.dirs = FALSE)[indice + 2]
  # + 2 incluído para ignorar nome da pasta que vem ao inicio
  })

tmp1 <- pegar_normas_dou(arquivos[[3]], "Agricultura") %>%
  estruturar_normas()
tmp2 <- pegar_normas_dou(arquivos[[5]], "Agricultura") %>%
  estruturar_normas()
normas <- rbind(tmp1, tmp2)

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
  expect_false(grepl(x = normas$DS_TITULO, pattern = 'DECISÕES') %>% any() )
  expect_false(grepl(x = normas$DS_TITULO, pattern = 'PORTARIAS') %>% any() )
  expect_false(grepl(x = normas$DS_TITULO, pattern = 'RETIFICAÇÕES') %>% any() )

})
