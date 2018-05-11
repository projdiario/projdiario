context("pega_numero()")

ato1 <- c("PORTARIA N\\u00ba 16, DE 21 DE FEVEREIRO DE 2017",
          "       O Superintendente da Superintend\\u00eancia Federal de Agricultura, Pecu\\u00e1ria e Abastecimento no Estado do Esp\\u00edrito Santo, no uso das atribui\\u00e7\\u00f5es que lhe confere o artigo 44, inciso XVIII, do Regimento Interno das SFAs, aprovada pela Portaria Ministerial n\\u00ba 428, de 09de junho de 2010, e do que consta no processo n\\u00ba")
ato2 <- c("DESPACHO DO SECRET\\u00c1RIO EXECUTIVO", "Em 22 de fevereiro de 2017",
          "       O SECRET\\u00c1RIO EXECUTIVO DO MINIST\\u00c9RIO DA AGRICULTURA, PECU\\u00c1RIA E ABASTECIMENTO, no uso da atribui\\u00e7\\u00e3o que lhe foi delegada pela Portaria MAPA n\\u00ba 717, de 16 de agosto de 2013, e no \\u00e2mbito do Decreto n\\u00b0 1.387/1995:")

test_that("pega_numero() funciona como esperado", {
  expect_equal(pegar_numero(desescapar(ato1)), '00000016')
  expect_equal(pegar_numero(desescapar(ato2)), NA_character_)
})
