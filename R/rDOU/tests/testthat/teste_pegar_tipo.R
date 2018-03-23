context("pega_tipo()")

ato1 <- c("PORTARIA Nº 16, DE 21 DE FEVEREIRO DE 2017", "       O Superintendente da Superintendência Federal de Agricultura, Pecuária e Abastecimento no Estado do Espírito Santo, no uso das atribuições que lhe confere o artigo 44, inciso XVIII, do Regimento Interno das SFAs, aprovada pela Portaria Ministerial nº 428, de 09de junho de 2010, e do que consta no processo nº")
ato2 <- c("DESPACHO DO SECRETÁRIO EXECUTIVO", "Em 22 de fevereiro de 2017", "       O SECRETÁRIO EXECUTIVO DO MINISTÉRIO DA AGRICULTURA, PECUÁRIA E ABASTECIMENTO, no uso da atribuição que lhe foi delegada pela Portaria MAPA nº 717, de 16 de agosto de 2013, e no âmbito do Decreto n° 1.387/1995:")

test_that("pega_tipo funciona", {
  expect_equal(pegar_tipo(ato1), "POR")
  expect_equal(pegar_tipo(ato2), "DCH")
})
