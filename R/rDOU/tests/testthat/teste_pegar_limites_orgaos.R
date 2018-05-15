context("pega_limites_orgaos()")

pagina1 <- c("GABINETE DO MINISTRO",
             "PORTARIA No - 392, DE 14 DE FEVEREIRO DE 2017",
             "O MINISTRO DE ESTADO DA AGRICULTURA, PECU\\u00c1- RIA E ABASTECIMENTO, usando da compet\\u00eancia que lhe foi de- legada pelo artigo 2\\u00ba inciso III, do Decreto n\\u00ba 4.734, de 11 de junho de 2003, publicado no Di\\u00e1rio Oficial do dia seguinte, e tendo em vista o que consta do processo 21012.000006/2017-74, resolve: Exonerar, a pedido, a partir de 2 de janeiro de 2017, JOS\\u00c9 ALB\\u00c9RIO VIEIRA DOS SANTOS J\\u00daNIOR, matr\\u00edcula SIAPE n\\u00ba 1821327, ocupante do cargo efetivo de Agente Administrativo, classe B, padr\\u00e3o I, do Quadro de Pessoal deste Minist\\u00e9rio, com fundamento no artigo 34, da Lei n\\u00ba 8.112/90",
             "BLAIRO MAGGI", "SECRETARIA EXECUTIVA",
             "PORTARIA No - 446, DE 21 DE FEVEREIRO DE 2017",
             "O SECRET\\u00c1RIO-EXECUTIVO DO MINIST\\u00c9RIO DA AGRICULTURA, PECU\\u00c1RIA E ABASTECIMENTO, no uso da compet\\u00eancia que lhe foi subdelegada pela Portaria Ministerial n\\u00ba 142, de 1o de agosto de 2016, publicada no Di\\u00e1rio Oficial da Uni\\u00e3o de 2 de agosto de 20l6, e tendo em vista as disposi\\u00e7\\u00f5es do Decreto n\\u00ba 8.852, de 20 de setembro de 2016, resolve: Nomear RAFAEL D\\'AQUINO MAFRA, matr\\u00edcula SIAPE n\\u00ba 2439487, para exercer o cargo em comiss\\u00e3o de Coordenador, c\\u00f3digo DAS 101.3, do Departamento de Administra\\u00e7\\u00e3o, da Secretaria-Exe- cutiva.",
             "EUMAR ROBERTO NOVACKI")

resposta <- c(1, 5)
names(resposta) <- c("GABINETE DO MINISTRO", "SECRETARIA EXECUTIVA")

test_that("Pega orgão corretamente", {
  expect_equal(pegar_limites_orgaos(desescapar(pagina1)), resposta)
})

