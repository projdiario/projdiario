context("pega_limites_orgaos()")

pagina1 <- c("GABINETE DO MINISTRO", "PORTARIA No - 392, DE 14 DE FEVEREIRO DE 2017",
          "O MINISTRO DE ESTADO DA AGRICULTURA, PECUÁ- RIA E ABASTECIMENTO, usando da competência que lhe foi de- legada pelo artigo 2º inciso III, do Decreto nº 4.734, de 11 de junho de 2003, publicado no Diário Oficial do dia seguinte, e tendo em vista o que consta do processo 21012.000006/2017-74, resolve: Exonerar, a pedido, a partir de 2 de janeiro de 2017, JOSÉ ALBÉRIO VIEIRA DOS SANTOS JÚNIOR, matrícula SIAPE nº 1821327, ocupante do cargo efetivo de Agente Administrativo, classe B, padrão I, do Quadro de Pessoal deste Ministério, com fundamento no artigo 34, da Lei nº 8.112/90",
          "BLAIRO MAGGI",
          "SECRETARIA EXECUTIVA", "PORTARIA No - 446, DE 21 DE FEVEREIRO DE 2017",
          "O SECRETÁRIO-EXECUTIVO DO MINISTÉRIO DA AGRICULTURA, PECUÁRIA E ABASTECIMENTO, no uso da competência que lhe foi subdelegada pela Portaria Ministerial nº 142, de 1o de agosto de 2016, publicada no Diário Oficial da União de 2 de agosto de 20l6, e tendo em vista as disposições do Decreto nº 8.852, de 20 de setembro de 2016, resolve: Nomear RAFAEL D'AQUINO MAFRA, matrícula SIAPE nº 2439487, para exercer o cargo em comissão de Coordenador, código DAS 101.3, do Departamento de Administração, da Secretaria-Exe- cutiva.",
          "EUMAR ROBERTO NOVACKI")

resposta <- c(1, 5)
names(resposta) <- c("GABINETE DO MINISTRO", "SECRETARIA EXECUTIVA")

test_that("Pega orgão corretamente", {
  expect_equal(pega_limites_orgaos(pagina1), resposta)
})

