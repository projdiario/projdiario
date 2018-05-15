context('pegar_resumo()')

arquivos <- lapply(list(1:4, 5:10, 11:18, 19:20, 21:23, 24:28), function(indice) {
  dir('exemplos', full.names = TRUE)[indice + 2]
  # + 2 incluído para ignorar nome da pasta que vem ao inicio
  })

tmp1 <- pegar_normas_dou(arquivos[[3]], orgao_alvo = "Agricultura") %>%
  estruturar_normas()
tmp2 <- pegar_normas_dou(arquivos[[5]], orgao_alvo = "Agricultura") %>%
  estruturar_normas()
normas <- rbind(tmp1, tmp2)

test_that('Não há casos com NAs nos exemplo', {
  expect_true(all(normas$TX_EMENTA !=  paste0('<p>', NA, '</p>')))
})

test_that('Resumos são como esperados', {
  resumo1 <- 'Homologa nos termos do Anexo da presente Portaria, os bens e objetos passíveis de apoio através da transferência de recursos, via contrato de repasse, operacionalizada pela Caixa Econômica Federal (CEF).'
  resumo2 <- 'Torna pública a RENÚNCIA EXPRESSA ao benefício da pensão, por ocupar Cargo Público Permanente, com efeitos financeiros a partir de 09/03/2017, conforme o artigo 222, Inciso VI, da Lei nº 8.112/90, formulada por MARTHA CRISTINA'
  resumo3 <- 'Na Portaria GAB/SFA/RJ Nº 103, de 30 de Março de 2017, publicada no D.O.U. de 03 de Abril de 2017, seção 2, página 7, Onde se lê: \"...acrescido de 17% (dezessete po cento) relativos à Gratificação Adicional por Tempo de Serviço...\", leia-se: \"...acrescido de 20% (vinte por cento) relativos à Gratificação Adicional por Tempo de Serviço...\".'
  resumo4 <- 'Designa a servidora MÔNICA AROUCHE LIMA, ocupante do cargo de Agente Administrativo, matrícula SIAPE nº 1829705, pertencente ao Quadro de Pessoal deste Ministério, para exercer o encargo de Substituto de Chefe da Divisão FCPE 101.2 BRUNO RAPHAEL RIBEIRO GUIMARÃES, matrícula SIAPE 1465514, desta Superintendência, nos seus afastamentos e impedimentos legais e regulamentares.'
  expect_equivalent(normas$TX_EMENTA[4], resumo1)
  expect_equivalent(normas$TX_EMENTA[23], resumo2)
  expect_equivalent(normas$TX_EMENTA[25], resumo3)
  expect_equivalent(normas$TX_EMENTA[19], resumo4)
})


