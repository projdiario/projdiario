context('pegar_sigla_orgao()')

expect_sigla <- function(nome, sigla) {
  expect_equal(rDOU:::pegar_sigla_orgao(nome), paste0(sigla, '/MAPA'))
}


test_that('Retorna sigla correta das secretarias' , {
  expect_sigla('SECRETARIA DE DEFESA AGROPECUÁRIA', 'SDA')
  expect_sigla('SECRETARIA DE POLÍTICA AGROPECUÁRIA', 'SDA')
  expect_sigla('SECRETARIA DE RELAÇÕES INTERNACIONAIS DO AGRONEGÓCIO', 'SRI')
  expect_sigla('SECRETARIA EXECUTIVA', 'SE')
  expect_sigla('GABINETE DO MINISTRO', 'GM')
  expect_sigla('GABINETE DA MINISTRA', 'GM')
})

test_that('Retorna sigla correta das superintendências' , {
  expect_sigla('SUPERINTENDÊNCIA FEDERAL NO ESTADO DO CEARÁ', 'SFA-CE')
  expect_sigla('SUPERINTENDÊNCIA FEDERAL NO ESTADO DA BAHIA', 'SFA-BA')
  expect_sigla('SUPERINTENDÊNCIA FEDERAL NO ESTADO DE MINAS GERAIS', 'SFA-MG')
  expect_sigla('SUPERINTENDÊNCIA FEDERAL NO ESTADO DO PARANÁ', 'SFA-PR')
  expect_sigla('SUPERINTENDÊNCIA FEDERAL NO ESTADO DO PARÁ', 'SFA-PA')
  expect_sigla('SUPERINTENDÊNCIA FEDERAL NO ESTADO DO AMAPÁ', 'SFA-AP')
})

test_that('Retorna sigla correta das superintendências' , {
  expect_sigla('SUPERINTENDENCA FEDERAL NO ESTADO DO CEARÁ', 'SFA-CE')
  expect_sigla('SUPERINTENDÊNCIA FEDERAL DO ESTADO DO PARÁ', 'SFA-PA')
  expect_sigla('SUPERINTENDÊNCIA NO ESTADO DO AMAPÁ', 'SFA-AP')
  expect_failure(expect_sigla('SECRETARIA-EXECUTIVA', 'SFA-AC'))
})

