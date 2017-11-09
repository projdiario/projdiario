context('eliminar_quebras()')

string1 <- 'Uma frase se referindo ao processo nº \n21000.123560/2017-00.'
string2 <- 'O fulano com o RG\n 123.456.78 SSP/DF ...' # com espaço depois
string3 <- 'De acordo com \na portaria 12 de 09 de setembro de 2017'
string4 <- 'De acordo com\n a portaria 12 de 09 de setembro de 2017'
string5 <- 'Multiplas\n quebras de linhas\n na mesma \nstring'

resposta1 <- 'Uma frase se referindo ao processo nº 21000.123560/2017-00.'
resposta2 <- 'O fulano com o RG 123.456.78 SSP/DF ...'
resposta3 <- 'De acordo com a portaria 12 de 09 de setembro de 2017'
resposta4 <- 'De acordo com a portaria 12 de 09 de setembro de 2017'
resposta5 <- 'Multiplas quebras de linhas na mesma string'

test_that('Elimina quebra seguida de numero', {
  expect_equal(eliminar_quebras(string1), resposta1)
  expect_equal(eliminar_quebras(string2), resposta2)
})

test_that('Elimina quebra seguida por letras', {
  expect_equal(eliminar_quebras(string3), resposta3)
  expect_equal(eliminar_quebras(string4), resposta4)
})

test_that('Elimina muitas quebras na mesma string', {
  expect_equal(eliminar_quebras(string5), resposta5)
})

