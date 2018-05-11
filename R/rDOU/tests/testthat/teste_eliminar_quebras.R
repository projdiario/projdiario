context('eliminar_quebras()')

string1 <- "Uma frase se referindo ao processo n\\u00ba \\n21000.123560/2017-00."
string2 <- "O fulano com o RG\\n 123.\\n456.78 SSP/DF ..." # com espaço depois
string3 <- "De acordo com \\na portaria 12 de 09 de setembro de 2017"
string3a <- "De acordo com\\n a portaria 12 de 09 de setembro de 2017"
string4 <- "Multiplas\\n quebras de linhas\\n na mesma \\nstring"
string5 <- "Uma frase corretamente quebrada.\\n" # depois de ponto
string6 <- "Uma frase corretamente quebrada.\\nA frase que se segue"
string7 <- "Fulano faz:\\na) Uma coisa;\\nb) Outra coisa"
string7a <- "Fulano faz:\\nI) Uma coisa;\\nII) Outra coisa"

resposta1 <- 'Uma frase se referindo ao processo nº 21000.123560/2017-00.'
resposta2 <- 'O fulano com o RG 123.456.78 SSP/DF ...'
resposta3 <- 'De acordo com a portaria 12 de 09 de setembro de 2017'
resposta4 <- 'Multiplas quebras de linhas na mesma string'

test_that('Elimina quebra seguida de numero', {
  expect_equal(eliminar_quebras(desescapar(string1)), resposta1)
  expect_equal(eliminar_quebras(desescapar(string2)), resposta2)
})

test_that('Elimina quebra seguida por letras', {
  expect_equal(eliminar_quebras(desescapar(string3)), resposta3)
  expect_equal(eliminar_quebras(desescapar(string3a)), resposta3)
})

test_that('Elimina muitas quebras na mesma string', {
  expect_equal(eliminar_quebras(desescapar(string4)), resposta4)
})

test_that('Não elimina casos de "quebras boas"', {
  expect_equal(eliminar_quebras(desescapar(string5)), desescapar(string5))
  expect_equal(eliminar_quebras(desescapar(string6)), desescapar(string6))
  expect_equal(eliminar_quebras(desescapar(string7)), desescapar(string7))
  expect_equal(eliminar_quebras(desescapar(string7a)), desescapar(string7a))
})

