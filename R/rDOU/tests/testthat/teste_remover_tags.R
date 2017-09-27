context("remove_tags()")

html_unico <- "<html><h1>título</h1><h2>sub-título</h2><div><p>parágrafo<span>texto em destaque</span></p></div></html>"

html <- c('<html lang = "pt-br">',
          '<h1 text-color="blue">Título',
          "</h1>",
          "</html>"
)

html2 <- c('<html lang = "pt-br">',
           '<h1 text-color="blue"',
           'font-family="verdana">Título',
           "</h1>",
           "</html>"
)


test_that("Funciona em texto numa única string", {
  expect_equal(remover_tags(html_unico), "títulosub-títuloparágrafotexto em destaque")
})

test_that("Funciona em texto como vetor", {
  expect_equal(remover_tags(html), " Título ")
})

test_that("Funciona em texto como vetor com quebra de tag entre as linhas", {
  expect_equal(remover_tags(html2), " Título ")
})
