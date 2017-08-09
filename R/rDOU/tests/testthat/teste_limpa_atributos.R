context("limpa_atributos()")

html_unico <- "<html><h1 backgroud-color:blue>título</h1><h2 font-family='verdana'>sub-título</h2></html>"

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

test_that("Limpa atributos das tag", {
  expect_equal(limpa_atributos(html), c('<html>',
                                        '<h1>Título',
                                        "</h1>",
                                        "</html>")
               )
  expect_equal(limpa_atributos(html2), c('<html>',
                                        '<h1>Título',
                                        "</h1>",
                                        "</html>")
               )
})

test_that("Limpa atributos das tag em html colapsado", {
  expect_equal(limpa_atributos(html_unico),
               "<html><h1>título</h1><h2>sub-título</h2></html>")
})
