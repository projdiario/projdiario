context("pega_data()")

ato1 <- c("PORTARIA N\\u00ba 9, DE 21 DE FEVEREIRO DE 2017",
          "O SUPERINTENDENTE DA SUPERINTEND\\u00caNCIA FE-")
ato2 <- c("PORTARIA N\\u00ba 376, DE 20 DE FEVEREIRO DE 2017",
          "21014.000380/2017-50. resolve:")
ato3 <- c("PORTARIA N\\u00ba xx, DE 20 DE MESERRADO DE 2017" , "xxxx")

test_that("Pega data corretamente", {
  expect_equal(pegar_data(ato1), as.Date("2017-02-21"))
  expect_equal(pegar_data(ato2), as.Date("2017-02-20"))
  expect_true(is.na(pegar_data(ato3)))
})
