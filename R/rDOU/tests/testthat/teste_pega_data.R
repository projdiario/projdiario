context("pega_data()")

ato1 <- c("PORTARIA No- 9, DE 21 DE FEVEREIRO DE 2017", "O SUPERINTENDENTE DA SUPERINTENDÊNCIA FE-")
ato2 <- c("PORTARIA Nº 376, DE 20 DE FEVEREIRO DE 2017" , "21014.000380/2017-50. resolve:")
ato3 <- c("PORTARIA Nº xx, DE 20 DE MESERRADO DE 2017" , "xxxx")

test_that("Pega data corretamente", {
  # expect_equal(pega_data(ato1), as.Date("2017-02-21"))
  # expect_equal(pega_data(ato2), as.Date("2017-02-20"))
  expect_true(is.na(pega_data(ato3)))
})
