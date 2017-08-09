context("Operador %em%")

limite <- c(1, 5)

conteudo <- c(2, 6, 15)

test_that("Função retorna o maior elemento do limite menor que ela", {
  expect_equal(conteudo %em% limite, c(1, 5, 5))
  expect_true(all(is.na(limite %em% limite)))
})
