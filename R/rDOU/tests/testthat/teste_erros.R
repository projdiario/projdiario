context("Erros")

arquivos <- map(
  list(1:24, 33:36, 25:32, 37:40),
  ~ dir('exemplos/completo', full.names = TRUE,pattern = '\\.txt')[.x]
)
sem_ultima_pagina <- arquivos[[1]][-length(arquivos[[1]])]

test_that("Erros são levantados em casos limite", {
  expect_warning(resp <- pegar_normas_dou(sem_ultima_pagina, "Agricultura"),
                 "Verifique se todas as p")
  expect_s3_class(resp, "norma")
  expect_silent(df_erro <- estruturar_normas(resp))
  expect_s3_class(df_erro, "data.frame")
  expect_equal(nrow(df_erro), 0)
  expect_warning(pegar_normas_dou(arquivos[[1]], "inexistente"),
                 desescapar("entre os \\u00f3rg\\u00e3os identificados"))
})
