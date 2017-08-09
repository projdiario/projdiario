context('pega_dados_dou()')

arquivos <- lapply(list(1:8, 9:10, 11:13, 14:18),
                   function(indice) dir('exemplos', full.names = TRUE)[indice])

test_that("Retorna número correto de normas", {
  # verifica tamanho da resposta
  expect_tamanho <- function(objeto, tamanho) {
    eval(bquote(expect_equal(nrow(objeto), tamanho) ) )
  }

  expect_tamanho(resp1 <<- pega_dados_dou(arquivos[[1]]), 12)
  # caso em que o ministério não aparece
  # é gambiarra! O melhor é incluir caso na função
  # deve voltar tibble vazia e aviso
  expect_error(resp2 <<- pega_dados_dou(arquivos[[2]]), 'must be length 1 or')
  expect_tamanho(resp3 <<- pega_dados_dou(arquivos[[3]]), 6)
  expect_tamanho(resp4 <<- pega_dados_dou(arquivos[[4]]), 12)
})

test_that("Resposta é tibble com 18 variáveis", {
  expect_identical(ncol(resp1), 18L)
  expect_identical(ncol(resp4), 18L)
  expect_is(resp3, 'tbl_df')
  expect_is(resp4, 'tbl_df')
})


test_that("Resposta tem nome e tipo correto das variáveis", {
  nomes <- names(resp1)
  nomes_esperados <- c('ID_LEGISLACAO', 'DS_RESUMO', 'DT_PUBLICACAO', 'NU_LEGISLACAO',
                       'DS_CONTEUDO', 'DT_LEI', 'NU_PAGINA', 'DS_INDEXACAO',
                       'ID_TIPO_LEGISLACAO', 'ID_TIPO_SITUACAO', 'CD_TIPO_LIBERACAO',
                       'ID_MODO_PUBLICACAO', 'ID_USUARIO_CADASTRO' , 'ID_USUARIO_LIBERACAO',
                       'ID_TIPO_SECAO', 'DT_CADASTRO', 'NU_PUBLICACAO', 'NU_VOLUME')
  classes <- sapply(resp4, class) %>% unname()
  classes_esperadas <- c('character', 'character', 'Date', 'numeric', 'character',
                         'Date', 'numeric', 'character', 'numeric', 'numeric',
                         'numeric' , 'numeric', 'numeric' , 'numeric', 'numeric',
                         'Date', 'numeric', 'numeric')

  expect_identical(nomes, nomes_esperados)

  expect_identical(classes, classes_esperadas)

})

