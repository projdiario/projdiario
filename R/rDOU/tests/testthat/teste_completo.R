suppressPackageStartupMessages(library(purrr))

context('Teste completo')

expect_margem <- function(obj, expectativa, margem = 0.95) {
  dif <- abs(expectativa - obj) / expectativa
  if (dif <= (1 - margem) && dif != 0) {
    warning('Teste passou dentro da margem.\n')
  }
  expect_lte(dif, 1 - margem)
}

arquivos <- lapply(list(1:24, 33:36, 25:32, 37:40), function(indice)
  dir('exemplos/completo', full.names = TRUE,
      pattern = '\\.txt')[indice])
normas <- lapply(arquivos, pegar_normas_dou)
tabelas <- map(normas, criar_tabela_app)

gabarito <- readxl::read_xlsx('exemplos/completo/gabarito.xlsx') %>%
  dplyr::mutate(NR_ATO = formatC(NR_ATO, width = 8, flag = '0')) %>%
  split(factor(paste(.$DT_PROMULGACAO, .$ID_TIPO_SECAO)))

test_that('Há apenas um dia em cada lista de arquivos', {
  datas <- map(normas, attr, 'data_dou')
  expect_identical(map_int(datas, length), c(1L, 1L, 1L, 1L))
  cat('|')
})
# Teste pode seguir

test_that('Resultado possui dimensões esperadas', {
  # linhas
  walk2(map_int(tabelas, nrow), map_int(gabarito, nrow), expect_margem)
  cat('|')
  # colunas
  walk2(map_int(tabelas, ncol), map_int(gabarito, ncol), expect_margem)
  cat('|')
})

# ORDEM DAS NORMAS
test_that('Ordem das normas', {
  # Órgãos
  map2(map(tabelas, 'SGL_ORGAO'), map(gabarito, 'SG_ORGAO'), expect_equal)
  cat('|')
  # Número
  map2(map(tabelas, 'NUM_ATO'), map(gabarito, 'NR_ATO'), expect_equal)
  cat('|')
})


# COMPARARAR (==) TODOS OS CAMPOS

# QUANTOS CORRETOS (AO LONGO DOS CAMPOS)


