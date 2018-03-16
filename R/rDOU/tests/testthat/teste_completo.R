suppressPackageStartupMessages(library(purrr))
# library(rDOU); library(testthat)
context('Teste completo')

expect_margem <- function(obj, expectativa, margem = 0.95) {
  dif <- abs(expectativa - obj) / expectativa
  if (dif <= (1 - margem) && dif != 0) {
    warning(paste0('Teste passou dentro da margem (', dif,').\n'))
  }
  expect_lte(dif, 1 - margem)
}

arquivos <- lapply(list(1:24, 33:36, 25:32, 37:40), function(indice)
  dir('exemplos/completo', full.names = TRUE,
      pattern = '\\.txt')[indice])
normas <- lapply(arquivos, pegar_normas_dou)
tabelas <- map(normas, criar_tabela_app)

gabarito <- readxl::read_xlsx('exemplos/completo/gabarito.xlsx') %>%
  dplyr::mutate(NR_ATO = ifelse(
    is.na(NR_ATO), NA_character_, formatC(NR_ATO, width = 8, flag = '0')
  )) %>%
  split(factor(paste(.$DT_PROMULGACAO, .$ID_TIPO_SECAO))) %>%
  set_names(NULL)

test_that('Há apenas um dia em cada lista de arquivos', {
  datas <- map(normas, attr, 'data_dou')
  expect_identical(map_int(datas, length), c(1L, 1L, 1L, 1L))
})

test_that('Consegue identificar o órgão de cada norma', {
  orgaos <- map(normas, attr, 'orgao')
  org_esperado <- list(
    c(rep("GABINETE DO MINISTRO", 9),
      "SECRETARIA DE MOBILIDADE SOCIAL, DO PRODUTOR RURAL E DO COOPERATIVISMO",
      "SUPERINTENDÊNCIA FEDERAL NO ESTADO DE GOIÁS",
      "SUPERINTENDÊNCIA FEDERAL NO ESTADO DE SANTA CATARINA"),
    c("GABINETE DO MINISTRO", "SECRETARIA EXECUTIVA", "INSTITUTO NACIONAL DE METEOROLOGIA",
      rep("SECRETARIA DE DEFESA AGROPECUÁRIA", 3),
      rep("SUPERINTENDÊNCIA FEDERAL NO ESTADO DA BAHIA", 2),
      "SUPERINTENDÊNCIA FEDERAL NO ESTADO DO PARÁ"),
    c(rep("GABINETE DO MINISTRO", 4), rep("SECRETARIA DE DEFESA AGROPECUÁRIA", 6),
      rep("SUPERINTENDÊNCIA FEDERAL NO ESTADO DO PARANÁ", 2)),
    c(rep("GABINETE DO MINISTRO", 3), rep("SECRETARIA EXECUTIVA", 6),
      "SUPERINTENDÊNCIA FEDERAL NO ESTADO DO AMAZONAS",
      "SUPERINTENDÊNCIA FEDERAL NO ESTADO DE MINAS GERAIS",
      "SUPERINTENDÊNCIA FEDERAL NO ESTADO DO RIO DE JANEIRO")
  )
  map2(orgaos, org_esperado, expect_identical)
  cat('|')
})
# Objeto norma veriricado
# Teste pode seguir para o objeto tabelas

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
  map2(map(tabelas, 'SGL_ORGAO'), map(gabarito, 'SG_ORGAO'), expect_identical)
  cat('|')
  # Número
  map2(map(tabelas, 'NUM_ATO'), map(gabarito, 'NR_ATO'), expect_identical)
  cat('|')
})

# O teste provavelmente está falhando porque
# há quebra de linha ruim no texto

# COMPARARAR (==) TODOS OS CAMPOS

# QUANTOS CORRETOS (AO LONGO DOS CAMPOS)


