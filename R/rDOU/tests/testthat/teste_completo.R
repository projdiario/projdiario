suppressPackageStartupMessages(library(purrr))
# library(rDOU); library(testthat)
context('Teste completo')

expect_gabarito <- function(campo){
  map2(
    map(tabelas, campo),
    map(gabarito, campo),
    expect_identical
    )
  cat('|')
}

formatar <- function(x){
  formatC(x, width = 8, flag = '0')
}

arquivos <- map(
  list(1:24, 33:36, 25:32, 37:40),
  ~ dir('exemplos/completo', full.names = TRUE,pattern = '\\.txt')[.x]
)
normas <- map(arquivos, pegar_normas_dou, orgao_alvo = "Agricultura")
tabelas <- map(normas, estruturar_normas)

gabarito <- readxl::read_xlsx('exemplos/completo/gabarito.xlsx') %>%
  dplyr::mutate(NR_ATO = ifelse(is.na(NR_ATO), NA_character_, formatar(NR_ATO)),
                AN_ATO = as.character(AN_ATO),
                DT_PROMULGACAO = as.Date(DT_PROMULGACAO)) %>%
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
  map2(map_int(tabelas, nrow), map_int(gabarito, nrow), expect_identical)
  cat('|')
  # colunas
  map2(map_int(tabelas, ncol), map_int(gabarito, ncol), expect_identical)
})

# ORDEM DAS NORMAS
context('Teste completo: campos')
test_that('Informações são extraídas exatamente como esperado', {
  # Número da norma
  expect_gabarito('NR_ATO')
  # # Tipo da norma
  expect_gabarito('SG_TIPO')
  # Ano da norma
  expect_gabarito('AN_ATO')
  # Órgãos emissor
  expect_gabarito('SG_ORGAO')
  # Data de promulgadação
  expect_gabarito('DT_PROMULGACAO')
  # Título da norma
  expect_gabarito('DS_TITULO')
  # # Página em que a norma foi encontrada
  expect_gabarito('NM_PAGINA')
  # Seção do DOU
  expect_gabarito('ID_TIPO_SECAO')
  # NUM_PAGINA é ordenado e crescente
  expect_equal(map_lgl(tabelas, ~all(diff(.x$NM_PAGINA) >= 0)), rep(TRUE, 4))
})



