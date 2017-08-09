requireNamespace("tools", quietly = TRUE)
requireNamespace("devtools", quietly = TRUE)

dic_tipos <- readxl::read_xlsx('inst/extdata/S_TIPO_LEGISLACAO.xlsx')

devtools::use_data(dic_tipos)
devtools::use_data(dic_tipos,
                   compress = tools::checkRdaFiles("data/dic_tipos.rda")$compress,
                   overwrite = TRUE)
