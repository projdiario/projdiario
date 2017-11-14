requireNamespace("tools", quietly = TRUE)
requireNamespace("devtools", quietly = TRUE)

dic_tipos <- readxl::read_excel('inst/extdata/des_tipo.xlsx')
dic_tipos$DES_TIPO <- toupper(dic_tipos$DES_TIPO)

devtools::use_data(dic_tipos)
devtools::use_data(dic_tipos,
                   compress = tools::checkRdaFiles("data/dic_tipos.rda")$compress,
                   overwrite = TRUE)
