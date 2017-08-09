requireNamespace("tools", quietly = TRUE)
requireNamespace("devtools", quietly = TRUE)

auxiliar <- data.frame(tag = c('h1', 'h2', 'p', 'span'),
                       stringsAsFactors = FALSE)

devtools::use_data(auxiliar)
devtools::use_data(auxiliar,
                   compress = tools::checkRdaFiles("data/auxiliar.rda")$compress,
                   overwrite = TRUE)
