rm(list=ls())
options()$restart()
cat('\014')
devtools::install('R/rDOU')
library(rDOU)

periodo <- 1 # altere para fazer mais dias
hoje <- Sys.Date() - (seq_len(periodo) - 1)
hoje <- hoje[! format(hoje, '%a') %in% c("sáb", "dom")]
ano <- format(hoje, format = "%Y")
mes <- format(hoje, format = "%B")
dia <- format(hoje, format = "%d")

pastas <- character(2 * length(hoje))

for (i in seq_along(hoje)) {
  pastas[c(i, i + length(hoje))] <- paste(paste0('\\\\hp64957/Base SISLEGIS/SISLEGIS/dados/txt/DOU', 1:2),
                  ano[i], mes[i], dia[i], sep = '/')
}


# Criar driver de conexão e abrir conexão
driver <- RJDBC::JDBC("oracle.jdbc.OracleDriver", "R/validador/www/ojdbc6.jar")
configs <- readLines('R/validador/www/config_oracle')
conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])

normas <- lapply(pastas, purrr::safely(parsear_e_escrever), conexao = conexao)

RJDBC::dbDisconnect(conexao)
