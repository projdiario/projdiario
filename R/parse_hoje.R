rm(list=ls())
options()$restart()
cat('\014')
library(rDOU)

hoje <- Sys.Date() 
ano <- format(hoje, format = "%Y")
mes <- format(hoje, format = "%B")
dia <- format(hoje, format = "%d")

pastas <- paste(paste0('\\\\hp64957/Base SISLEGIS/SISLEGIS/dados/txt/DOU', 1:3), ano, mes, dia,sep = '/')

# Criar driver de conexão e abrir conexão
driver <- RJDBC::JDBC("oracle.jdbc.OracleDriver", "R/validador/www/ojdbc6.jar")
configs <- readLines('R/validador/www/config_oracle')
conexao <- RJDBC::dbConnect(driver, configs[1], configs[2], configs[3])

normas <- lapply(pastas, parsear_e_escrever, conexao = conexao)

RJDBC::dbDisconnect(conexao)
