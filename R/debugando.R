rm(list=ls())
devtools::install('R/rDOU')
library(rDOU)

# 1 - ler todas as páginas de um dia
arquivos <- dir(path = "dados/html", pattern = "DOU.+html$", recursive = TRUE, full.names = TRUE)

# Separar em uma lista que contenha apenas os caminhos para cada dia
lista_arquivos <- split(arquivos,
                        stringr::str_extract(arquivos,
                                             "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
                          as.factor())

# erro1 <- lista_arquivos[[16]] 
# 
# erro2 <- lista_arquivos[[29]]
# 
# erro3 <- lista_arquivos[[32]]

# criar função que remove atributos
# df <- readRDS('app/shiny/dados/atos.RDS')

# df$data_dou[which(df$numero == "Nº 43")] # 2017-03-17 (41)

erro4 <- lista_arquivos[[41]]

debug(pega_dados_html)
pega_dados_html(erro4)



# debug_html <- function(arquivos) {
#   conteudo <- lapply(arquivos, readLines, encoding = "utf-8") %>% 
#     unlist() %>% gsub(pattern = "span", replacement = "span ")
#   MAPA <- procura_html2(conteudo, "span", "e Abastecimento")[1]
#   prox <- procura_html2(conteudo, "span", "Inovação")[1]
#   if (is.na(prox) || prox < MAPA) {
#     prox <- procura_html2(conteudo, "span", "Comunicações")[1]
#   }
#   conteudo_limpo <- stringr::str_sub(paste(conteudo, collapse = "\n"), 
#                                      MAPA, (prox - 1)) %>% stringr::str_split("\n") %>% extract2(1)
#   tags <- list(p = "p", h1 = "h1", h2 = "h2", span = "span")
#   portarias <- lapply(tags, procura_html2, html = conteudo_limpo, 
#                       termo = "PORTARIA.{1,150}", curto = TRUE) %>% unlist()
#   despachos <- lapply(tags, procura_html2, html = conteudo_limpo, 
#                       termo = "DESPACHO.*?", curto = TRUE) %>% unlist()
#   retificacao <- lapply(tags, procura_html2, html = conteudo_limpo, 
#                         termo = "RETIFICAÇÃO.*?", curto = TRUE) %>% unlist()
#   retificacoes <- lapply(tags, procura_html2, html = conteudo_limpo, 
#                          termo = "RETIFICAÇÕES.*?", curto = TRUE) %>% unlist()
#   ultimo_p <- procura_html2(conteudo_limpo, "p", "") %>% max(na.rm = TRUE)
#   atos <- c(portarias, despachos, retificacao, retificacoes, 
#             ultimo_p) %>% sort()
#   atos <- atos[atos != 0]
#   indices <- matrix(numeric((length(atos) - 1) * 2), ncol = 2)
#   for (i in seq_len(nrow(indices))) {
#     indices[i, ] <- c(atos[i], (atos[i + 1] - 1))
#   }
#   erros <- which(apply(indices, 1, diff) < 300)
#   if (length(erros) == 0) {
#     indices_limpos <- indices
#   }
#   else {
#     indices_limpos <- indices
#     indices_limpos[erros + 1, 1] <- indices[erros, 1]
#     indices_limpos <- indices_limpos[-erros, ]
#   }
#   html_atos <- stringr::str_sub(paste(conteudo_limpo, collapse = "\n"), 
#                                 start = indices_limpos[, 1], end = indices_limpos[, 2])
#   html_sem_tag <- sapply(html_atos, remove_tags)
#   writeLines(paste(seq_along(html_atos), html_atos), 'erro.txt')
# }
# 
# debug_html(erro1) # nao havia erro na debugação
# debug_html(erro2) # Encontra uma a mais (inicio): erro texto de <span> é '.'
# debug_html(erro3) # elemento 12 é errado




