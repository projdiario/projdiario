# carregar pacotes
rm(list=ls())
devtools::install('R/rDOU')
opt <- options()
opt$restart("cat('reiniciado')")
library(rDOU)

# 1 - ler todas as páginas de um dia
arquivos <- dir(path = "dados/txt", pattern = "DOU.+txt$", recursive = TRUE, full.names = TRUE)

diarios <- split(arquivos, stringr::str_extract(arquivos, "DOU[1-3]") %>%
                   as.factor())

# Separar em uma lista que contenha apenas os caminhos para cada dia
lista_arquivos <- lapply(diarios, function(x) {
  split(x, stringr::str_extract(x,
                                "[0-9]{4}_[0-9]{2}_[0-9]{2}") %>%
          as.factor())
})

# fazer lapply na lista com pega_dados_txt() debug(pega_dados_txt)
dados <- lapply(lista_arquivos, lapply, function(x) try(pega_dados_txt2(x, debug = TRUE))) # 10 dias por segundo
dados_limpo <- lapply(dados, function(x) x[! sapply(x, class) == "try-error"])
dados_df <- lapply(dados_limpo, do.call, what = rbind) %>% 
  do.call(what = rbind)

# Exportar xlsx para aplicação poder ler
# openxlsx::write.xlsx(dados_df, 'normas.xlsx') # muito lento
saveRDS(dados_df, 'normas.RDS')

####
rm(list=ls())
.rs.restartR("cat('foi!')")
Sys.sleep(3)
library(rDOU)

normas <- readRDS('normas.RDS')

normas$ID_TIPO_LEGISLACAO[sapply(normas$ID_TIPO_LEGISLACAO, is.null)] <- NA

normas$ID_TIPO_LEGISLACAO <- unlist(normas$ID_TIPO_LEGISLACAO)

remover <- c(grep("PORTARIAS", normas$DS_CONTEUDO), grep("DECISÕES", normas$DS_CONTEUDO))

multiplas <- lapply(normas$DS_CONTEUDO[remover], function(x){
  stringr::str_split(x, "\n")[[1]]
})

novas <- lapply(multiplas, multipla_para_individualizada)

tam_novas <- sapply(novas, length)

remover <- remover[tam_novas >= 1]

novas <- novas[tam_novas >= 1]

novas_vetor <- unlist(novas)

numero <- stringr::str_extract(novas_vetor, "Nº [0-9]{1,3}\\.?[0-9]{0,3}") %>% 
  stringr::str_replace_all("Nº ", "") %>% stringr::str_replace_all("\\.", "") %>% 
  as.numeric()

tamanhos <- sapply(novas, length)

repete_dado <- function(variavel) {
  res <- mapply(rep, normas[remover, variavel], list(tamanhos), USE.NAMES = FALSE)
  res[, 1]
}

# nome <- paste(stringr::str_sub(unlist(novas), end = stringr::str_locate(unlist(novas), pattern = "DE [0-9]{4}")[,2]),
#               "-", repete_dado("orgao"))

id1 <- mapply(rep, normas$ID_LEGISLACAO[remover], tamanhos, USE.NAMES = FALSE)
id2 <- lapply(tamanhos, seq_len) %>% unlist()
  
ids <- paste0(unlist(id1), "-", formatC(id2, width = 2, flag = '0'))

# n_pag <- NA
n_pag <- mapply(rep, normas$NU_PAGINA[remover], tamanhos, USE.NAMES = FALSE) %>% 
  unlist()

# sapply(normas, function(var) any(sapply(var, function(x) sum(is.null(x)))))

novas_obs <- tibble::tibble(ID_LEGISLACAO = ids, DS_RESUMO = sapply(novas_vetor, pega_resumo),
                            DT_PUBLICACAO = as.Date(repete_dado("DT_PUBLICACAO"), origin = "1970-01-01"),
                            NU_LEGISLACAO = numero, DS_CONTEUDO = novas_vetor,
                            DT_LEI = as.Date(sapply(novas_vetor, pega_data), origin = "1970-01-01"),
                            NU_PAGINA = n_pag, DS_INDEXACAO = NA,
                            ID_TIPO_LEGISLACAO = repete_dado("ID_TIPO_LEGISLACAO"),
                            ID_TIPO_SITUACAO = 99, # 99 =  não verificado
                            CD_TIPO_LIBERACAO = 1, # 1 = público, já que vem do DOU
                            ID_MODO_PUBLICACAO = repete_dado("ID_MODO_PUBLICACAO"),
                            ID_USUARIO_CADASTRO = 0, ID_USUARIO_LIBERACAO = 0,
                            ID_TIPO_SECAO = repete_dado("ID_TIPO_SECAO"),
                            DT_CADASTRO = Sys.Date(),
                            NU_PUBLICACAO = 0, NU_VOLUME = 0)

novo_df <- dplyr::bind_rows(normas[-remover, ], novas_obs)

saveRDS(novo_df, 'normas2.RDS')
