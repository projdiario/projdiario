#' Cria um objeto para cada portaria em objeto de portarias multiplas
#'
#' @export
#' @param portaria um vetor o texto da portaria multipla
#' @return um vetor com uma portaria em cada elemento
#' @examples
#' #Sem exemplo

multipla_para_individualizada <- function(portaria) {
  # portaria <- normas$DS_CONTEUDO[remover[8]] %>% stringr::str_split("\n") %>% .[[1]]

  if (length(procurar_inicio(portaria[length(portaria)], "<table><tr><td>")) == 1) {
    portaria <- portaria[-length(portaria)]
  }

  if (length(grep("resolve:", portaria)) == 0) {
    # 'proteção:'
    padrao <- 'Nº ?[0-9]+\\.?[0-9]*'
    inicios <- c(grep(padrao, portaria), length(portaria))

    nomes <- paste("DECISÃO", stringr::str_extract(portaria, padrao) %>%
                     extract(!is.na(.)),
                   stringr::str_sub(portaria[1], 11))

    cabeca <- paste0(portaria[2:( inicios[1]-1 )], collapse = " ")

    rodape <- portaria[length(portaria)] # por def tamanho 1

    indices <- vector("list", length(inicios) - 1)

    for (i in seq_along(indices)) {
      indices[[i]] <- inicios[i]:eval(inicios[i+1] - 1)
    }

    lista_portarias <- vector("list", length(indices))

    for (i in seq_along(lista_portarias)) {
        lista_portarias[[i]] <- paste0(nomes[i], "\n", cabeca, "\n",
                                       paste0(portaria[ indices[[i]] ], collapse = "\n"),
                                       "\n", rodape)
      }

      individualizadas <- gsub(paste0(padrao, ' ?-'), "", unlist(lista_portarias))
  } else if (length(grep("resolve:", portaria)) != 1) {
    novas_ind <- c(grep("resolve:", portaria), length(portaria) - 1)
    nome <- portaria[1]
    rodape <- portaria[length(portaria)]

    lista_ind <- vector("list", length(novas_ind) - 1 )

    for (i in seq_along(lista_ind)) {
      lista_ind[[i]] <- novas_ind[i]:(novas_ind[i + 1] - 1)
    }

    novas <- vector("list", length(lista_ind))

    for (i in seq_along(novas)) {
      novas[[i]] <- c(nome, portaria[ lista_ind[[i]] ], rodape)
    }

    if (any(sapply(novas, function(x) (length(grep("resolve:", x)) != 1) ))) {
      individualizadas <- "Erro"
    } else {
      individualizadas <- unlist(lapply(novas, multipla_para_individualizada))
    }

  } else {
    padrao <- 'Nº ?[0-9]+\\.?[0-9]*'
    inicios <- c(grep(padrao, portaria), length(portaria))

    nomes <- paste("PORTARIA", stringr::str_extract(portaria, padrao) %>%
                     extract(!is.na(.)),
                   stringr::str_sub(portaria[1], 11))

    cabeca <- paste0(portaria[2:( inicios[1]-1 )], collapse = " ")

    rodape <- portaria[length(portaria)] # por def tamanho 1

    indices <- vector("list", length(inicios) - 1)

    for (i in seq_along(indices)) {
      indices[[i]] <- inicios[i]:(inicios[i+1] - 1)
    }

    lista_portarias <- vector("list", length(indices))

    for (i in seq_along(lista_portarias)) {
      lista_portarias[[i]] <- paste0(nomes[i], "\n", cabeca, "\n",
                                     paste0(portaria[ indices[[i]] ], collapse = "\n"),
                                     "\n", rodape)
    }

    individualizadas <- gsub(paste0(padrao, ' ?-'), "", unlist(lista_portarias))
  }

  individualizadas
}

