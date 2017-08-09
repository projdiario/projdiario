#' Sugere atos que merecem atenção
#'
#' @param conferir um vetor com o texto a ser conferido
#' @param nomes o vetor com os nomes ou ids dos textos a serem conferidos
#' @param gabarito vetor com o qual as respostas devem ser próximas
#' @return Um data.frame com um vetor dizendo se a observação merece atenção
#'         e outro vetor com o nome da observação sugerida
#' @examples
#' #Sem exemplo

# atencao <- function(conferir, nomes, gabarito) {
#   requireNamespace('quanteda', quietly = TRUE)
#   names(conferir) <- nomes
#
#   resp <- logical(length(gabarito))
#
#   for (i in seq_along(resp)) {
#     docs <- quanteda::dfm(c(txt = gabarito[i], conferir), stem = TRUE,
#                 remove = stopwords("portuguese"))
#
#     # compute some document similarities
#     tmp <- quanteda::similarity(docs, margin = "documents")$txt
#     # output as a matrix
#     resp[i] <- names(tmp)[which.max(tmp)]
#   }
#
#   indice <- integer(length(resp))
#
#   for (j in seq_along(indice)) {
#     indice[j] <- which(resp[j] == nomes)
#   }
#
#   data.frame(atencao = nomes != resp,
#              sugerido = indice,
#              stringsAsFactors = FALSE)
# }
