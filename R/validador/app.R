sapply(c('shiny', 'shinyjs', 'httr'), function(pacote) {
  if (!require(pacote, character.only = TRUE)) {
    install.packages(pacote, dependencies = TRUE, verbose = FALSE,
                     repos = 'http://cran.fiocruz.br', quiet = TRUE)
    invisible(require(pacote, character.only = TRUE))
  }
})
runApp('.', launch.browser = FALSE, port = 6312)

