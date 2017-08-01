#!/usr/bin/env Rscript 
if (!"shiny" %in% installed.packages()) { 
  try(install.packages('shiny', repos = "http://cran.fiocruz.br/", 
                       dependencies = TRUE)) 
  if (!"shiny" %in% installed.packages()) { 
    pacotes <- dir('data/node_modules/', pattern = ".zip$", full.names = TRUE) 
    for (pacote in pacotes) { 
      install.packages(pacote, repos = NULL, type = 'source') 
    } 
    
  } 
} 

shiny::shinyAppDir(appDir = ".",
                   options = list(port = 8080, quiet = TRUE, launch.browser = TRUE)) 