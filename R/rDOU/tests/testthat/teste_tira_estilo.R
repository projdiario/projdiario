context("tira_estilo()")

html <- c("<html>",
          "<style> font-family=verdana",
          "background-color=red </style>",
          "<h1>Título</h1>", "<style>h1 {font-color:green}</style>",
          "<o:wrapblock>",
          "<v:shapetype>",
          "<p>Texto</p>",
          "</html>")


test_that("Remove apenas o estilo" , {
  expect_equal(tira_estilo(html), c("<html>", "",
                                    "<h1>Título</h1>", "",
                                    "", "",
                                    "<p>Texto</p>",
                                    "</html>"))
})
