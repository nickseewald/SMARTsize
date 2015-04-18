function(input, output, session) {
  output$renderPageHome <- renderUI({
    source(file.path("partials/home.R"),local=TRUE)
  })
  source(file.path("controllers/modalButton.R"))
}
