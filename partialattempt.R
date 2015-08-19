require(shiny)
require(shinyBS)

ui <- navbarPage( title = "SMART Sample Size Calculator", id = "partial", #collapsible = T,
                  tabPanel("Home", value="home", uiOutput("renderPageHome")),
                  tabPanel("Design A", value="designA", uiOutput("renderPageA"))
#                   tabPanel("Design B", value="designB", uiOutput("renderPageB")),
#                   tabPanel("Design C", value="designC", uiOutput("renderPageC")),
#                   tabPanel("Design D", value="designD", uiOutput("renderPageD"))
)

server <- function(input, output, session) {
  output$renderPageHome <- renderUI({
    source(file.path("partials/home.R"),local=TRUE, chdir=F, print.eval= FALSE)
  })
  output$renderPageA <- renderUI({
    source(file.path("partials/designA.R"),local=TRUE, chdir=F, print.eval = F)
  })
  # source(file.path("controllers/modalButton.R"))
}

shinyApp(ui=ui, server=server)