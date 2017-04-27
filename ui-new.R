require(shiny)
require(shinyBS)
require(shinythemes)

options(encoding='UTF-8')

navbarPage( title = "SMART Sample Size Calculator", id = "partial", collapsible = T,
            tabPanel("Home", value="home", uiOutput("renderPageHome")),
            tabPanel("Design A", value="designA", uiOutput("renderPageA")),
            tabPanel("Design B", value="designB", uiOutput("renderPageB")),
            tabPanel("Design C", value="designC", uiOutput("renderPageC")),
            tabPanel("Design D", value="designD", uiOutput("renderPageD"))
)