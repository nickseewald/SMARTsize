resultOptionsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6,
             radioButtons(ns("selectResults"), label = "Are you interested in finding sample size or power?",
                          choices  = list("Sample Size" = "sample", "Power" = "power"),
                          selected = "sample"),
             radioButtons(ns("selectAlternative"), label = "Do you want to perform a one- or two-sided test?",
                          choices  = list("One-Sided" = "one.sided", "Two-Sided" = "two.sided"),
                          selected = "two.sided")
      ),
      column(6,
             numericInput(ns("alpha"), label = HTML("Type I Error (&alpha;):"), 
                          value = 0.05, min = 0, max = 1, step = 0.01),
             # conditionalPanel(condition = paste0("input['", ns("selectResults"), "'] == 'sample'"),
             #                  numericInput(ns("inputPower"), 
             #                               label = HTML("Power of Trial (1-&beta;):"),
             #                               value = 0.8, min = 0, max = 1, step = 0.01)
             # ),
             # conditionalPanel(condition = paste0("input['", ns("selectResults"), "'] == 'power'"),
             #                  numericInput(ns("inputSampleSize"),
             #                               label = "Total Sample Size of Trial:",
             #                               value = 0, min = 0)
             # )
             uiOutput(ns("inputPowerSize"))
      )
    )
  )
}

resultOptions <- function(input, output, session) {
  ns <- session$ns
  output$inputPowerSize <- renderUI({
    if (input$selectResults == 'sample') {
      numericInput(ns("inputPower"), 
                   label = HTML("Power of Trial (1-&beta;):"),
                   value = 0.8, min = 0, max = 1, step = 0.01)
    } else {
      numericInput(ns("inputSampleSize"),
                   label = "Total Sample Size of Trial:",
                   value = 0, min = 0)
    }
  })
  
  return(reactive(
    list("resultType"  = input$selectResults,
         "alternative" = input$selectAlternative,
         "alpha"       = input$alpha,
         "inputPower"  = input$inputPower,
         "inputSize"   = input$inputSampleSize)))
}