primaryAimUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("aim"))
}

primaryAim <- function(input, output, session, rerand) {
  choice <- reactive({
    choices <- list("Compare First-stage Treatments" = "stage1")
    if ("responders" %in% rerand())
      choices[["Compare Second-stage Treatments among Responders"]] <- "stage2resp"
    if ("nonresponders" %in% rerand()) 
      choices[["Compare Second-stage Treatments among Non-Responders"]] <- "stage2nresp"
    if (is.null(rerand()))
      choices[["Compare Second-stage Treatments"]] <- "stage2"
    choices[["Compare Two Embedded Adaptive Interventions"]] <- "pairwise"
    choices[["Compare All Embedded Adaptive Interventions"]] <- "omnibus"
    choices
  })
  
  output$aim <- renderUI({
    ns <- session$ns
    radioButtons(ns("primaryAim"), 
                 label = "What is your primary aim?",
                 choices = choice(),
                 width = "100%",
                 selected = "stage1")
  })
  return(reactive({input$primaryAim}))
}