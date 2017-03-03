primaryAimUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("aim"))
}

primaryAim <- function(input, output, session, rerand) {
  choice <- reactive({
    choices <- list("Compare first-stage treatments as initial treatments in an adaptive intervention." = "stage1")
    if ("responders" %in% rerand())
      choices[["Compare second-stage treatments among responders as secondary treatments in an adaptive intervention."]] <- "stage2resp"
    if ("nonresponders" %in% rerand()) 
      choices[["Compare second-stage treatments among non-responders as secondary treatments in an adaptive intervention."]] <- "stage2nresp"
    if (is.null(rerand()))
      choices[["Compare second-stage treatments as secondary treatments in an adaptive intervention."]] <- "stage2"
    choices[["Compare two embedded adaptive interventions with different first-stage treatments."]] <- "dtrs"
    choices
  })
  
  output$aim <- renderUI({
    ns <- session$ns
    radioButtons(ns("primaryAim"), 
                 label = "What is your primary aim?",
                 choices = choice(),
                 width = "100%",
                 selected = "dtrs")
  })
  return(reactive({input$primaryAim}))
}