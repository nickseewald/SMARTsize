## 
selectDTROutcomeUI <- function(id) {
  # Namespace function
  ns <- NS(id)
  radioButtons(ns("selectOutcome"), 
               label = HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
               choices = list("Binary", "Continuous"), selected = "Binary")
}

selectDTROutcome <- function(input, output, session) {
  return(reactive(input$selectOutcome))
}