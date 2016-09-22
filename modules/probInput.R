probInputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    numericInput(ns("respA"),
                 label = HTML("<p>Concerning the tailoring variable, please provide the
             <strong>probability of response</strong> to the first-stage
                              intervention. If you are unsure, leave as 0 for a conservative
                              estimate.</p>"),
                 value = 0,min = 0,max = 1,step = 0.01),
    conditionalPanel(condition = 'output.designAoutcome == "Binary"',
                     eval(text.successProbLabel), uiOutput("binaryDTR1probA"),
                     conditionalPanel(condition = "input.cellOrConditionalA",
                                      fluidRow(column(11, offset = 1,
                                                      uiOutput("cellProbsDTR1A"))
                                      )
                     ),
                     uiOutput("binaryDTR2probA"),
                     conditionalPanel(condition = "input.cellOrConditionalA && input.secondDTRcompareA",
                                      fluidRow(column(11,offset = 1,
                                                      uiOutput("cellProbsDTR2A"))
                                      )
                     )
    ),
    conditionalPanel(condition = 'output.designAoutcome == "Continuous',
                     eval(text.stdEffectLabel), uiOutput("continuousProbA")
    ),
    
    ##### A INPUT OPTIONS #####
    # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
    # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
    # For continuous outcomes, option to input mean difference and standard-deviation
    
    conditionalPanel(condition = "input.firstDTRcompareA != 0 && input.secondDTRcompareA != 0",
                     br(),
                     conditionalPanel(condition = "output.designAoutcome == 1",
                                      eval(text.altInputHelp),
                                      checkboxInput("cellOrConditionalA", label = text.cellSpecLabel, value = FALSE),
                                      checkboxInput("targetDiffCheckA",   label = text.targDiffLabel, value = FALSE),
                                      checkboxInput("targetOddsCheckA",   label = text.targORLabel,   value = FALSE)
                     )
    )
    )
}

probInput <- function(input, output, session, outcomeType, refDTR, compareDTR) {
  inputSelector <- renderUI({
    validate(
      need(refDTR(), "Please select a Reference AI.")
    )
    if (outcomeType() == "Binary") {
      
    }
  })
  
  generateBinaryInputs1 <- reactive({
    if (input$cellOrConditionalA == TRUE) {
      return(disable(numericInput("DTRsuccA1disable", 
                                  label =   HTML("Probability of Success for Reference AI &nbsp; <img src='images/blue_dash.gif'>"),
                                  value = NA, min = 0, max = 1, step = 0.01)))
    } else {
      return(list(numericInput("DTRsuccA1", 
                               label =  HTML("Probability of Success for Reference AI &nbsp; <img src='images/blue_dash.gif'>"),
                               value = NA, min = 0, max = 1, step = 0.01),
                  bsTooltip(id = "DTRsuccA1", title = text.tooltip, 
                            placement = "right", trigger = "focus"))
      )}
  })
  
  generateBinaryInputs2A <- reactive({
    validate(
      need(compDTR != "", "Please select a Comparison AI.")
    )
    # Full DTR success probability
    if (input$targetDiffCheckA == FALSE && input$targetOddsCheckA == FALSE) {
      if (input$cellOrConditionalA == TRUE) {
        return(disable(numericInput(ns("DTRsuccA2disable"), 
                                    label = HTML("Probability of Success for Comparison AI &nbsp; <img src='images/red_dash.gif'>"),
                                    value = NA, min = 0, max = 1, step = 0.01)))
      }
      else{
        return(list(numericInput(ns("DTRsuccA2"), 
                                 label = HTML("Probability of Success for Comparison AI &nbsp; <img src='images/red_dash.gif'>"),
                                 value = NA, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = ns("DTRsuccA2"), title = text.tooltip,
                              placement = "right", trigger = "focus"))
        )}
    }
    # Target difference
    if (input$targetDiffCheckA == TRUE && input$targetOddsCheckA == FALSE) {
      return(list(
        numericInput("targetDiffA", label = text.targDiffLabel,
                     value = NULL, min = 0.01, max = 0.99, step = 0.01),
        bsTooltip(id = "targetDiffA", title = text.tooltip,
                  placement = "right", trigger = "focus"),
        radioButtons("diffDirectionA", label = text.diffDirection,
                     choices = list("Smaller" = -1, "Larger" = 1),
                     selected = -1)
      ))
    }
    # Target Odds Ratio
    if (input$targetOddsCheckA == TRUE) {
      return(list(
        numericInput("targetORA", label = text.targORInputLabel,
                     value = NULL, min = 0, step = 0.01),
        bsTooltip(id = "targetORA", title = text.tooltip,
                  placement = "right", trigger = "focus")
      ))
    }
  })
  
  output$probs <- renderUI({
    if (outcomeType == "binary") {
      eval(text.successProbLabel)
      generateBinaryInputs1()
      conditionalPanel(condition = "input.cellOrConditionalA",
                       fluidRow(column(11, offset = 1,
                                       uiOutput("cellProbsDTR1A"))
                       )
      ),
      uiOutput("binaryDTR2probA"),
      conditionalPanel(condition = "input.cellOrConditionalA && input.secondDTRcompareA",
                       fluidRow(column(11,offset = 1,
                                       uiOutput("cellProbsDTR2A"))
                       )
      )
    }
  })
}