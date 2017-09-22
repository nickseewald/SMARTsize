# This file is part of SMARTsize.
# 
# SMARTsize is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# SMARTsize is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with SMARTsize.  If not, see <http://www.gnu.org/licenses/>.

##### SERVER.R FOR SMART SAMPLE SIZE CALCULATOR #####
### NICK SEEWALD, 2014
### UNIVERSITY OF MICHIGAN
### DEPARTMENT OF BIOSTATISTICS

options(encoding = 'UTF-8')


### Start server operation

shinyServer(
  function(input, output, session){
    
    session$allowReconnect(TRUE)
    
    premade <- eventReactive(input$premade.startbutton,
                             list(
                               p("The three designs below are commonly seen in the field. If your design
                                 matches one, click the button below it to get started. Otherwise, you can
                                 design your own. Click an image to zoom."),
                               fluidRow(
                                 column(4,
                                        div(a(img(id = "D1.diagram.home", src = "images/SMARTdesignA__.gif",
                                                  class = "img-responsive img-zoom", alt = "Design I"),
                                              href = "images/SMARTdesignA__.gif", target = "_blank")),
                                        br(),
                                        actionButton("pickTabA", "Use Design I", class = "center-block"),
                                        p("Both responders and non-responders to first-stage treatment are re-randomized.
                                          There are 8 embedded adaptive interventions: {A,C,E}, {A,C,F}, {A,D,E}, {A,D,F},
                                          {B,G,I}, {B,G,J}, {B,H,I}, {B,H,J}.", 
                                          a("Click here for an example from the field.", `data-toggle` = "modal",
                                            `data-target` = "#exampleAmodal", style = "color:#6b6b6b")),
                                        includeHTML("www/html/exampleAmodal.html")
                                 ),
                                 column(4,
                                        div(a(img(id = "D2.diagram.home", src = "images/SMARTdesignB__.gif",
                                                  class = "img-responsive img-zoom", alt = "Design II"),
                                              href = "images/SMARTdesignB__.gif", target = "_blank")),
                                        br(),
                                        actionButton("pickTabB","Use Design II", class = "center-block"),
                                        p("Only non-responders to either first-stage treatment are re-randomized.
                                          There are 4 embedded adaptive interventions: {A,C,D}, {A,C,E}, {B,F,G}, {B,F,H}.",
                                          a("Click here for an example from the field.", `data-toggle` = "modal",
                                            `data-target` = "#exampleBmodal", style = "color:#6b6b6b")),
                                        includeHTML("www/html/exampleBmodal.html")
                                 ),
                                 column(4,
                                        div(a(img(id = "D3.diagram.home", src = "images/SMARTdesignC__.gif",
                                                  class = "img-responsive img-zoom", alt = "Design III"),
                                              href = "images/SMARTdesignC__.gif", target = "_blank")),
                                        br(),
                                        bsButton("pickTabC","Use Design III", class = "center-block"),
                                        p("Only non-responders to a particular first-stage treatment are re-randomized.
                                          There are 3 embedded adaptive interventions: {A,C,D}, {A,C,E}, {B,F,G}.",
                                          a("Click here for an example from the field.", `data-toggle` = "modal",
                                            `data-target` = "#exampleCmodal", style = "color:#6b6b6b")),
                                        includeHTML("www/html/exampleCmodal.html")
                                 )
    )
    # br(),
    # fluidRow(
    #   column(3),
    #   ,
    #   column(3)
    #   )
                                 )
                                 )
    
    output$premadeDesigns <- renderUI(premade())
    observeEvent(input$premade.startbutton, shinyjs::hide("premade.startbutton"))
    
    output$design1img.home <- renderImage(list(src = file.path("www", "images", "SMARTdesignA__.gif")), deleteFile = FALSE)
    
    ### Watch for clicks on pickTab actionButtons rendered under design diagrams
    ### On click, redirect to appropriate tab. (More intuitive navigation structure)
    # observeEvent(input$pickTabA, updateTabsetPanel(session, "SMARTsize", selected = "Design I"))
    # observeEvent(input$pickTabB, updateTabsetPanel(session, "SMARTsize", selected = "Design II"))
    # observeEvent(input$pickTabC, updateTabsetPanel(session, "SMARTsize", selected = "Design III"))
    observeEvent(input$DYO.startbutton, updateTabsetPanel(session, "SMARTsize", selected = "Design and Size"))
    
    ##### DESIGN I #####
    
    ##### Design I Header #####
    
    ### Render the design image which highlights selected DTRs
    ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
    ### Note that this requires a very specific naming convention for image assets
    ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0.
    
    output$designAimg <- renderImage(expr = {
      filename <- 
        normalizePath(file.path('./www/images',
                                paste('SMARTdesignA_', input$firstDTRcompareA,
                                      '_', input$secondDTRcompareA,'.gif', sep = ''))
        )
      list(src = filename)
    }, deleteFile = FALSE)
    
    
    ##### Design I Modules #####
    
    # Backend for outcome selection radioButtons
    design1.outcome <- callModule(selectDTROutcome, "design1.outcome")
    output$design1outcome <- renderText(design1.outcome())
    outputOptions(output, 'design1outcome', suspendWhenHidden = FALSE)
    
    # Backend for result options (alpha, desired power/sample size, etc.)
    design1.resultOptions <- callModule(resultOptions, "design1.resultOptions")
    output$design1resultOptions <- renderText(unlist(design1.resultOptions()))
    outputOptions(output, 'design1resultOptions', suspendWhenHidden = FALSE)
    
    # Backend for selection of primary comparison for sample size calculation
    design1.primaryAim <- callModule(primaryAim, "design1.primaryAim",
                                     rerand = reactive({c("responders", "nonresponders")}))
    output$design1primaryAim <- renderText(design1.primaryAim())
    outputOptions(output, 'design1primaryAim', suspendWhenHidden = FALSE)
    
    
    ### Render 'selectize' dropdown boxes with placeholder text for AI selection.
    ### Second DTR input populates with all DTRs that are NOT the first DTR
    ###   (eliminates ability to select same DTR twice)
    ### Placed in server.R rather than ui.R because of dependency on first DTR selection
    
    output$selectAI1A <- renderUI({
      AI <- selectizeInput("firstDTRcompareA", label = text.refDTRLabel,
                           choices = designA.AIs,
                           options = list(
                             placeholder = text.refDTRPlaceholder,
                             onInitialize = I('function() { this.setValue(0); }')
                           )
      )
      return(AI)
    })
    
    output$selectAI2A <- renderUI({
      AI <- selectizeInput("secondDTRcompareA",label = text.compDTRLabel,
                           choices = designA.AIs[substr(designA.AIs, 1, 1) !=
                                                   substringDTR1A()[2]],
                           options = list(
                             placeholder = text.compDTRPlaceholder,
                             onInitialize = I('function() { this.setValue(0); }')
                           )
      )
      return(AI)
    })
    
    
    ##### DESIGN A PROBABILITY INPUT #####
    
    ## Read in DTR names from dropdowns and parse them to determine 
    #   first and second stage treatments
    ## Outputs full DTR name, first-stage treatment (first character), 
    #   second-stage treatment if response (third character),
    #   and second-stage treatment if non-response(last character) 
    ## NOTE that these positions are exclusive to design A because responders
    #   have two second-stage treatment options
    
    substringDTR1A <- reactive({
      if (length(input$firstDTRcompareA) > 0) {
        DTR1           <- paste(input$firstDTRcompareA)
        firstStage1    <- substr(DTR1, 1, 1)
        secondStageR1  <- substr(DTR1, 3, 3)
        secondStageNR1 <- substr(DTR1, 6, 6)
        return(c(DTR1, firstStage1, secondStageR1, secondStageNR1))
      }
      else return(c(0, 0, 0, 0))
    })
    
    substringDTR2A <- reactive({
      if (length(input$secondDTRcompareA) > 0) {
        DTR2           <- paste(input$secondDTRcompareA)
        firstStage1    <- substr(DTR2, 1, 1)
        secondStageR1  <- substr(DTR2, 3, 3)
        secondStageNR1 <- substr(DTR2, 6, 6)
        return(c(DTR2, firstStage1, secondStageR1, secondStageNR1))
      }
      else return(c(0, 0, 0, 0))
    })
    
    ## When a first DTR is selected, render an input box corresponding to
    #   whatever input method is selected.
    ## For DTR-conditional or cell-specific inputs, first numericInput is 
    #   P(S|DTR1), enabled/disabled depending on cellOrConditionalA
    ## For target-difference or odds ratio, relevant numericInputs are rendered
    ## This is the ONLY location in which difference and odds ratio
    #   numericInputs are built.
    
    generateBinaryInputs1A <- reactive({
      validate(
        need(input$firstDTRcompareA, text.refDTRPlaceholder)
      )
      if (input$cellOrConditionalA == TRUE) {
        return(disable(numericInput("DTRsuccA1disable", 
                                    label = html.refDTRSuccess,
                                    value = NA, min = 0, max = 1, step = 0.01)))
      } else {
        return(list(numericInput("DTRsuccA1", 
                                 label = html.refDTRSuccess,
                                 value = NA, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "DTRsuccA1", title = text.tooltip, 
                              placement = "right", trigger = "focus"))
        )}
    })
    
    generateBinaryInputs2A <- reactive({
      validate(
        need(input$secondDTRcompareA != "", text.compDTRPlaceholder)
      )
      # Full DTR success probability
      if (input$targetDiffCheckA == FALSE && input$targetOddsCheckA == FALSE) {
        if (input$cellOrConditionalA == TRUE) {
          return(disable(numericInput("DTRsuccA2disable", 
                                      label = html.compDTRSuccess,
                                      value = NA, min = 0, max = 1, step = 0.01)))
        }
        else{
          return(list(numericInput("DTRsuccA2", label = html.compDTRSuccess,
                                   value = NA, min = 0, max = 1, step = 0.01),
                      bsTooltip(id = "DTRsuccA2", title = text.tooltip,
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
    
    ### Render UI components generated above
    
    output$binaryDTR1probA <- renderUI({
      generateBinaryInputs1A()
    })
    
    output$binaryDTR2probA <- renderUI({
      generateBinaryInputs2A()
    })
    
    ## For cell-specific probabilities, render a sequence of numericInputs
    #   labeled by information from DTR substrings
    ## When DTR1 and DTR2 begin with the same treatment, P(S|stage1trt,r)
    #   is rendered only once, in output$cellProbsDTR1A
    
    output$cellProbsDTR1A <- renderUI({
      return(list(
        numericInput("marginalFirstStageA1", 
                     label = paste(text.pathSuccess, substringDTR1A()[2], "r",
                                   substringDTR1A()[3], sep = ""),
                     value = 0, min = 0, max = 1, step = 0.01),
        numericInput("marginalSecondStageNRA1",
                     label = paste(text.pathSuccess, substringDTR1A()[2], "nr",
                                   substringDTR1A()[4], sep = ""),
                     value = 0, min = 0, max = 1, step = 0.01),
        bsTooltip(id = "marginalFirstStageA1", title = text.tooltip, 
                  placement = "right", trigger = "focus"),
        bsTooltip(id = "marginalSecondStageNRA1", title = text.tooltip,
                  placement = "right", trigger = "focus")
      ))
    })
    
    output$cellProbsDTR2A <- renderUI({
      return(list(
        numericInput("marginalFirstStageA2",
                     label = paste(text.pathSuccess, substringDTR2A()[2], "r",
                                   substringDTR2A()[3], sep = ""),
                     value = 0, min = 0, max = 1, step = 0.01),
        numericInput("marginalSecondStageNRA2",
                     label = paste(text.pathSuccess, substringDTR2A()[2], "nr",
                                   substringDTR2A()[4], sep = ""),
                     value = 0, min = 0, max = 1, step = 0.01),
        bsTooltip(id = "marginalFirstStageA2", title = text.tooltip,
                  placement = "right", trigger = "focus"),
        bsTooltip(id = "marginalSecondStageNRA2", title = text.tooltip,
                  placement = "right", trigger = "focus")
      ))
    })
    
    ### Render enabled/disabled numericInputs when outcome is continuous
    
    generateContinuousInputA <- reactive({
      validate(
        need(input$firstDTRcompareA, text.refDTRPlaceholder),
        need(input$secondDTRcompareA, text.compDTRPlaceholder)
      )
      return(list(
        numericInput("effectSizeA", label = text.effectSize,
                     value = 0, min = 0, max = 10, step = 0.01),
        bsTooltip(id = "effectSizeA", title = text.tooltip,
                  placement = "right", trigger = "focus"))
      )
    })
    
    output$continuousProbA <- renderUI({
      generateContinuousInputA()
    })
    
    
    ##### DESIGN A OBSERVERS #####
    
    ### Allow checkboxes to function like radio buttons, while also allowing none to be selected
    
    observe({
      if (input$targetDiffCheckA) {
        updateCheckboxInput(session, "cellOrConditionalA", value = FALSE)
        updateCheckboxInput(session, "targetOddsCheckA",   value = FALSE)
      }
    })
    
    observe({
      if (input$targetOddsCheckA) {
        updateCheckboxInput(session, "cellOrConditionalA", value = FALSE)
        updateCheckboxInput(session, "targetDiffCheckA",   value = FALSE)
      }
    })
    
    ### If cell-specific input is selected, deselect target difference and odds-ratio options
    ### Update disabled full DTR success inputs with computed probabilities
    
    observe({
      if (input$cellOrConditionalA == TRUE) {
        updateCheckboxInput(session, "targetDiffCheckA", value = FALSE)
        updateCheckboxInput(session, "targetOddsCheckA", value = FALSE)
        
        updateNumericInput(session, "DTRsuccA1disable", value = generateProbsA()[1])
        updateNumericInput(session, "DTRsuccA2disable", value = generateProbsA()[2])
        updateNumericInput(session, "DTRsuccA1",        value = generateProbsA()[1])
        updateNumericInput(session, "DTRsuccA2",        value = generateProbsA()[2])
      }
    }, priority = 2)
    
    ##### DESIGN A RESULT BACKEND #####
    
    ### Compute full DTR probabilities or effect size when providing cell-specific probabilities or mean/SD
    
    generateProbsA <- reactive({
      if (design1.outcome() == "Binary" && input$cellOrConditionalA == TRUE) {
        pDTR1 <- fullDTRprob(input$marginalFirstStageA1, input$respA, 
                             input$marginalSecondStageNRA1)
        pDTR2 <- fullDTRprob(input$marginalFirstStageA2, input$respA, 
                             input$marginalSecondStageNRA2)
      }
      else if (design1.outcome() == "Binary" && input$cellOrConditionalA == FALSE) {
        pDTR1 <- input$DTRsuccA1
        pDTR2 <- input$DTRsuccA2
      }
      
      return(c(pDTR1,pDTR2))
    })
    
    ### Make sure input probabilities are valid (i.e., check for blank entries)
    
    checkDTRinputsA <- reactive({
      return(as.logical((input$DTRsuccA1 > 0) & (input$DTRsuccA1 < 1) &
                          (input$DTRsuccA2 > 0) & (input$DTRsuccA2 < 1)))
    })
    
    ### Determine which inputs are being given, check their quality, then pass 
    ###  the appropriate arguments for computation
    ###  e.g., if target odds-ratio is given, compute a DTR success probability
    ###  assuming the other is 0.5
    dataCompilerA <- reactive({
      
      ### Error Check: unselected DTRs, blank/invalid response probability
      validate(
        need(!(is.na(input$respA)), text.noResponse) %then%
          need(0 <= input$respA && input$respA <= 1, text.invalidResponse)
      )
      
      ### Binary outcome, DTR-specific success probabilities
      if (design1.outcome() == "Binary" && input$cellOrConditionalA == FALSE &&
          input$targetDiffCheckA == FALSE && input$targetOddsCheckA == FALSE) {
        ### Error Check: unselected DTRs, blank success probabilities,
        ### equal success probabilities, invalid success probabilities
        validate(
          need(!is.null(input$DTRsuccA1), text.refDTRPlaceholder),
          need(!is.null(input$DTRsuccA2), text.compDTRPlaceholder) %then%
            need(!is.null(input$DTRsuccA1) && !is.null(input$DTRsuccA2), 
                 text.noSuccessProb) %then%
            need(input$DTRsuccA1 != input$DTRsuccA2, text.sameSuccessProb) %then%
            need(checkDTRinputsA(), text.invalidSuccessProb)
        )
        return(c(input$DTRsuccA1, input$DTRsuccA2))
      }
      
      # Binary outcome, cell-specific success probabilities
      if (design1.outcome() == "Binary" && input$cellOrConditionalA == TRUE &&
          input$targetDiffCheckA == FALSE && input$targetOddsCheckA == FALSE) {
        # Error Check: equal DTR-specific success probabilities
        validate(
          need(generateProbsA()[1] != generateProbsA()[2], text.invalidCellResult)
        )
        return(c(generateProbsA()[1], generateProbsA()[2]))
      }
      
      ### Binary outcome, target difference in success probabilities
      if (design1.outcome() == "Binary" && input$cellOrConditionalA == FALSE &&
          input$targetDiffCheckA == TRUE && input$targetOddsCheckA == FALSE) {
        # Error Check: invalid target difference (must be less than 0.5 since
        # we're using a conservative reference probability)
        validate(
          need(input$DTRsuccA1 + as.numeric(input$diffDirectionA) *
                 input$targetDiffA >= 0 && input$DTRsuccA1 + 
                 as.numeric(input$diffDirectionA) * input$targetDiffA <= 1,
               text.invalidDiffResult),
          need(input$targetDiffA > 0, text.invalidDiff)
        )
        return(c(input$DTRsuccA1, input$DTRsuccA1 + 
                   as.numeric(input$diffDirectionA) * input$targetDiffA))
      }
      
      ### Binary outcome, target odds ratio of success
      if (design1.outcome() == "Binary" && input$cellOrConditionalA == FALSE && 
          input$targetDiffCheckA == FALSE && input$targetOddsCheckA == TRUE) {
        ### Error check: missing/invalid odds ratio
        validate(
          need(is.numeric(input$targetORA), text.noOddsRatio) %then%
            need(input$targetORA != 1, text.invalidOR1) %then%
            need(input$targetORA != 0, text.invalidOR0)
        )
        q <- (input$DTRsuccA1 / (1 - input$DTRsuccA1)) * input$targetORA
        return(c(input$DTRsuccA1, q / (1 + q)))
      }
      
      ### Continuous outcome, standardized effect size
      if (design1.outcome() == "Continuous") {
        ### Error check: nonzero effect size
        validate(
          need(input$firstDTRcompareA, text.refDTRPlaceholder),
          need(input$secondDTRcompareA, text.compDTRPlaceholder),
          need(!is.null(input$effectSizeA), " ") %then%
            need(!is.na(input$effectSizeA), text.noEffectSize) %then%
            need(input$effectSizeA != 0, text.invalidEffectSize)
        )
        return(input$effectSizeA)
      }
    })
    
    ### Construct a portion of the explainer sentence that appears below the 
    ### result, depending on the provided inputs.
    sentenceCompilerA <- reactive({
      if (design1.outcome() == "Binary" && input$cellOrConditionalA == FALSE &&
          input$targetDiffCheckA == FALSE && input$targetOddsCheckA == FALSE) {
        return(paste(text.sentenceOverallSuccess, input$firstDTRcompareA,
                     " and ", input$secondDTRcompareA, ", are ", input$DTRsuccA1,
                     " and ", input$DTRsuccA2, ", respectively", sep = ""))
      }
      if (design1.outcome() == "Binary" && input$cellOrConditionalA == TRUE &&
          input$targetDiffCheckA == FALSE && input$targetOddsCheckA == FALSE) {
        return(paste(text.sentenceOverallSuccess, input$firstDTRcompareA, " and ",
                     input$secondDTRcompareA, ", are ", generateProbsA()[1],
                     " and ", generateProbsA()[2], ", respectively", sep = ""))
      }
      
      if (design1.outcome() == "Binary" && input$cellOrConditionalA == FALSE &&
          input$targetDiffCheckA == TRUE && input$targetOddsCheckA == FALSE) {
        return(paste(text.sentenceDiff, input$firstDTRcompareA, " and ", 
                     input$secondDTRcompareA, ", is ", input$targetDiffA, sep = ""))
      }
      
      if (design1.outcome() == "Binary" && input$cellOrConditionalA == FALSE &&
          input$targetDiffCheckA == FALSE && input$targetOddsCheckA == TRUE) {
        return(paste(text.sentenceOR, input$firstDTRcompareA, " and ",
                     input$secondDTRcompareA, ", is ",input$targetORA, sep = ""))
      }
      
      if (design1.outcome() == "Continuous") {
        return(paste(text.sentenceEffectSize, input$firstDTRcompareA, " and ", 
                     input$secondDTRcompareA, ", is ", input$effectSizeA, sep = ""))
      }
    })
    
    ##### DESIGN A RESULTS #####
    
    # Pass arguments from dataCompilerA() to appropriate R function
    # Extract and render relevant output
    
    ## Sample size calculation and output for binary outcome
    output$binarySampleSizeA <- renderUI({
      
      validate(
        need(input$inputPowerA > 0, text.power0),
        need(input$inputPowerA < 1, text.power100)
      )
      
      A <- dataCompilerA()[1]  / (1 - dataCompilerA()[1])
      B <- (dataCompilerA()[2] / (1 - dataCompilerA()[2])) / A
      
      designEffect <- 2
      finalAlpha    <- ifelse(input$selectAlternativeA == "two.sided",
                              input$alphaA / 2, input$alphaA)
      
      rawSampleSize   <- 2 * ((qnorm(1 - input$inputPowerA) + 
                                 qnorm(finalAlpha)) ^ 2) * 
        (((1 + A) ^ 2) * B + ((1 + A * B) ^ 2)) / (A * B * (log(B)) ^ 2)
      finalSampleSize <- ceiling(designEffect * rawSampleSize)
      
      formatPower  <- paste(input$inputPowerA * 100, "%", sep = "")
      formatAlpha  <- paste(input$alphaA      * 100, "%", sep = "")
      formatAltHyp <- switch(input$selectAlternativeA,
                             "one.sided" = "one-sided ",
                             "two.sided" = "two-sided ")
      formatResp   <- as.numeric(input$respA)
      
      HTML(paste("<h4 style='color:blue';> N=", paste(finalSampleSize)," </h4>
                 <p> We wish to find the sample size for a trial with a binary outcome 
                 where the probability of response to first-stage intervention is ",
                 formatResp, sentenceCompilerA(), ". Given a ", formatAltHyp, 
                 " test with ", formatAlpha, " type-I error, we require a sample
                 size of at least ", finalSampleSize, " to make this comparison
                 with ", formatPower, " power. </p>", sep = ""))
    })
    
    ## Power computation and output for binary outcome
    output$binaryPowerA <- renderUI({
      validate(
        need(input$alphaA != 0 && !is.null(input$alphaA), html.alpha0),
        need(input$inputSampleSizeA != 0, text.size0) %then%
          need(input$inputSampleSizeA >= 8, text.lowSizeA)
      )
      
      A <- dataCompilerA()[1] / (1 - dataCompilerA()[1])
      B <- (dataCompilerA()[2] / (1 - dataCompilerA()[2])) / A
      
      designEffect  <- 2
      finalAlpha    <- ifelse(input$selectAlternativeA == "two.sided",
                              input$alphaA / 2, input$alphaA)
      
      powerQuantile <- sqrt((input$inputSampleSizeA * A * B * log(B) ^ 2) /
                              (2 * designEffect * ((1 + A) ^ 2 * B +
                                                     (1 + A*B) ^ 2))) +
        qnorm(finalAlpha)
      
      finalPower    <- round(pnorm(powerQuantile), digits = 3)
      formatPower   <- paste(finalPower   * 100, "%", sep = "")
      formatAlpha   <- paste(input$AlphaA * 100, "%", sep = "")
      formatSize    <- as.numeric(input$inputSampleSizeA)
      formatResp    <- as.numeric(input$respA)
      
      HTML(paste("<h4 style='color:blue';> Power=", paste(formatPower), " </h4>
                 <p> For a trial of size N=", formatSize, " with a binary outcome where
                 the probability of response to first-stage intervention is ",
                 formatResp, sentenceCompilerA(),", we have at least ", 
                 formatPower, " power. </p>", sep = ""))
    })
    
    ## Sample size computation and output for continuous outcome
    output$continuousSampleSizeA <- renderUI({
      validate(
        need(input$inputPowerA > 0, text.power0),
        need(input$inputPowerA < 1, text.power100),
        need(dataCompilerA()   > 0, text.invalidEffectSize) %then%
          need(is.numeric(rawSampleSize), text.lowEffectSizeA)
      )
      
      alt.hyp       <- switch(input$selectAlternativeA,
                              "one.sided" = "greater")
      designEffect  <- 4
      
      rawSampleSize <- try(pwr.norm.test(d = dataCompilerA(),
                                         sig.level = input$alphaA,
                                         power = input$inputPowerA,
                                         alternative = alt.hyp)$n,
                           silent = T)
      
      finalSampleSize <- ceiling(2 * designEffect * rawSampleSize)
      formatPower <- paste(input$inputPowerA * 100, "%" , sep = "")
      formatAlpha <- paste(input$alphaA      * 100, "%" , sep = "")
      formatAltHyp <- switch(input$selectAlternativeA, 
                             "one.sided" = "one-sided ",
                             "two.sided" = "two-sided ")
      formatResp <- as.numeric(input$respA)
      
      HTML(paste("<h4 style='color:blue';> N=", paste(finalSampleSize), " </h4>
                 <p> We wish to find the sample size for a trial with a continuous
                 outcome where the probability of response to first-stage 
                 intervention is ", formatResp, sentenceCompilerA(),
                 ". Given a ", formatAltHyp, " test with ", formatAlpha,
                 " type-I error, we require a sample size of at least ",
                 finalSampleSize, " to make this comparison with ", formatPower,
                 " power. </p>", sep = ""))
    })
    
    output$continuousPowerA <- renderUI({
      validate(
        need(input$inputSampleSizeA != 0, text.size0) %then%
          need(size >= 1, text.lowSizeA)
      )
      
      alt.hyp <- switch(input$selectAlternativeA,
                        'one.sided' = 'greater')
      designEffect <- 4
      size <- (input$inputSampleSizeA/(2*designEffect))
      
      finalPower  <- round(try(pwr.norm.test(d = dataCompilerA(), 
                                             sig.level = input$alphaA,
                                             n = size,
                                             alternative = alt.hyp)$power,
                               silent = T), digits = 3)
      formatPower <- paste(finalPower   * 100, "%", sep = "")
      formatAlpha <- paste(input$alphaA * 100, "%", sep = "")
      formatSize  <- as.numeric(input$inputSampleSizeA)
      formatResp  <- as.numeric(input$respA)
      
      
      HTML(paste("<h4 style='color:blue';> Power=", paste(formatPower)," </h4>
                 <p> For a trial of size N=", formatSize, " with a continuous outcome
                 where the probability of response to first-stage intervention
                 is ", formatResp, sentenceCompilerA(), ", we have ",
                 formatPower, " power. </p>", sep = ""))
    })
    
    
    
    ##### DESIGN 2 #####
    
    ##### Design 2 Header #####
    
    ### Render the design image which highlights selected DTRs
    ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
    ### Note that this requires a specific naming convention for image assets
    ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0.
    
    output$designBimg <- renderImage({
      filename <- normalizePath(file.path('./www/images', paste('SMARTdesignB_',input$firstDTRcompareB,'_',input$secondDTRcompareB,'.gif',sep='')))
      list(src  = filename)
    }, deleteFile = FALSE)
    
    
    ##### Design 2 Modules #####
    
    # Backend for outcome selection radioButtons
    design2.outcome <- callModule(selectDTROutcome, "design2.outcome")
    output$design2outcome <- renderText(design2.outcome())
    outputOptions(output, 'design2outcome', suspendWhenHidden = FALSE)
    
    # Backend for result options (alpha, desired power/sample size, etc.)
    design2.resultOptions <- callModule(resultOptions, "design2.resultOptions")
    output$design2resultOptions <- renderText(unlist(design2.resultOptions()))
    outputOptions(output, 'design2resultOptions', suspendWhenHidden = FALSE)
    
    # Backend for selection of primary comparison for sample size calculation
    design2.primaryAim <- callModule(primaryAim, "design2.primaryAim",
                                     rerand = reactive("nonresponders"))
    output$design2primaryAim <- renderText(design2.primaryAim())
    outputOptions(output, 'design2primaryAim', suspendWhenHidden = FALSE)
    
    ### Render 'selectize' dropdown boxes with placeholder text for AI selection.
    ### Second DTR input populates with all DTRs that are NOT the first DTR--eliminates ability to select same DTR twice
    ### Placed in server.R rather than ui.R because of dependency on first DTR selection
    
    output$selectAI1B <- renderUI({
      AI <- selectizeInput("firstDTRcompareB", label = "Reference Adaptive Intevention",
                           choices = designB.AIs,
                           options = list(
                             placeholder = text.refDTRPlaceholder,
                             onInitialize = I('function() { this.setValue(0); }')
                           )
      )
      return(AI)
    })
    
    output$selectAI2B <- renderUI({
      AI <- selectizeInput("secondDTRcompareB",label = "Comparison Adaptive Intervention",
                           choices = designB.AIs[substr(designB.AIs, 1, 1) != substr(input$firstDTRcompareB, 1, 1)],
                           options = list(
                             placeholder = text.compDTRPlaceholder,
                             onInitialize = I('function() { this.setValue(""); }')
                           )
      )
      return(AI)
    })
    
    ##### DESIGN B PROBABILITY INPUT #####
    
    ### Read in DTR names from dropdowns and parse them to determine first and second stage treatments
    ### Reactive function allows on-the-fly changes in return values with changes in selection
    ### Outputs full DTR name, first-stage treatment (first character), second-stage treatment (last character)
    ### NOTE that these positions are exclusive to design B because responders share the same second-stage treatment
    
    substringDTR1B <- reactive({
      if (length(input$firstDTRcompareB) > 0) {
        DTR1 <- paste(input$firstDTRcompareB)
        firstStage1    <- substr(DTR1, 1, 1)
        secondStageR1  <- substr(DTR1, 3, 3)
        secondStageNR1 <- substr(DTR1, 6, 6)
        return(c(DTR1, firstStage1, secondStageR1, secondStageNR1))
      }
      else{
        return(c(0,0,0))
      }
    })
    
    substringDTR2B <- reactive({
      if (length(input$secondDTRcompareB) > 0) {
        DTR2 <- paste(input$secondDTRcompareB)
        firstStage2    <- substr(DTR2,1,1)
        secondStageR2  <- substr(DTR2,3,3)
        secondStageNR2 <- substr(DTR2,6,6)
        return(c(DTR2, firstStage2, secondStageR2, secondStageNR2))
      }
      else{
        return(c(0,0,0))
      }
    })
    
    #When a first DTR is selected, render an input box corresponding to whatever input method is selected.
    # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR1), enabled/disabled depending on cellOrConditionalB
    # For target-difference or OR, relevant numericInputs are rendered
    # This is the ONLY location in which difference and OR numericInputs are built.
    
    generateBinaryInputs1B <- reactive({
      validate(
        need(input$firstDTRcompareB, text.refDTRPlaceholder)
      )
      if (input$cellOrConditionalB == TRUE) {
        return(disable(numericInput("DTRsuccB1disable",
                                    label = HTML("Probability of Success for Reference AI &nbsp; <img src='images/blue_dash.gif'>"),
                                    value = NULL, min = 0, max = 1, step = 0.01)))
      } else
        return(list(numericInput("DTRsuccB1",
                                 label = HTML("Probability of Success for Reference AI &nbsp; <img src='images/blue_dash.gif'>"),
                                 value = NULL, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "DTRsuccB1", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                              placement = "right", trigger = "focus"))
        )
    })
    
    generateBinaryInputs2B <- reactive({
      validate(
        need(input$secondDTRcompareB != "", "Please select a Comparison AI.")
      )
      if(input$targetDiffCheckB == FALSE && input$targetOddsCheckB == FALSE){
        if(input$cellOrConditionalB == TRUE){
          return(disable(numericInput("DTRsuccB2disable",
                                      label = HTML("Probability of Success for Comparison AI &nbsp; <img src='images/red_dash.gif'>"),
                                      value = NULL, min = 0, max = 1, step = 0.01)))
        } else
          return(list(numericInput("DTRsuccB2",
                                   label = HTML("Probability of Success for Comparison AI &nbsp; <img src='images/red_dash.gif'>"),
                                   value = NA, min = 0, max = 1, step = 0.01),
                      bsTooltip(id = "DTRsuccB2",
                                title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                                placement = "right", trigger = "focus")
          ))
      }
      
      if(input$targetDiffCheckB == TRUE && input$targetOddsCheckB == FALSE){
        return(list(numericInput("targetDiffB", label = "Target Difference in Success Probabilities",
                                 value = NULL, min = 0.01, max = 0.99, step = 0.01),
                    bsTooltip("targetDiffB", title = "Input must be in decimal form, up to two places.",
                              placement = "right", trigger = "focus"),
                    radioButtons("diffDirectionB", label = "Is the probability of the Comparison AI smaller or larger than the probability of the Reference AI?",
                                 choices = list("Smaller" = -1, "Larger" = 1), selected = -1)
        ))
      }
      
      if(input$targetOddsCheckB == TRUE){
        return(list(numericInput("targetORB", label = "Target Odds Ratio of Success",
                                 value = NULL, min = 0, step = 0.01),
                    bsTooltip(id = "targetORB", title = "Input must be positive and in decimal form, up to two places.",
                              placement = "right", trigger = "focus")
        ))
      }
    })
    
    
    ### Render UI components generated above
    
    output$binaryDTR1probB <- renderUI({
      generateBinaryInputs1B()
    })
    
    
    output$binaryDTR2probB <- renderUI({
      generateBinaryInputs2B()
    })
    
    ### For cell-specific probabilities, render a series of numericInputs labeled by information from DTR substrings
    ### When DTR1 and DTR2 begin with the same treatment, P(S|stage1trt,r) is rendered only once, in output$cellProbsDTR1B
    
    output$cellProbsDTR1B <- renderUI({
      if(substringDTR1B()[1] != substringDTR2B()[1]){
        controlInputs <- list(numericInput("marginalFirstStageB1",
                                           label = paste("Probability of success for Path ", substringDTR1B()[2], "r", substringDTR1B()[3], sep = ""),
                                           value = NULL, min = 0, max = 1, step = 0.01),
                              numericInput("marginalSecondStageNRB1",
                                           label = paste("Probability of success for Path ",substringDTR1B()[2], "nr", substringDTR1B()[4], sep = ""),
                                           value = NULL, min = 0, max = 1, step = 0.01),
                              bsTooltip(id = "marginalFirstStageB1",    title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                                        placement = "right", trigger = "focus"),
                              bsTooltip(id = "marginalSecondStageNRB1", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                                        placement = "right", trigger = "focus")
        )
      }
      
    })
    
    output$cellProbsDTR2B <- renderUI({
      list(
        numericInput("marginalFirstStageB2",    label = paste("Probability of success for Path ", substringDTR2B()[2], "r", substringDTR2B()[3], sep = ""),
                     value = NULL, min = 0, max = 1, step = 0.01),
        numericInput("marginalSecondStageNRB2", label = paste("Probability of success for Path ", substringDTR2B()[2], "nr", substringDTR2B()[4], sep = ""),
                     value = NULL, min = 0, max = 1, step = 0.01),
        bsTooltip(id = "marginalFirstStageB2",    title = text.tooltip,
                  placement = "right", trigger = "focus"),
        bsTooltip(id = "marginalSecondStageNRB2", title = text.tooltip,
                  placement = "right", trigger = "focus"))
    })
    
    # Render enabled/disabled numericInputs when outcome is continuous
    
    generateContinuousInputB <- reactive({
      validate(
        need(input$firstDTRcompareB,  text.refDTRPlaceholder),
        need(input$secondDTRcompareB, text.compDTRPlaceholder)
      )
      return(list(numericInput( "effectSizeB", label = "Standardized Effect Size", value = 0, min = 0, max = 10, step = 0.01),
                  bsTooltip(id = "effectSizeB", title = "Input can range from 0-10 and must be in decimal form, up to two places.",
                            placement = "right", trigger = "focus"))
      )
    })
    
    output$continuousProbB <- renderUI({
      generateContinuousInputB()
    })
    
    
    ##### DESIGN B OBSERVERS #####
    
    ### For binary outcome, disallow cell-specific input if target difference input is selected
    ### If target difference is not selected, also deselect target OR
    
    observe({
      if (input$targetDiffCheckB) {
        updateCheckboxInput(session, "cellOrConditionalB", value = FALSE)
        updateCheckboxInput(session, "targetOddsCheckB",   value = FALSE)
      }
    })
    
    observe({
      if (input$targetOddsCheckB) {
        updateCheckboxInput(session, "cellOrConditionalB", value = FALSE)
        updateCheckboxInput(session, "targetDiffCheckB",   value = FALSE)
      }
    })
    
    ### If cell-specific input is selected, deselect target difference and OR options
    ### Update disabled full DTR success inputs with computed probabilities
    
    observe({
      if (input$cellOrConditionalB == TRUE) {
        updateCheckboxInput(session, "targetDiffCheckB", value = FALSE)
        updateCheckboxInput(session, "targetOddsCheckB", value = FALSE)
        
        updateNumericInput(session, "DTRsuccB1disable", value = generateProbsB()[1])
        updateNumericInput(session, "DTRsuccB2disable", value = generateProbsB()[2])
      }
    }, priority = 2)
    
    ### Compute full DTR probabilities when providing cell-specific probabilities
    
    generateProbsB <- reactive({
      if (input$selectOutcomeB == 1 && input$cellOrConditionalB == TRUE){
        pDTR1 <- fullDTRprob(input$marginalFirstStageB1, input$respB,input$marginalSecondStageNRB1)
        pDTR2 <- fullDTRprob(input$marginalFirstStageB2, input$respB,input$marginalSecondStageNRB2)
      }
      else if (input$selectOutcomeB == 1 && input$cellOrConditionalB == FALSE){
        # If not receiving cell-specific probabilities, pass DTR success probabilities
        pDTR1 <- input$DTRsuccB1
        pDTR2 <- input$DTRsuccB2
      }
      else {
        pDTR1 <- 0
        pDTR2 <- 0
      }
      
      return(c(pDTR1, pDTR2))
    })
    
    ##### DESIGN B RESULT BACKEND #####
    
    # Based on provided input probabilities and selected options, compute appropriate arguments to pass to power.prop.test or pwr.norm.test
    
    checkDTRinputsB <- reactive({
      return(as.logical((input$DTRsuccB1 > 0) & (input$DTRsuccB1 < 1) & (input$DTRsuccB2 > 0) & (input$DTRsuccB2 < 1)))
    })
    
    dataCompilerB <- reactive({
      
      validate(
        need(!(is.na(input$respB)), "Please provide a response probability. If unknown, enter 0 for a conservative estimate of power or sample size.") %then%
          need(0 <= input$respB && input$respB <= 1, "The provided response probability is not a valid probability. Please enter a value between 0 and 1.")
      )
      
      # Binary outcome, provided full-DTR probabilities
      if (input$selectOutcomeB == 1 && input$cellOrConditionalB == FALSE && input$targetDiffCheckB == FALSE && input$targetOddsCheckB == FALSE) {
        validate(
          need(!is.null(input$DTRsuccB1), "Select a Reference AI above."),
          need(!is.null(input$DTRsuccB2), "Select a Comparison AI above.") %then%
            need(!is.null(input$DTRsuccB1) && !is.null(input$DTRsuccB2), "The success probability is missing for at least one AI. Please provide a numeric input.") %then%
            need(input$DTRsuccB1 != input$DTRsuccB2, "Please provide unique success probabilities for each AI. Sample size is indeterminate for equal AI probabilities.") %then%
            need(checkDTRinputsB(), "The provided success probability for at least one AI is not a valid probability. Please enter a value between 0 and 1.")
        )
        return(c(input$DTRsuccB1, input$DTRsuccB2))
      }
      
      # Binary outcome, cell-specific probabilities
      if (input$selectOutcomeB == 1 && input$cellOrConditionalB == TRUE) {
        validate(
          need(generateProbsB()[1] != generateProbsB()[2], "The provided cell-specific probabilities yield identical AI-specific probabilities of success.
               Sample size is indeterminate for equal AI probabilities. Please adjust your inputs.")
          )
        return(c(generateProbsB()[1], generateProbsB()[2]))
      }
      
      # Binary outcome, target difference in full-DTR probabilities
      if (input$selectOutcomeB == 1 && input$targetDiffCheckB == TRUE) {
        prob2 <- input$DTRsuccB1 + as.numeric(input$diffDirectionB) * input$targetDiffB
        validate(
          need(0 < prob2 && prob2 < 1, "Target difference must be less than 0.5 to be valid input"),
          need(input$targetDiffB > 0, "Target difference must be greater than 0. Sample size is indeterminate for equal AI probabilities.
               Please adjust your inputs.")
          )
        return(c(input$DTRsuccB1, input$DTRsuccB1 + input$diffDirectionB * input$targetDiffB))
      }
      
      # Binary outcome, target odds-ratio relative to reference DTR
      if (input$selectOutcomeB == 1 && input$targetOddsCheckB == TRUE) {
        validate(
          need(is.numeric(input$targetORB), "Please enter an odds ratio.") %then%
            need(input$targetORB != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio.") %then%
            need(input$targetORB != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
        )
        q <- (input$DTRsuccB1 / (1 - input$DTRsuccB1)) * input$targetORB
        return(c(input$DTRsuccB1, q / (1 + q)))
      }
      
      if(input$selectOutcomeB==2){
        validate(
          need(input$firstDTRcompareB, "Select a Reference AI above."),
          need(input$secondDTRcompareB, "Select a Comparison AI above.") %then%
            need(input$effectSizeB != 0, "Sample size is indeterminate for an effect size of 0. Please enter a different target effect size.")
        )
        return(input$effectSizeB)
      }
    })
    
    sentenceCompilerB <- reactive({
      if(input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE && input$targetDiffCheckB==FALSE && input$targetOddsCheckB==FALSE){
        str <- paste(" and the overall probabilities of success in the two AIs of interest, ",input$firstDTRcompareB," and ",input$secondDTRcompareB,
                     ", are ",input$DTRsuccB1," and ",input$DTRsuccB2,", respectively", sep="")
        return(str)
      }
      if(input$selectOutcomeB==1 && input$cellOrConditionalB==TRUE && input$targetDiffCheckB==FALSE && input$targetOddsCheckB==FALSE){
        str <- paste(" and the overall probabilities of success in the two AIs of interest, ",input$firstDTRcompareB," and ",input$secondDTRcompareB,
                     ", are ",generateProbsB()[1]," and ",generateProbsB()[2],", respectively", sep="")
        return(str)
      }
      
      if(input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE && input$targetDiffCheckB==TRUE && input$targetOddsCheckB==FALSE){
        str <- paste(" and the difference in overall probabilities of success in the two AIs of interest, ",input$firstDTRcompareB," and ",input$secondDTRcompareB,
                     ", is ",input$targetDiffB, sep="")
        return(str)
      }
      
      if(input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE && input$targetDiffCheckB==FALSE && input$targetOddsCheckB==TRUE){
        str <- paste(" and the odds ratio of success for the two AIs of interest, ",input$firstDTRcompareB," and ",input$secondDTRcompareB,
                     ", is ",input$targetORB, sep="")
        return(str)
      }
      
      if(input$selectOutcomeB==2){
        str <- paste(" and the standardized effect size between the two AIs of interest, ",input$firstDTRcompareB," and ",input$secondDTRcompareB,
                     ", is ", input$effectSizeB, sep="")
        return(str)
      }
    })
    
    ##### DESIGN B RESULTS #####
    
    # Pass arguments from dataCompilerB() to appropriate R function; extract and render relevant output
    
    output$binarySampleSizeB <- renderUI({
      validate(
        need(input$inputPowerB > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
        need(input$inputPowerB < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1.")
      )
      
      A <- dataCompilerB()[1] / (1 - dataCompilerB()[1])
      B <- (dataCompilerB()[2] / (1 - dataCompilerB()[2])) / A
      
      designEffect    <- 2 * (1 - input$respB) + input$respB
      rawSampleSize   <- 2 * ((qnorm(1 - input$inputPowerB) + qnorm(input$alphaB / 2)) ^ 2) * (((1 + A) ^ 2) * B + ((1 + A * B) ^ 2)) / (A * B * (log(B)) ^ 2)
      finalSampleSize <- ceiling(designEffect * rawSampleSize)
      formatPower     <- paste(input$inputPowerB * 100, "%", sep = "")
      formatAlpha     <- paste(input$alphaB      * 100, "%", sep = "")
      formatResp      <- paste(input$respB       * 100, "%", sep = "")
      formatAltHyp    <- switch(input$selectAlternativeB, "one.sided" = "one-sided ", "two.sided" = "two-sided ")
      
      HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize),"</h4>
                 <p> We wish to find the sample size for a trial with a binary outcome where the probability of response to first-stage interventions is ",formatResp, sentenceCompilerB(),
                 ". Given a ",formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
                 finalSampleSize, " to make this comparison with ",formatPower," power. </p>",sep=""))
    })
    
    output$binaryPowerB <- renderUI({
      validate(
        need(input$inputSampleSizeB != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")
      )
      
      A <- dataCompilerB()[1] / (1 - dataCompilerB()[1])
      B <- (dataCompilerB()[2] / (1 - dataCompilerB()[2])) / A
      
      designEffect  <- 2 * (1 - input$respB) + input$respB
      powerQuantile <- sqrt((input$inputSampleSizeB * A * B * log(B) ^ 2) / (2 * designEffect * ((1 + A) ^ 2 * B + (1 + A*B) ^ 2))) + qnorm(input$alphaB / 2)
      finalPower    <- round(pnorm(powerQuantile), digits = 3)
      formatPower   <- paste(finalPower   * 100, "%", sep = "")
      formatAlpha   <- paste(input$alphaB *100,  "%", sep = "")
      formatResp    <- paste(input$respB  * 100, "%", sep = "")
      formatSize    <- as.numeric(input$inputSampleSizeB)
      
      validate(
        need(size >= 1, paste("The provided sample size is not large enough to yield a trial in which at least one person is consistent with each DTR.",
                              "Sample size must be at least",ceiling(2*designEffect),"to proceed."))
      )
      
      HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
                 <p> For a trial of size N=",formatSize," with a binary outcome where the probability of response to first-stage interventions is ",formatResp,
                 sentenceCompilerB(),", we have ",formatPower," power. ",sep=""))
    })
    
    output$continuousSampleSizeB <- renderUI({
      alt.hyp       <- switch(input$selectAlternativeB, "one.sided" = "greater")
      designEffect  <- 2 * (1 - input$respB) + input$respB
      rawSampleSize <- try(pwr.norm.test(d = dataCompilerB(), sig.level = input$alphaB, power = input$inputPowerB, alternative = alt.hyp)$n, silent = T)
      
      validate(
        need(input$inputPowerB > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
        need(input$inputPowerB < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1."),
        need(dataCompilerB()>0,"Sample size is indeterminate for an effect size of 0. Please specify a different effect size.") %then%
          need(is.numeric(rawSampleSize),paste("Given the provided effect size, fewer than",2*designEffect,"individuals are required to achieve the desired power.",
                                               "This is not enough individuals to run a SMART. You can test for a smaller effect size, or increase the desired power."))
      )
      
      finalSampleSize <- ceiling(2 * designEffect * rawSampleSize)
      formatPower     <- paste(input$inputPowerB * 100, "%", sep = "")
      formatAlpha     <- paste(input$alphaB * 100, "%",sep = "")
      formatAltHyp    <- switch(input$selectAlternativeB, "one.sided" = "one-sided ", "two.sided"="two-sided ")
      formatResp      <- as.numeric(input$respB)
      
      HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize),"</h4>
                 <p> We wish to find the sample size for a trial with a continuous outcome where the probability of response to first-stage interventions is ", formatResp, sentenceCompilerB(),
                 ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
                 finalSampleSize, " to make this comparison with ",formatPower," power. </p>",sep=""))
    })
    
    output$continuousPowerB <- renderUI({
      alt.hyp<-switch(input$selectAlternativeB,"one.sided"="greater")
      validate(
        need(input$inputSampleSizeB != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")
      )
      designEffect <- 2 * (1 - input$respB) + input$respB
      size         <- (input$inputSampleSizeB/(2*designEffect))
      
      validate(
        need(size >= 1, paste("The provided sample size is not large enough to yield a trial in which at least one person is consistent with each DTR.",
                              "Sample size must be at least",ceiling(2*designEffect),"to proceed."))
      )
      
      finalPower<-round(pwr.norm.test(d=dataCompilerB(),sig.level=input$alphaB,n=size,alternative=alt.hyp)$power,digits=3)
      formatPower<-paste(finalPower*100,"%",sep="")
      formatAlpha<-paste(input$alphaB*100,"%",sep="")
      formatSize<-as.numeric(input$inputSampleSizeB)
      formatResp<-as.numeric(input$respB)
      
      HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
                 <p> For a trial of size N=",formatSize," with a continuous outcome where the probability of response to first-stage interventions is ",formatResp,
                 sentenceCompilerB(),", we have ",formatPower," power.</p>",sep=""))
    })
    
    ##### DESIGN C #####
    
    ##### C HEADER #####
    
    ### Render the design image which highlights selected DTRs
    ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
    ### Note that this requires a very specific naming convention for image assets
    ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is empty.
    
    output$designCimg <- renderImage({
      filename<-filename<-normalizePath(file.path('./www/images',paste('SMARTdesignC_',input$firstDTRcompareC,'_',input$secondDTRcompareC,'.gif',sep='')))
      list(src=filename)
    },deleteFile=FALSE)
    
    ### Render 'selectize' dropdown boxes with placeholder text for AI selection.
    ### Second DTR input populates with all DTRs that are NOT the first DTR--eliminates ability to select same DTR twice
    ### Placed in server.R rather than ui.R because of dependency on first DTR selection
    
    output$selectAI1C <- renderUI({
      AI <- selectizeInput("firstDTRcompareC",label="Compare AI",
                           choices=designC.AIs,
                           options = list(
                             placeholder = 'Please select a Reference AI.',
                             onInitialize = I('function() { this.setValue(0); }')
                           )
      )
      return(AI)
    })
    
    output$selectAI2C <- renderUI({
      AI <- selectizeInput("secondDTRcompareC",label="to AI",
                           choices = designC.AIs[substr(designC.AIs, 1, 1) != substr(input$firstDTRcompareC, 1, 1)],
                           options = list(
                             placeholder = 'Please select a Comparison AI.',
                             onInitialize = I('function() { this.setValue(""); }')
                           )
      )
      return(AI)
    })
    
    ##### DESIGN C PROBABILITY INPUT #####
    
    ### Read in DTR names from dropdowns and parse them to determine first and second stage treatments
    ### Reactive function allows on-the-fly changes in return values with changes in selection
    ### Outputs full DTR name, first-stage treatment (first character), second-stage treatment if response (third character),
    ### and second-stage treatment if non-response(last character)
    ### NOTE that these positions are exclusive to design A because responders have two second-stage treatment options
    
    substringDTR1C <- reactive({
      if(length(input$firstDTRcompareC) > 0){
        DTR1           <- paste(input$firstDTRcompareC)
        firstStage1    <- substr(DTR1, 1, 1)
        secondStageR1  <- substr(DTR1, 3, 3)
        secondStageNR1 <- substr(DTR1, 6, 6)
        return(c(DTR1, firstStage1, secondStageR1, secondStageNR1))
      }
      else{
        return(c(0, 0, 0))
      }
    })
    
    substringDTR2C <- reactive({
      if(length(input$secondDTRcompareC) > 0){
        DTR2           <- paste(input$secondDTRcompareC)
        firstStage2    <- substr(DTR2, 1, 1)
        secondStageR2  <- substr(DTR2, 3, 3)
        secondStageNR2 <- substr(DTR2, 6, 6)
        return(c(DTR2, firstStage2, secondStageR2, secondStageNR2))
      }
      else{
        return(c(0, 0, 0))
      }
    })
    
    #When a first DTR is selected, render an input box corresponding to whatever input method is selected.
    # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR1), enabled/disabled depending on cellOrConditionalB
    # For target-difference or OR, relevant numericInputs are rendered
    # This is the ONLY location in which difference and OR numericInputs are built.
    
    generateBinaryInputs1C <- reactive({
      validate(
        need(input$firstDTRcompareC, "Please select a Reference AI.")
      )
      if (input$cellOrConditionalC == TRUE) {
        return(disable(numericInput("DTRsuccC1disable",
                                    label = HTML("Probability of Success for Reference AI &nbsp; <img src='images/blue_dash.gif'>"),
                                    value = NULL, min = 0, max = 1, step= 0.01)))
      } else
        return(list(numericInput("DTRsuccC1",
                                 label=HTML("Probability of Success for Reference AI &nbsp; <img src='images/blue_dash.gif'>"),
                                 value = NA, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "DTRsuccC1", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                              placement = "right", trigger = "focus"))
        )
    })
    
    # When a second DTR is selected, render an input box corresponding to whatever input method is selected.
    # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR2), enabled/disabled depending on cellOrConditionalB
    # For target-difference or OR, numericInputs are NOT rendered (those are handled in output$binaryDTR1probB)
    
    generateBinaryInputs2C <- reactive({
      validate(
        need(input$secondDTRcompareC, "Please select a Comparison AI.")
      )
      if (input$targetDiffCheckC == FALSE && input$targetOddsCheckC == FALSE) {
        if (input$cellOrConditionalC == TRUE) {
          return(disable(numericInput("DTRsuccC2disable",
                                      label = HTML("Probability of Success for Second AI &nbsp; <img src='images/red_dash.gif'>"),
                                      value = NA, min = 0, max = 1, step = 0.01)))
        } else
          return(list(numericInput("DTRsuccC2",
                                   label = HTML("Probability of Success for Second AI &nbsp; <img src='images/red_dash.gif'>"),
                                   value = NA, min = 0, max = 1, step = 0.01),
                      bsTooltip(id = "DTRsuccC2", title="Input can range from 0-1 and must be in decimal form, up to two places.",
                                placement = "right", trigger = "focus")
          ))
      }
      if (input$targetDiffCheckC == TRUE && input$targetOddsCheckC == FALSE && substringDTR2C()[1] != 0) {
        return(list(numericInput("targetDiffC",label="Target Difference in Success Probabilities",
                                 value = NA, min = 0.01, max = 0.99, step = 0.01),
                    bsTooltip(id = "targetDiffC", title = "Input must be in decimal form, up to two places.",
                              placement = "right", trigger = "focus"),
                    radioButtons("diffDirectionC", label = "Is the probability of the Comparison AI smaller or larger than the probability of the Referecen AI?",
                                 choices = list("Smaller" = -1, "Larger" = 1), selected = -1)
        ))
      }
      if (input$targetOddsCheckC == TRUE && substringDTR2C()[1] != 0) {
        return(list(numericInput("targetORC", label = "Target Odds Ratio of Success",
                                 value = NA, min = 0, step = 0.01),
                    bsTooltip(id = "targetORC", title = "Input must be positive and in decimal form, up to two places.",
                              placement = "right", trigger = "focus"))
        )
      }
    })
    
    ### Render UI components generated above
    
    output$binaryDTR1probC <- renderUI({
      generateBinaryInputs1C()
    })
    
    output$binaryDTR2probC <- renderUI({
      generateBinaryInputs2C()
    })
    
    ### For cell-specific probabilities, render a series of numericInputs labeled by information from DTR substrings
    ### When DTR1 and DTR2 begin with the same treatment, P(S|stage1trt,r) is rendered only once, in output$cellProbsDTR1B
    
    output$cellProbsDTR1C <- renderUI({
      controlInputs <- list(numericInput("marginalFirstStageC1",
                                         label = paste("Probability of success for Path ", substringDTR1C()[2], "r", substringDTR1C()[3], sep = ""),
                                         value = NA, min = 0, max = 1, step = 0.01),
                            numericInput("marginalSecondStageNRC1", label = paste("Probability of success for Path ", substringDTR1C()[2], "nr", substringDTR1C()[4], sep = ""),
                                         value = NA, min = 0, max = 1, step = 0.01),
                            bsTooltip(id = "marginalFirstStageC1",    title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                                      placement = "right", trigger = "focus"),
                            bsTooltip(id = "marginalSecondStageNRC1", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                                      placement = "right", trigger = "focus")
      )
    })
    
    output$cellProbsDTR2C <- renderUI({
      return(list(
        numericInput("marginalFirstStageC2",    label = paste("Probability of success for Path ",substringDTR2C()[2],  "r", substringDTR2C()[3],sep = ""),
                     value = NA, min = 0, max = 1, step = 0.01),
        numericInput("marginalSecondStageNRC2", label = paste("Probability of success for Path ",substringDTR2C()[2], "nr", substringDTR2C()[4], sep = ""),
                     value = NA, min = 0, max = 1, step = 0.01),
        bsTooltip(id="marginalFirstStageC2",    title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                  placement = "right", trigger = "focus"),
        bsTooltip(id="marginalSecondStageNRC2", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                  placement = "right", trigger = "focus")
      ))
    })
    
    ### Render numericInput for effect size when outcome is continuous
    
    generateContinuousInputC <- reactive({
      validate(
        need(input$firstDTRcompareC,  "Please select a Reference AI."),
        need(input$secondDTRcompareC, "Please select a Comparison AI.")
      )
      return(c(numericInput("effectSizeC", label = "Standardized Effect Size",
                            value = NA, min = 0, max = 10, step = 0.01),
               bsTooltip(id = "effectSizeC", title = "Input can range from 0-10 and must be in decimal form, up to two places.",
                         placement = "right", trigger = "focus"))
      )
    })
    
    output$continuousProbC <- renderUI({
      generateContinuousInputC()
    })
    
    
    ##### DESIGN C OBSERVERS #####
    
    ### For binary outcome, disallow cell-specific input if target difference input is selected
    ### If target difference is not selected, also deselect target OR
    
    observe({
      if (input$targetOddsCheckC) {
        updateCheckboxInput(session, "cellOrConditionalC", value = FALSE)
        updateCheckboxInput(session, "targetDiffCheckC",   value = FALSE)
      }
    })
    
    observe({
      if (input$targetDiffCheckC) {
        updateCheckboxInput(session, "targetOddsCheckC",   value = FALSE)
        updateCheckboxInput(session, "cellOrConditionalC", value = FALSE)
      }
    })
    
    ### If cell-specific input is selected, deselect target difference and OR options
    ### Update disabled full DTR success inputs with computed probabilities
    
    observe({
      if (input$cellOrConditionalC==TRUE) {
        updateCheckboxInput(session, "targetDiffCheckC", value = FALSE)
        updateCheckboxInput(session, "targetOddsCheckC", value = FALSE)
        
        updateNumericInput(session, "DTRsuccC1disable", value = generateProbsC()[1])
        updateNumericInput(session, "DTRsuccC2disable", value = generateProbsC()[2])
      }
    }, priority = 2)
    
    
    ### Compute full DTR probabilities or effect size when providing cell-specific probabilities or mean/SD
    
    generateProbsC <- reactive({
      if (input$selectOutcomeC == 1 && input$cellOrConditionalC == TRUE) {
        pDTR1 <- fullDTRprob(input$marginalFirstStageC1, input$respC, input$marginalSecondStageNRC1)
        pDTR2 <- fullDTRprob(input$marginalFirstStageC2, input$respC, input$marginalSecondStageNRC2)
      }
      else if (input$selectOutcomeC == 1 && input$cellOrConditionalC == FALSE){
        pDTR1 <- input$DTRsuccC1
        pDTR2 <- input$DTRsuccC2
      }
      else {
        pDTR1 <- 0
        pDTR2 <- 0
      }
      return(c(pDTR1, pDTR2))
    })
    
    ##### DESIGN C RESULT BACKEND #####
    
    # Based on provided input probabilities and selected options, compute appropriate arguments to pass to power.prop.test or pwr.norm.test
    
    checkDTRinputsC <- reactive({
      return(as.logical((input$DTRsuccC1 > 0) & (input$DTRsuccC1 < 1) & (input$DTRsuccC2 > 0) & (input$DTRsuccC2 < 1)))
    })
    
    dataCompilerC <- reactive({
      
      validate(
        need(!(is.null(input$respC)), "Please provide a response probability. If unknown, enter 0 for a conservative estimate of power or sample size.") %then%
          need(0 <= input$respC && input$respC <= 1, "The provided response probability is not a valid probability. Please enter a value between 0 and 1.")
      )
      
      # Binary outcome, provided full-DTR probabilities
      if (input$selectOutcomeC == 1 && input$cellOrConditionalC == FALSE && input$targetDiffCheckC == FALSE && input$targetOddsCheckC == FALSE) {
        validate(
          need(!is.null(input$DTRsuccC1), text.refDTRPlaceholder),
          need(!is.null(input$DTRsuccC2), text.compDTRPlaceholder) %then%
            need(!is.na(input$DTRsuccC1) && !is.na(input$DTRsuccC2), "The success probability is missing for at least one AI. Please provide a numeric input.") %then%
            need(input$DTRsuccC1 != input$DTRsuccC2, "Please provide unique success probabilities for each AI. Sample size is indeterminate for equal AI probabilities.") %then%
            need(checkDTRinputsC(), "The provided success probability for at least one AI is not a valid probability. Please enter a value between 0 and 1.")
        )
        return(c(input$DTRsuccC1, input$DTRsuccC2))
      }
      
      # Binary outcome, cell-specific probabilities
      if (input$selectOutcomeC == 1 && input$cellOrConditionalC == TRUE) {
        validate(
          need(generateProbsC()[1] != generateProbsC()[2], "The provided marginal probabilities yield identical overall AI success probabilities. Sample size is indeterminate for equal AI probabilities. Please adjust your inputs.")
        )
        return(c(generateProbsC()[1], generateProbsC()[2]))
      }
      
      # Binary outcome, target difference in full-DTR probabilities
      if (input$selectOutcomeC == 1 && input$targetDiffCheckC == TRUE) {
        prob2 <- input$DTRsuccC1 + as.numeric(input$diffDirectionC) * input$targetDiffC
        validate(
          need(!is.na(input$DTRsuccC1), missingRefProbError),
          need(!is.na(input$targetDiffC), missingDiffError) %then%
            need(0 < prob2 && prob2 < 1, invalidDiffError)
        )
        return(c(input$DTRsuccC1, prob2))
      }
      
      # Binary outcome, target odds ratio relative to reference DTR
      if (input$selectOutcomeC == 1 && input$targetOddsCheckC==TRUE) {
        validate(
          need(is.numeric(input$targetORC),"Please enter an odds ratio.") %then%
            need(input$targetORC != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio.") %then%
            need(input$targetORC != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
        )
        q <- (input$DTRsuccC1 / (1 - input$DTRsuccC1)) * input$targetORC
        return(c(input$DTRsuccC1, q / (1 + q)))
      }
      
      # Continuous outcome
      if (input$selectOutcomeC == 2) {
        validate(
          need(input$firstDTRcompareC, "Select a Reference AI above."),
          need(input$secondDTRcompareC, "Select a Comparison AI above.") %then%
            need(input$effectSizeC != 0, "Sample size is indeterminate for an effect size of 0. Please enter a different target effect size.")
        )
        return(input$effectSizeC)
      }
    })
    
    # Compute the "design effect" for design C. Varies based on whether DTRs are separate- or shared-path
    
    sentenceCompilerC <- reactive({
      if(input$selectOutcomeC==1 && input$cellOrConditionalC==FALSE && input$targetDiffCheckC==FALSE && input$targetOddsCheckC==FALSE){
        str <- paste(" and the overall probabilities of success in the two AIs of interest, ",input$firstDTRcompareC," and ",input$secondDTRcompareC,
                     ", are ",input$DTRsuccC1," and ",input$DTRsuccC2,", respectively", sep="")
        return(str)
      }
      if(input$selectOutcomeC==1 && input$cellOrConditionalC==TRUE){
        str <- paste(" and the overall probabilities of success in the two AIs of interest, ",input$firstDTRcompareC," and ",input$secondDTRcompareC,
                     ", are ",generateProbsC()[1]," and ",generateProbsC()[2],", respectively", sep="")
        return(str)
      }
      
      if(input$selectOutcomeC==1 && input$cellOrConditionalC==FALSE && input$targetDiffCheckC==TRUE && input$targetOddsCheckC==FALSE){
        str <- paste(" and the difference in overall probabilities of success in the two AIs of interest, ",input$firstDTRcompareC," and ",input$secondDTRcompareC,
                     ", is ",input$targetDiffC, sep="")
        return(str)
      }
      
      if(input$selectOutcomeC==1 && input$cellOrConditionalC==FALSE && input$targetOddsCheckC==TRUE && input$targetDiffCheckC==FALSE){
        str <- paste(" and the odds ratio of success for the two AIs of interest, ",input$firstDTRcompareC," and ",input$secondDTRcompareC,
                     ", is ",input$targetORC, sep="")
        return(str)
      }
      
      if(input$selectOutcomeC==2){
        str <- paste(" and the standardized effect size between the two AIs of interest, ",input$firstDTRcompareC," and ",input$secondDTRcompareC,
                     ", is ", input$effectSizeC, sep="")
        return(str)
      }
    })
    
    
    ##### DESIGN C RESULTS #####
    
    # Pass arguments from dataCompilerB() to appropriate R function; extract and render relevant output
    
    output$binarySampleSizeC <- renderUI({
      validate(
        need(input$inputPowerC > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
        need(input$inputPowerC < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1.")
      )
      
      A <- dataCompilerC()[1]  / (1 - dataCompilerC()[1])
      B <- (dataCompilerC()[2] / (1 - dataCompilerC()[2])) / A
      
      designEffect    <- (3/2) * (1 - input$respC) + input$respC
      rawSampleSize   <- 2 * ((qnorm(1 - input$inputPowerC) + qnorm(input$alphaC / 2)) ^ 2) * (((1 + A) ^ 2) * B + ((1 + A * B) ^ 2)) / (A * B * (log(B)) ^ 2)
      finalSampleSize <- ceiling(designEffect * rawSampleSize)
      formatPower     <- paste(input$inputPowerC * 100, "%", sep = "")
      formatAlpha     <- paste(input$alphaC      * 100, "%", sep = "")
      formatResp      <- paste(input$respC       * 100, "%", sep = "")
      formatAltHyp    <- switch(input$selectAlternativeC, "one.sided" = "one-sided ", "two.sided" = "two-sided ")
      
      HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
                 <p> We wish to find the sample size for a trial with a binary outcome where the probability of response to first-stage interventions is ", formatResp, sentenceCompilerC(),
                 ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
                 finalSampleSize, " to make this comparison with at least ",formatPower," power. </p>",sep=""))
    })
    
    output$binaryPowerC <- renderUI({
      validate(
        need(input$inputSampleSizeC != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")
      )
      
      A <- dataCompilerC()[1]  / (1 - dataCompilerC()[1])
      B <- (dataCompilerC()[2] / (1 - dataCompilerC()[2])) / A
      
      designEffect  <- (3/2) * (1 - input$respC) + input$respC
      powerQuantile <- sqrt((input$inputSampleSizeC * A * B * log(B) ^ 2) / (2 * designEffect * ((1 + A) ^ 2 * B + (1 + A*B) ^ 2))) + qnorm(input$alphaC / 2)
      finalPower    <- round(pnorm(powerQuantile), digits = 3)
      formatPower   <- paste(finalPower   * 100, "%", sep = "")
      formatAlpha   <- paste(input$alphaC * 100, "%", sep = "")
      formatResp    <- paste(input$respC  * 100, "%", sep = "")
      formatSize    <-as.numeric(input$inputSampleSizeC)
      
      HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
                 <p> For a trial of size N=",formatSize," with a binary outcome where the probability of response to first-stage interventions is ",formatResp,
                 sentenceCompilerC(),", we have at least ",formatPower," power. </p>",sep=""))
    })
    
    output$continuousSampleSizeC <- renderUI({
      alt.hyp       <- switch(input$selectAlternativeC, "one.sided" = "greater")
      designEffect  <- (3/2) * (1 - input$respC) + input$respC
      rawSampleSize <- try(pwr.norm.test(d = dataCompilerC(), sig.level = input$alphaC, power = input$inputPowerC, alternative = alt.hyp)$n, silent = T)
      
      validate(
        need(input$inputPowerC > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero.") %then%
          need(input$inputPowerC < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1.") %then%
          need(dataCompilerC()   > 0, "Sample size is indeterminate for an effect size of 0. Please specify a different effect size.") %then%
          need(is.numeric(rawSampleSize),paste("Given the provided effect size, fewer than",2*designEffect,"individuals are required to achieve the desired power.",
                                               "This is not enough individuals to run a SMART. You can test for a smaller effect size, or increase the desired power."))
      )
      
      finalSampleSize <- ceiling(2 * designEffect * rawSampleSize)
      formatPower     <- paste(input$inputPowerC * 100, "%", sep = "")
      formatAlpha     <- paste(input$alphaC      * 100, "%", sep = "")
      formatResp      <- paste(input$respC       * 100, "%", sep = "")
      formatAltHyp    <- switch(input$selectAlternativeC, "one.sided" = "one-sided ", "two.sided" = "two-sided ")
      
      HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize),"</h4>
                 <p> We wish to find the sample size for a trial with a continuous outcome where the probability of response to first-stage interventions is ", formatResp, sentenceCompilerC(),
                 ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
                 finalSampleSize, " to make this comparison with at least ", formatPower," power. </p>",sep = ""))
    })
    
    output$continuousPowerC <- renderUI({
      alt.hyp<-switch(input$selectAlternativeC,"one.sided" = "greater")
      validate(
        need(input$inputSampleSizeC != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")
      )
      designEffect <- (3/2) * (1 - input$respC) + input$respC
      size         <- (input$inputSampleSizeC / (2 * designEffect))
      finalPower   <- round(pwr.norm.test(d = dataCompilerC(), sig.level = input$alphaC, n = size, alternative = alt.hyp)$power, digits = 3)
      formatPower  <- paste(finalPower   * 100, "%", sep = "")
      formatAlpha  <- paste(input$alphaC * 100, "%", sep = "")
      formatResp   <- paste(input$respB  * 100, "%", sep = "")
      formatSize   <- as.numeric(input$inputSampleSizeC)
      
      HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
                 <p> For a trial of size N=",formatSize," with a continuous outcome where the probability of response to first-stage interventions is ",formatResp,
                 sentenceCompilerC(),", we have at least ",formatPower," power. </p>",sep=""))
    })
    
    
    ##### Modules #####
    
    ## Backend for outcome selection radioButtons
    dyo.outcome <- callModule(selectDTROutcome, "dyoOutcome")
    output$dyooutcome <- renderText(dyo.outcome())
    outputOptions(output, 'dyooutcome', suspendWhenHidden = FALSE)
    
    ## Backend for result options (alpha, desired power/sample size, etc.)
    dyo.resultOptions <- callModule(resultOptions, "dyo.resultOptions")
    output$dyoresultOptions <- renderText(unlist(dyo.resultOptions()))
    outputOptions(output, 'dyoresultOptions', suspendWhenHidden = FALSE)
    
    ## Backend for selection of primary aim (e.g., pairwise DTR comparison, omnibus DTR, etc.)
    dyo.primaryAim.rerand <- reactive({
      validate(need(input$dyo.rerand.resp == "Yes" | input$dyo.rerand.nresp == "Yes", text.mustRerandomize))
      c("responders", "nonresponders")[c(input$dyo.rerand.resp == "Yes", input$dyo.rerand.nresp == "Yes")]
    })
    primaryAim <- callModule(primaryAim, "dyo.primaryAim", rerand = dyo.primaryAim.rerand)
    output$dyoprimaryAim <- renderText(primaryAim())
    outputOptions(output, 'dyoprimaryAim', suspendWhenHidden = FALSE)
    
    ##### Response Probability UI ####
    output$dyo.stage1.resprobUI <- renderUI({
      if (input$conservative) {
        if (input$dyo.stage1.resprob.eq == "No") {
          lapply(1:input$dyo.stage1.ntxt,
                 function(i) { 
                   disabled(numericInput(paste0("dyo.stage1.", LETTERS[i], ".resprob"),
                                label = paste0("Probability of Response to ", LETTERS[i], "."),
                                min = 0, max = 1, value = NA, step = 0.01))
                 })
        } else {
          disabled(numericInput("dyo.stage1.allTxt.resprob",
                       label = eval(text.responseProbLabel),
                       min = 0, max = 1, value = NA, step = 0.01))
        }
      } else {
        if (input$dyo.stage1.resprob.eq == "No") {
          lapply(1:input$dyo.stage1.ntxt,
                 function(i) { 
                   numericInput(paste0("dyo.stage1.", LETTERS[i], ".resprob"),
                                label = paste0("Probability of Response to ", LETTERS[i], "."),
                                min = 0, max = 1, value = NA, step = 0.01)
                 })
        } else {
          numericInput("dyo.stage1.allTxt.resprob",
                       label = eval(text.responseProbLabel),
                       min = 0, max = 1, value = NA, step = 0.01)
        }
      }
      
    })
    
    ##### Re-Randomization Setup UI #####
    output$dyo.rerand.respUI <- renderUI({
      list(checkboxGroupInput("dyo.rerand.resp.whichtxt",
                              label = "Select first-stage treatments for which responders are re-randomized.",
                              choices = lapply(1:input$dyo.stage1.ntxt, function(i) LETTERS[i]),
                              selected = LETTERS[1:input$dyo.stage1.ntxt], inline = TRUE))
    })
    outputOptions(output, 'dyo.rerand.respUI', suspendWhenHidden = FALSE)
    
    output$dyo.rerand.nrespUI <- renderUI({
      list(checkboxGroupInput("dyo.rerand.nresp.whichtxt",
                              label = "Select first-stage treatments for which non-responders are re-randomized.",
                              choices = lapply(1:input$dyo.stage1.ntxt, function(i) LETTERS[i]),
                              selected = LETTERS[1:input$dyo.stage1.ntxt], inline = TRUE))
    })
    outputOptions(output, 'dyo.rerand.nrespUI', suspendWhenHidden = FALSE)
    
    ##### Randomization Probabilities UI #####
    ### Create inputs for varying randomization probabilites 
    output$dyo.stage1.rprobUI <- renderUI({
      list(lapply(1:input$dyo.stage1.ntxt, 
                  function(i) {
                    numericInput(paste0("dyo.stage1.txt", i, ".rprob"),
                                 label = paste0("Probability of allocation to Treatment ", i), 
                                 min = 0, max = 1, value = NA, step = 0.01)
                  }),
           helpText(paste("The probability of allocation to Treatment", isolate(input$dyo.stage1.ntxt),
                          "will automatically update so that all allocation probabilities sum to 1.")))
    })
    output$dyo.rerand.resp.rprobUI <- renderUI({
      list(lapply(1:input$dyo.rerand.resp.ntxt, 
                  function(i) {
                    numericInput(paste0("dyo.rerand.resp.txt", i, ".rprob"),
                                 label = paste0("Probability of allocation to Treatment ", i), 
                                 min = 0, max = 1, value = NA, step = 0.01)
                  }),
           helpText(paste("The probability of allocation to Treatment", isolate(input$dyo.rerand.resp.ntxt),
                          "will automatically update so that all allocation probabilities sum to 1."))
      )
    })
    output$dyo.rerand.nresp.rprobUI <- renderUI({
      list(lapply(1:(input$dyo.rerand.nresp.ntxt), 
                  function(i) {
                    numericInput(paste0("dyo.rerand.nresp.txt", i, ".rprob"),
                                 label = paste("Probability of allocation to Treatment", i), 
                                 min = 0, max = 1, value = NA, step = 0.01)
                  }),
           helpText(paste("The probability of allocation to Treatment", isolate(input$dyo.rerand.nresp.ntxt),
                          "will automatically update so that all allocation probabilities sum to 1.")))
      
    })
    
    ### Update randomization probabilities for last treatment in the list so everything sums to 1
    ## Compute sums for each randomization
    dyo.stage1.allocProbs.sum <- reactive({
      do.call(sum, lapply(1:(input$dyo.stage1.ntxt - 1), 
                          function(i) input[[paste0('dyo.stage1.txt', i, '.rprob')]]))
    })
    dyo.rerand.resp.allocProbs.sum <- reactive({
      do.call(sum, lapply(1:(input$dyo.rerand.resp.ntxt - 1), 
                          function(i) input[[paste0('dyo.rerand.resp.txt', i, '.rprob')]]))
    })
    dyo.rerand.nresp.allocProbs.sum <- reactive({
      do.call(sum, lapply(1:(input$dyo.rerand.nresp.ntxt - 1), 
                          function(i) input[[paste0('dyo.rerand.nresp.txt', i, '.rprob')]]))
    })
    
    ## Whenever the sum of the first n-1 treatment probabilities changes, update the last probability
    observe({
      updateNumericInput(session, paste0('dyo.stage1.txt', isolate(input$dyo.stage1.ntxt), '.rprob'),
                         value = 1 - dyo.stage1.allocProbs.sum())})
    observe({
      updateNumericInput(session, paste0('dyo.rerand.resp.txt', isolate(input$dyo.rerand.resp.ntxt), '.rprob'),
                         value = 1 - dyo.rerand.resp.allocProbs.sum())})
    observe({
      updateNumericInput(session, paste0('dyo.rerand.nresp.txt', isolate(input$dyo.rerand.nresp.ntxt), '.rprob'),
                         value = 1 - dyo.rerand.nresp.allocProbs.sum())})
    
    ## Disable continue button in second stage randomization when nobody is rerandomized
    observe({
      shinyjs::toggleState("dyo.rerand.continue", input$dyo.rerand.resp == "Yes" | input$dyo.rerand.nresp == "Yes")
    })
    
    dyo.stage1.allocProbs.values <- reactive({
      values <- sapply(1:input$dyo.stage1.ntxt,
                       function(i) {
                         eval(parse(text = paste0("input$dyo.stage1.txt", i, ".rprob")))
                       })
      sapply(values, 
             function(x) {
               ifelse(is.null(x), "", x)
             })
    })
    
    ##### DTR Name Generation #####
    
    ## Goal: Create DTR "names" (i.e., ordered triples identifying embedded DTRs) by 
    ## listing all possible pairs of treatments for responders and non-responders 
    ## starting with the same first-stage treatment
    
    dtr.names <- reactive({
      ## Construct treatment names
      
      stage2r.names <- lapply(1:input$dyo.stage1.ntxt, function(i) {
        if (input$dyo.rerand.resp == "Yes") {
          if (LETTERS[i] %in% input$dyo.rerand.resp.whichtxt) {
            paste0(LETTERS[i], "r", 1:input$dyo.rerand.resp.ntxt)
          } else {
            paste0(LETTERS[i], "r")
          }
        } else {
          paste0(LETTERS[i], "r")
        }
      })
      
      stage2nr.names <- lapply(1:input$dyo.stage1.ntxt, function(i) {
        if (input$dyo.rerand.nresp == "Yes") {
          if (LETTERS[i] %in% input$dyo.rerand.nresp.whichtxt) {
            paste0(LETTERS[i], "nr", 1:input$dyo.rerand.nresp.ntxt)
          } else {
            paste0(LETTERS[i], "nr")
          }
        } else {
          paste0(LETTERS[i], "nr")
        }
      })
      
      ## Generate DTRs with formatting 
      stagenames <- list("r" = stage2r.names, "nr" = stage2nr.names)
      namepairs <- do.call(rbind, lapply(1:input$dyo.stage1.ntxt, function(i) {
        expand.grid("r" = stagenames$r[[i]], "nr" = stagenames$nr[[i]])
      }))
      namepairs <- namepairs[order(namepairs$r), ]
      namepairs <- cbind("s1" = substr(namepairs$r, 1, 1), namepairs)
      paste0("{", namepairs$s1, ", ", namepairs$r, ", ", namepairs$nr, "}")
    })
    
    
    
    
    ##### Binary DTR Probability Inputs #####
    
    ### Generate DTR Selection Inputs
    output$dyo.refdtrSelect <- renderUI({
      selectizeInput("dyoRefDTR",
                     label = text.refDTRLabel,
                     choices = dtr.names(),
                     options = list(
                       placeholder = text.refDTRPlaceholder,
                       onInitialize = I('function() { this.setValue(0); }')
                     )
      )})
    
    output$dyo.compdtrSelect <- renderUI({
      selectizeInput("dyoCompDTR",
                     label = text.compDTRLabel,
                     choices = dtr.names()[substr(dtr.names(), 2, 2) != substr(input$dyoRefDTR, 2, 2)],
                     options = list(
                       placeholder = text.compDTRPlaceholder,
                       onInitialize = I("function() { this.setValue(''); }")
                     )
      )})
    
    ### Parse names of selected DTRs
    refDTR.substr <- reactive({
      if (length(input$dyoRefDTR) > 0) {
        DTR    <- paste(input$dyoRefDTR)
        DTR    <- substr(DTR, 2, nchar(DTR) - 1)
        DTRvec <- unlist(strsplit(DTR, split = ", "))
        return(c(input$dyoRefDTR, DTRvec))
      }
      else return(c(0, 0, 0, 0))
    })
    
    compDTR.substr <- reactive({
      if (length(input$dyoCompDTR) > 0) {
        DTR    <- paste(input$dyoCompDTR)
        DTR    <- substr(DTR, 2, nchar(DTR) - 1)
        DTRvec <- unlist(strsplit(DTR, split = ", "))
        return(c(input$dyoCompDTR, DTRvec))
      }
      else return(c(0, 0, 0, 0))
    })
    
    ## Test proper DTR name parsing
    output$refDTRname <- renderPrint(refDTR.substr())
    output$compDTRname <- renderPrint(compDTR.substr())
    
    ### Generate inputs for marginal success probabilities
    output$binaryRefInput <- renderUI({
      if (isTruthy(primaryAim() == "dtrs")){
        validate(
          need(isTruthy(refDTR.substr()[1]), text.refDTRPlaceholder)
        )
        return(list(numericInput("refDTRProb",
                                 label = html.refDTRSuccess,
                                 value = NA, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "refDTRProb", title = text.tooltip,
                              placement = "right", trigger = "focus")))
      }
    })
    
    output$binaryCompInput <- renderUI({
      if (isTruthy(primaryAim() == "dtrs")){
        validate(
          need(isTruthy(compDTR.substr()[1]), text.compDTRPlaceholder)
        )
        return(list(numericInput("compDTRProb", label = html.compDTRSuccess,
                                 value = NA, min = 0, max = 1, step = 0.01),
                    bsTooltip(id = "compDTRProb", title = text.tooltip,
                              placement = "right", trigger = "focus")))
      }
    })
    
    ### If requested by user, provide inputs for "cell-specific" probabilities, 
    ### i.e., P(success | stage 1 treatment, response)
    output$binaryRefCellProbs <- renderUI({
      req(input$dyoRefDTR)
      validate(
        if (input$dyo.stage1.resprob.eq == "Yes") {
          need(isTruthy(input$dyo.stage1.allTxt.resprob), text.noResponse)
        } else {
          need(isTruthy(input[[paste0("dyo.stage1.", refDTR.substr()[2], ".resprob")]]),
               paste0("Please provide the probability of response to treatment ",
                      refDTR.substr()[2], ". If you are unsure, enter 0 for a conservative estimate."))
        }
      )
      list(numericInput("refDTRProb.responders",
                        label = HTML(paste0("Probability of success for path <strong>",
                                            refDTR.substr()[2], ", ",
                                            refDTR.substr()[3], "</strong>")),
                        value = NULL, min = 0, max = 1, step = 0.01),
           numericInput("refDTRProb.nonresponders",
                        label = HTML(paste0("Probability of success for path <strong>",
                                            refDTR.substr()[2], ", ",
                                            refDTR.substr()[4], "</strong>")),
                        value = NULL, min = 0, max = 1, step = 0.01),
           bsTooltip(id = "refDTRProb.responders",
                     title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                     placement = "right", trigger = "focus"),
           bsTooltip(id = "refDTRProb.nonresponders",
                     title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                     placement = "right", trigger = "focus")
      )
    })
    
    output$binaryCompCellProbs <- renderUI({
      req(input$dyoCompDTR)
      validate(
        if (input$dyo.stage1.resprob.eq == "Yes") {
          need(isTruthy(input$dyo.stage1.allTxt.resprob), text.noResponse)
        } else {
          need(isTruthy(input[[paste0("dyo.stage1.", compDTR.substr()[2], ".resprob")]]),
               paste0("Please provide the probability of response to treatment ",
                      compDTR.substr()[2], ". If you are unsure, enter 0 for a conservative estimate."))
        }
      )
      list(numericInput("compDTRProb.responders",
                        label = HTML(paste0("Probability of success for path <strong>",
                                            compDTR.substr()[2], ", ",
                                            compDTR.substr()[3], "</strong>")),
                        value = NULL, min = 0, max = 1, step = 0.01),
           numericInput("compDTRProb.nonresponders",
                        label = HTML(paste0("Probability of success for path <strong>",
                                            compDTR.substr()[2], ", ",
                                            compDTR.substr()[4], "</strong>")),
                        value = NULL, min = 0, max = 1, step = 0.01),
           bsTooltip(id = "compDTRProb.responders",    
                     title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                     placement = "right", trigger = "focus"),
           bsTooltip(id = "compDTRProb.nonresponders",
                     title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                     placement = "right", trigger = "focus")
      )
    })
    
    ### Disable marginal probability inputs when cell-specific inputs are provided
    observeEvent(input$cellOrMarginal, {
      shinyjs::toggleState("refDTRProb")
      shinyjs::toggleState("compDTRProb")
    })
    
    observe({
      if (input$cellOrMarginal == TRUE) {
        updateNumericInput(session, "refDTRProb",  value = generateProbs()[1])
        updateNumericInput(session, "compDTRProb", value = generateProbs()[2])
      }
    }, priority = 2)
    
    ### Compute full DTR probabilities or effect size when providing cell-specific probabilities or mean/SD
    
    generateProbs <- reactive({
      if (design1.outcome() == "Binary" && input$cellOrMarginal == TRUE) {
        if (input$dyo.stage1.resprob.eq == "Yes") {
          pDTR1 <- marginalizeDTRProbs(input$refDTRProb.responders,
                                       input$dyo.stage1.allTxt.resprob, 
                                       input$refDTRProb.nonresponders)
          pDTR2 <- marginalizeDTRProbs(input$compDTRProb.responders,
                                       input$dyo.stage1.allTxt.resprob, 
                                       input$compDTRProb.nonresponders)
        } else {
          pDTR1 <- marginalizeDTRProbs(input$refDTRProb.responders,
                                       input[[paste0("dyo.stage1.", refDTR.substr()[2], ".resprob")]], 
                                       input$refDTRProb.nonresponders)
          pDTR2 <- marginalizeDTRProbs(input$compDTRProb.responders,
                                       input[[paste0("dyo.stage1.", compDTR.substr()[2], ".resprob")]], 
                                       input$compDTRProb.nonresponders)
        }
      } else if (design1.outcome() == "Binary" && input$cellOrMarginal == FALSE) {
        pDTR1 <- input$refDTRProb
        pDTR2 <- input$compDTRProb
      }
      
      return(c(pDTR1,pDTR2))
    })
    
    
    ##### Diagram Creation #####
    
    ## Call shiny module to write strings which define the mermaid graph 
    dyo.stage2respDiagram <- callModule(module = dyoDiagramStage2, id = "dyo.stage2respDiagram",
                                        RNR = "R", rerand = reactive(input$dyo.rerand.resp),
                                        whichtxts = reactive(input$dyo.rerand.resp.whichtxt),
                                        ntxt.stage1 = reactive(input$dyo.stage1.ntxt),
                                        ntxt.stage2 = reactive(input$dyo.rerand.resp.ntxt))
    dyo.stage2nrespDiagram <- callModule(module = dyoDiagramStage2, id = "dyo.stage2nrespDiagram",
                                         RNR = "NR", rerand = reactive(input$dyo.rerand.nresp),
                                         whichtxts = reactive(input$dyo.rerand.nresp.whichtxt),
                                         ntxt.stage1 = reactive(input$dyo.stage1.ntxt),
                                         ntxt.stage2 = reactive(input$dyo.rerand.nresp.ntxt))
    
    ## Create diagram components for first randomization to first-stage treatments
    dyo.stage1.diagram <- reactive({
      # for every stage 1 treatment, draw an arrow from the first randomization to txt
      # label each arrow with the randomization probability
      paste(sapply(1:input$dyo.stage1.ntxt,
                   function(i) paste0("R1-->",
                                      LETTERS[i], "[", LETTERS[i], "]")), collapse = "\n")
    })
    
    dyo.diagramNodeStyles <- reactive({
      ## Add nodes to be styled into rclass (reference class) and cclass (comparison class)
      ## based on selected primary aim
      ## TODO: Add styling for other primary aims
      if (is.null(input[["dyo.primaryAim-primaryAim"]])) {
        rclass <- ""
        cclass <- ""
      } else {
        if (input[["dyo.primaryAim-primaryAim"]] == "dtrs") {
          if (refDTR.substr()[1] != "")
            rclass <- paste0("class ", paste0(refDTR.substr()[2:4], collapse = ","), " refdtr; \n ")
          else 
            rclass <- ""
          if (compDTR.substr()[1] != "")
            cclass <- paste0("class ", paste0(compDTR.substr()[2:4], collapse = ","), " compdtr; \n")
          else
            cclass <- ""
        } else {
          rclass <- ""
          cclass <- ""
        }
      }
      paste(rclass, cclass, sep = "\n")
    })
    
    dyo.diagramLinkStyles <- reactive({
      ## Mermaid IDs "links" (edges) by the order in which they're drawn.
      ## (This is tremendously obnoxious.)
      if (is.null(input[["dyo.primaryAim-primaryAim"]])) {
        rclass <- ""
        cclass <- ""
      } else {
        if (input[["dyo.primaryAim-primaryAim"]] == "dtrs") {
          if (refDTR.substr()[1] != "") {
            r1class <- paste("linkStyle",
                             as.character(which(refDTR.substr()[2] == LETTERS) - 1),
                             "fill:none,stroke:#06c,stroke-width:3px,stroke-dasharray:8,8;")
          } else 
            r1class <- ""
          if (compDTR.substr()[1] != "") {
            c1class <- paste("linkStyle",
                             as.character(which(compDTR.substr()[2] == LETTERS) - 1),
                             "fill:none,stroke:#c33,stroke-width:3px,stroke-dasharray:6,4;")
          } else
            c1class <- ""
          paste(r1class, c1class, sep = "\n")
        } else
          NULL
      }
    })
    
    ## Render graph definition string for verification
    output$graphstring <- renderText(paste("graph LR;",
                                           "classDef default fill:#fff,stroke:#000,stroke-width:1px;",
                                           "classDef randomize fill:#fff,stroke:#000,stroke-width:2px;",
                                           "classDef compdtr fill:#fcc,stroke:#c33,stroke-width:3px,stroke-dasharray:6,4;", # red
                                           "classDef refdtr fill:#ccf,stroke:#06c,stroke-width:3px,stroke-dasharray:8,8;", #blue
                                           "R1((R));",
                                           "class R1 randomize;",
                                           dyo.stage1.diagram(),
                                           dyo.stage2respDiagram(),
                                           dyo.stage2nrespDiagram(),
                                           dyo.diagramNodeStyles(),
                                           dyo.diagramLinkStyles(),
                                           sep = " \n")
    )
    
    diagram <- reactive({
      DiagrammeR(diagram = paste("graph LR;",
                                 "classDef default fill:#fff,stroke:#000,stroke-width:1px;",
                                 "classDef randomize fill:#fff,stroke:#000,stroke-width:2px;",
                                 "classDef compdtr fill:#fcc,stroke:#c33,stroke-width:3px,stroke-dasharray:6,4;", # red
                                 "classDef refdtr fill:#ccf,stroke:#06c,stroke-width:3px,stroke-dasharray:8,8;", #blue
                                 "R1((R));",
                                 "class R1 randomize;",
                                 dyo.stage1.diagram(),
                                 dyo.stage2respDiagram(),
                                 dyo.stage2nrespDiagram(),
                                 dyo.diagramNodeStyles(),
                                 dyo.diagramLinkStyles(),
                                 sep = " \n"),
                 type = "mermaid")
    })
    
    ## Render diagram
    output$dyo.diagram <- renderDiagrammeR({
      # Make sure either responders or non-responders are re-randomized before rendering diagram
      # (Need at least one group to be re-randomized or else design is not a SMART)
      validate(
        need(input$dyo.rerand.resp == "Yes" | input$dyo.rerand.nresp == "Yes", text.mustRerandomize)
      )
      if (input$dyo.rerand.resp == "Yes") {
        validate(need(length(input$dyo.rerand.resp.whichtxt) > 0, text.rerandMismatchResp))
      } else if (input$dyo.rerand.nresp == "Yes") {
        validate(need(length(input$dyo.rerand.nresp.whichtxt) > 0, text.rerandMismatchNresp))
      }
      diagram()
    })
    
    output$dyo.diagram.modal <- renderDiagrammeR({diagram()})
    
    ## Zoom Diagram Modal
    observeEvent(input$zoom,
                 showModal(modalDialog(
                   DiagrammeROutput("dyo.diagram.modal", 
                                    height = paste0(60 * input$dyo.stage1.ntxt * (input$dyo.rerand.nresp.ntxt + input$dyo.rerand.resp.ntxt), "px")),
                   p("Save options are coming soon."),
                   easyClose = T
                 )))
    
    #### Collapse and Tabset Handlers ####
    
    # Observer for button "dyo.stage1.continue" inside stage 1 design spec collapse panel
    # When button is pressed, update the collapse so that the "close" panel closes and the "open" panel opens
    observeEvent(input$dyo.outcome.continue,
                 updateCollapse(session, "dyo.setup.collapse",
                                open = "dyo.resultOptions.describe",
                                close = "dyo.outcome.describe"))
    observeEvent(input$dyo.resultOptions.continue,
                 updateCollapse(session, "dyo.setup.collapse",
                                open = "dyo.stage1.describe",
                                close = "dyo.resultOptions.describe"))
    observeEvent(input$dyo.stage1.continue, 
                 updateCollapse(session, "dyo.setup.collapse", 
                                open = "dyo.resp.describe", 
                                close = "dyo.stage1.describe"))
    observeEvent(input$dyo.resp.continue, 
                 updateCollapse(session, "dyo.setup.collapse", 
                                open = "dyo.rerand.describe", 
                                close = "dyo.resp.describe"))
    # back buttons
    observeEvent(input$dyo.resultOptions.back, 
                 updateCollapse(session, "dyo.setup.collapse", 
                                open = "dyo.outcome.describe", 
                                close = "dyo.resultOptions.describe"))
    observeEvent(input$dyo.stage1.back, 
                 updateCollapse(session, "dyo.setup.collapse", 
                                open = "dyo.resultOptions.describe", 
                                close = "dyo.stage1.describe"))
    observeEvent(input$dyo.resp.back, 
                 updateCollapse(session, "dyo.setup.collapse", 
                                open = "dyo.stage1.describe", 
                                close = "dyo.resp.describe"))
    observeEvent(input$dyo.rerand.back, 
                 updateCollapse(session, "dyo.setup.collapse", 
                                open = "dyo.resp.describe", 
                                close = "dyo.rerand.describe"))
    
    
    ##### Sample Size Computation #####
    
    ## Gather inputs needed to compute A and B, then pass them to ABcomp (global.R) to compute A and B
    AB <- reactive({
      req(input$dyo.stage1.eqrand, input$dyo.stage1.resprob.eq, input$dyo.rerand.resp)
      
      validate(  # Check for necessary inputs
        need(refDTR.substr()[1]  != 0, text.refDTRPlaceholder),
        need(compDTR.substr()[1] != 0, text.compDTRPlaceholder)
      )
      
      ## Gather appropriate inputs
      if (input$dyo.stage1.eqrand == "Yes") {
        # If stage 1 is randomized equally, pi1 and pi0 are both 1/(number of first-stage treatments)
        pi1 <- pi0 <- 1 / input$dyo.stage1.ntxt
      } else {
        # If stage 1 is not randomized equally, pull pi1 and pi0 from appropriate inputs
        validate(  # check appropriate inputs are available
          need(isTruthy(input[[paste0("dyo.stage1.txt", which(LETTERS == refDTR.substr()[2]), ".rprob")]]),
               paste0("Please provide the probability of randomization to treatment ", refDTR.substr()[2], ".")),
          need(isTruthy(input[[paste0("dyo.stage1.txt", which(LETTERS == compDTR.substr()[2]), ".rprob")]]),
               paste0("Please provide the probability of randomization to treatment ", compDTR.substr()[2], "."))
        )
        pi1 <- input[[paste0("dyo.stage1.txt", which(LETTERS == refDTR.substr()[2]), ".rprob")]]
        pi0 <- input[[paste0("dyo.stage1.txt", which(LETTERS == compDTR.substr()[2]), ".rprob")]]
      }
      
      ## Get response probabilities for first-stage treatments recommended by refDTR and compDTR
      if (input$dyo.stage1.resprob.eq == "Yes") {  # Check if response probability is equal across all first-stage treatments
        validate(  # Verify there's an input for shared response probability; if not, show message
          need(input$dyo.stage1.allTxt.resprob, "Please provide the probability of response to all first-stage treatments")  
        )
        r1 <- r0 <- input$dyo.stage1.allTxt.resprob  # After verifying input, store value
      } else {  # If response probability varies by first-stage treatment,
        validate(  # Check that appropriate inputs have been provided
          need(isTruthy(input[[paste0("dyo.stage1.", which(LETTERS == refDTR.substr()[2]), ".resprob")]]),
               paste0("Please provide a response probability for first-stage treatment ", refDTR.substr()[2], ".")),
          need(isTruthy(input[[paste0("dyo.stage1.", which(LETTERS == compDTR.substr()[2]), ".resprob")]]),
               paste0("Please provide a response probability for first-stage treatment ", compDTR.substr()[2], "."))
        )
        # get response probabilities
        r1 <- input[[paste0("dyo.stage1.", which(LETTERS == refDTR.substr()[2]), ".resprob")]]
        r0 <- input[[paste0("dyo.stage1.", which(LETTERS == compDTR.substr()[2]), ".resprob")]]
      }
      
      ## Get re-randomization probabilities for responders 
      if (input$dyo.rerand.resp == "Yes") {  # are any responders re-randomized in the design?
        if (LETTERS[refDTR.substr()[2]] %in% input$dyo.rerand.resp.whichtxt) {  # are responders to the first-stage treatment recommended by the reference DTR re-randomized?
          if (input$dyo.rerand.resp.eqrand == "Yes") { # are responders in reference DTR re-randomized equally?
            pi2R.1 <- 1 / input$dyo.rerand.resp.ntxt  # if yes, randomization proability is 1/(number of options)
          } else {  # if no, pull randomization probability from the appropriate input
            validate(  # check that appropriate input exists
              need(isTruthy(input[[paste0("dyo.rerand.resp.txt", which(LETTERS == refDTR.substr()[3]), ".rprob")]]), 
                   paste0("Please provide a randomization probability for responders to first-stage treatment ", refDTR.substr()[2]), ".")
            )
            pi2R.1 <- input[[paste0("dyo.rerand.resp.txt", which(LETTERS == refDTR.substr()[3]), ".rprob")]]
          }
        } else {  # if responders to refDTR's first-stage treatment are not re-randomized, assignment to A2 is w/ prob 1
          pi2R.1 <- 1
        }
        if (LETTERS[compDTR.substr()[2]] %in% input$dyo.rerand.resp.whichtxt) {  # are responders to first-stage treatment recommended by compDTR re-randomized?
          if (input$dyo.rerand.resp.eqrand == "Yes") {
            pi2R.0 <- 1 / input$dyo.rerand.resp.ntxt  # if yes, randomization probability is 1/(number of options)
          } else {
            validate(  # check that appropriate input exists
              need(isTruthy(input[[paste0("dyo.rerand.resp.txt", which(LETTERS == compDTR.substr()[3]), ".rprob")]]), 
                   paste0("Please provide a randomization probability for responders to first-stage treatment ", compDTR.substr()[2]), ".")
            )
            pi2R.0 <- input[[paste0("dyo.rerand.resp.txt", which(LETTERS == compDTR.substr()[3]), ".rprob")]]  # If no, pull randomization proability from appropriate input
          }
        } else {
          pi2R.0 <- 1  # If responders to compDTR's first-stage treatment are not re-randomized, assignment to A2 is w/ prob 1
        }
      } else {
        pi2R.1 <- pi2R.0 <- 1  # If no responders in the design are re-randomized, assignment to A2 is w/ prob 1 for refDTR and compDTR
      }
      
      ## Get re-randomization probabilities for non-responders
      if (input$dyo.rerand.nresp == "Yes") {  # Are any non-responders re-randomized in the design?
        if (LETTERS[refDTR.substr()[2]] %in% input$dyo.rerand.nesp.whichtxt) {  # If yes, are NRs to the first-stage treatment recommended by refDTR re-randomized?
          if (input$dyo.rerand.nresp.eqrand == "Yes") {  # Are NRs re-randomized with equal probability?
            pi2NR.1 <- 1 / input$dyo.rerand.nresp.ntxt 
          } else {  # If no, pull appropriate probability
            validate(  # Make sure appropriate probability is available
              need(isTruthy(input[[paste0("dyo.rerand.nresp.txt", which(LETTERS == refDTR.substr()[4]), ".rprob")]]),
                   paste0("Please provide a randomization probability for non-responders to first-stage treatment ", refDTR.substr()[2], "."))
            )
            pi2NR.1 <- input[[paste0("dyo.rerand.nresp.txt", which(LETTERS == refDTR.substr()[4]), ".rprob")]]
          }
        } else {
          pi2NR.1 <- 1  # If NRs in refDTR aren't re-randomized, assignment to A2 is w/ prob 1
        }
        if (LETTERS[compDTR.substr()[2]] %in% input$dyo.rerand.nresp.whichtxt) {  # Are NRs to first-stage treatment recommended by compDTR re-randomized?
          if (input$dyo.rerand.nresp.eqrand == "Yes") {  # Are they re-randomized equally?
            pi2NR.0 <- 1 / input$dyo.rerand.nresp.ntxt  # If yes, A2 assigned w/ prob 1/(number of options)
          } else {
            validate(  # check that appropriate input exists
              need(isTruthy(input[[paste0("dyo.rerand.nresp.txt", which(LETTERS == compDTR.substr()[3]), ".rprob")]]), 
                   paste0("Please provide a randomization probability for non-responders to first-stage treatment ", compDTR.substr()[2]), ".")
            )
            pi2NR.0 <- input[[paste0("dyo.rerand.nresp.txt", which(LETTERS == compDTR.substr()[4]), ".rprob")]]  # If not, pull from input
          }
        } else {
          pi2NR.0 <- 1  # If NRs in compDTR aren't re-randomized, assignment to A2 w/ prob 1
        }
      } else {
        pi2NR.1 <- pi2NR.0 <- 1  # If no NRs in the design are re-randomized, assignment to A2 is w/ prob 1 for refDTR and compDTR
      }
      
      ## Compute A and B using the gathered inputs
      A <- ABcomp(pi1, r1, pi2R.1, pi2NR.1)
      B <- ABcomp(pi0, r0, pi2R.0, pi2NR.0)
      
      return(list("A" = A, "B" = B))
    })
    
    
    ##### Input Processing #####
    ### Validate and compile inputs into reactive functions which can be called in sampleSize() function
    
    respProb <- reactive({
      if (input$conservative) {
        return(rep(0, input$dyo.stage1.ntxt))
      } else {
        if (input$dyo.stage1.resprob.eq == "Yes") {
          validate(
            need(isTruthy(input$dyo.stage1.allTxt.resprob), text.noResponse) %then%
              need(0 <= input$dyo.stage1.allTxt.resprob & input$dyo.stage1.allTxt.resprob <= 1, text.invalidResponse)
          )
          return(rep(input$dyo.stage1.allTxt.resprob, input$dyo.stage1.ntxt))
        } else {
          return(sapply(1:input$dyo.stage1.ntxt, function(i) {
            input[[paste0("dyo.stage1.", LETTERS[i], ".resprob")]]
          }))
        }
      }
    })
    
    pi2R <- reactive({
      if (input$dyo.rerand.resp == "Yes") {
        l <- sapply(1:input$dyo.rerand.resp.ntxt, function(i) {
          if (LETTERS[i] %in% input$dyo.rerand.resp.whichtxt) {
            if (input$dyo.rerand.resp.eqrand == "Yes") {
              1 / input$dyo.rerand.resp.ntxt
            } else {
              input[[paste0("dyo.rerand.resp.txt", LETTERS[i])]]
            }
          } else {
            1
          }
        })
      } else {
        l <- sapply(1:input$dyo.rerand.resp.ntxt, function(i) 1)
      }
      l
    })
    
    pi2NR <- reactive({
      if (input$dyo.rerand.nresp == "Yes") {
        l <- sapply(1:input$dyo.rerand.nresp.ntxt, function(i) {
          if (LETTERS[i] %in% input$dyo.rerand.nresp.whichtxt) {
            if (input$dyo.rerand.nresp.eqrand == "Yes") {
              1 / input$dyo.rerand.nresp.ntxt
            } else {
              input[[paste0("dyo.rerand.nresp.txt", LETTERS[i])]]
            }
          } else {
            1
          }
        })
      } else {
        l <- sapply(1:input$dyo.rerand.nresp.ntxt, function(i) 1)
      }
      l
    })
    
    ##### Results #####
    
    ### Write the sentence that appears under the final result
    sentenceCompiler <- reactive({
      if (dyo.outcome() == "Binary" && input$cellOrMarginal == FALSE) {
        return(paste(text.sentenceOverallSuccess, input$dyoRefDTR,
                     " and ", input$dyoCompDTR, ", are ", input$refDTRProb,
                     " and ", input$compDTRProb, ", respectively", sep = ""))
      }
      if (dyo.outcome() == "Binary" && input$cellOrMarginal == TRUE) {
        return(paste(text.sentenceOverallSuccess, input$dyoRefDTR, " and ",
                     input$dyoCompDTR, ", are ", generateProbs()[1],
                     " and ", generateProbs()[2], ", respectively", sep = ""))
      }
      if (dyo.outcome() == "Continuous") {
        return(paste(text.sentenceEffectSize, input$firstDTRcompareA, " and ", 
                     input$secondDTRcompareA, ", is ", input$effectSizeA, sep = ""))
      }
    })
    
    output$sentence <- renderText(sentenceCompiler())
    
    
    output$binarySampleSize <- renderUI({
      if (isTruthy(primaryAim() == "dtrs")){
        validate(
          need(isTruthy(refDTR.substr()[1]), text.refDTRPlaceholder),
          need(isTruthy(compDTR.substr()[1]), text.compDTRPlaceholder)  %then%
            validate(
              need(isTruthy(input$refDTRProb),  paste0("The success probability is missing for AI ", refDTR.substr()[1],  ". Please provide an input.")),
              need(isTruthy(input$compDTRProb), paste0("The success probability is missing for AI ", compDTR.substr()[1], ". Please provide an input."))
            )
        )
      }
      
      validate(
        need(isTruthy(dyo.resultOptions()$inputPower), "Please provide a target power.") %then%
        need(dyo.resultOptions()$inputPower > 0, text.power0) %then%
        need(dyo.resultOptions()$inputPower < 1, text.power100)
      )
      
      ## Compile success probabilities
      p1 <- generateProbs()[1]
      p2 <- ifelse(input$cellOrMarginal, generateProbs()[2], input$compDTRProb)
      
      ## Compile randomization probabilities
      pi.stage1A   <- ifelse(input$dyo.stage1.eqrand == "Yes",
                             1 / input$dyo.stage1.ntxt,
                             input[[paste0("dyo.stage1.txt", which(LETTERS == refDTR.substr()[2]))]])
      pi.stage2RA  <- ifelse(substr(refDTR.substr()[3], 3, 3) == "", 1,  pi2R()[as.numeric(substr(refDTR.substr()[3], 3, 3))])
      pi.stage2NRA <- ifelse(substr(refDTR.substr()[4], 4, 4) == "", 1, pi2NR()[as.numeric(substr(refDTR.substr()[4], 4, 4))])
      
      pi.stage1B   <- ifelse(input$dyo.stage1.eqrand == "Yes",
                             1 / input$dyo.stage1.ntxt,
                             input[[paste0("dyo.stage1.txt", which(LETTERS == compDTR.substr()[2]))]])
      pi.stage2RB  <- ifelse(substr(compDTR.substr()[3], 3, 3) == "", 1,  pi2R()[as.numeric(substr(compDTR.substr()[3], 3, 3))])
      pi.stage2NRB <- ifelse(substr(compDTR.substr()[4], 4, 4) == "", 1, pi2NR()[as.numeric(substr(compDTR.substr()[4], 4, 4))])
      
      ## Compute and round sample size
      finalSampleSize <-                 
        ceiling(sampleSize(alpha = dyo.resultOptions()$alpha, power = as.numeric(dyo.resultOptions()$inputPower), 
                   p1 = generateProbs()[1], p2 = generateProbs()[2],
                   respA = respProb()[which(LETTERS == refDTR.substr()[2])],
                   pi.stage1A = pi.stage1A,
                   pi.stage2RA = pi.stage2RA,
                   pi.stage2NRA = pi.stage2NRA,
                   respB = respProb()[which(LETTERS == compDTR.substr()[2])],
                   pi.stage1B = pi.stage1B,
                   pi.stage2RB = pi.stage2RB,
                   pi.stage2NRB = pi.stage2NRB,
                   aim = primaryAim(), conservative = input$conservative))
      
      ## Format inputs for use in result sentence
      formatPower     <- paste(dyo.resultOptions()$inputPower * 100, "%", sep = "")
      formatAlpha     <- paste(dyo.resultOptions()$alpha      * 100, "%", sep = "")
      formatResp      <- paste(input$resp       * 100, "%", sep = "")
      formatAltHyp    <- switch(input$selectAlternativeC, "one.sided" = "one-sided ", "two.sided" = "two-sided ")
      
      ## Result sentence
      HTML(paste("<h4 style='color:blue';> N=", finalSampleSize, " </h4>
                 <p> We wish to find the sample size for a trial with a binary outcome where the probability of response to first-stage interventions is ", formatResp, sentenceCompiler(),
                 ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
                 finalSampleSize, " to make this comparison with at least ",formatPower," power. </p>",sep=""))
    })
    
    
    
    
    ##### Miscellaneous Event Handlers #####
    
    # If user decides to input cell-specific probabilities and hasn't provided 
    # a response probability, open the appropriate panel.
    observeEvent(req(input$cellOrMarginal), {
      updateCheckboxInput(session, "targetOddsRatio", value = FALSE)
      updateCheckboxInput(session, "targetDifference", value = FALSE)
      if (input$dyo.stage1.resprob.eq == "Yes") {
        if (!isTruthy(input$dyo.stage1.allTxt.resprob)) {
          # updateCollapse(session, "dyo.setup.collapse", close = input$dyo.setup.collapse)
          updateCollapse(session, "dyo.setup.collapse", open = "dyo.resp.describe")
        }
      }
    })
    
    # If a user checks the conservative sample size box, disable response probability box
    observeEvent(input$conservative, {
      shinyjs::toggleState("dyo.stage1.resprob.eq", condition = !input$conservative)
      updateRadioButtons(session, "dyo.stage1.resprob.eq", selected = "Yes")
    })
    observe({
      if (input$conservative) {
        shinyBS::createAlert(session, anchorId = "premade-design-disabled-resp-describe",
                             alertId = "alert-conservative",
                             title = "This section has been disabled.",
                             content = paste("Conservative estimates of sample size do not depend on response rates.",
                                             "If you have estimates of response rate(s), uncheck the box."))
        if (input$dyo.stage1.resprob.eq == "Yes")
          shinyjs::disable("dyo.stage1.allTxt.resprob")
      } else {
        shinyBS::closeAlert(session, "alert-conservative")
      }
    })
    
    # If the user checks the targetDifference box, uncheck the other options
    observeEvent(req(input$targetDifference), {
        updateCheckboxInput(session, "cellOrMarginal",  value = FALSE)
        updateCheckboxInput(session, "targetOddsRatio", value = FALSE)
    })
    
    observeEvent(req(input$targetOddsRatio), {
      updateCheckboxInput(session, "cellOrMarginal",  value = FALSE)
      updateCheckboxInput(session, "targetDifference", value = FALSE)
    })
    
    
    ##### Premade Design Autofill #####
    ### Design I
    
    observeEvent(input$pickTabA, {    
      # Input updates: stage 1 treatments
      updateSliderInput(session, "dyo.stage1.ntxt", value = 2)
      updateRadioButtons(session, "dyo.stage1.eqrand", selected = "Yes")
      shinyjs::disable("dyo.stage1.ntxt")
      shinyjs::disable("dyo.stage1.eqrand")
      
      # Input updates: equal response
      updateRadioButtons(session, "dyo.stage1.resprob.eq", selected = "Yes")
      shinyjs::disable("dyo.stage1.resprob.eq")
      
      # Input updates: stage 2 treatments
      updateRadioButtons(session, "dyo.rerand.resp", selected = "Yes")
      updateSliderInput(session, "dyo.rerand.resp.ntxt", value = 2)
      updateRadioButtons(session, "dyo.rerand.resp.eqrand", selected = "Yes")
      updateRadioButtons(session, "dyo.rerand.nresp", selected = "Yes")
      updateSliderInput(session, "dyo.rerand.nresp.ntxt", value = 2)
      updateRadioButtons(session, "dyo.rerand.nresp.eqrand", selected = "Yes")
      shinyjs::disable("dyo.rerand.resp")
      shinyjs::disable("dyo.rerand.resp.ntxt")
      shinyjs::disable("dyo.rerand.resp.eqrand")
      shinyjs::disable("dyo.rerand.nresp")
      shinyjs::disable("dyo.rerand.nresp.ntxt")
      shinyjs::disable("dyo.rerand.nresp.eqrand")
      
      # Raise alerts about disabled inputs
      shinyBS::createAlert(session,
                           anchorId = "premade-design-disabled-stage1-describe",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premade-design-disabled-resp-describe",
                           title = "Part of this section has been disabled.",
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premadeDesignInputsDisabled-stage2Alert",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      
      # UI updates
      updateCollapse(session, "dyo.setup.collapse",
                     close = "dyo.stage1.describe")
      shinyjs::hide("dyo.stage1.resprob.continue")
      shinyjs::disable("dyo.stage1.describe")
      updateTabsetPanel(session, "SMARTsize", selected = "Design and Size")
      shinyjs::show("unlock-dyo") # unhide button to unlock UI
    })
    
    ## Design II
    observeEvent(input$pickTabB, {    
      # Input updates: stage 1 treatments
      updateSliderInput(session, "dyo.stage1.ntxt", value = 2)
      updateRadioButtons(session, "dyo.stage1.eqrand", selected = "Yes")
      shinyjs::disable("dyo.stage1.ntxt")
      shinyjs::disable("dyo.stage1.eqrand")
      
      # Input updates: equal response
      updateRadioButtons(session, "dyo.stage1.resprob.eq", selected = "Yes")
      shinyjs::disable("dyo.stage1.resprob.eq")
      
      # Input updates: stage 2 treatments
      updateRadioButtons(session, "dyo.rerand.resp", selected = "No")
      updateRadioButtons(session, "dyo.rerand.resp.eqrand", selected = "Yes")
      updateRadioButtons(session, "dyo.rerand.nresp", selected = "Yes")
      updateSliderInput(session, "dyo.rerand.nresp.ntxt", value = 2)
      updateRadioButtons(session, "dyo.rerand.nresp.eqrand", selected = "Yes")
      shinyjs::disable("dyo.rerand.resp")
      shinyjs::disable("dyo.rerand.resp.ntxt")
      shinyjs::disable("dyo.rerand.resp.eqrand")
      shinyjs::disable("dyo.rerand.nresp")
      shinyjs::disable("dyo.rerand.nresp.ntxt")
      shinyjs::disable("dyo.rerand.nresp.eqrand")
      
      # Raise alerts about disabled inputs
      shinyBS::createAlert(session,
                           anchorId = "premade-design-disabled-stage1-describe",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premade-design-disabled-resp-describe",
                           title = "Part of this section has been disabled.",
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premadeDesignInputsDisabled-stage2Alert",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      
      # UI updates
      updateCollapse(session, "dyo.setup.collapse",
                     close = "dyo.stage1.describe")
      shinyjs::hide("dyo.stage1.resprob.continue")
      shinyjs::disable("dyo.stage1.describe")
      updateTabsetPanel(session, "SMARTsize", selected = "Design and Size")
      shinyjs::show("unlock-dyo") # unhide button to unlock UI
    })
    
    observeEvent(input$pickTabC, {    
      # Input updates: stage 1 treatments
      updateSliderInput(session, "dyo.stage1.ntxt", value = 2)
      updateRadioButtons(session, "dyo.stage1.eqrand", selected = "Yes")
      shinyjs::disable("dyo.stage1.ntxt")
      shinyjs::disable("dyo.stage1.eqrand")
      
      # Input updates: equal response
      updateRadioButtons(session, "dyo.stage1.resprob.eq", selected = "Yes")
      shinyjs::disable("dyo.stage1.resprob.eq")
      
      # Input updates: stage 2 treatments
      updateRadioButtons(session, "dyo.rerand.resp", selected = "No")
      updateRadioButtons(session, "dyo.rerand.resp.eqrand", selected = "Yes")
      updateRadioButtons(session, "dyo.rerand.nresp", selected = "Yes")
      updateCheckboxGroupInput(session, "dyo.rerand.nresp.whichtxt", 
                               selected = c("A"))
      updateSliderInput(session, "dyo.rerand.nresp.ntxt", value = 2)
      updateRadioButtons(session, "dyo.rerand.nresp.eqrand", selected = "Yes")
      shinyjs::disable("dyo.rerand.resp")
      shinyjs::disable("dyo.rerand.resp.ntxt")
      shinyjs::disable("dyo.rerand.resp.eqrand")
      shinyjs::disable("dyo.rerand.nresp")
      shinyjs::disable("dyo.rerand.nresp.ntxt")
      shinyjs::disable("dyo.rerand.nresp.eqrand")
      
      # Raise alerts about disabled inputs
      shinyBS::createAlert(session,
                           anchorId = "premade-design-disabled-stage1-describe",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premade-design-disabled-resp-describe",
                           title = "Part of this section has been disabled.",
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premadeDesignInputsDisabled-stage2Alert",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      
      # UI updates
      updateCollapse(session, "dyo.setup.collapse",
                     close = "dyo.stage1.describe")
      shinyjs::hide("dyo.stage1.resprob.continue")
      shinyjs::disable("dyo.stage1.describe")
      updateTabsetPanel(session, "SMARTsize", selected = "Design and Size")
      shinyjs::show("unlock-dyo") # unhide button to unlock UI
    })
    
  }) # END shinyServer