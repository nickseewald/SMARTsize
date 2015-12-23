##### SERVER.R FOR SMART SAMPLE SIZE CALCULATOR #####
### NICK SEEWALD, 2014
### UNIVERSITY OF MICHIGAN
### DEPARTMENT OF BIOSTATISTICS

library(shiny)
library(pwr)
library(shinyBS)
options(encoding = 'UTF-8')

# TODO: Deprecate choice of test sidedness for binary outcome
# TODO: Centralize error messages in error_messages.R
# TODO: Make progress on partials
# TODO: Move non-reactive function declarations, etc. into global.R

### Start server operation

shinyServer(
  function(input,output,session){
output$tab <- renderText(input$SMARTsize)
  ##### HOME #####
  ### Watch for clicks on pickTab actionButtons rendered under design diagrams
  ### On click, redirect to appropriate tab. (More intuitive navigation structure)

    observeEvent(input$pickTabA, updateTabsetPanel(session, "SMARTsize", selected = "Design I"))
    observeEvent(input$pickTabB, updateTabsetPanel(session, "SMARTsize", selected = "Design II"))
    observeEvent(input$pickTabC, updateTabsetPanel(session, "SMARTsize", selected = "Design III"))

  ##### DESIGN A #####

  ##### A HEADER #####

  ### Render the design image which highlights selected DTRs
  ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
  ### Note that this requires a very specific naming convention for image assets
  ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0.

  output$designAimg <- renderImage(expr = {
    filename <- 
      normalizePath(file.path('./www/images',
                              paste('SMARTdesignA_', input$firstDTRcompareA,
                                    '_', input$secondDTRcompareA,'.gif', sep = '')))
    list(src = filename)
  }, deleteFile = FALSE)

  ### Render 'selectize' dropdown boxes with placeholder text for AI selection.
  ### Second DTR input populates with all DTRs that are NOT the first DTR
  ###   (eliminates ability to select same DTR twice)
  ### Placed in server.R rather than ui.R because of dependency on first DTR selection

    output$selectAI1A <- renderUI({
      AI <- selectizeInput("firstDTRcompareA", label = text.refDTRLabel,
                           choices = designA.DTRs,
                           options = list(
                             placeholder = text.refDTRPlaceholder,
                             onInitialize = I('function() { this.setValue(0); }')
                             )
                           )
      return(AI)
      })

    output$selectAI2A <- renderUI({
      AI <- selectizeInput("secondDTRcompareA",label = text.compDTRLabel,
                           choices = designA.DTRs[substr(designA.DTRs, 1, 1) !=
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
    if (input$targetOddsCheckA==TRUE) {
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
    if(input$targetDiffCheckA){
      updateCheckboxInput(session,"cellOrConditionalA",value=FALSE)
      updateCheckboxInput(session,"targetOddsCheckA",value=FALSE)
    }
  })

  observe({
    if(input$targetOddsCheckA){
      updateCheckboxInput(session,"cellOrConditionalA",value=FALSE)
      updateCheckboxInput(session,"targetDiffCheckA",value=FALSE)
    }
  })

  ### If cell-specific input is selected, deselect target difference and odds-ratio options
  ### Update disabled full DTR success inputs with computed probabilities

  observe({
    if(input$cellOrConditionalA==TRUE){
      updateCheckboxInput(session,"targetDiffCheckA",value=FALSE)
      updateCheckboxInput(session,"targetOddsCheckA",value=FALSE)

      updateNumericInput(session,"DTRsuccA1disable",value=generateProbsA()[1])
      updateNumericInput(session,"DTRsuccA2disable",value=generateProbsA()[2])
      updateNumericInput(session,"DTRsuccA1",value=generateProbsA()[1])
      updateNumericInput(session,"DTRsuccA2",value=generateProbsA()[2])
    }
  }, priority=2)

  ##### DESIGN A RESULT BACKEND #####

  ### Compute full DTR probabilities or effect size when providing cell-specific probabilities or mean/SD

  generateProbsA <- reactive({
    if (input$selectOutcomeA == 1 && input$cellOrConditionalA == TRUE) {
      pDTR1 <- fullDTRprob(input$marginalFirstStageA1,input$respA,input$marginalSecondStageNRA1)
      pDTR2 <- fullDTRprob(input$marginalFirstStageA2,input$respA,input$marginalSecondStageNRA2)
    }
    else if (input$selectOutcomeA == 1 && input$cellOrConditionalA == FALSE) {
      pDTR1 <- input$DTRsuccA1
      pDTR2 <- input$DTRsuccA2
    }

    return(c(pDTR1,pDTR2))
  })

  ### Make sure input probabilities are valid (i.e., check for blank entries)

  checkDTRinputsA <- reactive({
    return(as.logical((input$DTRsuccA1 > 0) & (input$DTRsuccA1 < 1) & (input$DTRsuccA2 > 0) & (input$DTRsuccA2 < 1)))
  })

  ### Determine which inputs are being given, check their quality, then pass the appropriate arguments for computation
    ### e.g., if target odds-ratio is given, compute a DTR success probability assuming the other is 0.5
  dataCompilerA <- reactive({

    ### Error Check: unselected DTRs, blank/invalid response probability
    validate(
      need(!(is.na(input$respA)), "Please provide a response probability. If unknown, enter 0 for a conservative estimate of power or sample size.") %then%
        need(0 <= input$respA && input$respA <= 1, "The provided response probability is not a valid probability. Please enter a value between 0 and 1.")
    )

    ### Binary outcome, DTR-specific success probabilities
    if(input$selectOutcomeA==1 && input$cellOrConditionalA==FALSE && input$targetDiffCheckA==FALSE && input$targetOddsCheckA==FALSE){
      ### Error Check: unselected DTRs, blank success probabilities, equal success probabilities, invalid success probabilities
      validate(
        need(!is.null(input$DTRsuccA1), "Select a Reference AI above."),
        need(!is.null(input$DTRsuccA2), "Select a Comparison AI above.") %then%
          need(!is.null(input$DTRsuccA1) && !is.null(input$DTRsuccA2), "The success probability is missing for at least one AI. Please provide a numeric input.") %then%
          need(input$DTRsuccA1 != input$DTRsuccA2, "Please provide unique success probabilities for each AI. Sample size is indeterminate for equal AI probabilities.") %then%
          need(checkDTRinputsA(), "The provided success probability for at least one AI is not a valid probability. Please enter values greater than 0 and less than 1.")
      )
      return(c(input$DTRsuccA1, input$DTRsuccA2))
    }

    ### Binary outcome, cell-specific success probabilities
    if(input$selectOutcomeA == 1 && input$cellOrConditionalA == TRUE && input$targetDiffCheckA == FALSE && input$targetOddsCheckA == FALSE){
      ### Error Check: equal DTR-specific success probabilities
      validate(
        need(generateProbsA()[1] != generateProbsA()[2], "The provided cell-specific probabilities yield identical overall AI-specific probabilities
             of success. Sample size is indeterminate for equal AI probabilities. Please adjust your inputs.")
      )
      return(c(generateProbsA()[1], generateProbsA()[2]))
    }

    ### Binary outcome, target difference in success probabilities
    if(input$selectOutcomeA == 1 && input$cellOrConditionalA == FALSE && input$targetDiffCheckA == TRUE && input$targetOddsCheckA == FALSE){
      ### Error Check: invalid target difference (must be less than 0.5 since we're using a conservative reference probability)
      validate(
        need(input$DTRsuccA1 + as.numeric(input$diffDirectionA) * input$targetDiffA >= 0 && input$DTRsuccA1 + as.numeric(input$diffDirectionA) * input$targetDiffA <= 1,
             "The target difference is too large to produce a valid probability between 0 and 1. Please adjust your inputs."),
        need(input$targetDiffA > 0, "The target difference must be greater than 0, since sample size is indeterminate for equal AI probabilities.
             Please adjust your inputs.")
        )
      return(c(input$DTRsuccA1, input$DTRsuccA1 + as.numeric(input$diffDirectionA) * input$targetDiffA))
    }

    ### Binary outcome, target odds ratio of success
    if(input$selectOutcomeA == 1 && input$cellOrConditionalA == FALSE && input$targetDiffCheckA == FALSE && input$targetOddsCheckA == TRUE){
      ### Error check: missing/invalid odds ratio
      validate(
        need(is.numeric(input$targetORA),"Please enter an odds ratio.") %then%
        need(input$targetORA != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio.") %then%
        need(input$targetORA != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
      )
      q <- (input$DTRsuccA1 / (1 - input$DTRsuccA1)) * input$targetORA
      return(c(input$DTRsuccA1, q / (1 + q)))
    }

    ### Continuous outcome, standardized effect size
    if(input$selectOutcomeA == 2){
      ### Error check: nonzero effect size
      validate(
        need(input$firstDTRcompareA, "Select a reference AI above."),
        need(input$secondDTRcompareA, "Select a comparison AI above."),
        need(!is.null(input$effectSizeA)," ") %then%
          need(!is.na(input$effectSizeA),"The standardized effect size is missing. Please enter a value between 0 and 10.") %then%
            need(input$effectSizeA != 0, "Sample size is indeterminate for an effect size of 0. Please enter a different target effect size.")
      )
      return(input$effectSizeA)
    }
  })

  ### Construct a portion of the explainer sentence that appears below the result, depending on the provided inputs.
  sentenceCompilerA <- reactive({
    if(input$selectOutcomeA==1 && input$cellOrConditionalA==FALSE && input$targetDiffCheckA==FALSE && input$targetOddsCheckA==FALSE){
      str <- paste(" and the overall probabilities of success in the two AIs of interest, ",input$firstDTRcompareA," and ",input$secondDTRcompareA,
                   ", are ",input$DTRsuccA1," and ",input$DTRsuccA2,", respectively", sep="")
      return(str)
    }
    if(input$selectOutcomeA==1 && input$cellOrConditionalA==TRUE && input$targetDiffCheckA==FALSE && input$targetOddsCheckA==FALSE){
      str <- paste(" and the overall probabilities of success in the two AIs of interest, ",input$firstDTRcompareA," and ",input$secondDTRcompareA,
                   ", are ",generateProbsA()[1]," and ",generateProbsA()[2],", respectively", sep="")
      return(str)
    }

    if(input$selectOutcomeA==1 && input$cellOrConditionalA==FALSE && input$targetDiffCheckA==TRUE && input$targetOddsCheckA==FALSE){
      str <- paste(" and the difference in overall probabilities of success in the two AIs of interest, ",input$firstDTRcompareA," and ",input$secondDTRcompareA,
                   ", is ",input$targetDiffA, sep="")
      return(str)
    }

    if(input$selectOutcomeA==1 && input$cellOrConditionalA==FALSE && input$targetDiffCheckA==FALSE && input$targetOddsCheckA==TRUE){
      str <- paste(" and the odds ratio of success for the two AIs of interest, ",input$firstDTRcompareA," and ",input$secondDTRcompareA,
                   ", is ",input$targetORA, sep="")
      return(str)
    }

    if(input$selectOutcomeA==2){
      str <- paste(" and the standardized effect size between the two AIs of interest, ",input$firstDTRcompareA," and ",input$secondDTRcompareA,
                   ", is ", input$effectSizeA, sep="")
      return(str)
    }
  })

  ##### DESIGN A RESULTS #####

  # Pass arguments from dataCompilerA() to appropriate R function; extract and render relevant output

    output$binarySampleSizeA <- renderUI({
    designEffect    <- 2

    A <- dataCompilerA()[1] / (1 - dataCompilerA()[1])
    B <- (dataCompilerA()[2] / (1 - dataCompilerA()[2])) / A

    rawSampleSize   <- 2 * ((qnorm(1 - input$inputPowerA) + qnorm(input$alphaA / 2)) ^ 2) * (((1 + A) ^ 2) * B + ((1 + A * B) ^ 2)) / (A * B * (log(B)) ^ 2)
    finalSampleSize <- ceiling(designEffect * rawSampleSize)

    formatPower     <- paste(input$inputPowerA * 100, "%", sep = "")
    formatAlpha     <- paste(input$alphaA * 100, "%", sep = "")
    formatAltHyp    <- switch(input$selectAlternativeA, "one.sided" = "one-sided ", "two.sided" = "two-sided ")
    formatResp      <- as.numeric(input$respA)

    validate(
      need(input$inputPowerA > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
      need(input$inputPowerA < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1.")
    )
    HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
         <p> We wish to find the sample size for a trial with a binary outcome where the probability of response to first-stage intervention is ", formatResp, sentenceCompilerA(),
         ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
         finalSampleSize, " to make this comparison with ", formatPower, " power. </p>", sep=""))
  })

  output$binaryPowerA <- renderUI({
    validate(
      need(input$inputSampleSizeA != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")
    )

    A <- dataCompilerA()[1] / (1 - dataCompilerA()[1])
    B <- (dataCompilerA()[2] / (1 - dataCompilerA()[2])) / A

    designEffect  <- 2
    powerQuantile <- sqrt((input$inputSampleSizeA * A * B * log(B) ^ 2) / (2 * designEffect * ((1 + A) ^ 2 * B + (1 + A*B) ^ 2))) + qnorm(input$alphaA / 2)
    finalPower    <- round(pnorm(powerQuantile), digits = 3)
    formatPower   <- paste(finalPower   * 100, "%", sep = "")
    formatAlpha   <- paste(input$alphaA * 100, "%", sep = "")
    formatSize    <- as.numeric(input$inputSampleSizeA)
    formatResp    <- as.numeric(input$respA)

    validate(
      need(input$inputSampleSizeA >= 1, paste("The provided sample size is not large enough to yield a trial in which at least
        one person is consistent with each AI. Sample size must be at least 8 to proceed."))
    )
    HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
         <p> For a trial of size N=",formatSize," with a binary outcome where the probability of response to first-stage intervention is ", formatResp,
               sentenceCompilerA(),", we have at least ",formatPower," power. </p>", sep = ""))
  })

  output$continuousSampleSizeA <- renderUI({
    alt.hyp       <- switch(input$selectAlternativeA,"one.sided" = "greater")
    designEffect  <- 4
    rawSampleSize <- try(pwr.norm.test(d = dataCompilerA(), sig.level = input$alphaA, power = input$inputPowerA, alternative = alt.hyp)$n, silent = T)

    validate(
      need(input$inputPowerA > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
      need(input$inputPowerA < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1."),
      need(dataCompilerA()   > 0, "Sample size is indeterminate for an effect size of 0. Please specify a different effect size.") %then%
        need(is.numeric(rawSampleSize), "Given the provided effect size, fewer than 8 individuals are required to achieve the
             desired power. This is not enough individuals to run a SMART. You can test for a smaller effect size, or increase
             the desired power.")
    )

    finalSampleSize <- ceiling(2 * designEffect * rawSampleSize)
    formatPower <- paste(input$inputPowerA * 100, "%" , sep = "")
    formatAlpha <- paste(input$alphaA      * 100, "%" , sep = "")
    formatAltHyp <- switch(input$selectAlternativeA, "one.sided" = "one-sided ", "two.sided" = "two-sided ")
    formatResp <- as.numeric(input$respA)

    HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
         <p> We wish to find the sample size for a trial with a continuous outcome where the probability of response to first-stage intervention is ", formatResp, sentenceCompilerA(),
               ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
               finalSampleSize, " to make this comparison with ",formatPower," power. </p>",sep = ""))
  })

  output$continuousPowerA <- renderUI({
    validate(
      need(input$inputSampleSizeA != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")
    )

    alt.hyp <- switch(input$selectAlternativeA, 'one.sided' = 'greater')
    designEffect <- 4
    size <- (input$inputSampleSizeA/(2*designEffect))

    validate(
      need(size >= 1, paste("The provided sample size is not large enough to yield a trial in which at least one
                            person is consistent with each DTR. Sample size must be at least 8 to proceed."))
    )

    finalPower  <- round(pwr.norm.test(d = dataCompilerA(), sig.level = input$alphaA, n = size, alternative = alt.hyp)$power, digits = 3)
    formatPower <- paste(finalPower   * 100, "%", sep = "")
    formatAlpha <- paste(input$alphaA * 100, "%", sep = "")
    formatSize  <- as.numeric(input$inputSampleSizeA)
    formatResp  <- as.numeric(input$respA)


    HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
         <p> For a trial of size N=",formatSize," with a continuous outcome where the probability of response to first-stage intervention is ",formatResp,
               sentenceCompilerA(),", we have ",formatPower," power. </p>",sep=""))
  })



  ##### DESIGN B #####

  ##### B HEADER #####

  ### Render the design image which highlights selected DTRs
  ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
  ### Note that this requires a specific naming convention for image assets
      ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0.

  output$designBimg <- renderImage({
       filename <- normalizePath(file.path('./www/images',paste('SMARTdesignB_',input$firstDTRcompareB,'_',input$secondDTRcompareB,'.gif',sep='')))
       list(src  = filename)
   }, deleteFile = FALSE)

  ### Render 'selectize' dropdown boxes with placeholder text for AI selection.
    ### Second DTR input populates with all DTRs that are NOT the first DTR--eliminates ability to select same DTR twice
    ### Placed in server.R rather than ui.R because of dependency on first DTR selection

  output$selectAI1B <- renderUI({
    AI <- selectizeInput("firstDTRcompareB", label = "Reference Adaptive Intevention",
                       choices = designB.DTRs,
                       options = list(
                         placeholder = 'Please select a Reference AI.',
                         onInitialize = I('function() { this.setValue(0); }')
                       )
          )
    return(AI)
  })

  output$selectAI2B <- renderUI({
    AI <- selectizeInput("secondDTRcompareB",label = "Comparison Adaptive Intervention",
                     choices = designB.DTRs[substr(designB.DTRs, 1, 1) != substr(input$firstDTRcompareB, 1, 1)],
                     options = list(
                       placeholder = 'Please select a Comparison AI.',
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
      need(input$firstDTRcompareB, "Please select a Reference AI.")
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
      bsTooltip(id = "marginalFirstStageB2",    title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                placement = "right", trigger = "focus"),
      bsTooltip(id = "marginalSecondStageNRB2", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                placement = "right", trigger = "focus"))
  })

  # Render enabled/disabled numericInputs when outcome is continuous

  generateContinuousInputB <- reactive({
    validate(
      need(input$firstDTRcompareB,  "Please select a Reference AI."),
      need(input$secondDTRcompareB, "Please select a Comparison AI.")
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
                        choices=designC.DTRs,
                        options = list(
                          placeholder = 'Please select a Reference AI.',
                          onInitialize = I('function() { this.setValue(0); }')
                        )
   )
   return(AI)
 })

 output$selectAI2C <- renderUI({
   AI <- selectizeInput("secondDTRcompareC",label="to AI",
                        choices = designC.DTRs[substr(designC.DTRs, 1, 1) != substr(input$firstDTRcompareC, 1, 1)],
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
       need(!is.null(input$DTRsuccC1), "Select a Reference AI above."),
       need(!is.null(input$DTRsuccC2), "Select a Comparison AI above.") %then%
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
              finalSampleSize, " to make this comparison with at least ",formatPower," power. </p>",sep=""))
 })

 output$continuousPowerC <- renderUI({
   alt.hyp<-switch(input$selectAlternativeC,"one.sided"="greater")
   validate(
     need(input$inputSampleSizeC!=0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")
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
})
