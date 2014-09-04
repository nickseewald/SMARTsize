##### SERVER.R FOR SMART SAMPLE SIZE CALCULATOR #####
### NICK SEEWALD, 2014
### UNIVERSITY OF MICHIGAN
### DEPARTMENT OF BIOSTATISTICS

library(shiny)
options(encoding='UTF-8')

##### NON-REACTIVE FUNCTION DECLARATIONS #####

### Force package installation/loading on running device
force.package<-function(package){
  #adapted from 
  #http://r.789695.n4.nabble.com/Install-package-automatically-if-not-there-tp2267532p2267659.html
  package<-as.character(substitute(package))
  if (package %in% .packages(all.available=TRUE)){
    eval(parse(text=paste("require(",package,")", sep="")))
  }
  else {
    eval(parse(text=paste("install.packages('",package,"')",sep="")))
    eval(parse(text=paste("require(",package,")", sep="")))
  }
}

### Function creates disabled (greyed-out) inputs
### Taken from https://groups.google.com/d/msg/shiny-discuss/uSetp4TtW-s/Jktu3fS60RAJ
disable <- function(x) {
  if (inherits(x, 'shiny.tag')) {
    if (x$name %in% c('input', 'select'))
      x$attribs$disabled <- 'disabled'
    x$children <- disable(x$children)
  }
  else if (is.list(x) && length(x) > 0) {
    for (i in 1:length(x))
      x[[i]] <- disable(x[[i]])
  }
  x
}

### Function evaluates full-DTR probabilities; not reactive
fullDTRprob <- function(cell1, resp, cell2){
  pDTR <- cell1 * resp + cell2 * (1-resp)
  return(pDTR)
}

### Create operator to sequentially evaluate need() statements
`%then%` <- shiny:::`%OR%`

##### NON-REACTIVE INITIALIZATIONS ######

force.package(shiny)
force.package(pwr)
force.package(shinyBS)

### Create vectors of all embedded DTRs for each design
designA.DTRs <- c("ArCnrE", "ArCnrF", "ArDnrE", "ArDnrF", "BrGnrI", "BrGnrJ", "BrHnrI", "BrHnrJ")
designB.DTRs <- c("ArCnrD", "ArCnrE", "BrFnrG", "BrFnrH")
designC.DTRs <- c("ArCnrD", "ArCnrE", "BrFnrG")
designD.DTRs <- c("AC", "AD", "BE", "BF")

### Start server operation

shinyServer(function(input,output,session){
  
  ##### HOME #####
  ### Watch for clicks on pickTab actionButtons rendered under design diagrams
  ### On click, redirect to appropriate tab. (More intuitive navigation structure)
  
  observe({
    if(input$pickTabA) updateTabsetPanel(session,"SMARTsize",selected="Design A")
    if(input$pickTabB) updateTabsetPanel(session,"SMARTsize",selected="Design B")
    if(input$pickTabC) updateTabsetPanel(session,"SMARTsize",selected="Design C")
    if(input$pickTabD) updateTabsetPanel(session,"SMARTsize",selected="Design D")
  })
  
  ##### DESIGN A #####
  
  ##### A HEADER #####
  
  ### Render the design image which highlights selected DTRs
  ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
  ### Note that this requires a very specific naming convention for image assets
  ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0. 
  
  output$designAimg <- renderImage({
    filename<-filename<-normalizePath(file.path('./www/images',paste('SMARTdesignA_',input$firstDTRcompareA,'_',input$secondDTRcompareA,'.gif',sep='')))
    list(src=filename)
  },deleteFile=FALSE)
  
  ### Render 'selectize' dropdown boxes with placeholder text for AI selection.
  ### Second DTR input populates with all DTRs that are NOT the first DTR--eliminates ability to select same DTR twice
  ### Placed in server.R rather than ui.R because of dependency on first DTR selection
  
  output$selectAI1A <- renderUI({
    AI <- selectizeInput("firstDTRcompareA",label="Compare AI",
                         choices=designA.DTRs,
                         options = list(
                           placeholder = 'Please select a first AI.',
                           onInitialize = I('function() { this.setValue(0); }')
                         )
    )
    return(AI)
  })
  
  output$selectAI2A <- renderUI({ 
    AI <- selectizeInput("secondDTRcompareA",label="to AI",
                         choices=setdiff(designA.DTRs,input$firstDTRcompareA),
                         options = list(
                           placeholder = 'Please select a second AI.',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
    )
    return(AI)
  })  
  
  output$printer <- renderText({
    resp<-as.numeric(input$resptest)
    resp4 <- resp*4
    return(resp4)
    })
  
  ##### DESIGN A PROBABILITY INPUT #####
  
  ### Read in DTR names from dropdowns and parse them to determine first and second stage treatments
  ### Reactive function allows on-the-fly changes in return values with changes in selection
  ### Outputs full DTR name, first-stage treatment (first character), second-stage treatment if response (third character),
  ### and second-stage treatment if non-response(last character)
  ### NOTE that these positions are exclusive to design A because responders have two second-stage treatment options
  
  substringDTR1A <- reactive({
    if(length(input$firstDTRcompareA) > 0){
      DTR1 <- paste(input$firstDTRcompareA)
      firstStage1 <- substr(DTR1,1,1)
      secondStageR1 <- substr(DTR1,3,3)
      secondStageNR1 <- substr(DTR1,6,6)
      return(c(DTR1, firstStage1, secondStageR1, secondStageNR1))
    }
    else{
      return(c(0,0,0,0))
    }   
  })
  
  substringDTR2A <- reactive({
    if(length(input$secondDTRcompareA) >0){
      DTR2 <- paste(input$secondDTRcompareA)
      firstStage1 <- substr(DTR2,1,1)
      secondStageR1 <- substr(DTR2,3,3)
      secondStageNR1 <- substr(DTR2,6,6)
      return(c(DTR2, firstStage1, secondStageR1, secondStageNR1))
    }
    else{
      return(c(0,0,0,0))
    }
  })
  
  #When a first DTR is selected, render an input box corresponding to whatever input method is selected.
  # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR1), enabled/disabled depending on cellOrConditionalB
  # For target-difference or OR, relevant numericInputs are rendered
  # This is the ONLY location in which difference and OR numericInputs are built.
  
  generateBinaryInputs1A <- reactive({
    validate(
      need(input$firstDTRcompareA, "Please select a first AI.")
    )
    if(input$targetDiffCheckA==FALSE && input$targetOddsCheckA==FALSE){
      if(input$cellOrConditionalA==TRUE){
        return(disable(numericInput("DTRsuccA1disable",label="Probability of Success for First AI",value=0,min=0,max=1,step=0.01)))
      }
      else {
        output <- c(numericInput("DTRsuccA1",label="Probability of Success for First AI",value=0,min=0,max=1,step=0.01),
                    bsTooltip(id="DTRsuccA1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
        return(output)
      }
    }
    
    if(input$targetDiffCheckA==TRUE && input$targetOddsCheckA==FALSE && substringDTR2A()[1] != 0){
      return(c(numericInput("targetDiffA",label="Target Difference in Success Probabilities",value=0.1,min=0.01,max=0.5,step=0.01),
               bsTooltip(id="targetDiffA",title="Input can range from 0-0.5 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
               ))
    }
    
    if(input$targetOddsCheckA==TRUE && substringDTR2A()[1] != 0){
      return(c(numericInput("targetORA",label="Target Odds Ratio of Success",value=2,min=0,step=0.01),
               bsTooltip(id="targetORA",title="Input must be positive and in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
               ))
    }
    
    if(input$targetDiffCheckA==TRUE && substringDTR2A()[1] == 0){
      return()
    }
  })
  
  # When a second DTR is selected, render an input box corresponding to whatever input method is selected.
  # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR2), enabled/disabled depending on cellOrConditionalB
  # For target-difference or OR, numericInputs are NOT rendered (those are handled in output$binaryDTR1probB)
  
  generateBinaryInputs2A <- reactive({
    validate(
      need(input$secondDTRcompareA, "Please select a second AI.")
    )
    if(input$targetDiffCheckA==FALSE && input$targetOddsCheckA==FALSE){
      if(input$cellOrConditionalA==TRUE){
        return(disable(numericInput("DTRsuccA2disable",label="Probability of Success for Second AI",value=0,min=0,max=1,step=0.01)))
      }
      else{
        output <- c(numericInput("DTRsuccA2",label="Probability of Success for Second AI",value=0,min=0,max=1,step=0.01), 
                    bsTooltip(id="DTRsuccA2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
        return(output)
      }
    }
  })
  
  ### Render UI components generated above
  
  output$binaryDTR1probA <- renderUI({
    generateBinaryInputs1A()
  })
  
  output$binaryDTR2probA <- renderUI({
    generateBinaryInputs2A()
  })
  
  ### For cell-specific probabilities, render a series of numericInputs labeled by information from DTR substrings
  ### When DTR1 and DTR2 begin with the same treatment, P(S|stage1trt,r) is rendered only once, in output$cellProbsDTR1B
  
  output$cellProbsDTR1A <- renderUI({
    controlInputs<-c(
           numericInput("marginalFirstStageA1",label=paste("Probability of success for Path ",substringDTR1A()[2],"r",substringDTR1A()[3],sep=""),value=0,min=0,max=1,step=0.01),
           numericInput("marginalSecondStageNRA1",label=paste("Probability of success for Path ",substringDTR1A()[2],"nr",substringDTR1A()[4],sep=""), value=0,min=0,max=1,step=0.01),
           bsTooltip(id="marginalFirstStageA1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
           bsTooltip(id="marginalSecondStageNRA1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
    )
  })
  
  output$cellProbsDTR2A <- renderUI({
    if(substringDTR1A()[2]==substringDTR2A()[2] && substringDTR1A()[3]==substringDTR2A()[3] && substringDTR1A()[4]!=substringDTR2A()[4]){
      controlInputs<-c(numericInput("marginalSecondStageNRA2",label=paste("Probability of success for Path ",substringDTR1A()[2],"nr",substringDTR2A()[4],sep=""),value=0,min=0,max=1,step=0.01),
                       bsTooltip(id="marginalSecondStageNRA2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
      )
    }
        
    else if(substringDTR1A()[2]==substringDTR2A()[2] && substringDTR1A()[3]!=substringDTR2A()[3] && substringDTR1A()[4]==substringDTR2A()[4]){
      controlInputs<-c(numericInput("marginalSecondStageNRA2",label=paste("Probability of success for Path ",substringDTR2A()[2],"r",substringDTR2A()[3],sep=""),value=0,min=0,max=1,step=0.01),
                       bsTooltip(id="marginalSecondStageNRA2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
      )
    }
    
    else{
      controlInputs<-c(
        numericInput("marginalFirstStageA2",label=paste("Probability of success for Path ",substringDTR2A()[2],"r", substringDTR2A()[3],sep=""),value=0,min=0,max=1,step=0.01),
        numericInput("marginalSecondStageNRA2",label=paste("Probability of success for Path ",substringDTR2A()[2],"nr",substringDTR2A()[4],sep=""),value=0,min=0,max=1,step=0.01),
        bsTooltip(id="marginalFirstStageA2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
        bsTooltip(id="marginalSecondStageNRA2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
      )
    }
    controlInputs
  })
  
  ### Render enabled/disabled numericInputs when outcome is continuous
  
  generateContinuousInputA <- reactive({
    validate(
      need(input$firstDTRcompareA, "Please select a first AI."),
      need(input$secondDTRcompareA, "Please select a second AI.")
    )
    return(c(numericInput("effectSizeA",label="Standardized Effect Size",value=0,min=0,max=10,step=0.01),
             bsTooltip(id="effectSizeA",title="Input can range from 0-10 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
    )
  })
  
  output$continuousProbA<-renderUI({
    generateContinuousInputA()
  })
  
  output$meanEstA <- renderUI({
    contInput<-c(numericInput("mean1A",label="Mean outcome for first AI",value=0,step=0.01),
                 numericInput("mean2A",label="Mean outcome for second AI",value=0,step=0.01),
                 numericInput("sdDiffA",label="Standard error of the difference in above means:", value=0, min=0, step=0.01),
                 bsTooltip(id="mean1A",title="Input must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                 bsTooltip(id="mean2A",title="Input must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                 bsTooltip(id="sdDiffA",title="Input must be positive and in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
    contInput
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
    }
  },priority=2) 
   
  ##### DESIGN A RESULT BACKEND #####
  
  ### Compute full DTR probabilities or effect size when providing cell-specific probabilities or mean/SD 
  
  generateProbsA <- reactive({
    if(input$selectOutcomeA==1 && substringDTR1A()[2]==substringDTR2A()[2] && substringDTR1A()[3]==substringDTR2A()[3] && input$cellOrConditionalA==TRUE){
      pDTR1 <- fullDTRprob(input$marginalFirstStageA1,input$respA,input$marginalSecondStageNRA1)
      pDTR2 <- fullDTRprob(input$marginalFirstStageA1,input$respA,input$marginalSecondStageNRA2)
    }
    
    else if (input$selectOutcomeA==1 && substringDTR1A()[2]!=substringDTR2A()[2] && substringDTR1A()[3]!=substringDTR2A()[3] && input$cellOrConditionalA==TRUE){
      pDTR1 <- fullDTRprob(input$marginalFirstStageA1,input$respA,input$marginalSecondStageNRA1)
      pDTR2 <- fullDTRprob(input$marginalFirstStageA2,input$respA,input$marginalSecondStageNRA2)
    }
    else if (input$selectOutcomeA==1 && input$cellOrConditionalA==FALSE){
      pDTR1 <- input$DTRsuccA1
      pDTR2 <- input$DTRsuccA2
    }
    
    return(c(pDTR1,pDTR2))
  })  
  
  ### Make sure input probabilities are valid (i.e., check for blank entries)
  
  checkDTRinputsA <- reactive({
    test<-findInterval(c(input$DTRsuccA1, input$DTRsuccA2),c(0,1))
    if (test[1]==1 && test[2]==1)
      return(TRUE)
    else
      return(FALSE)
  })
  
  ### Determine which inputs are being given, check their quality, then pass the appropriate arguments for power.prop.test or pwr.norm.test
    ### e.g., if target odds-ratio is given, compute a DTR success probability assuming the other is 0.5
  dataCompilerA <- reactive({
    
    ### Error Check: unselected DTRs, blank/invalid response probability
    validate(
      need(!(is.na(input$respA)), "Please provide a response probability. If unknown, enter 0 for a conservative estimate of power or sample size.") %then%
        need(0<=input$respA && input$respA<=1, "The provided response probability is not a valid probability. Please enter a value between 0 and 1.")
    )
    
    ### Binary outcome, DTR-specific success probabilities
    if(input$selectOutcomeA==1 && input$cellOrConditionalA==FALSE && input$targetDiffCheckA==FALSE && input$targetOddsCheckA==FALSE){
      ### Error Check: unselected DTRs, blank success probabilities, equal success probabilities, invalid success probabilities
      validate(
        need(!is.null(input$DTRsuccA1), "Select a first AI above."),
        need(!is.null(input$DTRsuccA2), "Select a second AI above.") %then%
          need(!is.na(input$DTRsuccA1) && !is.na(input$DTRsuccA2), "The success probability is missing for at least one AI. Please provide a numeric input.") %then%
          need(input$DTRsuccA1 != input$DTRsuccA2, "Please provide unique success probabilities for each AI. Sample size is indeterminate for equal AI probabilities.") %then%
          need(checkDTRinputsA(), "The provided success probability for at least one AI is not a valid probability. Please enter a value between 0 and 1.")
      )
      return(c(input$DTRsuccA1,input$DTRsuccA2))
    }
    
    ### Binary outcome, cell-specific success probabilities 
    if(input$selectOutcomeA==1 && input$cellOrConditionalA==TRUE && input$targetDiffCheckA==FALSE && input$targetOddsCheckA==FALSE){  
      ### Error Check: equal DTR-specific success probabilities
      validate(
        need(generateProbsA()[1] != generateProbsA()[2], "The provided cell-specific probabilities yield identical overall AI-specific probabilities of success. Sample size is indeterminate for equal AI probabilities. Please adjust your inputs.")
      )
      return(c(generateProbsA()[1],generateProbsA()[2]))
    }
    
    ### Binary outcome, target difference in success probabilities
    if(input$selectOutcomeA==1 && input$cellOrConditionalA==FALSE && input$targetDiffCheckA==TRUE && input$targetOddsCheckA==FALSE){
      ### Error Check: invalid target difference (must be less than 0.5 since we're using a conservative reference probability)
      validate(
        need(input$targetDiffA <= 0.5, "Target difference must be less than 0.5 to be valid input"),
        need(input$targetDiffA > 0, "Target difference must be greater than 0. Sample size is indeterminate for equal AI probabilities.
             Please adjust your inputs.")
        )
      return(c(0.5,0.5+input$targetDiffA))
    }
    
    ### Binary outcome, target odds ratio of success
    if(input$selectOutcomeA==1 && input$cellOrConditionalA==FALSE && input$targetDiffCheckA==FALSE && input$targetOddsCheckA==TRUE){
      ### Error check: missing/invalid odds ratio
      validate(
        need(is.numeric(input$targetORA),"Please enter an odds ratio.") %then%
        need(input$targetORA != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio.") %then%
        need(input$targetORA != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
      )
      return(c(0.5,input$targetORA/(1+input$targetORA)))
    }
    
    ### Continuous outcome, standardized effect size
    if(input$selectOutcomeA==2){
      ### Error check: nonzero effect size
      validate(
        need(input$firstDTRcompareA, "Select a first AI above."),
        need(input$secondDTRcompareA, "Select a second AI above."),
        need(!is.null(input$effectSizeA)," ") %then%
          need(!is.na(input$effectSizeA),"The standardized effect size is missing. Please enter a value between 0 and 10.") %then%
            need(input$effectSizeA != 0, "Sample size is indeterminate for an effect size of 0. Please enter a different target effect size.")
        
      )
      return(input$effectSizeA)
    }
  
  })
  
  ### Compute the "design effect" for design A. Varies based on whether DTRs are separate- or shared-path
  selectEffectA <- reactive({
    if(substringDTR1A()[2] == substringDTR2A()[2] 
       && substringDTR1A()[3] == substringDTR2A()[3]
       && substringDTR1A()[4] != substringDTR2A()[4]
       ){
      validate(
        need(!is.na(input$respA),"Please provide a response probability.") %then%
        need(input$respA < 1, "The selected AIs cannot be compared if the probability of response is 1. Please enter a smaller response probability.")
      )
      designEffect <- 4/(1-input$respA)
    }
    else if (substringDTR1A()[2] == substringDTR2A()[2] 
       && substringDTR1A()[3] != substringDTR2A()[3]
       && substringDTR1A()[4] == substringDTR2A()[4]){
      validate(
        need(!is.na(input$respA),"Please provide a response probability.") %then%
        need(input$respA > 0, "The selected AIs cannot be compared if the probability of response is 0. Please enter a non-zero response probability.")
        )
      designEffect <- 4/(input$respA)
    }
    else{
      designEffect <- 4
    }
    return(designEffect)
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
    designEffect<-selectEffectA()
    rawSampleSize<-power.prop.test(p1=dataCompilerA()[1],p2=dataCompilerA()[2],power=input$inputPowerA,sig.level=input$alphaA,alternative=c(input$selectAlternativeA))$n
    finalSampleSize<-ceiling(designEffect * rawSampleSize)
    formatPower<-paste(input$inputPowerA*100,"%",sep="")
    formatAlpha<-paste(input$alphaA*100,"%",sep="")
    formatAltHyp <- switch(input$selectAlternativeA, "one.sided"="one-sided ", "two.sided"="two-sided ")
    formatResp <- as.numeric(input$respA)
    
    validate(
      need(input$inputPowerA > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
      need(input$inputPowerA < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1.")
    )
    HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
         <p> We wish to find the sample size for a trial with a binary outcome where the probability of response to first-stage intervention is ", formatResp, sentenceCompilerA(),
         ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
         finalSampleSize, " to make this comparison with ",formatPower," power. </p>",sep=""))
  })
  
  output$binaryPowerA <- renderUI({
    validate(
      need(input$inputSampleSizeA != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
    )
    designEffect<-selectEffectA()
    size<-(input$inputSampleSizeA/designEffect)
    finalPower<-round(power.prop.test(p1=dataCompilerA()[1],p2=dataCompilerA()[2],n=size,sig.level=input$alphaA,alternative=c(input$selectAlternativeA))$power,digits=3)
    formatPower<-paste(finalPower*100,"%",sep="")
    formatAlpha<-paste(input$alphaA*100,"%",sep="")
    formatSize <- as.numeric(input$inputSampleSizeA)
    formatResp <- as.numeric(input$respA)
    validate(
      need(size >= 1, paste("The provided sample size is not large enough to yield a trial in which at least one person is consistent with each DTR.", 
                            "Sample size must be at least",ceiling(designEffect),"to proceed."))
    )
    HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
         <p> For a trial of size N=",formatSize," with a binary outcome where the probability of response to first-stage intervention is ",formatResp,
               sentenceCompilerA(),", we have at least ",formatPower," power. </p>",sep=""))
  })
  
  output$continuousSampleSizeA <- renderUI({
    alt.hyp<-switch(input$selectAlternativeA,"one.sided"="greater")
    designEffect<-selectEffectA()
    rawSampleSize<-try(pwr.norm.test(d=dataCompilerA(),sig.level=input$alphaA,power=input$inputPowerA,alternative=alt.hyp)$n,silent=T)
    
    validate(
      need(input$inputPowerA > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
      need(input$inputPowerA < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1."),
      need(dataCompilerA()>0,"Sample size is indeterminate for an effect size of 0. Please specify a different effect size.") %then%
        need(is.numeric(rawSampleSize),paste("Given the provided effect size, fewer than",2*designEffect,"individuals are required to achieve the desired power.",
                                             "This is not enough individuals to run a SMART. You can test for a smaller effect size, or increase the desired power."))
    )
    
    finalSampleSize<-ceiling(2*designEffect*rawSampleSize)
    formatPower<-paste(input$inputPowerA*100,"%",sep="")
    formatAlpha<-paste(input$alphaA*100,"%",sep="")
    formatAltHyp<-switch(input$selectAlternativeA, "one.sided"="one-sided ", "two.sided"="two-sided ")
    formatResp<-as.numeric(input$respA)
        
    HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
         <p> We wish to find the sample size for a trial with a continuous outcome where the probability of response to first-stage intervention is ", formatResp, sentenceCompilerA(),
               ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
               finalSampleSize, " to make this comparison with ",formatPower," power. </p>",sep=""))
  })
  
  output$continuousPowerA <- renderUI({
    validate(
      need(input$inputSampleSizeA != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
    )
    
    alt.hyp<-switch(input$selectAlternativeA, 'one.sided'='greater')
    designEffect<-selectEffectA()
    size<-(input$inputSampleSizeA/(2*designEffect)) 
    
    validate(
      need(size >= 1, paste("The provided sample size is not large enough to yield a trial in which at least one person is consistent with each DTR.", 
                            "Sample size must be at least",ceiling(2*designEffect),"to proceed."))
    )
    
    finalPower<-round(pwr.norm.test(d=dataCompilerA(),sig.level=input$alphaA,n=size,alternative=alt.hyp)$power,digits=3)
    formatPower<-paste(finalPower*100,"%",sep="")
    formatAlpha<-paste(input$alphaA*100,"%",sep="")
    formatSize<-as.numeric(input$inputSampleSizeA)
    formatResp<-as.numeric(input$respA)
    
        
    HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
         <p> For a trial of size N=",formatSize," with a continuous outcome where the probability of response to first-stage intervention is ",formatResp,
               sentenceCompilerA(),", we have ",formatPower," power. </p>",sep=""))
  })
  
  
  
  ##### DESIGN B #####
  
  ##### B HEADER #####
  
  ### Render the design image which highlights selected DTRs
  ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
  ### Note that this requires a very specific naming convention for image assets
      ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0. 
  
  output$designBimg <- renderImage({
       filename<-filename<-normalizePath(file.path('./www/images',paste('SMARTdesignB_',input$firstDTRcompareB,'_',input$secondDTRcompareB,'.gif',sep='')))
       list(src=filename)
   },deleteFile=FALSE)

  ### Render 'selectize' dropdown boxes with placeholder text for AI selection.
    ### Second DTR input populates with all DTRs that are NOT the first DTR--eliminates ability to select same DTR twice
    ### Placed in server.R rather than ui.R because of dependency on first DTR selection
  
  output$selectAI1B <- renderUI({
    AI <- selectizeInput("firstDTRcompareB",label="Compare AI",
                       choices=designB.DTRs,
                       options = list(
                         placeholder = 'Please select a first AI.',
                         onInitialize = I('function() { this.setValue(0); }')
                       )
          )
    return(AI)
  })
  
  output$selectAI2B <- renderUI({ 
    AI <- selectizeInput("secondDTRcompareB",label="to AI",
                     choices=setdiff(designB.DTRs,input$firstDTRcompareB),
                     options = list(
                       placeholder = 'Please select a second AI.',
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
    if(length(input$firstDTRcompareB) > 0){
      DTR1 <-paste(input$firstDTRcompareB)
      firstStage1<-substr(DTR1,1,1)
      secondStageR1<-substr(DTR1,3,3)
      secondStageNR1<-substr(DTR1,6,6)
      return(c(DTR1, firstStage1,secondStageR1, secondStageNR1))
    }
    else{
      return(c(0,0,0))
    }   
  })
  
  substringDTR2B <- reactive({
    if(length(input$secondDTRcompareB) >0){
      DTR2 <-paste(input$secondDTRcompareB)
      firstStage2<-substr(DTR2,1,1)
      secondStageR2<-substr(DTR2,3,3)
      secondStageNR2<-substr(DTR2,6,6)
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
      need(input$firstDTRcompareB, "Please select a first AI.")
    )
    if(input$targetDiffCheckB==FALSE && input$targetOddsCheckB==FALSE){
      if(input$cellOrConditionalB==TRUE){
        return(disable(numericInput("DTRsuccB1disable",label="Probability of Success for First AI",value=0,min=0,max=1,step=0.01)))
      }
      else
        return(c(numericInput("DTRsuccB1",label="Probability of Success for First AI",value=generateProbsB()[1],min=0,max=1,step=0.01),
                 bsTooltip(id="DTRsuccB1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
        )
    }
    
    if(input$targetDiffCheckB==TRUE && input$targetOddsCheckB==FALSE){
      return(c(numericInput("targetDiffB",label="Target Difference in Success Probabilities",value=0.1,min=0.01,max=0.5,step=0.01),
               bsTooltip(id="targetDiffB",title="Input can range from 0-0.5 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
      )
    }
    
    if(input$targetOddsCheckB==TRUE){
      return(c(numericInput("targetORB",label="Target Odds Ratio of Success",value=2,min=0,step=0.01),
               bsTooltip(id="targetORB",title="Input must be positive and in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
      )
    }
    
    if(input$targetDiffCheckB==TRUE && substringDTR2B()[1] == 0){
      return()
    }
  })
  
  # When a second DTR is selected, render an input box corresponding to whatever input method is selected.
  # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR2), enabled/disabled depending on cellOrConditionalB
  # For target-difference or OR, numericInputs are NOT rendered (those are handled in output$binaryDTR1probB)
  
  generateBinaryInputs2B <- reactive({
    validate(
      need(input$secondDTRcompareB, "Please select a second AI.")
    )
    if(input$targetDiffCheckB==FALSE && input$targetOddsCheckB==FALSE && substringDTR1B()[1] != substringDTR2B()[1]){
      if(input$cellOrConditionalB==TRUE){
        return(disable(numericInput("DTRsuccB2disable",label="Probability of Success for Second AI",value=0,min=0,max=1,step=0.01)))
      }
      else
        return(c(numericInput("DTRsuccB2",label="Probability of Success for Second AI",value=generateProbsB()[2],min=0,max=1,step=0.01),
                 bsTooltip(id="DTRsuccB2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
        )
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
    validate(
      need(substringDTR1B()[1] != 0, "Cell-specific probabilities cannot be input until a first AI is selected.")
    )
    if(substringDTR1B()[1] != substringDTR2B()[1]){
        controlInputs<-c(numericInput("marginalFirstStageB1",label=paste("Probability of success for Path ",substringDTR1B()[2],"r",substringDTR1B()[3],sep=""),value=0,min=0,max=1,step=0.01),
                         numericInput("marginalSecondStageNRB1",label=paste("Probability of success for Path ",substringDTR1B()[2],"nr",substringDTR1B()[4],sep=""),
                                      value=0,min=0,max=1,step=0.01),
                         bsTooltip(id="marginalFirstStageB1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                         bsTooltip(id="marginalSecondStageNRB1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
        )
    }
    
  })
  
  output$cellProbsDTR2B <- renderUI({
    if(substringDTR1B()[2]==substringDTR2B()[2]){
        controlInputs<-c(numericInput("marginalSecondStageNRB2",label=paste("Probability of success for Path ",substringDTR2B()[2],"nr",substringDTR2B()[4],,sep=""),
                         value=0,min=0,max=1,step=0.01),
                         bsTooltip(id="marginalSecondStageNRB2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
        )
      }
    else{
        controlInputs<-c(
                         numericInput("marginalFirstStageB2",label=paste("Probability of success for Path ",substringDTR2B()[2],"r",substringDTR2B()[3],sep=""),value=0,min=0,max=1,step=0.01),
                         numericInput("marginalSecondStageNRB2",label=paste("Probability of success for Path ",substringDTR2B()[2],"nr",substringDTR2B()[4],sep=""),
                                      value=0,min=0,max=1,step=0.01),
                         bsTooltip(id="marginalFirstStageB2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                         bsTooltip(id="marginalSecondStageNRB2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
        )
    }
    controlInputs
  })
  
  # Render enabled/disabled numericInputs when outcome is continuous
  
  generateContinuousInputB <- reactive({
    validate(
      need(input$firstDTRcompareB, "Please select a first AI."),
      need(input$secondDTRcompareB, "Please select a second AI.")
    )
    return(c(numericInput("effectSizeB",label="Standardized Effect Size",value=0,min=0,max=10,step=0.01),
               bsTooltip(id="effectSizeB",title="Input can range from 0-10 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
    )
  })
  
  output$continuousProbB<-renderUI({
    generateContinuousInputB()
  })
  
  output$meanEstB <- renderUI({
    contInput<-c(numericInput("mean1B",label="Mean outcome for first AI",value=0,step=0.01),
                 numericInput("mean2B",label="Mean outcome for second AI",value=0,step=0.01),
                 numericInput("sdDiffB",label="Standard error of the difference in above means:", value=0, min=0, step=0.01),
                 bsTooltip(id="mean1B",title="Input must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                 bsTooltip(id="mean2B",title="Input must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                 bsTooltip(id="sdDiffB",title="Input must be positive and in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
    )
    contInput
  })

  
  
  ##### DESIGN B OBSERVERS #####
  
  ### For binary outcome, disallow cell-specific input if target difference input is selected
  ### If target difference is not selected, also deselect target OR
  
  observe({    
    if(input$targetDiffCheckB){
      updateCheckboxInput(session,"cellOrConditionalB",value=FALSE)
      updateCheckboxInput(session,"targetOddsCheckB",value=FALSE)
    }
  })
 
  observe({
    if(input$targetOddsCheckB){
      updateCheckboxInput(session,"cellOrConditionalB",value=FALSE)
      updateCheckboxInput(session,"targetDiffCheckB",value=FALSE)
    }
  })
  
  ### If cell-specific input is selected, deselect target difference and OR options
  ### Update disabled full DTR success inputs with computed probabilities
  
  observe({
    if(input$cellOrConditionalB==TRUE){
      updateCheckboxInput(session,"targetDiffCheckB",value=FALSE)
      updateCheckboxInput(session,"targetOddsCheckB",value=FALSE)
      
      updateNumericInput(session,"DTRsuccB1disable",value=generateProbsB()[1])
      updateNumericInput(session,"DTRsuccB2disable",value=generateProbsB()[2])
    }
  },priority=2)
    
  ### Compute full DTR probabilities or effect size when providing cell-specific probabilities or mean/SD 
  
  generateProbsB <- reactive({
    if(input$selectOutcomeB==1 && substringDTR1B()[2]==substringDTR2B()[2] && input$cellOrConditionalB==TRUE){
      pDTR1 <- fullDTRprob(input$marginalFirstStageB1,input$respB,input$marginalSecondStageNRB1)
      pDTR2 <- fullDTRprob(input$marginalFirstStageB1,input$respB,input$marginalSecondStageNRB2)
    }
    
    else if (input$selectOutcomeB==1 && substringDTR1B()[2]!=substringDTR2B()[2] && input$cellOrConditionalB==TRUE){
      pDTR1 <- fullDTRprob(input$marginalFirstStageB1,input$respB,input$marginalSecondStageNRB1)
      pDTR2 <- fullDTRprob(input$marginalFirstStageB2,input$respB,input$marginalSecondStageNRB2)
    }
    else if (input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE){
      pDTR1 <- input$DTRsuccB1
      pDTR2 <- input$DTRsuccB2
    }
    else {
      pDTR1 <- 0
      pDTR2 <- 0
    }
    
    return(c(pDTR1,pDTR2))
  })

  ##### DESIGN B RESULT BACKEND #####
  
  # Based on provided input probabilities and selected options, compute appropriate arguments to pass to power.prop.test or pwr.norm.test
  
  checkDTRinputsB <- reactive({
    test<-findInterval(c(input$DTRsuccB1, input$DTRsuccB2),c(0,1))
    if (test[1]==1 && test[2]==1)
      return(TRUE)
    else
      return(FALSE)
  })
  
  dataCompilerB <- reactive({
    
    validate(
      need(!(is.na(input$respB)), "Please provide a response probability. If unknown, enter 0 for a conservative estimate of power or sample size.") %then%
      need(0<=input$respB && input$respB<=1, "The provided response probability is not a valid probability. Please enter a value between 0 and 1.")
    )
    
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE && input$targetDiffCheckB==FALSE && input$targetOddsCheckB==FALSE){
      validate(
          need(!is.null(input$DTRsuccB1) && !is.null(input$DTRsuccB2), "The success probability is missing for at least one AI. Please provide a numeric input.") %then%
          need(input$DTRsuccB1 != input$DTRsuccB2, "Please provide unique success probabilities for each AI. Sample size is indeterminate for equal AI probabilities.") %then%
          need(checkDTRinputsB(), "The provided success probability for at least one AI is not a valid probability. Please enter a value between 0 and 1.")
      )
      return(c(input$DTRsuccB1,input$DTRsuccB2))
    }
    
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==TRUE){  
      validate(
        need(generateProbsB()[1] != generateProbsB()[2], "The provided cell-specific probabilities yield identical AI-specific probabilities of success. Sample size is indeterminate for equal AI probabilities. Please adjust your inputs.")
      )
      
      return(c(generateProbsB()[1],generateProbsB()[2]))
     }
    
    if(input$selectOutcomeB==1 && input$targetDiffCheckB==TRUE){
      validate(
        need(input$targetDiffB <= 0.5, "Target difference must be less than 0.5 to be valid input"),
        need(input$targetDiffB > 0, "Target difference must be greater than 0. Sample size is indeterminate for equal AI probabilities.
             Please adjust your inputs.")
      )
      return(c(0.5,0.5+input$targetDiffB))
    }
    
    if(input$selectOutcomeB==1 && input$targetOddsCheckB==TRUE){
      validate(
        need(is.numeric(input$targetORB),"Please enter an odds ratio.") %then%
          need(input$targetORB != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio.") %then%
          need(input$targetORB != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
      )
      return(c(0.5,input$targetORB/(1+input$targetORB)))
    }
    
    if(input$selectOutcomeB==2){
      validate(
        need(input$firstDTRcompareB, "Select a first AI above."),
        need(input$secondDTRcompareB, "Select a second AI above.") %then%
        need(input$effectSizeB != 0, "Sample size is indeterminate for an effect size of 0. Please enter a different target effect size.")
      )
      
      return(input$effectSizeB)
    }  
  })
  
  # Compute the "design effect" for design B. Varies based on whether DTRs are separate- or shared-path

  selectEffectB <- reactive({
    if(substringDTR1B()[2] != substringDTR2B()[2]){
      designEffect <- 2*(2*(1-input$respB)+input$respB)
    }
    else{
      validate(
        need(input$respB < 1, "The selected AIs cannot be compared if the probability of response is 1. Please enter a smaller response probability.")
        )
      designEffect <- 4/(1-input$respB)
    }
    return(designEffect)
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
    designEffect<-selectEffectB()
    rawSampleSize<-power.prop.test(p1=dataCompilerB()[1],p2=dataCompilerB()[2],power=input$inputPowerB,sig.level=input$alphaB,alternative=c(input$selectAlternativeB))$n
    finalSampleSize<-ceiling(designEffect * rawSampleSize)
    formatPower<-paste(input$inputPowerB*100,"%",sep="")
    formatAlpha<-paste(input$alphaB*100,"%",sep="")
    formatAltHyp<-switch(input$selectAlternativeB, "one.sided"="one-sided ", "two.sided"="two-sided ")
    formatResp<-as.numeric(input$respB)
    
    HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize),"</h4>
         <p> We wish to find the sample size for a trial with a binary outcome where the probability of response to first-stage interventions is ",formatResp, sentenceCompilerB(),
               ". Given a ",formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
               finalSampleSize, " to make this comparison with ",formatPower," power. </p>",sep=""))
  })

  output$binaryPowerB <- renderUI({
    validate(
      need(input$inputSampleSizeB != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
    )
    designEffect<-selectEffectB()
    size<-(input$inputSampleSizeB/designEffect)
    
    validate(
      need(size >= 1, paste("The provided sample size is not large enough to yield a trial in which at least one person is consistent with each DTR.", 
                            "Sample size must be at least",ceiling(2*designEffect),"to proceed."))
    )
    
    finalPower<-round(power.prop.test(p1=dataCompilerB()[1],p2=dataCompilerB()[2],n=size,sig.level=input$alphaB,alternative=c(input$selectAlternativeB))$power,digits=3)
    formatPower<-paste(finalPower*100,"%",sep="")
    formatAlpha<-paste(input$alphaB*100,"%",sep="")
    formatSize<-as.numeric(input$inputSampleSizeB)
    formatResp<-as.numeric(input$respB)
    
    HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
         <p> For a trial of size N=",formatSize," with a binary outcome where the probability of response to first-stage interventions is ",formatResp,
               sentenceCompilerB(),", we have ",formatPower," power. ",sep=""))
  })
  
  output$continuousSampleSizeB <- renderUI({
    alt.hyp<-switch(input$selectAlternativeB,"one.sided"="greater")
    designEffect<-selectEffectB()
    rawSampleSize<-try(pwr.norm.test(d=dataCompilerB(),sig.level=input$alphaB,power=input$inputPowerB,alternative=alt.hyp)$n,silent=T)
    
    validate(
      need(input$inputPowerB > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
      need(input$inputPowerB < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1."),
      need(dataCompilerB()>0,"Sample size is indeterminate for an effect size of 0. Please specify a different effect size.") %then%
      need(is.numeric(rawSampleSize),paste("Given the provided effect size, fewer than",2*designEffect,"individuals are required to achieve the desired power.",
                                           "This is not enough individuals to run a SMART. You can test for a smaller effect size, or increase the desired power."))
    )
    
    finalSampleSize<-ceiling(2*designEffect*rawSampleSize)
    formatPower<-paste(input$inputPowerB*100,"%",sep="")
    formatAlpha<-paste(input$alphaB*100,"%",sep="")
    formatAltHyp<-switch(input$selectAlternativeB, "one.sided"="one-sided ", "two.sided"="two-sided ")
    formatResp<-as.numeric(input$respB)
    
    HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
         <p> We wish to find the sample size for a trial with a continuous outcome where the probability of response to first-stage interventions is ", formatResp, sentenceCompilerB(),
               ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
               finalSampleSize, " to make this comparison with ",formatPower," power. </p>",sep=""))
  })
  
  output$continuousPowerB <- renderUI({
    alt.hyp<-switch(input$selectAlternativeB,"one.sided"="greater")
    validate(
      need(input$inputSampleSizeB != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
    )
    designEffect<-selectEffectB()
    size<-(input$inputSampleSizeB/(2*designEffect))
    
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
                          placeholder = 'Please select a first AI.',
                          onInitialize = I('function() { this.setValue(0); }')
                        )
   )
   return(AI)
 })
 
 output$selectAI2C <- renderUI({ 
   AI <- selectizeInput("secondDTRcompareC",label="to AI",
                        choices=setdiff(designC.DTRs,input$firstDTRcompareC),
                        options = list(
                          placeholder = 'Please select a second AI.',
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
      DTR1 <-paste(input$firstDTRcompareC)
      firstStage1<-substr(DTR1,1,1)
      secondStageR1<-substr(DTR1,3,3)
      secondStageNR1<-substr(DTR1,6,6)
      return(c(DTR1, firstStage1,secondStageR1,secondStageNR1))
    }
    else{
      return(c(0,0,0))
    }   
  })
  
  substringDTR2C <- reactive({
    if(length(input$secondDTRcompareC) >0){
      DTR2 <-paste(input$secondDTRcompareC)
      firstStage2<-substr(DTR2,1,1)
      secondStageR2<-substr(DTR2,3,3)
      secondStageNR2<-substr(DTR2,6,6)
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
 
 generateBinaryInputs1C <- reactive({
   validate(
     need(input$firstDTRcompareC, "Please select a first AI.")
   )
   if(input$targetDiffCheckC==FALSE && input$targetOddsCheckC==FALSE){
     if(input$cellOrConditionalC==TRUE){
       return(disable(numericInput("DTRsuccC1disable",label="Probability of Success for First AI",value=0,min=0,max=1,step=0.01)))
     }
     else
       return(c(numericInput("DTRsuccC1",label="Probability of Success for First AI",value=0,min=0,max=1,step=0.01),
                bsTooltip(id="DTRsuccC1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
       )
   }
   
   if(input$targetDiffCheckC==TRUE && input$targetOddsCheckC==FALSE && substringDTR2C()[1] != 0){
     return(c(numericInput("targetDiffC",label="Target Difference in Success Probabilities",value=0.1,min=0.01,max=0.5,step=0.01),
              bsTooltip(id="targetDiffC",title="Input can range from 0-0.5 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
     )
   }
   
   if(input$targetOddsCheckC==TRUE && substringDTR2C()[1] != 0){
     return(c(numericInput("targetORC",label="Target Odds Ratio of Success",value=2,min=0,step=0.01),
              bsTooltip(id="targetORC",title="Input must be positive and in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
     )
   }
   
   if(input$targetDiffCheckC==TRUE && substringDTR2C()[1] == 0){
     return()
   }
 })
 
 # When a second DTR is selected, render an input box corresponding to whatever input method is selected.
 # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR2), enabled/disabled depending on cellOrConditionalB
 # For target-difference or OR, numericInputs are NOT rendered (those are handled in output$binaryDTR1probB)
 
 generateBinaryInputs2C <- reactive({
   validate(
     need(input$secondDTRcompareC, "Please select a second AI.")
   )
   if(input$targetDiffCheckC==FALSE && input$targetOddsCheckC==FALSE){
     if(input$cellOrConditionalC==TRUE){
       return(disable(numericInput("DTRsuccC2disable",label="Probability of Success for Second AI",value=0,min=0,max=1,step=0.01)))
     }
     else
       return(c(numericInput("DTRsuccC2",label="Probability of Success for Second AI",value=0,min=0,max=1,step=0.01),
                bsTooltip(id="DTRsuccC2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
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
   controlInputs<-c(numericInput("marginalFirstStageC1",label=paste("Probability of success for Path ",substringDTR1C()[2],"r", substringDTR1C()[3],sep=""),value=0,min=0,max=1,step=0.01),
                    numericInput("marginalSecondStageNRC1",label=paste("Probability of success for Path ",substringDTR1C()[2],"nr",substringDTR1C()[4],sep=""),
                                 value=0,min=0,max=1,step=0.01),
                    bsTooltip(id="marginalFirstStageC1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                    bsTooltip(id="marginalSecondStageNRC1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
   )
 })
 
 output$cellProbsDTR2C <- renderUI({
   if(substringDTR1C()[2]==substringDTR2C()[2] && substringDTR1C()[3]==substringDTR2C()[3]){
     controlInputs<-c(numericInput("marginalSecondStageNRC2",label=paste("Probability of success for Path ",substringDTR2C()[2],"nr",substringDTR2C()[4],sep=""),
                                   value=0,min=0,max=1,step=0.01),
                      bsTooltip(id="marginalSecondStageNRC2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
     )
   }
   else{
     controlInputs<-c(
       numericInput("marginalFirstStageC2",label=paste("Probability of success for Path ",substringDTR2C()[2], "r", substringDTR2C()[3],sep=""),value=0,min=0,max=1,step=0.01),
       numericInput("marginalSecondStageNRC2",label=paste("Probability of success for Path ",substringDTR2C()[2],"nr",substringDTR2C()[4],sep=""),
                    value=0,min=0,max=1,step=0.01),
       bsTooltip(id="marginalFirstStageC2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
       bsTooltip(id="marginalSecondStageNRC2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
     )
   }
   controlInputs
 })
 
 ### Render numericInput for effect size when outcome is continuous
 
 generateContinuousInputC <- reactive({
   validate(
     need(input$firstDTRcompareC, "Please select a first AI."),
     need(input$secondDTRcompareC, "Please select a second AI.")
   )
   return(c(numericInput("effectSizeC",label="Standardized Effect Size",value=0,min=0,max=10,step=0.01),
            bsTooltip(id="effectSizeC",title="Input can range from 0-10 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
   )
 })
 
 output$continuousProbC<-renderUI({
   generateContinuousInputC()
 })
 
 
 ##### DESIGN C OBSERVERS #####
 
 ### For binary outcome, disallow cell-specific input if target difference input is selected
 ### If target difference is not selected, also deselect target OR
 
 observe({    
   if(input$targetOddsCheckC){
     updateCheckboxInput(session,"cellOrConditionalC",value=FALSE)
     updateCheckboxInput(session,"targetDiffCheckC",value=FALSE)
   }
 })
 
 observe({
   if(input$targetDiffCheckC){
     updateCheckboxInput(session,"targetOddsCheckC",value=FALSE)
     updateCheckboxInput(session,"cellOrConditionalC",value=FALSE)
   }
 })
 
 ### If cell-specific input is selected, deselect target difference and OR options
 ### Update disabled full DTR success inputs with computed probabilities
 
 observe({
   if(input$cellOrConditionalC==TRUE){
     updateCheckboxInput(session,"targetDiffCheckC",value=FALSE)
     updateCheckboxInput(session,"targetOddsCheckC",value=FALSE)
     
     updateNumericInput(session,"DTRsuccC1disable",value=generateProbsC()[1])
     updateNumericInput(session,"DTRsuccC2disable",value=generateProbsC()[2])
   }
 },priority=2)
 
 
 ### Compute full DTR probabilities or effect size when providing cell-specific probabilities or mean/SD 
 
 generateProbsC <- reactive({
  if(input$selectOutcomeC==1 && substringDTR1C()[2]==substringDTR2C()[2] && substringDTR1C()[3]==substringDTR2C()[3] && input$cellOrConditionalC==TRUE){
     pDTR1 <- fullDTRprob(input$marginalFirstStageC1,input$respC,input$marginalSecondStageNRC1)
     pDTR2 <- fullDTRprob(input$marginalFirstStageC1,input$respC,input$marginalSecondStageNRC2)
  }
   
   else if (input$selectOutcomeC==1 && substringDTR1C()[2]!=substringDTR2C()[2] && substringDTR1C()[3]!=substringDTR2C()[3] && input$cellOrConditionalC==TRUE){
     pDTR1 <- fullDTRprob(input$marginalFirstStageC1,input$respC,input$marginalSecondStageNRC1)
     pDTR2 <- fullDTRprob(input$marginalFirstStageC2,input$respC,input$marginalSecondStageNRC2)
   }
   else if (input$selectOutcomeC==1 && input$cellOrConditionalC==FALSE){
     pDTR1 <- input$DTRsuccC1
     pDTR2 <- input$DTRsuccC2
   }
   else {
     pDTR1 <- 0
     pDTR2 <- 0
   }
   
   return(c(pDTR1,pDTR2))
 })
 
 ##### DESIGN C RESULT BACKEND #####
 
 # Based on provided input probabilities and selected options, compute appropriate arguments to pass to power.prop.test or pwr.norm.test
 
 checkDTRinputsC <- reactive({
   test<-findInterval(c(input$DTRsuccC1, input$DTRsuccC2),c(0,1))
   if (test[1]==1 && test[2]==1)
     return(TRUE)
   else
     return(FALSE)
 })
 
 dataCompilerC <- reactive({
   
   validate(
     need(!(is.null(input$respC)), "Please provide a response probability. If unknown, enter 0 for a conservative estimate of power or sample size.") %then%
       need(0<=input$respC && input$respC<=1, "The provided response probability is not a valid probability. Please enter a value between 0 and 1.")
   )
   
   if(input$selectOutcomeC==1 && input$cellOrConditionalC==FALSE && input$targetDiffCheckC==FALSE && input$targetOddsCheckC==FALSE){
     validate(
       need(!is.null(input$DTRsuccC1), "Select a first AI above."),
       need(!is.null(input$DTRsuccC2), "Select a second AI above.") %then%
         need(!is.null(input$DTRsuccC1) && !is.null(input$DTRsuccC2), "The success probability is missing for at least one AI. Please provide a numeric input.") %then%
         need(input$DTRsuccC1 != input$DTRsuccC2, "Please provide unique success probabilities for each AI. Sample size is indeterminate for equal AI probabilities.") %then%
         need(checkDTRinputsC(), "The provided success probability for at least one AI is not a valid probability. Please enter a value between 0 and 1.")
     )
     return(c(input$DTRsuccC1,input$DTRsuccC2))
   }
   
   if(input$selectOutcomeC==1 && input$cellOrConditionalC==TRUE){  
     validate(
       need(generateProbsC()[1] != generateProbsC()[2], "The provided marginal probabilities yield identical overall AI success probabilities. Sample size is indeterminate for equal AI probabilities. Please adjust your inputs.")
     )
     
     return(c(generateProbsC()[1],generateProbsC()[2]))
   }
   
   if(input$selectOutcomeC==1 && input$targetDiffCheckC==TRUE){
     validate(
       need(input$targetDiffC <= 0.5, "Target difference must be less than 0.5 to be valid input"),
       need(input$targetDiffC > 0, "Target difference must be greater than 0. Sample size is indeterminate for equal AI probabilities.
            Please adjust your inputs.")
       )
     return(c(0.5,0.5+input$targetDiffC))
   }
   
   if(input$selectOutcomeC==1 && input$targetOddsCheckC==TRUE){
     validate(
       need(is.numeric(input$targetORC),"Please enter an odds ratio.") %then%
         need(input$targetORC != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio.") %then%
         need(input$targetORC != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
     )
     return(c(0.5,input$targetORC/(1+input$targetORC)))
   }
   
   if(input$selectOutcomeC==2){
     validate(
       need(input$firstDTRcompareC, "Select a first AI above."),
       need(input$secondDTRcompareC, "Select a second AI above.")
     )
     return(input$effectSizeC)
   }
 })
 
 # Compute the "design effect" for design C. Varies based on whether DTRs are separate- or shared-path
 
 selectEffectC <- reactive({
   if(substringDTR1C()[2] != substringDTR2C()[2]){
     designEffect <- 3*(1-input$respC) + 2*input$respC  
   }
   
   else{
     validate(
       need(input$respC < 1, "The selected AIs cannot be compared if the probability of response is 1. Please enter a smaller response probability.")
     )
     designEffect <- 4/(1-input$respC)
   }
   return(designEffect)
 })
 
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
   designEffect<-selectEffectC()
   rawSampleSize<-power.prop.test(p1=dataCompilerC()[1],p2=dataCompilerC()[2],power=input$inputPowerC,sig.level=input$alphaC,alternative=c(input$selectAlternativeC))$n
   finalSampleSize<-ceiling(designEffect * rawSampleSize)
   formatPower<-paste(input$inputPowerC*100,"%",sep="")
   formatAlpha<-paste(input$alphaC*100,"%",sep="")
   formatAltHyp<-switch(input$selectAlternativeC, "one.sided"="one-sided ", "two.sided"="two-sided ")
   formatResp<-as.numeric(input$respC)
   
   HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
          <p> We wish to find the sample size for a trial with a binary outcome where the probability of response to first-stage interventions is ", formatResp, sentenceCompilerC(),
              ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
              finalSampleSize, " to make this comparison with at least ",formatPower," power. </p>",sep=""))
 })
 
 output$binaryPowerC <- renderUI({
   validate(
     need(input$inputSampleSizeC != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
   )
   designEffect<-selectEffectC()
   size<-(input$inputSampleSizeC/designEffect)
   finalPower<-round(power.prop.test(p1=dataCompilerC()[1],p2=dataCompilerC()[2],n=size,sig.level=input$alphaC,alternative=c(input$selectAlternativeC))$power,digits=3)
   formatPower<-paste(finalPower*100,"%",sep="")
   formatAlpha<-paste(input$alphaC*100,"%",sep="")
   formatSize<-as.numeric(input$inputSampleSizeC)
   formatResp<-as.numeric(input$respC)
   
   HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
              <p> For a trial of size N=",formatSize," with a binary outcome where the probability of response to first-stage interventions is ",formatResp,
              sentenceCompilerC(),", we have at least ",formatPower," power. </p>",sep=""))
 })
 
 output$continuousSampleSizeC <- renderUI({
   alt.hyp<-switch(input$selectAlternativeC,"one.sided"="greater")
   designEffect<-selectEffectC()
   rawSampleSize<-try(pwr.norm.test(d=dataCompilerC(),sig.level=input$alphaC,power=input$inputPowerC,alternative=alt.hyp)$n,silent=T)
   
   validate(
     need(input$inputPowerC > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
     need(input$inputPowerC < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1."),
     need(dataCompilerC()>0,"Sample size is indeterminate for an effect size of 0. Please specify a different effect size.") %then%
       need(is.numeric(rawSampleSize),paste("Given the provided effect size, fewer than",2*designEffect,"individuals are required to achieve the desired power.",
                                            "This is not enough individuals to run a SMART. You can test for a smaller effect size, or increase the desired power."))
   )
   
   finalSampleSize<-ceiling(2*designEffect*rawSampleSize)
   formatPower<-paste(input$inputPowerC*100,"%",sep="")
   formatAlpha<-paste(input$alphaC*100,"%",sep="")
   formatAltHyp<-switch(input$selectAlternativeC, "one.sided"="one-sided ", "two.sided"="two-sided ")
   formatResp<-as.numeric(input$respC)
   
   HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
         <p> We wish to find the sample size for a trial with a continuous outcome where the probability of response to first-stage interventions is ", formatResp, sentenceCompilerC(),
              ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
              finalSampleSize, " to make this comparison with at least ",formatPower," power. </p>",sep=""))
 })
 
 output$continuousPowerC <- renderUI({
   alt.hyp<-switch(input$selectAlternativeC,"one.sided"="greater")
   validate(
     need(input$inputSampleSizeC!=0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
   )
   designEffect<-selectEffectC()
   size<-(input$inputSampleSizeC/(2*designEffect))
   finalPower<-round(pwr.norm.test(d=dataCompilerC(),sig.level=input$alphaC,n=size,alternative=alt.hyp)$power,digits=3)
   formatPower<-paste(finalPower*100,"%",sep="")
   formatAlpha<-paste(input$alphaC*100,"%",sep="")
   formatSize<-as.numeric(input$inputSampleSizeC)
   formatResp<-as.numeric(input$respC)
   
   HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
              <p> For a trial of size N=",formatSize," with a continuous outcome where the probability of response to first-stage interventions is ",formatResp,
              sentenceCompilerC(),", we have at least ",formatPower," power. </p>",sep=""))
 })

 
 ##### DESIGN D #####
 
 ##### D HEADER #####
 
 ### Render the design image which highlights selected DTRs
 ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
 ### Note that this requires a very specific naming convention for image assets
 ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0. 
 
 output$designDimg <- renderImage({
   filename<-filename<-normalizePath(file.path('./www/images',paste('SMARTdesignD_',input$firstDTRcompareD,'_',input$secondDTRcompareD,'.gif',sep='')))
   list(src=filename)
 },deleteFile=FALSE)
 
 ### Render 'selectize' dropdown boxes with placeholder text for AI selection.
 ### Second DTR input populates with all DTRs that are NOT the first DTR--eliminates ability to select same DTR twice
 ### Placed in server.R rather than ui.R because of dependency on first DTR selection
 
 output$selectAI1D <- renderUI({
   AI <- selectizeInput("firstDTRcompareD",label="Compare intervention path",
                        choices=designD.DTRs,
                        options = list(
                          placeholder = 'Select a first intervention.',
                          onInitialize = I('function() { this.setValue(0); }')
                        )
   )
   return(AI)
 })
 
 output$selectAI2D <- renderUI({ 
   AI <- selectizeInput("secondDTRcompareD",label="to intervention path",
                        choices=setdiff(designD.DTRs,input$firstDTRcompareD),
                        options = list(
                          placeholder = 'Select a second intervention.',
                          onInitialize = I('function() { this.setValue(""); }')
                        )
   )
   return(AI)
 })  
 
 ##### DESIGN D PROBABILITY INPUT #####
 
 ### Read in DTR names from dropdowns and parse them to determine first and second stage treatments
 ### Reactive function allows on-the-fly changes in return values with changes in selection
 ### Outputs full DTR name, first-stage treatment (first character), second-stage treatment if response (third character),
 ### and second-stage treatment if non-response(last character)
 ### NOTE that these positions are exclusive to design A because responders have two second-stage treatment options
 
 substringDTR1D <- reactive({
   if(length(input$firstDTRcompareD) > 0){
     DTR1 <-paste(input$firstDTRcompareD)
     firstStage1<-substr(DTR1,1,1)
     secondStage1<-substr(DTR1,2,2)
     return(c(DTR1, firstStage1,secondStage1))
   }
   else{
     return(c(0,0,0))
   }   
 })
 
 substringDTR2D <- reactive({
   if(length(input$secondDTRcompareD) >0){
     DTR2 <-paste(input$secondDTRcompareD)
     firstStage2<-substr(DTR2,1,1)
     secondStage2<-substr(DTR2,2,2)
     return(c(DTR2, firstStage2, secondStage2))
   }
   else{
     return(c(0,0,0))
   }
 })
 
 #When a first DTR is selected, render an input box corresponding to whatever input method is selected.
 # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR1), enabled/disabled depending on cellOrConditionalB
 # For target-difference or OR, relevant numericInputs are rendered
 # This is the ONLY location in which difference and OR numericInputs are built.
 
 generateBinaryInputs1D <- reactive({
   validate(
     need(input$firstDTRcompareD, "Please select a first intervention path.")
   )
   if(input$targetDiffCheckD==FALSE && input$targetOddsCheckD==FALSE){
       return(c(numericInput("DTRsuccD1",label="Probability of Success for First Intervention Path",value=0,min=0,max=1,step=0.01),
                bsTooltip(id="DTRsuccD1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
       )
   }
   
   if(input$targetDiffCheckD==TRUE && substringDTR2D()[1] != 0){
     return(c(numericInput("targetDiffD",label="Target Difference in Success Probabilities",value=0.1,min=0.01,max=0.5,step=0.01),
              bsTooltip(id="targetDiffD",title="Input can range from 0-0.5 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
     )
   }
   
   if(input$targetOddsCheckD==TRUE && substringDTR2D()[1] != 0){
     return(c(numericInput("targetORD",label="Target Odds Ratio of Success",value=2,min=0,step=0.01),
              bsTooltip(id="targetORD",title="Input must be positive and in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
     )
   }
 })
 
 # When a second DTR is selected, render an input box corresponding to whatever input method is selected.
 # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR2), enabled/disabled depending on cellOrConditionalB
 # For target-difference or OR, numericInputs are NOT rendered (those are handled in output$binaryDTR1probB)
 
 generateBinaryInputs2D <- reactive({
   validate(
     need(input$secondDTRcompareD, "Please select a second intervention path.")
   )
   if(input$targetDiffCheckD==FALSE && input$targetOddsCheckD==FALSE){
       return(c(numericInput("DTRsuccD2",label="Probability of Success for Second Intervention Path",value=0,min=0,max=1,step=0.01),
                bsTooltip(id="DTRsuccD2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
       )
   }
 })
 
 ### Render UI components generated above
 
 output$binaryDTR1probD <- renderUI({
   generateBinaryInputs1D()
 })
 
 output$binaryDTR2probD <- renderUI({
   generateBinaryInputs2D()
 })
 
 ### For cell-specific probabilities, render a series of numericInputs labeled by information from DTR substrings
 ### When DTR1 and DTR2 begin with the same treatment, P(S|stage1trt,r) is rendered only once, in output$cellProbsDTR1B
 
 output$cellProbsDTR1D <- renderUI({
   controlInputs<-c(numericInput("marginalFirstStageD1",label=paste("Probability of success for Path ",substringDTR1C()[2],"r", substringDTR1C()[3],sep=""),value=0,min=0,max=1,step=0.01),
                    numericInput("marginalSecondStageNRD1",label=paste("Probability of success for Path ",substringDTR1C()[2],"nr",substringDTR1C()[4],sep=""),
                                 value=0,min=0,max=1,step=0.01),
                    bsTooltip(id="marginalFirstStageD1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                    bsTooltip(id="marginalSecondStageNRD1",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
   )
 })
 
 output$cellProbsDTR2D <- renderUI({
   if(substringDTR1C()[2]==substringDTR2D()[2] && substringDTR1D()[3]==substringDTR2D()[3]){
     controlInputs<-c(numericInput("marginalSecondStageNRD2",label=paste("Probability of success for Path ",substringDTR2C()[2],"nr",substringDTR2C()[4],,sep=""),
                                   value=0,min=0,max=1,step=0.01),
                      bsTooltip(id="marginalSecondStageNRD2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
     )
   }
   else{
     controlInputs<-c(
       numericInput("marginalFirstStageD2",label=paste("Probability of success for Path ",substringDTR2D()[2],"r", substringDTR2D()[3],sep=""),value=0,min=0,max=1,step=0.01),
       numericInput("marginalSecondStageNRD2",label=paste("Probability of success for Path ",substringDTR2D()[2],"nr",substringDTR2D()[4],sep=""),
                    value=0,min=0,max=1,step=0.01),
       bsTooltip(id="marginalFirstStageD2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
       bsTooltip(id="marginalSecondStageNRD2",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus")
     )
   }
   controlInputs
 })
 
 ### Render enabled/disabled numericInputs when outcome is continuous
 
 generateContinuousInputD <- reactive({
   validate(
     need(input$firstDTRcompareD, "Please select a first intervention path."),
     need(input$secondDTRcompareD, "Please select a second intervention path.")
   )
   return(c(numericInput("effectSizeD",label="Standardized Effect Size",value=0,min=0,max=10,step=0.01),
            bsTooltip(id="effectSizeD",title="Input can range from 0-10 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
   )
 })
 
 output$continuousProbD <- renderUI({
   generateContinuousInputD()
 })
 
 output$meanEstD <- renderUI({
   contInput<-c(numericInput("mean1D",label="Mean outcome for first AI",value=0,step=0.01),
                numericInput("mean2D",label="Mean outcome for second AI",value=0,step=0.01),
                numericInput("sdDiffD",label="Standard error of the difference in above means:", value=0, min=0, step=0.01),
                bsTooltip(id="mean1D",title="Input must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                bsTooltip(id="mean2D",title="Input must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                bsTooltip(id="sdDiffD",title="Input must be positive and in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"))
   contInput
 })
 
 ##### DESIGN D OBSERVERS #####
 
 ### For binary outcome, disallow cell-specific input if target difference input is selected
 ### If target difference is not selected, also deselect target OR
 
 observe({
   if(input$targetDiffCheckD){
     updateCheckboxInput(session,"targetOddsCheckD",value=FALSE)
   }
 }) 
 
 observe({
   if(input$targetOddsCheckD){
     updateCheckboxInput(session,"targetDiffCheckD",value=FALSE)
   }
 }) 


 ##### DESIGN D RESULT BACKEND#####
 
 # Based on provided input probabilities and selected options, compute appropriate arguments to pass to power.prop.test or pwr.norm.test
 
 checkDTRinputsD <- reactive({
   test<-findInterval(c(input$DTRsuccD1, input$DTRsuccD2),c(0,1))
   if (test[1]==1 && test[2]==1)
     return(TRUE)
   else
     return(FALSE)
 })
 
 dataCompilerD <- reactive({
     
   if(input$selectOutcomeD==1 && input$targetDiffCheckD==FALSE && input$targetOddsCheckD==FALSE){
     validate(
       need(!is.null(input$DTRsuccD1), "Select a first intervention path above."),
       need(!is.null(input$DTRsuccD2), "Select a second intervention path above.") %then%
         need(!is.null(input$DTRsuccD1) && !is.null(input$DTRsuccD2), "The success probability is missing for at least one intervention path. Please provide a numeric input.") %then%
         need(input$DTRsuccD1 != input$DTRsuccD2, "Please provide unique success probabilities for each intervention path. Sample size is indeterminate for equal intervention path probabilities.") %then%
         need(checkDTRinputsD(), "The provided success probability for at least one intervention path is not a valid probability. Please enter a value between 0 and 1.")
     )
     return(c(input$DTRsuccD1,input$DTRsuccD2))
   }
   
   if(input$selectOutcomeD==1 && input$targetDiffCheckD==TRUE){
     validate(
       need(input$targetDiffD <= 0.5, "Target difference must be less than 0.5 to be valid input"),
       need(input$targetDiffD > 0, "Target difference must be greater than 0. Sample size is indeterminate for equal intervention path probabilities.
            Please adjust your inputs.")
       )
     return(c(0.5,0.5+input$targetDiffD))
   }
   
   if(input$selectOutcomeD==1 && input$targetOddsCheckD==TRUE){
     validate(
       need(is.numeric(input$targetORD),"Please enter an odds ratio.") %then%
         need(input$targetORD != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio.") %then%
         need(input$targetORD != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
     )
     return(c(0.5,input$targetORD/(1+input$targetORD)))
   }
   
   if(input$selectOutcomeD==2){
     validate(
       need(input$firstDTRcompareD, "Select a first intervention path above."),
       need(input$secondDTRcompareD, "Select a second intervention path above.")
     )
     
     return(input$effectSizeD)
   } 
 })
 
 # Compute the "design effect" for design D. Varies based on whether DTRs are separate- or shared-path
 
 selectEffectD <- reactive({
   return(4)
 })
 
 sentenceCompilerD <- reactive({
   if(input$selectOutcomeD==1 && input$targetDiffCheckD==FALSE && input$targetOddsCheckD==FALSE){
     str <- paste(" the overall probabilities of success in the two intervention paths of interest, ",input$firstDTRcompareD," and ",input$secondDTRcompareD,
                  ", are ",input$DTRsuccD1," and ",input$DTRsuccD2,", respectively", sep="")
     return(str)
   }
   
   if(input$selectOutcomeD==1 && input$targetDiffCheckD==TRUE && input$targetOddsCheckD==FALSE){
     str <- paste(" the difference in overall probabilities of success in the two intervention paths of interest, ",input$firstDTRcompareD," and ",input$secondDTRcompareD,
                  ", is ",input$targetDiffD, sep="")
     return(str)
   }
   
   if(input$selectOutcomeD==1 && input$targetOddsCheckD==TRUE && input$targetDiffCheckD==FALSE){
     str <- paste(" the odds ratio of success for the two intervention paths of interest, ",input$firstDTRcompareD," and ",input$secondDTRcompareD,
                  ", is ",input$targetORD, sep="")
     return(str)
   }
   
   if(input$selectOutcomeD==2){
     str <- paste(" the standardized effect size between the two intervention paths of interest, ",input$firstDTRcompareD," and ",input$secondDTRcompareD,
                  ", is ", input$effectSizeD, sep="")
     return(str)
   } 
 })
 
 ##### DESIGN D RESULTS #####
 
 # Pass arguments from dataCompilerB() to appropriate R function; extract and render relevant output
 
 output$binarySampleSizeD <- renderUI({
   validate(
     need(input$inputPowerD > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
     need(input$inputPowerD < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1.")
   )
   designEffect<-selectEffectD()
   rawSampleSize<-power.prop.test(p1=dataCompilerD()[1],p2=dataCompilerD()[2],power=input$inputPowerD,sig.level=input$alphaD,alternative=c(input$selectAlternativeD),n=NULL)$n
   finalSampleSize<-ceiling(designEffect * rawSampleSize)
   formatPower<-paste(input$inputPowerD*100,"%",sep="")
   formatAlpha<-paste(input$alphaD*100,"%",sep="")
   formatAltHyp<-switch(input$selectAlternativeD, "one.sided"="one-sided ", "two.sided"="two-sided ")
   
   HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
         <p> We wish to find the sample size for a trial with a binary outcome where ",sentenceCompilerD(), ". 
              Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
              finalSampleSize, " to make this comparison with at least ",formatPower," power. </p>",sep=""))
 })
 
 output$binaryPowerD <- renderUI({
   validate(
     need(input$inputSampleSizeD != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
   )
   designEffect<-selectEffectD()
   size<-(input$inputSampleSizeD/designEffect)
   finalPower<-round(power.prop.test(p1=dataCompilerD()[1],p2=dataCompilerD()[2],n=size,sig.level=input$alphaD,alternative=c(input$selectAlternativeD),power=NULL)$power,digits=3)
   formatPower<-paste(finalPower*100,"%",sep="")
   formatAlpha<-paste(input$alphaD*100,"%",sep="")
   formatSize<-as.numeric(input$inputSampleSizeD)
   
   HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
         <p> For a trial of size N=",formatSize," with a binary outcome where ",sentenceCompilerD(),
              ", we have at least ",formatPower," power. </p>",sep=""))
 })

 output$continuousSampleSizeD <- renderUI({
   alt.hyp<-switch(input$selectAlternativeD,"one.sided"="greater")
   designEffect<-selectEffectD()
   rawSampleSize<-try(pwr.norm.test(d=dataCompilerD(),sig.level=input$alphaD,power=input$inputPowerD,alternative=alt.hyp)$n,silent=T)
   
   validate(
     need(input$inputPowerD > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
     need(input$inputPowerD < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1."),
     need(dataCompilerD()>0,"Sample size is indeterminate for an effect size of 0. Please specify a different effect size.") %then%
     need(is.numeric(rawSampleSize),paste("Given the provided effect size, fewer than",2*designEffect,"individuals are required to achieve the desired power.",
                                          "This is not enough individuals to run a SMART. You can test for a smaller effect size, or increase the desired power."))
   )
   
   finalSampleSize<-ceiling(2*designEffect*rawSampleSize)
   formatPower<-paste(input$inputPowerD*100,"%",sep="")
   formatAlpha<-paste(input$alphaD*100,"%",sep="")
   formatAltHyp<-switch(input$selectAlternativeD, "one.sided"="one-sided ", "two.sided"="two-sided ")
   
   HTML(paste("<h4 style='color:blue';> N=",paste(finalSampleSize)," </h4>
         <p> We wish to find the sample size for a trial with a continuous outcome where ", sentenceCompilerD(),
              "Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
              finalSampleSize, " to make this comparison with at least ",formatPower," power. </p>",sep=""))
 })
 
 output$continuousPowerD <- renderUI({
   alt.hyp<-switch(input$selectAlternativeD,"one.sided"="greater")
   validate(
     need(input$inputSampleSizeD!=0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
   )
   designEffect<-selectEffectD()
   size<-(input$inputSampleSizeD/(2*designEffect))
   finalPower<-round(pwr.norm.test(d=dataCompilerD(),sig.level=input$alphaD,n=size,alternative=alt.hyp)$power,digits=3)
   formatPower<-paste(finalPower*100,"%",sep="")
   formatAlpha<-paste(input$alphaD*100,"%",sep="")
   formatSize<-as.numeric(input$inputSampleSizeD)
   
   HTML(paste("<h4 style='color:blue';> Power=",paste(formatPower)," </h4>
         <p> For a trial of size N=",formatSize," with a continuous outcome where ",sentenceCompilerD(),
              ", we have at least ",formatPower," power. </p>",sep=""))
 })
  
})