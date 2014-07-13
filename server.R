library(shiny)
library(pwr)

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

### BEGIN SERVER OPERATIONS ###

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
  
#   ##### DESIGN A #####
#   
#   ##### A IMAGE HEADER #####
#   ### Render the design image which highlights selected DTRs
#   ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
#   ### Note that this requires a very specific naming convention for image assets
#   ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0. 
#   
#   output$designAimg <- renderImage({
#     filename<-filename<-normalizePath(file.path('./www/images',paste('SMARTdesignA_',input$firstDTRcompareA,'_',input$secondDTRcompareA,'.gif',sep='')))
#     list(src=filename)
#   },deleteFile=FALSE)
#   
#   
#   ##### DESIGN A PROBABILITY INPUT #####
#   
#   ### Read in DTR names from dropdowns and parse them to determine first and second stage treatments
#   ### Reactive function allows on-the-fly changes in return values with changes in selection
#   ### Outputs full DTR name, first-stage treatment (first character), second-stage treatment (last character)
#   ### NOTE that these positions are exclusive to design B because responders continue first-stage treatment
#   
#   substringDTR1A <- reactive({
#     DTR1 <- paste(input$firstDTRcompareA)
#     firstStage1 <- substr(DTR1,1,1)
#     secondStageR1 <- substr(DTR1,3,3)
#     secondStageNR1 <- substr(DTR1,6,6)
#     
#     return(c(DTR1, firstStage1,secondStageR1, secondStageNR1))
#     
#   })
#   
#   substringDTR2A <- reactive({
#     DTR2 <- paste(input$secondDTRcompareA)
#     firstStage2 <- substr(DTR2,1,1)
#     secondStageR2 <- substr(DTR2,3,3)
#     secondStageNR2 <- substr(DTR2,6,6)
#     
#     return(c(DTR2, firstStage2, secondStageR2, secondStageNR2))
#     
#   })
#   
#   ### Series of observe functions disallows selection of more than one input option 
#   ### (i.e. cell-specific, target difference, odds ratio)
#   
#   noCellProbA <- observe({    
#     if(input$targetDiffCheckA==TRUE){
#       updateCheckboxInput(session,"cellOrConditionalA",value=FALSE)
#     }
#   })
#   
#   noTargetsA<-observe({
#     if(input$cellOrConditionalA==TRUE){
#       updateCheckboxInput(session,"targetDiffCheckA",value=FALSE)
#       updateCheckboxInput(session,"targetOddsCheckA",value=FALSE)
#     }
#   },priority=1)
#   
  
  ##### DESIGN B #####
  
  ##### B IMAGE HEADER #####
  ### Render the design image which highlights selected DTRs
  ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
  ### Note that this requires a very specific naming convention for image assets
      ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0. 
  
  output$designBimg <- renderImage({
       filename<-filename<-normalizePath(file.path('./www/images',paste('SMARTdesignB_',input$firstDTRcompareB,'_',input$secondDTRcompareB,'.gif',sep='')))
       list(src=filename)
   },deleteFile=FALSE)
  
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
  
  output$DTR1B <- renderPrint({
    input$firstDTRcompareB  
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
      ### NOTE that these positions are exclusive to design B because responders continue first-stage treatment
  
  substringDTR1B <- reactive({
    if(length(input$firstDTRcompareB) > 0){
      DTR1 <-paste(input$firstDTRcompareB)
      firstStage1<-substr(DTR1,1,1)
      secondStageNR1<-substr(DTR1,6,6)
      return(c(DTR1, firstStage1,secondStageNR1))
    }
    else{
      return(c(0,0,0))
    }   
  })
  
  substringDTR2B <- reactive({
    if(length(input$secondDTRcompareB) >0){
      DTR2 <-paste(input$secondDTRcompareB)
      firstStage2<-substr(DTR2,1,1)
      secondStageNR2<-substr(DTR2,6,6)
      return(c(DTR2, firstStage2, secondStageNR2))
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
    if(input$targetDiffCheckB==FALSE && substringDTR1B()[1] != substringDTR2B()[1]){
      if(input$cellOrConditionalB==TRUE){
        return(disable(numericInput("DTRsuccB1disable",label="Probability of Success for First AI",value=0,min=0,max=1,step=0.01)))
      }
      else
        return(numericInput("DTRsuccB1",label="Probability of Success for First AI",value=generateProbsB()[1],min=0,max=1,step=0.01))
    }
    
    if(input$targetDiffCheckB==TRUE && input$targetOddsCheckB==FALSE && substringDTR2B()[1] != 0){
      return(numericInput("targetDiffB",label="Target Difference in Success Probabilities",value=0.1,min=0.0001,max=0.5,step=0.01))
    }
    
    if(input$targetOddsCheckB==TRUE && substringDTR2B()[1] != 0){
      return(numericInput("targetORB",label="Target Odds Ratio of Success",value=2,min=0,step=0.01))
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
        return(numericInput("DTRsuccB2",label="Probability of Success for Second AI",value=generateProbsB()[2],min=0,max=1,step=0.01))
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
        controlInputs<-c(numericInput("marginalFirstStageB1",label=paste("P(S|",substringDTR1B()[2],",r)",sep=""),value=0,min=0,max=1,step=0.01),
                         numericInput("marginalSecondStageNRB1",label=paste("P(S|",substringDTR1B()[2],",nr,",substringDTR1B()[3],")",sep=""),
                                      value=0,min=0,max=1,step=0.01)
        )
    }
    
  })
  
  output$cellProbsDTR2B <- renderUI({
    if(substringDTR1B()[1] != substringDTR2B()[1]){
      if(substringDTR1B()[2]==substringDTR2B()[2]){
        controlInputs<-c(numericInput("marginalSecondStageNRB2",label=paste("P(S|",substringDTR2B()[2],",nr,",substringDTR2B()[3],")",sep=""),
                         value=0,min=0,max=1,step=0.01)
        )
      }
      else{
        controlInputs<-c(
                         numericInput("marginalFirstStageB2",label=paste("P(S|",substringDTR2B()[2],",r)",sep=""),value=0,min=0,max=1,step=0.01),
                         numericInput("marginalSecondStageNRB2",label=paste("P(S|",substringDTR2B()[2],",nr,",substringDTR2B()[3],")",sep=""),
                                      value=0,min=0,max=1,step=0.01)
        )
      }
    }
    controlInputs
  })
  
  # Render enabled/disabled numericInputs when outcome is continuous
  
  generateContinuousInputB <- reactive({
    validate(
      need(input$firstDTRcompareB, "Please select a first AI."),
      need(input$secondDTRcompareB, "Please select a second AI.")
    )
    if(input$meanSdCheckB==TRUE && substringDTR1B()[1] != substringDTR2B()[1]){
      return(disable(numericInput("effectSizeBdisable",label="Standardized Effect Size",value=0,min=0,max=1,step=0.01)))
    }
    else {
      return(numericInput("effectSizeB",label="Standardized Effect Size",value=generateProbsB()[3],min=0,max=1,step=0.01))
    }
  })
  
  output$continuousProbB<-renderUI({
    generateContinuousInputB()
  })
  
  output$meanEstB <- renderUI({
    contInput<-c(numericInput("meanDiffB",label="Difference in mean outcomes between the two selected DTRs:",value=0,min=0,step=0.01),
                 numericInput("sdDiffB",label="Standard deviation of the above difference in means:", value=0, min=0, step=0.01))
    contInput
  })

  
  
  ##### DESIGN B OBSERVERS #####
  
  ### For binary outcome, disallow cell-specific input if target difference input is selected
  ### If target difference is not selected, also deselect target OR
  
  observe({    
    if(input$targetDiffCheckB){
      updateCheckboxInput(session,"cellOrConditionalB",value=FALSE)
    }
    if(input$targetDiffCheckB==FALSE){
      updateCheckboxInput(session,"targetOddsCheckB",value=FALSE)
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
      effectSize <- 0
    }
    
    else if (input$selectOutcomeB==1 && substringDTR1B()[2]!=substringDTR2B()[2] && input$cellOrConditionalB==TRUE){
      pDTR1 <- fullDTRprob(input$marginalFirstStageB1,input$respB,input$marginalSecondStageNRB1)
      pDTR2 <- fullDTRprob(input$marginalFirstStageB2,input$respB,input$marginalSecondStageNRB2)
      effectSize <- 0
    }
    else if (input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE){
      pDTR1 <- input$DTRsuccB1
      pDTR2 <- input$DTRsuccB2
      effectSize <- 0
    }
    else {
      pDTR1 <- 0
      pDTR2 <- 0
      effectSize <- (input$meanDiffB / input$sdDiffB)
    }
    
    return(c(pDTR1,pDTR2,effectSize))
  })
  
  ### If providing mean difference and SD, update disabled effect size input with computed value
  
  observe({
    if(input$meanSdCheckB==TRUE){
      updateNumericInput(session,"effectSizeBdisable",value=generateProbsB()[3])
    }
  })
  

  ##### DESIGN B RESULT #####
  
  # Based on provided input probabilities and selected options, compute appropriate arguments to pass to power.prop.test or pwr.norm.test
  
  checkDTRinputsB <- reactive({
    if ((0<=input$DTRsuccB1 & input$DTRsuccB1 <=1) & (0<=input$DTRsuccB2 & input$DTRsuccB2<=1))
      return(TRUE)
    else
      return(FALSE)
  })
  
  dataCompilerB <- reactive({
    
    validate(
      need(!(is.na(input$respB)), "Please provide a response probability. If unknown, enter 0 for a conservative estimate of power or sample size.") %then%
      need(0<=input$respB && input$respB<=1, "The provided response probability is not a valid probability. Please enter a value between 0 and 1.")
    )
    
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE && input$targetDiffCheckB==FALSE){
      validate(
        need(input$firstDTRcompareB, "Select a first AI above."),
        need(input$secondDTRcompareB, "Select a second AI above.") %then%
        need(input$DTRsuccB1 != input$DTRsuccB2, "Please provide unique success probabilities for each AI. Sample size is indeterminate for equal AI probabilities.") %then%
        need(checkDTRinputsB(), "The provided success probability for at least one AI is not a valid probability. Please enter a value between 0 and 1.")
      )
      return(c(input$DTRsuccB1,input$DTRsuccB2))
    }
    
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==TRUE){  
      validate(
        need(input$firstDTRcompareB, "Select a first AI above."),
        need(input$firstDTRcompareB, "select a second AI above."),
        need(generateProbsB()[1] != generateProbsB()[2], "The provided marginal probabilities yield identical overall AI success probabilities. Sample size is indeterminate for equal AI probabilities. Please adjust your inputs.")
      )
      
      return(c(generateProbsB()[1],generateProbsB()[2]))
     }
    
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE && input$targetDiffCheckB==TRUE && input$targetOddsCheckB==FALSE){
      validate(
        need(input$targetDiffB <= 0.5, "Target difference must be less than 0.5 to be valid input"),
        need(input$targetDiffB > 0, "Target difference must be greater than 0. Sample size is indeterminate for equal AI probabilities.
             Please adjust your inputs.")
      )
      return(c(0.5,0.5+input$targetDiffB))
    }
    
    if(input$selectOutcomeB==1 && input$targetOddsCheckB==TRUE && input$targetDiffCheckB==TRUE){
      validate(
        need(input$targetORB != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio."),
        need(input$targetORB != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
      )
      return(c(0.5,input$targetORB/(1+input$targetORB)))
    }
    
    if(input$selectOutcomeB==2 && input$meanSdCheckB==FALSE){
      validate(
        need(input$firstDTRcompareB, "Select a first AI above."),
        need(input$secondDTRcompareB, "Select a second AI above.") %then%
        need(input$effectSizeB != 0, "Sample size is indeterminate for an effect size of 0. Please enter a different target effect size.")
      )
      
      return(input$effectSizeB)
    }
    
    if(input$selectOutcomeB==2 && input$meanSdCheckB==TRUE){
      validate(
        need(input$meanDiffB != 0, "Sample size is indeterminate for a mean difference of 0."),
        need(input$sdDiffB != 0, "Sample size is indeterminate for a standard deviation of 0.")
      )
      
      return(generateProbsB()[3])
    }  
  })
  
  # Compute the "design effect" for design B. Varies based on whether DTRs are separate- or shared-path

  selectEffectB <- reactive({
    if(substringDTR1B()[2] != substringDTR2B()[2]){
      designEffect <- 2*(2*(1-input$respB)+input$respB)
    }
    else{
      designEffect <- 4/(1-input$respB)
    }
    return(designEffect)
  })
  
  sentenceCompiler <- reactive({
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE && input$targetDiffCheckB==FALSE){
      str <- paste("with success probabilities of",input$DTRsuccB1,"and",input$DTRsuccB2,"respectively,")
      return(str)
    }
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==TRUE){
      str <- paste("with success probabilities of",generateProbsB()[1],"and",generateProbsB()[2],"respectively,")
      return(str)
    }
    
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE && input$targetDiffCheckB==TRUE && input$targetOddsCheckB==FALSE){
      str <- paste("with a difference in success probabilities of",input$targetDiffB)
      return(str)
    }
    
    if(input$selectOutcomeB==1 && input$targetOddsCheckB==TRUE && input$targetDiffCheckB==TRUE){
      str <- paste("with an odds ratio of",input$targetORB)
      return(str)
    }
    
    if(input$selectOutcomeB==2 && input$meanSdCheckB==FALSE){
      str <- paste("with a standardized effect size of", input$effectSizeB)
      return(str)
    }
    
    if(input$selectOutcomeB==2 && input$meanSdCheckB==TRUE){
      str <- paste("(with a standardized effect size of ", generateProbsB()[3],")",sep="")
      return(str)
    }  
  })
  
  # Pass arguments from dataCompilerB() to appropriate R function; extract and render relevant output
  
  output$binarySampleSizeB <- renderUI({
    validate(
      need(input$inputPowerB > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
      need(input$inputPowerB < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1.")
    )
    designEffect<-selectEffectB()
    rawSampleSize<-power.prop.test(p1=dataCompilerB()[1],p2=dataCompilerB()[2],power=input$inputPowerB,sig.level=input$alphaB)$n
    finalSampleSize<-ceiling(designEffect * rawSampleSize)
    formatPower<-paste(input$inputPowerB*100,"%",sep="")
    formatAlpha<-paste(input$alphaB*100,"%",sep="")
    
    HTML("<h4> <font color='blue'> N=",paste(finalSampleSize),"</font> </h4>
         <p><em> We require a sample size of",finalSampleSize,"to compare adaptive interventions",input$firstDTRcompareB, "and",
         input$secondDTRcompareB,sentenceCompiler(), "with", formatPower,"power and",formatAlpha,"type-I error. </em></p>")
  })

  output$binaryPowerB <- renderUI({
    validate(
      need(input$inputSampleSizeB != 0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
    )
    designEffect<-selectEffectB()
    size<-(input$inputSampleSizeB/designEffect)
    finalPower<-round(power.prop.test(p1=dataCompilerB()[1],p2=dataCompilerB()[2],n=size,sig.level=input$alphaB)$power,digits=3)
    formatPower<-paste(finalPower*100,"%",sep="")
    formatAlpha<-paste(input$alphaB*100,"%",sep="")
    
    HTML("<h4> <font color='blue'> power=",paste(finalPower),"</font> </h4>
         <p><em> Given a sample size of",input$inputSampleSizeB,"a comparison of adaptive interventions",input$firstDTRcompareB, "and",
         input$secondDTRcompareB,sentenceCompiler(), "can be made with", formatPower,"power and",formatAlpha,"type-I error. </em></p>")
  })
  
  output$continuousSampleSizeB <- renderUI({
    validate(
      need(input$inputPowerB > 0, "Sample size is indeterminate for 0% power. Please specify a power greater than zero."),
      need(input$inputPowerB < 1, "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1.")
    )
    designEffect<-selectEffectB()
    rawSampleSize<-pwr.norm.test(d=dataCompilerB(),sig.level=input$alphaB,power=input$inputPowerB,alternative="two.sided")
    finalSampleSize<-ceiling(2*designEffect*rawSampleSize$n)
    formatPower<-paste(input$inputPowerB*100,"%",sep="")
    formatAlpha<-paste(input$alphaB*100,"%",sep="")
    
    HTML("<h4> <font color='blue'> N=",paste(finalSampleSize),"</font> </h4>
         <p><em> We require a sample size of",finalSampleSize,"to compare adaptive interventions",input$firstDTRcompareB, "and",
         input$secondDTRcompareB,sentenceCompiler(), "with", formatPower,"power and",formatAlpha,"type-I error. </em></p>")
  })
  
  output$continuousPowerB <- renderUI({
    validate(
      need(input$inputSampleSizeB!=0, "Power is indeterminate for a sample size of 0. Please provide a valid sample size.")  
    )
    designEffect<-selectEffectB()
    size<-(input$inputSampleSizeB/(2*designEffect))
    finalPower<-round(pwr.norm.test(d=dataCompilerB(),sig.level=input$alphaB,n=size,alternative="two.sided")$power,digits=3)
    formatPower<-paste(finalPower*100,"%",sep="")
    formatAlpha<-paste(input$alphaB*100,"%",sep="")
    
    HTML("<h4> <font color='blue'> power=",paste(finalPower),"</font> </h4>
         <p><em> Given a sample size of",input$inputSampleSizeB,"a comparison of adaptive interventions",input$firstDTRcompareB, "and",
         input$secondDTRcompareB,sentenceCompiler(), "can be made with", formatPower,"power and",formatAlpha,"type-I error. </em></p>")
  })

  
})
