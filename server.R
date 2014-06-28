library(shiny)
library(pwr)

`%then%` <- shiny:::`%OR%`

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
  
  ##### A IMAGE HEADER #####
  ### Render the design image which highlights selected DTRs
  ### Takes the names of selected DTRs, generates a filepath, and renders the image in the UI
  ### Note that this requires a very specific naming convention for image assets
  ### 'SMARTdesignX_DTR1_DTR2.gif'; if DTRX is unselected, DTR name is 0. 
  
  output$designAimg <- renderImage({
    filename<-filename<-normalizePath(file.path('./www/images',paste('SMARTdesignA_',input$firstDTRcompareA,'_',input$secondDTRcompareA,'.gif',sep='')))
    list(src=filename)
  },deleteFile=FALSE)
  
  
  ##### DESIGN A PROBABILITY INPUT #####
  
  ### Read in DTR names from dropdowns and parse them to determine first and second stage treatments
  ### Reactive function allows on-the-fly changes in return values with changes in selection
  ### Outputs full DTR name, first-stage treatment (first character), second-stage treatment (last character)
  ### NOTE that these positions are exclusive to design B because responders continue first-stage treatment
  
  substringDTR1A <- reactive({
    DTR1 <- paste(input$firstDTRcompareA)
    firstStage1 <- substr(DTR1,1,1)
    secondStageR1 <- substr(DTR1,3,3)
    secondStageNR1 <- substr(DTR1,6,6)
    
    return(c(DTR1, firstStage1,secondStageR1, secondStageNR1))
    
  })
  
  substringDTR2A <- reactive({
    DTR2 <- paste(input$secondDTRcompareA)
    firstStage2 <- substr(DTR2,1,1)
    secondStageR2 <- substr(DTR2,3,3)
    secondStageNR2 <- substr(DTR2,6,6)
    
    return(c(DTR2, firstStage2, secondStageR2, secondStageNR2))
    
  })
  
  ### Series of observe functions disallows selection of more than one input option 
  ### (i.e. cell-specific, target difference, odds ratio)
  
  noCellProbA <- observe({    
    if(input$targetDiffCheckA==TRUE){
      updateCheckboxInput(session,"cellOrConditionalA",value=FALSE)
    }
  })
  
  noTargetsA<-observe({
    if(input$cellOrConditionalA==TRUE){
      updateCheckboxInput(session,"targetDiffCheckA",value=FALSE)
      updateCheckboxInput(session,"targetOddsCheckA",value=FALSE)
    }
  },priority=1)
  
  
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
  
  ##### DESIGN B PROBABILITY INPUT #####
  
  ### Read in DTR names from dropdowns and parse them to determine first and second stage treatments
  ### Reactive function allows on-the-fly changes in return values with changes in selection
  ### Outputs full DTR name, first-stage treatment (first character), second-stage treatment (last character)
      ### NOTE that these positions are exclusive to design B because responders continue first-stage treatment
  
  substringDTR1B <- reactive({
    DTR1 <-paste(input$firstDTRcompareB)
    firstStage1<-substr(DTR1,1,1)
    secondStageNR1<-substr(DTR1,6,6)
    
    return(c(DTR1, firstStage1,secondStageNR1))
    
  })
  
  substringDTR2B <- reactive({
    DTR2 <-paste(input$secondDTRcompareB)
    firstStage2<-substr(DTR2,1,1)
    secondStageNR2<-substr(DTR2,6,6)
    
    return(c(DTR2, firstStage2, secondStageNR2))
    
  })
  
  ### Series of observe functions disallows selection of more than one input option 
  ### (i.e. cell-specific, target difference, odds ratio)
  
  noCellProbB <- observe({    
    if(input$targetDiffCheckB){
      updateCheckboxInput(session,"cellOrConditionalB",value=FALSE)
    }
  })
  
  noTargetsB <- observe({
    if(input$cellOrConditionalB){
      updateCheckboxInput(session,"targetDiffCheckB",value=FALSE)
      updateCheckboxInput(session,"targetOddsCheckB",value=FALSE)
    }
  },priority=1)
  
  # When a first DTR is selected, render an input box corresponding to whatever input method is selected.
    # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR1)
    # For target-difference or OR, relevant numericInputs are rendered
      # This is the ONLY location in which difference and OR numericInputs are built.
  
  output$binaryDTR1probB <- renderUI({
    validate(
      need(input$firstDTRcompareB!=0, "Please select a first DTR.")
    )
    if(input$targetDiffCheckB==FALSE){
      if(substringDTR1B()[1] != substringDTR2B()[1]){
        DTR1inputs<-c(numericInput("DTRsuccB1",label="Probability of Success for First DTR",value=0,min=0,max=1,step=0.0001))
      }
    }
    if(input$targetDiffCheckB==TRUE && substringDTR2B()[1] != 0){
      DTR1inputs<-c(numericInput("targetDiffB",label="Target Difference in Success Probabilities",value=0.1,min=0.0001,max=0.5,step=0.0001))
    }
    
    if(input$targetOddsCheckB==TRUE && substringDTR2B()[1] != 0){
      DTR1inputs<-c(numericInput("targetORB",label="Target Odds Ratio of Success",value=2,min=0,step=0.0001))
    }
      
    if(input$targetDiffCheckB==TRUE && substringDTR2B()[1] == 0){
      DTR1inputs<-c()
    }
    
    DTR1inputs
  })
  
  # When a second DTR is selected, render an input box corresponding to whatever input method is selected.
  # For DTR-conditional or cell-specific inputs, first numericInput is P(S|DTR2)
    # For target-difference or OR, numericInputs are NOT rendered (those are handled in output$binaryDTR1probB)
  
  output$binaryDTR2probB <- renderUI({
    validate(
      need(input$secondDTRcompareB!=0, "Please select a second DTR.")
    )
    if(input$targetDiffCheckB==FALSE && input$targetOddsCheckB==FALSE){
      if(substringDTR1B()[1] != substringDTR2B()[1]){
        numericInput("DTRsuccB2",label="Probability of Success for Second DTR",value=0,min=0,max=1,step=0.0001)
      }
    }
  })
  
  # For cell-specific probabilities, render a series of numericInputs labeled by information from DTR substrings
    # When DTR1 and DTR2 begin with the same treatment, P(S|stage1trt,r) is rendered only once, in output$cellProbsDTR1B
  
  output$cellProbsDTR1B <- renderUI({
    validate(
      need(substringDTR1B()[1] != 0, "Cell-specific probabilities cannot be input until a first DTR is selected.")
    )    
    if(substringDTR1B()[1] != substringDTR2B()[1]){
        controlInputs<-c(numericInput("marginalFirstStageB1",label=paste("P(S|",substringDTR1B()[2],",r)",sep=""),value=0,min=0,max=1,step=0.0001),
                         numericInput("marginalSecondStageNRB1",label=paste("P(S|",substringDTR1B()[2],",nr,",substringDTR1B()[3],")",sep=""),
                                      value=0,min=0,max=1,step=0.0001)
        )
    }
    
  })

  output$cellProbsDTR2B <- renderUI({
    validate(
      need(substringDTR2B()[1] != 0, "Cell-specific probabilities cannot be input until a second DTR is selected.")
    )
    if(substringDTR1B()[1] != substringDTR2B()[1]){
      if(substringDTR1B()[2]==substringDTR2B()[2]){
        controlInputs<-c(numericInput("marginalSecondStageNRB2",label=paste("P(S|",substringDTR2B()[2],",nr,",substringDTR2B()[3],")",sep=""),
                         value=0,min=0,max=1,step=0.0001)
        )
        
      }
      else{
        controlInputs<-c(
                         numericInput("marginalFirstStageB2",label=paste("P(S|",substringDTR2B()[2],",r)",sep=""),value=0,min=0,max=1,step=0.0001),
                         numericInput("marginalSecondStageNRB2",label=paste("P(S|",substringDTR2B()[2],",nr,",substringDTR2B()[3],")",sep=""),
                                      value=0,min=0,max=1,step=0.0001)
        )
      }
    }
    controlInputs
  })
  
  
  # In the case that the second DTR is selected before the first and input is provided for P(S|stage1trt,r),
  # then the first DTR is selected and begins with the same intial treatment, carry over the provided input 
  # for P(S|stage1trt,r) into the new input box listed under first-stage treatment
  
  observe({
    if(substringDTR1B()[2]==substringDTR2B()[2]){
      updateNumericInput(session,"marginalFirstStageB1",value=input$marginalFirstStageB2)
    }
  })

  # When taking cell-specific inputs, update full-DTR probabilities to computed values
    # High-priority observer operates before any other observers, so response is highly reactive
  
  observe({
    if(input$cellOrConditionalB==TRUE){
      if(substringDTR1B()[2]==substringDTR2B()[2]){
        pDTR1 <- input$respB * input$marginalFirstStageB1 + (1-input$respB)*input$marginalSecondStageNRB1
        pDTR2 <- input$respB * input$marginalFirstStageB1 + (1-input$respB)*input$marginalSecondStageNRB2
      }
      
      else{
        pDTR1<- input$respB * input$marginalFirstStageB1 + (1-input$respB)*input$marginalSecondStageNRB1
        pDTR2<- input$respB * input$marginalFirstStageB2 + (1-input$respB)*input$marginalSecondStageNRB2
      }
      updateNumericInput(session,"DTRsuccB1",value=pDTR1)
      updateNumericInput(session,"DTRsuccB2",value=pDTR2)
    }
  },priority=2)
  
  # Render numericInputs for relevant values needed when outcome is continuous
  
  output$continuousProbB<-renderUI({
    validate(
      need(input$firstDTRcompareB!=0, "Please select a first DTR."),
      need(input$secondDTRcompareB!=0, "Please select a second DTR.")
    )
    if(substringDTR1B()[1]!=substringDTR2B()[1] && input$meanSdCheckB == FALSE){
      contInput<-c(numericInput("effectSizeB",label="Standardized Effect Size:",value=0,min=0,step=0.0001))
    }
    if(substringDTR1B()[1]!=substringDTR2B()[1] && input$meanSdCheckB == TRUE){
      contInput<-c(numericInput("meanDiffB",label="Difference in mean outcomes between the two selected DTRs:",value=0,min=0,step=0.0001),
                   numericInput("sdDiffB",label="Standard deviation of the above difference in means:", value=0, min=0, step=0.0001))
    }
    contInput
  })
  

  ##### DESIGN B RESULT #####
  
  # Based on provided input probabilities and selected options, compute appropriate arguments to pass to power.prop.test or pwr.norm.test
  
  dataCompilerB <- reactive({
    
    validate(need(!(is.na(input$respB)), "Please provide a response probability. If unknown, enter 0 for a conservative estimate of power or sample size."))
    
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==FALSE){
      validate(
        need(input$firstDTRcompareB != 0, "Select a first DTR above."),
        need(input$secondDTRcompareB != 0, "Select a second DTR above.") %then%
        need(input$DTRsuccB1 != input$DTRsuccB2, "Please provide unique success probabilities for each DTR. Sample size is indeterminate for equal DTR probabilities.")  
      )
      return(c(input$DTRsuccB1,input$DTRsuccB2))
    }
    
    if(input$selectOutcomeB==1 && input$cellOrConditionalB==TRUE){
          
      if(substringDTR1B()[2]==substringDTR2B()[2]){
        pDTR1 <- input$marginalSecondStageNRB1
        pDTR2 <- input$marginalSecondStageNRB2
      }
      
      else{
        pDTR1<- input$respB * input$marginalFirstStageB1 + (1-input$respB)*input$marginalSecondStageNRB1
        pDTR2<- input$respB * input$marginalFirstStageB2 + (1-input$respB)*input$marginalSecondStageNRB2
      }
      
      validate(
        need(input$firstDTRcompareB != 0, "Select a first DTR above."),
        need(input$firstDTRcompareB != 0, "select a second DTR above."),
        need(pDTR1 != pDTR2, "The provided marginal probabilities yield identical overall DTR success probabilities. Sample size is indeterminate for equal DTR probabilities. Please adjust your inputs.")
      )
      
      return(c(pDTR1,pDTR2))
     }
    
    if(input$selectOutcomeB==1 && input$targetDiffCheckB==TRUE){
      validate(
        need(input$targetDiffB <= 0.5, "Target difference must be less than 0.5 to be valid input"),
        need(input$targetDiffB > 0, "Target difference must be greater than 0. Sample size is indeterminate for equal DTR probabilities.
             Please adjust your inputs.")
      )
      return(c(0.5,0.5+input$targetDiffB))
    }
    
    if(input$selectOutcomeB==1 && input$targetOddsCheckB==TRUE){
      validate(
        need(input$targetORB != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio."),
        need(input$targetORB != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
      )
      return(c(0.5,input$targetORB/(1+input$targetORB)))
    }
    
    if(input$selectOutcomeB==2 && input$meanSdCheckB==FALSE){
      validate(
        need(input$effectSizeB != 0, "Sample size is indeterminate for an effect size of 0. Please enter a different target effect size.")
      )
      
      return(input$effectSizeB)
    }
    
    if(input$selectOutcomeB==2 && input$meanSdCheckB==TRUE){
      validate(
        need(input$meanDiffB != 0, "Sample size is indeterminate for a mean difference of 0."),
        need(input$sdDiffB != 0, "Sample size is indeterminate for a standard deviation of 0.")
      )
      effectSize <- abs(input$meanDiffB / input$sdDiffB)
      
      return(effectSize)
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
  
  # Pass arguments from dataCompilerB() to appropriate R function; extract and render relevant output
  
  output$binarySampleSizeB <- renderPrint({
    designEffect<-selectEffectB()
    rawSampleSize<-power.prop.test(p1=dataCompilerB()[1],p2=dataCompilerB()[2],power=input$inputPowerB,sig.level=input$alphaB)$n
    finalSampleSize<-ceiling(designEffect * rawSampleSize)
    cat("N=",finalSampleSize)
  })

  output$binaryPowerB <- renderPrint({
    designEffect<-selectEffectB()
    size<-(input$inputSampleSizeB/designEffect)
    finalPower<-round(power.prop.test(p1=dataCompilerB()[1],p2=dataCompilerB()[2],n=size,sig.level=input$alphaB)$power,digits=3)
    cat("Power =",finalPower)
  })
  
  output$continuousSampleSizeB <- renderPrint({
    designEffect<-selectEffectB()
    rawSampleSize<-pwr.norm.test(d=dataCompilerB(),sig.level=input$alphaB,power=input$inputPowerB,alternative="two.sided")
    finalSampleSize<-ceiling(2*designEffect*rawSampleSize$n)
    cat("N=",finalSampleSize)
  })
  
  output$continuousPowerB <- renderPrint({
    designEffect<-selectEffectB()
    size<-(input$inputSampleSizeB/(2*designEffect))
    finalPower<-round(pwr.norm.test(d=dataCompilerB(),sig.level=input$alphaB,n=size,alternative="two.sided")$power,digits=3)
    cat("Power =",finalPower)
  })


})
