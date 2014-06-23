library(shiny)
library(pwr)

`%then%` <- shiny:::`%OR%`

shinyServer(function(input,output,session){
   
  output$dialog <- renderPrint({
    code <- input$console
    output <- eval(parse(text=code))
    return(output)
  })
  
  ##### DESIGN B IMAGE HEADER #####
  
  output$designBimg <- renderImage({
       filename<-filename<-normalizePath(file.path('./www/images',paste('SMARTdesignB_',input$firstDTRcompareB,'_',input$secondDTRcompareB,'.gif',sep='')))
       list(src=filename)
   },deleteFile=FALSE)
  
  ##### DESIGN B PROBABILITY INPUT #####

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
  
  
#   observe({
#     if(input$targetDiffCheckB==FALSE){
#       updateCheckboxInput(session,"meanSDB",value=FALSE)
#     }
#     
#     if(input$selectOutcomeB==2){
#       updateCheckboxInput(session,"targetDiffCheckB",label="Check this box to input a target standardized effect size.")
#     }
#     
#     if(input$selectOutcomeB==1){
#       updateCheckboxInput(session,"targetDiffCheckB",label="Check this box to input a target difference in proportions.")
#     }
#   })
  
  # Series of observe functions disallows selection of more than one
  # input option (e.g. cell-specific, target diff, odds ratio)
  
  observe({    
    if(input$targetDiffCheckB==TRUE){
      updateCheckboxInput(session,"cellOrConditionalB",value=FALSE)
    }
  })
  
  observe({
    if(input$cellOrConditionalB==TRUE){
      updateCheckboxInput(session,"targetDiffCheckB",value=FALSE)
      updateCheckboxInput(session,"targetOddsCheckB",value=FALSE)
    }
  })

  
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
  

#       if(input$cellOrConditionalB==TRUE && substringDTR1B()[1] != substringDTR2B()[1]){
#         controlInputs<-c(numericInput("DTRsuccB1",label="Probability of Success for First DTR",value=0,min=0,max=1,step=0.0001),
#                          textOutput("blankspace"),
#                          numericInput("marginalSecondStageNRB1",label=paste("P(S|",substringDTR1B()[2],",nr,",substringDTR1B()[3],")",sep=""),
#                                                  value=0,min=0,max=1,step=0.0001),
#                          textOutput("blankspace"),
#                          numericInput("marginalSecondStageNRB2",label=paste("P(S|",substringDTR2B()[2],",nr,",substringDTR2B()[3],")",sep=""),
#                                                 value=0,min=0,max=1,step=0.0001),
#                          numericInput("DTRsuccB2",label="Probability of Success for Second DTR",value=0,min=0,max=1,step=0.0001))
#       }
# 
#     if(substringDTR1B()[1]==substringDTR2B()[1] && input$selectBinarySuccessB <= 2){
#       controlInputs<-(helpText("Select two non-identical DTRs to continue."))
#     }
#     
#     return(controlInputs)
#     
#   })
  
#   output$binaryDTRinputsB <- renderUI({
#     
#     if(input$selectBinarySuccessB==2 && substringDTR1B()[1] != substringDTR2B()[1]){
#       controlInputs<-c(numericInput("DTRsuccB1",label="Probability of Success for First DTR",value=0,min=0,max=1,step=0.0001),
#                        numericInput("DTRsuccB2",label="Probability of Success for Second DTR",value=0,min=0,max=1,step=0.0001)
#       )
#     }
#     
#     if(input$selectBinarySuccessB==1 && substringDTR1B()[2]==substringDTR2B()[2]){
#       updateSelectInput(session,inputId="selectBinarySuccessB",selected=2)      
#     }
#     
#     if(input$targetDiffCheckB==TRUE){
#       controlInputs<-c(numericInput("targetDiffB",label="Target Difference in Success Probabilities",value=0.1,min=0.0001,max=0.5,step=0.0001))
#     }
#     
#     if(input$selectBinarySuccessB==4){
#       controlInputs<-c(numericInput("targetORB",label="Target Odds Ratio of Success",value=2,min=0,step=0.0001))
#     }
#     
#     if(substringDTR1B()[1]==substringDTR2B()[1] && input$selectBinarySuccessB <= 2){
#       controlInputs<-(helpText("Select two non-identical DTRs to continue."))
#     }
#         
#   })

  ##### DESIGN B RESULT #####
  
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
    
    #if(input$)
    
  })

  selectEffectB <- reactive({
    if(substringDTR1B()[2] != substringDTR2B()[2]){
      designEffect <- 2*(2*(1-input$respB)+input$respB)
    }
    else{
      designEffect <- 4/(1-input$respB)
    }
    return(designEffect)
  })

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
