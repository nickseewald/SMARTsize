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
       list(src=filename,height=450)
   },deleteFile=FALSE)
  
  ##### DESIGN B PROBABILITY INPUT #####

  substringDTR1B <- reactive({
    DTR1 <-paste(input$firstDTRcompareB)
    firstStage1<-substr(DTR1,1,1)
    secondStageNR1<-substr(DTR1,6,6)
    
    if (input$selectOutcomeB==1 & input$selectBinarySuccessB <= 2){
      validate(
        need(DTR1!=0, "Please select a first DTR.")
      )
    }    
    
    return(c(DTR1, firstStage1,secondStageNR1))
    
  })
  
  substringDTR2B <- reactive({
    DTR2 <-paste(input$secondDTRcompareB)
    firstStage2<-substr(DTR2,1,1)
    secondStageNR2<-substr(DTR2,6,6)
    
    if (input$selectOutcomeB==1 && input$selectBinarySuccessB <= 2){
      validate(
        need(DTR2!=0, "Please select a second DTR.")
      )
    }    
    
    return(c(DTR2, firstStage2, secondStageNR2))
    
  })
  
  output$binaryDTR1probB <- renderUI({
    if(substringDTR1B()[1] != substringDTR2B()[1]){
      numericInput("DTRsuccB1",label="Probability of Success for First DTR",value=0,min=0,max=1,step=0.0001)
    }
  })
  
  output$binaryDTR2probB <- renderUI({
    if(substringDTR1B()[1] != substringDTR2B()[1]){
      numericInput("DTRsuccB2",label="Probability of Success for Second DTR",value=0,min=0,max=1,step=0.0001)
    }
  })
  
  output$cellProbsDTR1B <- renderUI({
    if(substringDTR1B()[1] != substringDTR2B()[1]){
      if(substringDTR1B()[2]==substringDTR2B()[2]){
        controlInputs<-c(numericInput("marginalSecondStageNRB1",label=paste("P(S|",substringDTR1B()[2],",nr,",substringDTR1B()[3],")",sep=""),
                                      value=0,min=0,max=1,step=0.0001)
        )
        
      }
      else{
        controlInputs<-c(numericInput("marginalFirstStageB1",label=paste("P(S|",substringDTR1B()[2],",r)",sep=""),value=0,min=0,max=1,step=0.0001),
                         numericInput("marginalSecondStageNRB1",label=paste("P(S|",substringDTR1B()[2],",nr,",substringDTR1B()[3],")",sep=""),
                                      value=0,min=0,max=1,step=0.0001)
        )
      }
    }
    
  })

  output$cellProbsDTR2B <- renderUI({
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
    
    suppressWarnings(controlInputs)
    
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
  
  output$binaryDTRinputsB <- renderUI({
    
    if(input$selectBinarySuccessB==2 && substringDTR1B()[1] != substringDTR2B()[1]){
      controlInputs<-c(numericInput("DTRsuccB1",label="Probability of Success for First DTR",value=0,min=0,max=1,step=0.0001),
                       numericInput("DTRsuccB2",label="Probability of Success for Second DTR",value=0,min=0,max=1,step=0.0001)
      )
    }
    
    
    
    if(input$selectBinarySuccessB==1 && substringDTR1B()[2]==substringDTR2B()[2]){
      updateSelectInput(session,inputId="selectBinarySuccessB",selected=2)      
    }
    
    if(input$selectBinarySuccessB==3){
      controlInputs<-c(numericInput("targetDiffB",label="Target Difference in Success Probabilities",value=0.1,min=0.0001,max=0.5,step=0.0001))
    }
    
    if(input$selectBinarySuccessB==4){
      controlInputs<-c(numericInput("targetORB",label="Target Odds Ratio of Success",value=2,min=0,step=0.0001))
    }
    
    if(substringDTR1B()[1]==substringDTR2B()[1] && input$selectBinarySuccessB <= 2){
      controlInputs<-(helpText("Select two non-identical DTRs to continue."))
    }
        
  })

  ##### DESIGN B RESULT #####
  
  dataCompilerB <- reactive({
    
    validate(need(!(is.na(input$respB)), "Please provide a response probability. If unknown, enter 0 for a conservative estimate of power or sample size."))
    
    if(input$selectOutcomeB==1 && input$selectBinarySuccessB==1){
      validate(
        need(input$firstDTRcompareB != 0, "Select a first DTR above."),
        need(input$secondDTRcompareB != 0, "Select a second DTR above.") %then%
        need(input$DTRsuccB1 != input$DTRsuccB2, "Please provide unique success probabilities for each DTR. Sample size is indeterminate for equal DTR probabilities.")  
      )
      return(c(input$DTRsuccB1,input$DTRsuccB2))
    }
    
    if(input$selectOutcomeB==1 && input$selectBinarySuccessB==2){
          
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
    
    if(input$selectOutcomeB==1 && input$selectBinarySuccessB==3){
      validate(
        need(input$targetDiffB <= 0.5, "Target difference must be less than 0.5 to be valid input"),
        need(input$targetDiffB > 0, "Target difference must be greater than 0. Sample size is indeterminate for equal DTR probabilities.
             Please adjust your inputs.")
      )
      return(c(0.5,0.5+input$targetDiffB))
    }
    
    if(input$selectOutcomeB==1 && input$selectBinarySuccessB==4){
      validate(
        need(input$targetORB != 1, "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio."),
        need(input$targetORB != 0, "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio.")
      )
      return(c(0.5,input$targetORB/(1+input$targetORB)))
    }
    
    if(input$selectOutcomeB==2){
      validate(
        need(input$effectSizeB != 0, "Sample size is indeterminate for an effect size of 0. Please enter a different target effect size.")
      )
      return(input$effectSizeB)
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
    size<-input$inputSampleSizeB/designEffect
    finalPower<-round(power.prop.test(p1=dataCompilerB()[1],p2=dataCompilerB()[2],n=size,sig.level=input$alphaB)$power,digits=3)
    cat("Power =",finalPower)
  })
  
  output$continuousSampleSizeB <- renderPrint({
    designEffect<selectEffectB()
    rawSampleSize<-pwr.norm.test(d=dataCompilerB(),sig.level=input$alphaB,power=input$inputPowerB,alternative="two.sided")
    finalSampleSize<-ceiling(2*designEffect*rawSampleSize$n)
    cat("N=",finalSampleSize)
  })
  

})
