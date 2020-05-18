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
    
    ## JQuery Interactivity
    jqui_draggable("#sizeWizardDiv,#dyo.diagram")
    jqui_resizable("#sizeWizardDiv")
    
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
                                          a("Click here for a
                                            n example from the field.", `data-toggle` = "modal",
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
    
    ##### DYO Header #####
    shinyBS::createAlert(session, anchorId = "dyo-functional",
                         title = "Use with Caution!",
                         content = paste("<p>This section is only fully functional for comparisons of",
                                         "embedded AIs with a binary outcome. Attempts to use this page",
                                         "for other primary aims may result in errors or unexpected behavior.</p>",
                                         "<p><strong>Full functionality is coming soon.</strong></p>"),
                         style = "warning", dismiss = FALSE)
    
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
                                 label = paste0("Probability of allocation to Treatment ", LETTERS[i]), 
                                 min = 0, max = 1, value = NA, step = 0.01)
                  }),
           helpText(paste("The probability of allocation to Treatment", LETTERS[isolate(input$dyo.stage1.ntxt)],
                          "will automatically update so that all allocation probabilities sum to 1.")))
    })
    output$dyo.rerand.resp.rprobUI <- renderUI({
      list(lapply(1:input$dyo.rerand.resp.ntxt, 
                  function(i) {
                    numericInput(paste0("dyo.rerand.resp.txt", i, ".rprob"),
                                 label = paste0("Probability of allocation to Treatment ", LETTERS[i]), 
                                 min = 0, max = 1, value = NA, step = 0.01)
                  }),
           helpText(paste("The probability of allocation to Treatment", LETTERS[isolate(input$dyo.rerand.resp.ntxt)],
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
    
    
    ##### Stage 1 Treatment Comparison Selectors #####
    
    stage1TxNames <- reactive({
      LETTERS[1:input$dyo.stage1.ntxt]
    })
    
    output$stage1Tx1Select <- renderUI({
      selectizeInput("dyo.stage1.Tx1",
                     label = "",
                     choices = stage1TxNames(),
                     options = list(
                       placeholder = "Select a first-stage treatment.",
                       onInitialize = I("function() { this.setValue(''); }")
                     ))
    })
    
    output$stage1Tx2Select <- renderUI({
      selectizeInput("dyo.stage1.Tx2",
                     label = "",
                     choices = stage1TxNames()[stage1TxNames() != input$dyo.stage1.Tx1],
                     options = list(
                       placeholder = "Select a first-stage treatment.",
                       onInitialize = I("function() { this.setValue(''); }")
                     )
      )})
    
    ##### Stage 2 Responder Treatment Comparison Selectors #####
    
    stage2RTxNames <- reactive({
      validate(
        need(isTruthy(input$dyo.rerand.resp.ntxt), 'oops')
      )
      paste0("r", 1:input$dyo.rerand.resp.ntxt)
    })
    
    output$stage2RTx1Select <- renderUI({
      selectizeInput("dyo.stage2R.Tx1",
                     label = "",
                     choices = stage2RTxNames(),
                     options = list(
                       placeholder = "Select a second-stage treatment.",
                       onInitialize = I("function() { this.setValue(''); }")
                     )
                     )
    })
    
    output$stage2RTx2Select <- renderUI({
      selectizeInput("dyo.stage2R.Tx2",
                     label = "",
                     choices = stage2RTxNames()[stage2RTxNames() != input$dyo.stage2R.Tx1],
                     options = list(
                       placeholder = "Select a second-stage treatment.",
                       onInitialize = I("function() { this.setValue(''); }")
                     )
      )
    })
    
    ##### Stage 2 Non-Responder Treatment Comparison Selectors #####
    
    stage2NRTxNames <- reactive({
      validate(
        need(isTruthy(input$dyo.rerand.nresp.ntxt), 'oops')
      )
      paste0("nr", 1:input$dyo.rerand.nresp.ntxt)
    })
    
    output$stage2NRTx1Select <- renderUI({
      selectizeInput("dyo.stage2NR.Tx1",
                     label = "",
                     choices = stage2NRTxNames(),
                     options = list(
                       placeholder = "Select a second-stage treatment.",
                       onInitialize = I("function() { this.setValue(''); }")
                     )
      )
    })
    
    output$stage2NRTx2Select <- renderUI({
      selectizeInput("dyo.stage2NR.Tx2",
                     label = "",
                     choices = stage2NRTxNames()[stage2NRTxNames() != input$dyo.stage2NR.Tx1],
                     options = list(
                       placeholder = "Select a second-stage treatment.",
                       onInitialize = I("function() { this.setValue(''); }")
                     )
      )
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
    
    ##### DTR Selection Inputs #####
    
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
    
    ##### Binary DTR Probability Inputs #####
    
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
    
    
    ##### Binary Stage 1 Probability Inputs #####
    output$binaryStage1Probs <- renderUI({
      
      if (input$dyo.stage1.ntxt > 2) {
        validate(
          need(isTruthy(input$dyo.stage1.Tx1), "Please select a first first-stage treatment to compare."),
          need(isTruthy(input$dyo.stage1.Tx2), "Please select a second first-stage treatment to compare.")
        )
      }
      
      # get names of treatments to build inputs for
      if (input$dyo.stage1.ntxt == 2) {
        treatmentNames <- LETTERS[1:2]
      } else if (input$dyo.stage1.ntxt > 2) {
        treatmentNames <- c(input$dyo.stage1.Tx1, input$dyo.stage1.Tx2)
      }
      
      lapply(1:length(treatmentNames), function(i) {
        numericInput(inputId = paste0("stage1probInput", i),
                     label = paste("Success probability for Treatment", 
                                   treatmentNames[i]),
                     value = NA)
      })
    })
    
    ##### Binary Stage 2R Probability Inputs #####
    output$binaryStage2RProbs <- renderUI({
      
      if (input$dyo.rerand.resp.ntxt > 2) {
        validate(
          need(isTruthy(input$dyo.stage2R.Tx1), 
               "Please select a first second-stage treatment to compare."),
          need(isTruthy(input$dyo.stage2R.Tx2),
               "Please select a second second-stage treatment to compare.")
        )
      }
      
      # get names of treatments to build inputs for
      if (input$dyo.rerand.resp.ntxt == 2) {
        treatmentNames <- c("r1", "r2")
      } else if (input$dyo.rerand.resp.ntxt > 2) {
        treatmentNames <- c(input$dyo.stage2R.Tx1, input$dyo.stage2R.Tx2)
      }
      
      lapply(1:length(treatmentNames), function(i) {
        numericInput(inputId = paste0("stage2RprobInput", i),
                     label = paste("Success probability for Treatment", 
                                   treatmentNames[i]),
                     value = NA)
      })
    })
    
    ##### Binary Stage 2NR Probability Inputs #####
    output$binaryStage2NRProbs <- renderUI({
      
      if (input$dyo.rerand.nresp.ntxt > 2) {
        validate(
          need(isTruthy(input$dyo.stage2NR.Tx1), 
               "Please select a first second-stage treatment to compare."),
          need(isTruthy(input$dyo.stage2NR.Tx2),
               "Please select a second second-stage treatment to compare.")
        )
      }
      
      # get names of treatments to build inputs for
      if (input$dyo.rerand.nresp.ntxt == 2) {
        treatmentNames <- c("nr1", "nr2")
      } else if (input$dyo.rerand.nresp.ntxt > 2) {
        treatmentNames <- c(input$dyo.stage2NR.Tx1, input$dyo.stage2NR.Tx2)
      }
      
      lapply(1:length(treatmentNames), function(i) {
        numericInput(inputId = paste0("stage2NRprobInput", i),
                     label = paste("Success probability for Treatment", 
                                   treatmentNames[i]),
                     value = NA)
      })
    })
    
    ##### Continuous DTR Effect Size #####
    output$contDTRInput <- renderUI({
      if (isTruthy(primaryAim() == "dtrs")) {
        validate(
          need(isTruthy(refDTR.substr()[1]), text.refDTRPlaceholder),
          need(isTruthy(compDTR.substr()[1]), text.compDTRPlaceholder)
        )
      }
      list(helpText("this is not currently functional"),
           numericInput("dyo.contEffectSize", label = text.stdEffectLabel,
                        value = NA,
                        min = 0, max = 10, step = 0.01)
      )
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
          if (input$dyo.stage1.ntxt == 2)
            TxNames <- c("A", "B")
          
          if (input[["dyo.primaryAim-primaryAim"]] == "stage1") {
            
            if (input$dyo.stage1.ntxt > 2) 
              TxNames <- c(input$dyo.stage1.Tx1, input$dyo.stage1.Tx2)
            
            Tx1RNames <- 
              paste0(TxNames[1],
                     paste0("r",
                            if (TxNames[1] %in% input$dyo.rerand.resp.whichtxt & 
                                input$dyo.rerand.resp == "Yes")
                              1:input$dyo.rerand.resp.ntxt
                            else
                              ""))
            Tx1NRNames <-
              paste0(TxNames[1],
                     paste0("nr",
                            if (TxNames[1] %in% input$dyo.rerand.nresp.whichtxt & 
                                input$dyo.rerand.nresp == "Yes")
                              1:input$dyo.rerand.nresp.ntxt
                            else
                              ""))
            Tx2RNames <- 
              paste0(TxNames[2],
                     paste0("r",
                            if (TxNames[2] %in% input$dyo.rerand.resp.whichtxt & 
                                input$dyo.rerand.resp == "Yes")
                              1:input$dyo.rerand.resp.ntxt
                            else
                              ""))
            Tx2NRNames <-
              paste0(TxNames[2],
                     paste0("nr",
                            if (TxNames[2] %in% input$dyo.rerand.nresp.whichtxt & 
                                input$dyo.rerand.nresp == "Yes")
                              1:input$dyo.rerand.nresp.ntxt
                            else
                              ""))
            
            rclass <- paste0("class ", TxNames[1], ",",
                             paste(Tx1RNames, collapse = ","),
                             ",", paste(Tx1NRNames, collapse = ","),
                             " refdtr;\n")
            cclass <- paste0("class ", TxNames[2], ",",
                             paste(Tx2RNames, collapse = ","),
                             ",", paste(Tx2NRNames, collapse = ","),
                             " compdtr;\n")
          } else if (input[["dyo.primaryAim-primaryAim"]] == "stage2resp") {
            if (input$dyo.stage1.ntxt > 2) 
              TxNames <- input$dyo.rerand.resp.whichtxt
            if (input$dyo.rerand.resp.ntxt == 2) {
              rclass <- 
                paste0("class ", paste0(TxNames, "r1", collapse = ","), " refdtr;\n")
              # Tx2RNames2 <- paste0(TxNames, "r2", collapse = ",")
              cclass <- 
                paste0("class ", paste0(TxNames, "r2", collapse = ","), " compdtr;\n")
            } else if (input$dyo.rerand.resp.ntxt > 2) {
              if (isTruthy(input$dyo.stage2R.Tx1))
                rclass <-
                  paste0("class ", 
                         paste0(TxNames, input$dyo.stage2R.Tx1, collapse = ","),
                         " refdtr;\n")
              else rclass <- ""
              if (isTruthy(input$dyo.stage2R.Tx2))
                cclass <- 
                  paste0("class ", 
                         paste0(TxNames, input$dyo.stage2R.Tx2, collapse = ","),
                         " compdtr;\n")
              else cclass <- ""
            }
          } else if (input[["dyo.primaryAim-primaryAim"]] == "stage2nresp") {
            if (input$dyo.stage1.ntxt > 2) 
              TxNames <- input$dyo.rerand.nresp.whichtxt
            if (input$dyo.rerand.resp.ntxt == 2) {
              rclass <- 
                paste0("class ", paste0(TxNames, "nr1", collapse = ","), " refdtr;\n")
              cclass <- 
                paste0("class ", paste0(TxNames, "nr2", collapse = ","), " compdtr;\n")
            } else if (input$dyo.rerand.nresp.ntxt > 2) {
              if (isTruthy(input$dyo.stage2NR.Tx1))
                rclass <-
                  paste0("class ", 
                         paste0(TxNames, input$dyo.stage2NR.Tx1, collapse = ","),
                         " refdtr;\n")
              else rclass <- ""
              if (isTruthy(input$dyo.stage2NR.Tx2))
                cclass <- 
                  paste0("class ", 
                         paste0(TxNames, input$dyo.stage2NR.Tx2, collapse = ","),
                         " compdtr;\n")
              else cclass <- ""
            }
          }
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
    
    updateWizard <- function(tabID, wizardID) {
      updateTabsetPanel(session, wizardID, selected = tabID)
    }
    
    # Observer for button "dyo.stage1.continue" inside stage 1 design spec collapse panel
    # When button is pressed, update the collapse so that the "close" panel closes and the "open" panel opens
    observeEvent(input$outcome.continue,
                 updateWizard("wizard.stage1.describe", "designWizard"))
    # observeEvent(input$resultOptions.continue,
    #              updateWizard("wizard.stage1.describe"))
    observeEvent(input$stage1.continue,
                 updateWizard("wizard.resp.describe", "designWizard"))
    observeEvent(input$resp.continue,
                 updateWizard("wizard.stage2.describe", "designWizard"))
    
    observeEvent(input$stage1.back,
                 updateWizard("wizard.outcome.describe", "designWizard"))
    observeEvent(input$resp.back,
                 updateWizard("wizard.stage1.describe", "designWizard"))
    observeEvent(input$stage2.back,
                 updateWizard("wizard.resp.describe", "designWizard"))
    
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
    
    
    ##### Compute A and B #####
    
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
          for (i in 1:input$dyo.stage1.ntxt) {
            validate(need(isTruthy(input[[paste0("dyo.stage1.", LETTERS[i], ".resprob")]]),
                          paste0("Please provide the probability of response to treatment ", LETTERS[i], ".")) %then%
                     need(input[[paste0("dyo.stage1.", LETTERS[i], ".resprob")]] >= 0 & input[[paste0("dyo.stage1.", LETTERS[i], ".resprob")]] <= 1,
                          "hello")
                     )
          }
          rprobs <- sapply(1:input$dyo.stage1.ntxt, function(i) {
            input[[paste0("dyo.stage1.", LETTERS[i], ".resprob")]]
          })
          return(rprobs)
        }
      }
    })
    
    output$rprobs <- renderText(respProb())
    
    # if the responders are re-randomized, get randomization probabilities for
    # each second-stage treatment for responders
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
    
    # if the non-responders are re-randomized, get randomization probabilities
    # for each second-stage treatment for non-responders
    pi2NR <- reactive({
      if (input$dyo.rerand.nresp == "Yes") {
        l <- sapply(1:input$dyo.rerand.nresp.ntxt, function(i) {
          if (LETTERS[i] %in% input$dyo.rerand.nresp.whichtxt) {
            if (input$dyo.rerand.nresp.eqrand == "Yes") { #equally randomized?
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
        return(paste(text.sentenceAIinterest, input$dyoRefDTR,
                     " and ", input$dyoCompDTR, text.sentenceOverallSuccess, input$refDTRProb,
                     " and ", input$compDTRProb, ", respectively", sep = ""))
      }
      if (dyo.outcome() == "Binary" && input$cellOrMarginal == TRUE) {
        return(paste(text.sentenceAIinterest, input$dyoRefDTR, " and ",
                     input$dyoCompDTR, text.sentenceOverallSuccess, generateProbs()[1],
                     " and ", generateProbs()[2], ", respectively", sep = ""))
      }
      if (dyo.outcome() == "Continuous") {
        return(paste(text.sentenceEffectSize, input$firstDTRcompareA, " and ", 
                     input$secondDTRcompareA, ", is ", input$effectSizeA, sep = ""))
      }
    })
    
    sentenceCompiler.response <- reactive({
      if (!input$conservative) {
        if (input$dyo.stage1.resprob.eq == "Yes") {
          formatResp <- input$dyo.stage1.allTxt.resprob
        } else {
          formatResp <- sapply(1:input$dyo.stage1.ntxt, function(i) {
            input[[paste0("dyo.stage1.", LETTERS[i], ".resprob")]]
          })
        }
      }
      
      paste0("in which ", 
             ifelse(input$dyo.stage1.resprob.eq == "Yes", 
                    paste0("the probability of response to ", 
                           ifelse(input$dyo.stage1.ntxt == 2, "both ", "all "),
                           "first-stage treatments is ", unique(respProb())),
                    paste0(
                      paste(
                        "the probability of response to",
                        sapply(1:(input$dyo.stage1.ntxt - 1), function(i) {
                          paste("first-stage treatment", LETTERS[i], "is",
                                respProb()[i])
                        }),
                        collapse = ", "
                      ),
                      ", and the probability of response to first-stage treatment ",
                      LETTERS[input$dyo.stage1.ntxt],
                      " is ",
                      respProb()[input$dyo.stage1.ntxt],
                      ". "
                    )))
    })
    
    output$sentence <- renderText(sentenceCompiler())
    
    output$continuousSampleSize <- renderUI({
      validate(
        need(isTruthy(dyo.resultOptions()$inputPower), text.noPower) %then%
          need(dyo.resultOptions()$inputPower > 0, text.power0) %then%
          need(dyo.resultOptions()$inputPower < 1, text.power100)
      )
      
      
    })
    
    output$binarySampleSize <- renderUI({
      validate(
        need(isTruthy(dyo.resultOptions()$inputPower), text.noPower) %then%
          need(dyo.resultOptions()$inputPower > 0, text.power0) %then%
          need(dyo.resultOptions()$inputPower < 1, text.power100)
      )
      
      if (isTruthy(primaryAim() == "dtrs")){
        validate(
          need(isTruthy(refDTR.substr()[1]), text.refDTRPlaceholder),
          need(isTruthy(compDTR.substr()[1]), text.compDTRPlaceholder)  %then%
            validate(
              need(isTruthy(input$refDTRProb),  paste0("The success probability is missing for AI ", refDTR.substr()[1],  ". Please provide an input.")),
              need(isTruthy(input$compDTRProb), paste0("The success probability is missing for AI ", compDTR.substr()[1], ". Please provide an input."))
            )
        )
      } else if (isTruthy(primaryAim() == "stage1")) {
        # need(isTruthy())
      }
      
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
      formatAltHyp    <- switch(input$selectAlternativeC, "one.sided" = "one-sided ", "two.sided" = "two-sided ")
      
      ## Result sentence
      HTML(paste0("<h4 style='color:blue';> N=", finalSampleSize, "</h4>
                 <p> We wish to find the sample size for a SMART with a binary outcome ",
                 ifelse(input$conservative, "using a conservative approach. ", 
                        sentenceCompiler.response()),
                 sentenceCompiler(),
                 ". Given a ", formatAltHyp, " test with ", formatAlpha, " type-I error, we require a sample size of at least ",
                 finalSampleSize, " to make this comparison with at least ",formatPower," power. </p>"))
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
    
    # If the user checks the targetDifference box, uncheck the other options
    observeEvent(req(input$targetDifference), {
      updateCheckboxInput(session, "cellOrMarginal",  value = FALSE)
      updateCheckboxInput(session, "targetOddsRatio", value = FALSE)
    })
    
    # If the user checks the targetOddsRatio box, uncheck the other options
    observeEvent(req(input$targetOddsRatio), {
      updateCheckboxInput(session, "cellOrMarginal",  value = FALSE)
      updateCheckboxInput(session, "targetDifference", value = FALSE)
    })
    
    # If a user checks the conservative sample size box, disable response probability box
    observeEvent(input$conservative, {
      shinyjs::toggleState("dyo.stage1.resprob.eq", condition = !input$conservative)
      updateRadioButtons(session, "dyo.stage1.resprob.eq", selected = "Yes")
      shinyjs::toggleState("cellOrMarginal", condition = !input$conservative)
      updateCheckboxInput(session, "cellOrMarginal", value = FALSE)
    })
    observe({
      if (input$conservative) {
        shinyBS::createAlert(session, anchorId = "premade-design-disabled-resp-describe",
                             alertId = "alert-conservative",
                             title = "This section has been disabled.",
                             content = paste("Conservative estimates of sample size do not depend on response rates.",
                                             "If you have estimates of response rate(s), uncheck the box."))
        shinyBS::createAlert(session, anchorId = "cellOrMarginalDisabled",
                             alertId = "alert-conservative-cellOrMarginalDisabled",
                             content = paste("Cell-specific probabilities cannot be provided when",
                                             "you request a conservative sample size. If you would like to provide"))
        if (input$dyo.stage1.resprob.eq == "Yes")
          shinyjs::disable("dyo.stage1.allTxt.resprob")
      } else {
        shinyBS::closeAlert(session, "alert-conservative")
        shinyBS::closeAlert(session, "alert-conservative-cellOrMarginalDisabled")
      }
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
                           alertId = "premade-design-stage1-alert",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premade-design-disabled-resp-describe",
                           alertId = "premade-design-resp-alert",
                           title = "Part of this section has been disabled.",
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premadeDesignInputsDisabled-stage2Alert",
                           alertId = "premade-design-stage2-alert",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      
      # UI updates
      updateCollapse(session, "dyo.setup.collapse",
                     close = "dyo.stage1.describe")
      shinyjs::hide("dyo.stage1.resprob.continue")
      shinyjs::disable("dyo.stage1.describe")
      updateTabsetPanel(session, "SMARTsize", selected = "Design and Size")
      shinyjs::show("unlockDYO") # unhide button to unlock UI
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
                           alertId = "premade-design-stage1-alert",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premade-design-disabled-resp-describe",
                           alertId = "premade-design-resp-alert",
                           title = "Part of this section has been disabled.",
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premadeDesignInputsDisabled-stage2Alert",
                           alertId = "premade-design-stage2-alert",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      
      # UI updates
      updateCollapse(session, "dyo.setup.collapse",
                     close = "dyo.stage1.describe")
      shinyjs::hide("dyo.stage1.resprob.continue")
      shinyjs::disable("dyo.stage1.describe")
      updateTabsetPanel(session, "SMARTsize", selected = "Design and Size")
      shinyjs::show("unlockDYO") # unhide button to unlock UI
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
                           alertId = "premade-design-stage1-alert",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premade-design-disabled-resp-describe",
                           alertId = "premade-design-resp-alert",
                           title = "Part of this section has been disabled.",
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      shinyBS::createAlert(session,
                           anchorId = "premadeDesignInputsDisabled-stage2Alert",
                           alertId = "premade-design-stage2-alert",
                           title = text.alert.premadeDisabled.title,
                           content = "Because you've selected a premade design, you are unable to edit these parameters.")
      
      # UI updates
      updateCollapse(session, "dyo.setup.collapse",
                     close = "dyo.stage1.describe")
      shinyjs::hide("dyo.stage1.resprob.continue")
      shinyjs::disable("dyo.stage1.describe")
      updateTabsetPanel(session, "SMARTsize", selected = "Design and Size")
      shinyjs::show("unlockDYO") # unhide button to unlock UI
    })
    
    ## Unlock disabled sections
    observeEvent(input$unlockDYO, {
      # Input updates: stage 1 treatments
      shinyjs::enable("dyo.stage1.ntxt")
      shinyjs::enable("dyo.stage1.eqrand")
      
      # Input updates: equal response
      shinyjs::enable("dyo.stage1.resprob.eq")
      
      # Input updates: stage 2 treatments
      shinyjs::enable("dyo.rerand.resp")
      shinyjs::enable("dyo.rerand.resp.ntxt")
      shinyjs::enable("dyo.rerand.resp.eqrand")
      shinyjs::enable("dyo.rerand.nresp")
      shinyjs::enable("dyo.rerand.nresp.ntxt")
      shinyjs::enable("dyo.rerand.nresp.eqrand")
      
      # Close alerts
      closeAlert(session, "premade-design-stage1-alert")
      closeAlert(session, "premade-design-resp-alert")
      closeAlert(session, "premade-design-stage2-alert")
    })
    
  }) # END shinyServer