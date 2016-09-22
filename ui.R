### UI.R FOR SMART SAMPLE SIZE CALCULATOR ###
### NICK SEEWALD, 2014
### UNIVERSITY OF MICHIGAN
### DEPARTMENT OF BIOSTATISTICS

options(encoding = 'UTF-8')

shinyUI(
  # shinybootstrap2::withBootstrap2({
  tagList(useShinyjs(),
          navbarPage("SMART Sample Size Calculator", id = "SMARTsize", collapsible = TRUE, footer = " ",
             
             
             ##### Home Tab #####
             tabPanel("Home",
                     ### CSS HEADER ###
                      ### Apply style attributes across the application
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css",
                                     href = "https://fonts.googleapis.com/css?family=Roboto|Roboto+Condensed"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "css/customize.css"),
                        tags$script(HTML("$(document).ready(function(){
                                      $('.version').text('SMARTsize Version 1.2, last updated 29 July 2016');
                                    });"))
                        ,
                        tags$script(HTML(paste("$(document).ready(function(){
                                      $('.Rversion').text('", paste(R.Version()$major, R.Version()$minor, sep = "."),"');
                                         });", sep = "")))
                      ),

                      sidebarPanel(
                        includeHTML("www/html/sidebarHome.html")
                      ),
                      
                      mainPanel(
                        HTML("<div class='page-header'>
                             <h1>Sample Size Calculator for SMARTs with Binary or Continuous Outcomes</h1></div>"),
                        p("Choose the SMART design of interest by clicking the corresponding tab
                          at the top of the window, or the button below the corresponding diagram.
                          Notation is established in the sidebar. See below for more background on SMARTs."),
                        br(),
                        fluidRow(
                          column(6,
                                 img(src = "images/SMARTdesignA__.gif", class = "img-responsive"),
                                 actionButton("pickTabA", "Design I"),
                                 HTML("<p> 8 embedded adaptive interventions: {A,C,E}, {A,C,F}, {A,D,E}, {A,D,F}, {B,G,I}, {B,G,J}, {B,H,I}, {B,H,J}.
                                      <a data-toggle='modal' data-target='#exampleAmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleAmodal.html")
                          ),
                          column(6,
                                 img(src = "images/SMARTdesignB__.gif", class = "img-responsive"),
                                 actionButton("pickTabB","Design II"),
                                 HTML("<p> 4 embedded adaptive interventions: {A,C,D}, {A,C,E}, {B,F,G}, {B,F,H}.
                                      <a data-toggle='modal' data-target='#exampleBmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleBmodal.html")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(3),
                          column(6,
                                 img(src = "images/SMARTdesignC__.gif", class = "img-responsive"),
                                 actionButton("pickTabC","Design III"),
                                 HTML("<p> 3 embedded adaptive interventions: {A,C,D}, {A,C,E}, {B,F,G}.
                                      <a data-toggle='modal' data-target='#exampleCmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleCmodal.html")
                          ),
                          column(3)
                        ),
                        br(),
                        includeHTML("www/html/homeBackground.html"),
                        
                        ##### References #####
                        
                        tags$hr(),
                        h4("References"),
                        tags$ol(
                          tags$li(HTML("Oetting, A., Levy, J., Weiss, R. and Murphy, S. (2007)<I>, &quot;Statistical methodology for a SMART design in the development of adaptive treatment strategies
                                       ,&quot;</I><I> in <I>Causality and Psychopathology: Finding the Determinants of Disorders and their Cures (American Psychopathological Association)</I></I>, 
                                       Arlington, VA: American Psychiatric Publishing, Inc., pp. 179-205.")),
                          tags$li("Nahum-Shani, I., Qian, M., Almirall, D., Pelham, W. E., Gnagy, B., Fabiano, G. A., Waxmonsky, J. G., Yu, J. and Murphy, S. A. (2012), 
                                  ''Experimental design and primary data analysis methods for comparing adaptive interventions.''",em("Psychological methods,"), "17, 457.")
                        ),
                        
                        includeHTML("www/html/references.html") # Collapse for additional selected readigs
                      )
             ), 
             
             ##### DESIGN I #####
             
             ##### D1 Sidebar #####
             
             tabPanel("Design I",
                      
                      sidebarPanel(
                        includeHTML("www/html/sidebarA.html")
                      ),
             
            ##### D1 Main Panel #####
                      mainPanel(
                        
                        ##### D1 Page Header #####
                        
                        h1("Design I"),
                        tags$hr(),
                        
                        ##### D1 Outcome, Result, and Aim #####
                        selectDTROutcomeUI("design1.outcome"),
                        tags$hr(),
                        resultOptionsUI("design1.resultOptions"),
                        tags$hr(),
                        primaryAimUI("design1.primaryAim"),
                        tags$hr(),
                        
                        ##### D1 AI Selection #####
                        # Dropdown menus provide options to select AIs for comparison.

                        eval(text.selectDTRcompare), 
                        
                        fluidRow(
                          column(6, uiOutput("selectAI1A")),
                          column(6, uiOutput("selectAI2A"))
                        ),
                        
                        tags$hr(),
                        
                        ##### A IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designAimg", width = "100%", height = "100%")),
                          column(5, 
                                 numericInput("respA",
                                              label = eval(text.responseProbLabel), 
                                              value = 0,min = 0,max = 1,step = 0.01),
                                 conditionalPanel(condition = 'output.design1outcome == "Binary"',
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
                                 conditionalPanel(condition = 'output.design1outcome == "Continuous"',
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
                        ),
                        
                        tags$hr(),
                        
                        
                        ##### A RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectResultsA", label = text.sampleSizeOrPower,
                                                choices  = list("Sample Size" = "sample", "Power" = "power"),
                                                selected = "sample"),
                                 radioButtons("selectAlternativeA", label = text.oneOrTwoSidedTest,
                                              choices=list("One-Sided" = "one.sided","Two-Sided" = "two.sided"),
                                              selected = "two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaA",label=HTML("Type I Error (&alpha;):"),value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsA=='sample'",
                                                  numericInput("inputPowerA",label=HTML("Power of Trial (1-&beta;):"),value=0.8, min=0, max=1,step=0.01)
                                 ),
                                 conditionalPanel(condition="input.selectResultsA=='power'",
                                                  numericInput("inputSampleSizeA",label="Total Sample Size of Trial:",value=0, min=0)
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### A RESULTS #####
                        # Choose which result to display based on binary/continuous outcome and selected result option
                        
                        h3("Results"),
                        conditionalPanel(condition = "output.design1outcome == 'Binary' & input.selectResultsA == 'sample'",
                                         htmlOutput("binarySampleSizeA")
                        ),
                        conditionalPanel(condition = "output.design1outcome =='Binary' & input.selectResultsA == 'power'",
                                         htmlOutput("binaryPowerA")
                        ),
                        conditionalPanel(condition = "output.design1outcome == 'Continuous' & input.selectResultsA == 'sample'",
                                         htmlOutput("continuousSampleSizeA")
                        ),
                        conditionalPanel(condition = "output.design1outcome == 'Continuous' & input.selectResultsA == 'power'",
                                         htmlOutput("continuousPowerA")
                        )
                      ),
                      
                      ##### A TOOLTIPS #####
                      ### Add bootstrap-style tooltips to inputs coaching proper formatting
                      ### Below are tooltips for inputs rendered statically in ui.R
                      ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                      
                      bsTooltip(id = "respA", title = "Input can range from 0-1 and must be in decimal form, up to two places.", 
                                placement = "right", trigger = "focus"),      
                      bsTooltip(id = "alphaA", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                                placement = "right", trigger = "focus"),
                      bsTooltip(id = "inputPowerA", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                                placement = "right", trigger = "focus"),
                      bsTooltip(id = "inputSampleSizeA", title = "Input must be an integer greater than zero.", 
                                placement = "right", trigger = "focus")                    
                      
             ),
             
             
             ########### DESIGN B ##########
             
             tabPanel("Design II",
                      
                      ##### B SIDEBAR #####
                      
                      sidebarPanel(
                        includeHTML("www/html/sidebarB.html")
                      ),
                      
                      mainPanel(
                        
                        ##### D2 Page Header #####
                        
                        h1(textOutput("tab")),
                        tags$hr(),
                        
                        ##### D2 Outcome, Result, and Aim #####
                        selectDTROutcomeUI("design2.outcome"),
                        tags$hr(),
                        resultOptionsUI("design2.resultOptions"),
                        tags$hr(),
                        primaryAimUI("design2.primaryAim"),
                        tags$hr(),
                        
                        ##### B OUTCOME SELECTION #####
                        
                        radioButtons("selectOutcomeB", label = HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                     choices = list("Binary" = 1,"Continuous" = 2), selected = 1),
                        
                        tags$hr(),                        
                        
                        ##### B DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison, rendered in server.R
                        
                        p("Which two", strong("adaptive interventions"), "would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the AIs you select."),
                        
                        fluidRow(
                          column(6, uiOutput("selectAI1B")),
                          column(6, uiOutput("selectAI2B"))
                        ),
                        
                        tags$hr(),
                        
                        ##### B IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designBimg")),
                          column(5,
                                 numericInput("respB",
                                              label = HTML("Concerning the tailoring variable, please provide the <strong> probability of response
                                                         </strong> to the first-stage intervention. If you are unsure, leave as 0 for a 
                                                         conservative estimate."),
                                              value = 0, min = 0, max = 1, step = 0.01),
                                 conditionalPanel(condition = "input.selectOutcomeB == 1",
                                                  p("Concerning the primary outcome, please provide the", strong("probability of success"), 
                                                    "for each of the AIs of interest."),
                                                  uiOutput("binaryDTR1probB"),
                                                  conditionalPanel(condition="input.cellOrConditionalB",
                                                                   fluidRow(column(1),
                                                                            column(11, uiOutput("cellProbsDTR1B"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probB"),
                                                  conditionalPanel(condition="input.cellOrConditionalB",
                                                                   fluidRow(column(1),
                                                                            column(11, uiOutput("cellProbsDTR2B"))
                                                                   )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeB==2",
                                                  p("Concerning the primary outcome, please provide the", strong("standardized effect size"), "between the
                                                    AIs of interest."),
                                                  uiOutput("continuousProbB")
                                 ),
                                 
                                 ##### B INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareB != 0 && input.secondDTRcompareB != 0",
                                                  br(),
                                                  conditionalPanel(condition="input.selectOutcomeB==1",
                                                                   helpText("If you prefer to provide different information, check the appropriate box below."),
                                                                   checkboxInput("cellOrConditionalB",label="Cell-Specific Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetDiffCheckB",label="Target Difference in Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetOddsCheckB",label="Target Odds Ratio",value=FALSE)
                                                  )
                                 )
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### B RESULT OPTIONS #####
                        ### Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,radioButtons("selectResultsB",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample"),
                                 radioButtons("selectAlternativeB",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaB",label=HTML("Type I Error (&alpha;):"),value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsB=='sample'",
                                                  numericInput("inputPowerB",label=HTML("Power of Trial (1-&beta;):"),value=0.8, min=0, max=1,step=0.01)
                                 ),
                                 conditionalPanel(condition="input.selectResultsB=='power'",
                                                  numericInput("inputSampleSizeB",label="Total Sample Size of Trial:",value=0, min=0)
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### B RESULTS #####
                        ### Choose which result to display based on binary/continuous outcome and selected result option
                
                          h3("Results"),
                          conditionalPanel(condition="input.selectOutcomeB==1 & input.selectResultsB=='sample'",
                                           htmlOutput("binarySampleSizeB")
                          ),
                          conditionalPanel(condition="input.selectOutcomeB==1 & input.selectResultsB=='power'",
                                          htmlOutput("binaryPowerB")
                          ),
                          conditionalPanel(condition="input.selectOutcomeB==2 & input.selectResultsB=='sample'",
                                           htmlOutput("continuousSampleSizeB")
                          ),
                          conditionalPanel(condition="input.selectOutcomeB==2 & input.selectResultsB=='power'",
                                           htmlOutput("continuousPowerB")
                          )
                    ),
                    
                    ##### B TOOLTIPS #####
                    ### Add bootstrap-style tooltips to inputs coaching proper formatting
                    ### Below are tooltips for inputs rendered statically in ui.R
                    ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                    
                    bsTooltip(id="respB",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),      
                    bsTooltip(id="alphaB",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                    bsTooltip(id="inputPowerB",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                    bsTooltip(id="inputSampleSizeB",title="Input must be an integer greater than zero.",placement="right",trigger="focus")                   
             ),
             
             ########### DESIGN C ##########
  
             tabPanel("Design III",
                      
                      ##### C SIDEBAR #####
                      
                      sidebarPanel(includeHTML("www/html/sidebarC.html")),
                      mainPanel(
                        
                        ##### C PAGE HEADER #####
                        
                        h1("Design III"),
                        tags$hr(),
                        
                        ##### C OUTCOME SELECTION #####
                        
                        radioButtons("selectOutcomeC", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                     choices=list("Binary"=1,"Continuous"=2),selected=1),
                        tags$hr(),
                        
                        ##### C DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        
                        p("Which two", strong("adaptive interventions"), "would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the AIs you select."),
                        
                        fluidRow(
                          column(6,
                                 uiOutput("selectAI1C")
                          ),
                          column(6,
                                 uiOutput("selectAI2C")
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### C IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designCimg",height="100%")),
                          column(5, 
                                 numericInput("respC",
                                              label=HTML("Concerning the tailoring variable, please provide the <strong> probability of response
                                                         </strong> to the first-stage intervention. If you are unsure, leave as 0 for a 
                                                         conservative estimate."),
                                              value=0,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectOutcomeC==1",
                                                  p("Concerning the primary outcome, please provide the", strong("probability of success"),
                                                    "for each of the AIs of interest."),
                                                  uiOutput("binaryDTR1probC"),
                                                  conditionalPanel(condition="input.cellOrConditionalC",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("cellProbsDTR1C"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probC"),
                                                  conditionalPanel(condition="input.cellOrConditionalC",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("cellProbsDTR2C"))
                                                                   )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeC==2",
                                                  p("Concerning the primary outcome, please provide the", strong("standardized effect size"), "between the
                                                    AIs of interest."),
                                                  uiOutput("continuousProbC")
                                 ),
                                 
                                 ##### C INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareC != 0 && input.secondDTRcompareC != 0",
                                                  br(),
                                                  conditionalPanel(condition="input.selectOutcomeC==1",
                                                                   helpText("If you prefer to provide different information, check the appropriate box below."),
                                                                   checkboxInput("cellOrConditionalC",label="Cell-Specific Probabilities",value=FALSE),
                                                                   checkboxInput("targetDiffCheckC",label="Target Difference in Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetOddsCheckC",label="Target Odds Ratio",value=FALSE)
                                                  )
                                 )
                          )
                        ),
                        
                        tags$hr(),
                        
                        
                        ##### C RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,radioButtons("selectResultsC",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample"),
                                 radioButtons("selectAlternativeC",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaC",label=HTML("Type I Error (&alpha;):"),value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsC=='sample'",
                                                  numericInput("inputPowerC",label=HTML("Power of Trial (1-&beta;):"),value=0.8, min=0, max=1,step=0.01)
                                 ),
                                 conditionalPanel(condition="input.selectResultsC=='power'",
                                                  numericInput("inputSampleSizeC",label="Total Sample Size of Trial:",value=0, min=0)
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### C Results #####
                        # Choose which result to display based on binary/continuous outcome and selected result option
                        
                        h3("Results"),
                        conditionalPanel(condition="input.selectOutcomeC==1 & input.selectResultsC=='sample'",
                                         htmlOutput("binarySampleSizeC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC == 1 & input.selectResultsC == 'power'",
                                         htmlOutput("binaryPowerC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==2 & input.selectResultsC=='sample'",
                                         htmlOutput("continuousSampleSizeC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==2 & input.selectResultsC=='power'",
                                         htmlOutput("continuousPowerC")
                        )
                      ),
                      
                      ##### C Tooltips #####
                      ### Add bootstrap-style tooltips to inputs coaching proper formatting
                      ### Below are tooltips for inputs rendered statically in ui.R
                      ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                      
                      bsTooltip(id = "respC", title = text.tooltip,
                                placement = "right", trigger = "focus"),      
                      bsTooltip(id = "alphaC", title = text.tooltip,
                                placement = "right", trigger = "focus"),
                      bsTooltip(id = "inputPowerC", title = text.tooltip,
                                placement = "right", trigger = "focus"),
                      bsTooltip(id = "inputSampleSizeC", 
                                title = "Input must be an integer greater than zero.",
                                placement = "right", trigger = "focus")
             ),
            
            ##### Design Your Own #####
            
            tabPanel("Design Your Own",
                     sidebarPanel(p("Insert description, etc.")),
                     mainPanel(
                       
                       ##### DYO Page Header #####
                       h1("Design Your Own SMART"),
                       h4("Follow the steps below to describe and size a custom SMART"),
                       tags$hr(),
                       
                       ##### DYO Outcome Selection #####
                       selectDTROutcomeUI("dyo.outcome"),
                       tags$hr(),
                       resultOptionsUI("dyo.resultOptions"),
                       tags$hr(),
                       
                       tabsetPanel(
                         ##### DYO Trial Design #####
                         tabPanel(title = "Design",
                                  bsCollapse(
                                    bsCollapsePanel(title = "Describe the First Randomization", 
                                                    fluidRow(column(6, sliderInput("dyo.stage1.ntxt", 
                                                                                   label = "How many initial treatments do you have?",
                                                                                   min = 2, max = 5, value = 2, step = 1, ticks = FALSE, width = "50%")),
                                                             column(6, radioButtons("dyo.stage1.eqrand",
                                                                                    label = "Are participants randomized equally between treatments?",
                                                                                    choices = list("Yes", "No"), selected = "Yes"),
                                                                    conditionalPanel(condition = 'input["dyo.stage1.eqrand"] == "No"',
                                                                                     list(p("What are the allocation probabilities?"),
                                                                                          fluidRow(column(11, uiOutput("dyo.stage1.rprobUI"), offset = 1)))))
                                                             ),
                                                    fluidRow(column(10),
                                                             column(2, bsButton("dyo.stage1.continue", "Continue"))),
                                                    value = "dyo.stage1.describe"),
                                    bsCollapsePanel(title = "Describe the Second Randomization",
                                                    fluidRow(column(6, h4("Responders"), tags$hr(),
                                                                    radioButtons("dyo.rerand.resp",
                                                                                    label = "Are responders to first-stage treatment re-randomized?",
                                                                                    choices = list("Yes", "No"), selected = "Yes"),
                                                                    conditionalPanel(condition = 'input["dyo.rerand.resp"] == "Yes"',
                                                                                     list(sliderInput("dyo.rerand.resp.ntxt",
                                                                                                      label = "How many treatments are responders re-randomized between?",
                                                                                                      min = 2, max = 5, value = 2, step = 1, ticks = FALSE, width = "80%"),
                                                                                          radioButtons("dyo.rerand.resp.eqrand",
                                                                                                       label = "Are responders re-randomized equally between treatments?",
                                                                                                       choices = list("Yes", "No"), selected = "Yes"),
                                                                                          conditionalPanel(condition = 'input["dyo.rerand.resp.eqrand"]  == "No"',
                                                                                                           list(p("What are the allocation probabilites?"),
                                                                                                                fluidRow(column(11, uiOutput("dyo.rerand.resp.rprobUI"), offset = 1))))))),
                                                             column(6, h4("Non-Responders"), tags$hr(),
                                                                    radioButtons("dyo.rerand.nresp",
                                                                                    label = "Are non-responders to first-stage treatment re-randomized?",
                                                                                    choices = list("Yes", "No"), selected = "Yes"),
                                                                    conditionalPanel(condition = 'input["dyo.rerand.nresp"] == "Yes"',
                                                                                     list(sliderInput("dyo.rerand.nresp.ntxt",
                                                                                                      label = "How many treatments are non-responders re-randomized between?",
                                                                                                      min = 2, max = 5, value = 2, step = 1, ticks = FALSE, width = "80%"),
                                                                                          radioButtons("dyo.rerand.nresp.eqrand", 
                                                                                                       label = "Are non-responders re-randomized equally between treatments?",
                                                                                                       choices = list("Yes", "No"), selected = "Yes"),
                                                                                          conditionalPanel(condition = 'input["dyo.rerand.nresp.eqrand"] == "No"',
                                                                                                           list(p("What are the allocation probabilities?"),
                                                                                                                fluidRow(column(11, uiOutput("dyo.rerand.nresp.rprobUI"), offset = 1)))))))),
                                                    bsAlert(anchorId = "mustRerandomize"),
                                                    fluidRow(column(3, bsButton("dyo.rerand.back", "Back")),
                                                             column(7),
                                                             column(2, bsButton("dyo.rerand.continue", "Continue"))),
                                                    value = "dyo.rerand.describe"),
                                    id = "dyo.design.collapse", multiple = FALSE, open = "dyo.stage1.describe")
                                  ), # end Design tabPanel
                         tabPanel("Size",
                                  br(),
                                  fluidRow(column(6,
                                                  radioButtons("dyo.stage1.resprob.eq", "Are the response rates the same for all first-stage treatments?",
                                                               choices = c("Yes", "No"), selected = "Yes"),
                                                  fluidRow(column(11, uiOutput("dyo.stage1.resprobUI"), offset = 1))),
                                           column(6, primaryAimUI("dyo.primaryAim"))),
                                  tags$hr()
                                  ), #end Size tabPanel
                       id = "dyo.tabset", selected = "Design", type = "tabs"),
                       tags$hr(),
                       br(),
                       verbatimTextOutput("graphstring"),
                       DiagrammeROutput("dyo.diagram", height = "500px")
                     ) # end mainPanel
                     ) # end tabPanel

)))