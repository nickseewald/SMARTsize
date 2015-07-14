##### UI.R FOR SMART SAMPLE SIZE CALCULATOR #####
### NICK SEEWALD, 2014
### UNIVERSITY OF MICHIGAN
### DEPARTMENT OF BIOSTATISTICS

library(shiny)
library(pwr)
library(shinyBS)
options(encoding='UTF-8')

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

shinyUI(
  # shinybootstrap2::withBootstrap2({
  navbarPage("SMART Sample Size Calculator", id = "SMARTsize", collapsible=TRUE, 
             footer = HTML("<p> Kidwell et al (in preparation). </p>
                         <p style='font-size:12px'> Please direct correspondence to <a href='mailto:nseewald@umich.edu'>nseewald@umich.edu</a></p>
                         <div style='color:grey;font-size:8px'>  SMARTsize Application Version 1.0.0, last updated 19 November 2014 </div>"),
             
             ##### HOME TAB#####
             
             
             
             tabPanel("Home",
                     ### CSS HEADER ###
                      ### Apply style attributes across the application
                      tags$head(
                        tags$style(type = 'text/css',"img {
                                            height=auto; max-width=100%;
                                        }"),
                        tags$style(type = 'text/css',"input[type='number'] {width:60px}"), #set width of numericInputs  
                        tags$link(rel = "stylesheet", href = "//fonts.googleapis.com/css?family=Roboto|Roboto+Condensed"),
                        tags$style("body {font-family: 'Roboto', sans-serif;} 
                                    h1 {font-family: 'Roboto Condensed', sans-serif;} 
                                    h2 {font-family: 'Roboto Condensed', sans-serif;}
                                    h3 {font-family: 'Roboto Condensed', sans-serif;} 
                                    h4 {font-family: 'Roboto Condensed', sans-serif;} 
                                    h5 {font-family: 'Roboto Condensed', sans-serif;} 
                                    h6 {font-family: 'Roboto Condensed', sans-serif;}"), #apply font styles
                        tags$style(type = 'text/css', 
                                "modal-backdrop{                  
                                   position: fixed;
                                   height: 100%;
                                   top: 0;
                                   right: 0;
                                   bottom: 0;
                                   left: 0;
                                   z-index: -1;
                                   background-color: #000; opacity: 80%;
                                   }
                                .modal-content {
                                   overflow: auto;
                                }
                                @media (min-width: 992px) {
                                  .modal-lg {
                                     width: 900px;
                                  }
                                }
                                  img {
      border: 1;
                                max-width: 100%;
                                }
                                element.style {
                                width: 33.33%;
                                }")
                        ),
                     
                      sidebarPanel(
                        includeHTML("www/html/sidebarHome.html")
                      ),
                      
                      mainPanel(                        
                        h1("Sample Size Calculator for SMARTs with Binary or Continuous Outcomes"),
                        br(),
                        p("Choose the SMART design of interest by clicking the corresponding tab at the top of the window, or the button below the corresponding diagram. 
                          Notation is established in the sidebar. See below for more background on SMARTs."),
                        br(),
                        fluidRow(
                          column(6,
                                 img(src = "images/SMARTdesignA__.gif", class = "img-responsive"),
                                 actionButton("pickTabA","Design A"),
                                 HTML("<p> 8 embedded adaptive interventions: ArCnrE, ArCnrF, ArDnrE, ArDnrF, BrGnrI, BrGnrJ, BrHnrI, BrHnrJ.
                                      <a data-toggle='modal' data-target='#exampleAmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleAmodal.html")
                          ),
                          column(6,
                                 img(src = "images/SMARTdesignB__.gif", class = "img-responsive"),
                                 actionButton("pickTabB","Design B"),
                                 HTML("<p> 4 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG, BrFnrH.
                                      <a data-toggle='modal' data-target='#exampleBmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleBmodal.html")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(6,
                                 img(src = "images/SMARTdesignC__.gif", class = "img-responsive"),
                                 actionButton("pickTabC","Design C"),
                                 HTML("<p> 3 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG.
                                      <a data-toggle='modal' data-target='#exampleCmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleCmodal.html")
                          ),
                          column(6,
                                 img(src = "images/SMARTdesignD__.gif", class = "img-responsive"),
                                 actionButton("pickTabD","Design D"),
                                 HTML("<p> 4 embedded non-adaptive interventions: AC, AD, BE, BF.
                                      <a data-toggle='modal' data-target='#exampleDmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleDmodal.html")
                          )
                        ),
                        br(),
                        includeHTML("www/html/homeBackground.html"),
                        
                        ##### REFERENCES #####
                        
                        tags$hr(),
                        h4("References"),
                        tags$ol(
                          tags$li(HTML("Oetting, A., Levy, J., Weiss, R. and Murphy, S. (2007)<I>, &quot;Statistical methodology for a SMART design in the development of adaptive treatment strategies
                                       ,&quot;</I><I> in <I>Causality and Psychopathology: Finding the Determinants of Disorders and their Cures (American Psychopathological Association)</I></I>, 
                                       Arlington, VA: American Psychiatric Publishing, Inc., pp. 179-205.")),
                          tags$li("Nahum-Shani, I., Qian, M., Almirall, D., Pelham, W. E., Gnagy, B., Fabiano, G. A., Waxmonsky, J. G., Yu, J. and Murphy, S. A. (2012), 
                                  ''Experimental design and primary data analysis methods for comparing adaptive interventions.''",em("Psychological methods,"), "17, 457.")
                        ),
                        
                        bsCollapse(multiple = FALSE, id = "moreRefs",
                                   bsCollapsePanel("Additional Selected Readings", source(file="./www/R/references.R",local=T,echo=F)$value)
                                   )
                      )
             ), 
             
             ##### DESIGN A #####
             
             ##### A SIDEBAR #####
             
             tabPanel("Design A",
                      
                      sidebarPanel(
                        includeHTML("www/html/sidebarA.html")
                      ),
                      
                      mainPanel(
                        
                        ##### A PAGE HEADER #####
                        
                        h1("Design A"),
                        tags$hr(),
                        
                        ##### A OUTCOME SELECTION #####
                        radioButtons("selectOutcomeA", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                     choices=list("Binary"=1,"Continuous"=2),selected=1),
                        
                        tags$hr(),
                        
                        ##### A DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        # Currently the menus are not reactively-repopulating (making it possible to select the same DTR twice). Possible future improvement.
                                                
                        p("Which two", strong("adaptive interventions"), "would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the AIs you select."),
                        
                        fluidRow(
                          column(6,
                                 uiOutput("selectAI1A")
                          ),
                          column(6,
                                 uiOutput("selectAI2A")
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### A IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designAimg", width = "100%")
                                 # ,htmlOutput("designAdiagram",class='img-responsive')
                                 ),
                          column(5, 
                                 numericInput("respA",
                                              label=HTML("Concerning the tailoring variable, please provide the <strong> probability of response </strong> to the 
                                                         first-stage intervention. If you are unsure, leave as 0 for a conservative estimate."), 
                                              value=0,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectOutcomeA==1",
                                                  p("Concerning the primary outcome, please provide the", strong("probability of success"), "for each of the
                                                    AIs of interest."),
                                                  uiOutput("binaryDTR1probA"),
                                                  conditionalPanel(condition="input.cellOrConditionalA",
                                                                   fluidRow(column(11,offset=1,
                                                                                   uiOutput("cellProbsDTR1A"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probA"),
                                                  
                                                  conditionalPanel(condition="input.cellOrConditionalA",
                                                                   fluidRow(column(11,offset=1,
                                                                                   uiOutput("cellProbsDTR2A"))
                                                                   )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeA==2",
                                                  p("Concerning the primary outcome, please provide the", strong("standardized effect size"), "between the
                                                    AIs of interest."),
                                                  uiOutput("continuousProbA")
                                 ),
                                 
                                 ##### A INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareA != 0 && input.secondDTRcompareA != 0",
                                                  br(),
                                                  conditionalPanel(condition="input.selectOutcomeA==1",
                                                                   helpText("If you prefer to provide different information, check the appropriate box below."),
                                                                   checkboxInput("cellOrConditionalA",label="Cell-Specific Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetDiffCheckA",label="Target Difference in Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetOddsCheckA",label="Target Odds Ratio",value=FALSE)
                                                  )
                                 )
                          )
                        ),
                        
                        tags$hr(),
                        
                        
                        ##### A RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectResultsA",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample"),
                                 radioButtons("selectAlternativeA",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
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
                        conditionalPanel(condition="input.selectOutcomeA==1 & input.selectResultsA=='sample'",
                                         htmlOutput("binarySampleSizeA")
                        ),
                        conditionalPanel(condition="input.selectOutcomeA==1 & input.selectResultsA=='power'",
                                         htmlOutput("binaryPowerA")
                        ),
                        conditionalPanel(condition="input.selectOutcomeA==2 & input.selectResultsA=='sample'",
                                         htmlOutput("continuousSampleSizeA")
                        ),
                        conditionalPanel(condition="input.selectOutcomeA==2 & input.selectResultsA=='power'",
                                         htmlOutput("continuousPowerA")
                        )
                      ),
                      
                      ##### A TOOLTIPS #####
                      ### Add bootstrap-style tooltips to inputs coaching proper formatting
                      ### Below are tooltips for inputs rendered statically in ui.R
                      ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                      
                      bsTooltip(id="respA",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),      
                      bsTooltip(id="alphaA",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputPowerA",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputSampleSizeA",title="Input must be an integer greater than zero.",placement="right",trigger="focus")                    
                      
             ),
             
             
             ########### DESIGN B ##########
             
             tabPanel("Design B",
                      
                      ##### B SIDEBAR #####
                      
                      sidebarPanel(
                        includeHTML("www/html/sidebarB.html")
                      ),
                      
                      mainPanel(
                        
                        ##### B PAGE HEADER #####
                        
                        h1("Design B"),
                        tags$hr(),
                        
                        ##### B OUTCOME SELECTION #####
                        
                        radioButtons("selectOutcomeB", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                     choices=list("Binary"=1,"Continuous"=2),selected=1),
                        
                        tags$hr(),                        
                        
                        ##### B DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison, rendered in server.R
                        
                        p("Which two", strong("adaptive interventions"), "would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the AIs you select."),
                        
                        fluidRow(
                          column(6,
                                 uiOutput("selectAI1B")
                          ),
                          column(6,
                                 uiOutput("selectAI2B")  
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### B IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designBimg")),
                          column(5,
                                 numericInput("respB",
                                              label=HTML("Concerning the tailoring variable, please provide the <strong> probability of response
                                                         </strong> to the first-stage intervention. If you are unsure, leave as 0 for a 
                                                         conservative estimate."),
                                              value=0,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectOutcomeB==1",
                                                  p("Concerning the primary outcome, please provide the", strong("probability of success"), 
                                                    "for each of the AIs of interest."),
                                                  uiOutput("binaryDTR1probB"),
                                                  conditionalPanel(condition="input.cellOrConditionalB",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("cellProbsDTR1B"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probB"),
                                                  conditionalPanel(condition="input.cellOrConditionalB",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("cellProbsDTR2B"))
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
  
             tabPanel("Design C",
                      
                      ##### C SIDEBAR #####
                      
                      sidebarPanel(h4("About this design:"),
                                   p("In this design, only participants who do not respond to a particular first-stage treatment are randomized twice. Initially all participants are randomized to A vs. B.
                                     Participants identified as responders to A (B) are not re-randomized and are assigned to treatment C (F). Participants identified as non-responders to A are re-randomized 
                                     to D vs. E; those identified as non-responders to B are not re-randomized and are assigned to treatment G. There are 3 embedded AIs in this SMART. They are ArCnrD, 
                                     ArCnrE, and BrFnrG."),
                                   tags$hr(),
                                   h4("Inputs Required:"),
                                   tags$ul(
                                     tags$li("Indication of continuous or binary outcome."),
                                     tags$li("The probability of response to the first-stage treatment. We assume this probability is the same for both initial treatments. If you cannot provide an
                                             estimate of this parameter, please indicate 0 to provide conservative output."),
                                     tags$li("Indication of a one- or two-sided test."),
                                     tags$li("Specification of a type-I error rate (level of significance). The default value is 0.05, but may range between 0 and 1."),
                                     tags$li("If interest is in sample size, specify the power of the study. The default power is 0.80, but may range between 0 and 1.
                                             If interest is in power, specify the sample size of the study. This must be input as an integer greater than zero."),
                                     tags$li("For binary outcomes:",
                                             tags$ul(
                                               tags$li("The default inputs are the probabilities of success for each of the selected AIs. These are the overall probabilities of success for those
                                                       participants following each of the selected embedded AIs. For example, the probability of success for ArCnrE."),
                                               tags$li("Alternatively, cell-specific probabilities may be specified. These refer to the probabilities of success for those consistent with a particular
                                                       intervention pathway. For example, the probability of success for ArC."),
                                               tags$li("Alternatively, if cell-specific or AI-specific probabilities cannot be specified, a target difference in probabilities (must be between 0 and 1)
                                                       or a target odds-ratio (must be positive and not equal to 1), may be selected and provided. With both of these selections, we assume one
                                                       of the AIs has probability of success equal to 0.5 to yield conservative results.")
                                             )),
                                     tags$li("For continuous outcomes:",
                                             tags$ul(
                                               tags$li("Specify the standardized effect size between the selected AIs. We use Cohen's definition for standardized effect size such that for the two selected AIs
                                                       it is the difference between the means of the two AIs divided by the square root of the pooled variance (square root of the average of the variances of
                                                       the two groups). This may range between 0 and 10.")
                                             ))
                                     ),
                                   h5("Input Formatting Rules:"),
                                   p("All inputs must be given in decimal form with leading zero (no fractions, please), and can be precise to two decimal places.
                                                   For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("We wish to find the sample size for a SMART with a binary outcome where the probability of response to first stage interventions is 0.40. We estimate the overall probabilities
                                     of success in the two AIs of interest, ArCnrE and BrFnrG, to be 0.75 and 0.60, respectively. Given a two-sided 5% type-I error, we require a sample size of 395 to make this
                                     comparison with 80% power.")
                      ),
                      mainPanel(
                        
                        ##### C PAGE HEADER #####
                        
                        h1("Design C"),
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
                        
                        ##### C RESULTS #####
                        # Choose which result to display based on binary/continuous outcome and selected result option
                        
                        h3("Results"),
                        conditionalPanel(condition="input.selectOutcomeC==1 & input.selectResultsC=='sample'",
                                         htmlOutput("binarySampleSizeC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==1 & input.selectResultsC=='power'",
                                         htmlOutput("binaryPowerC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==2 & input.selectResultsC=='sample'",
                                         htmlOutput("continuousSampleSizeC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==2 & input.selectResultsC=='power'",
                                         htmlOutput("continuousPowerC")
                        )
                      ),
                      
                      ##### C TOOLTIPS #####
                      ### Add bootstrap-style tooltips to inputs coaching proper formatting
                      ### Below are tooltips for inputs rendered statically in ui.R
                      ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                      
                      bsTooltip(id="respC",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),      
                      bsTooltip(id="alphaC",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputPowerC",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputSampleSizeC",title="Input must be an integer greater than zero.",placement="right",trigger="focus")
             ),
             
             ########### DESIGN D ##########
             
             tabPanel("Design D",
                      
                      ##### D SIDEBAR #####
                      
                      sidebarPanel(h4("About this design:"),
                                   p("In this design, all participants are randomized twice, and treatment options are not influenced by a tailoring variable. There are 4 embedded non-adaptive intervention
                                     paths in this SMART. They are AC, AD, BE, and BF."),
                                   tags$hr(),
                                   h4("Inputs Required:"),
                                   tags$ul(tags$li("Indication of continuous or binary outcome."),
                                           tags$li("Indication of a one- or two-sided test."),
                                           tags$li("Specification of a type-I error rate (level of significance). The default value is 0.05, but may range between 0 and 1."),
                                           tags$li("If interest is in sample size, specify the power of the study. The default power is 0.80, but may range between 0 and 1.
                                             If interest is in power, specify the sample size of the study. This must be input as an integer greater than zero."),
                                           tags$li("For binary outcomes:",
                                                   tags$ul(
                                                     tags$li("The default inputs are the probabilities of success for each of the selected intervention paths. These are the overall probabilities of success for those
                                                       participants following each of the selected embedded intervention paths. For example, the probability of success for AE."),
                                                     tags$li("Alternatively, if intervention path-specific probabilities cannot be specified, a target difference in probabilities (must be between 0 and 1)
                                                       or a target odds-ratio (must be positive and not equal to 1), may be selected and provided. With both of these selections, we assume one
                                                       of the intervention paths has probability of success equal to 0.5 to yield conservative results.")
                                                   )),
                                           tags$li("For continuous outcomes:",
                                                   tags$ul(
                                                     tags$li("Specify the standardized effect size between the selected AIs. We use Cohen's definition for standardized effect size such that
                                                             for the two selected AIs it is the difference between the means of the two AIs divided by the square root of the pooled variance (square root of the 
                                                             average of the variances of the two groups). This may range between 0 and 10.")
                                                   ))
                                   ),
                                   h5("Input Formatting Rules:"),
                                   p("All inputs must be given in decimal form with leading zero (no fractions, please), and can be precise to two decimal places.
                                                   For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("We wish to find the sample size for a trial with a binary outcome where the probability of response to first stage intervention is 0.40. We estimate the overall
                                     probabilities of success in the two intervention paths of interest, AC and BF to be 0.75 and 0.60, respectively. Given a two-sided 5% type-I error, we require a 
                                     sample size of 608 to make this comparison with 80% power.")
                      ),
                      mainPanel(
                        
                        ##### D PAGE HEADER #####
                        
                        h1("Design D"),
                        tags$hr(),
                        
                        ##### D OUTCOME SELECTION #####
                        
                        radioButtons("selectOutcomeD", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                     choices=list("Binary"=1,"Continuous"=2),selected=1),
                        tags$hr(),
                        
                        ##### D DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        
                        p("Which two", strong("non-adaptive intervention paths"), "would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the intervention paths you select."),
                        
                        fluidRow(
                          column(6,
                                 uiOutput("selectAI1D")
                          ),
                          column(6,
                                 uiOutput("selectAI2D")
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### D IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designDimg",height="100%")),
                          column(5, 
                                 conditionalPanel(condition="input.selectOutcomeD==1",
                                                  p("Concerning the primary outcome, please provide the", strong("probability of success"),
                                                    "for each of the intervention paths of interest."),
                                                  uiOutput("binaryDTR1probD"),
                                                  uiOutput("binaryDTR2probD")
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeD==2",
                                                  p("Concerning the primary outcome, please provide the", strong("standardized effect size"), "between the
                                                    intervention paths of interest."),
                                                  uiOutput("continuousProbD")
                                 ),
                                 
                                 ##### D INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareD != 0 && input.secondDTRcompareD != 0",
                                                  br(),
                                                  conditionalPanel(condition="input.selectOutcomeD==1",
                                                                   helpText("If you prefer to provide different information, check the appropriate box below."),
                                                                   checkboxInput("targetDiffCheckD",label="Check this box to input a target difference in probabilities.",value=FALSE),
                                                                   checkboxInput("targetOddsCheckD",label="Check this box to input a target odds-ratio instead of a target difference.",value=FALSE)
                                                  )
                                 )
                          )
                        ),
                        
                        tags$hr(),
                        
                        
                        ##### D RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,radioButtons("selectResultsD",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample"),
                                 radioButtons("selectAlternativeD",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaD",label=HTML("Type I Error (&alpha;):"),value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsD=='sample'",
                                                  numericInput("inputPowerD",label=HTML("Power of Trial (1-&beta;):"),value=0.8, min=0, max=1,step=0.01)
                                 ),
                                 conditionalPanel(condition="input.selectResultsD=='power'",
                                                  numericInput("inputSampleSizeD",label="Total Sample Size of Trial:",value=0, min=0)
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### D RESULTS #####
                        # Choose which result to display based on binary/continuous outcome and selected result option
                        
                        h3("Results"),
                        conditionalPanel(condition="input.selectOutcomeD==1 & input.selectResultsD=='sample'",
                                         htmlOutput("binarySampleSizeD")
                        ),
                        conditionalPanel(condition="input.selectOutcomeD==1 & input.selectResultsD=='power'",
                                         htmlOutput("binaryPowerD")
                        ),
                        conditionalPanel(condition="input.selectOutcomeD==2 & input.selectResultsD=='sample'",
                                         htmlOutput("continuousSampleSizeD")
                        ),
                        conditionalPanel(condition="input.selectOutcomeD==2 & input.selectResultsD=='power'",
                                         htmlOutput("continuousPowerD")
                        )
                      ),
                      
                      ##### D TOOLTIPS #####
                      ### Add bootstrap-style tooltips to inputs coaching proper formatting
                      ### Below are tooltips for inputs rendered statically in ui.R
                      ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                      
                      bsTooltip(id="respD",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),      
                      bsTooltip(id="alphaD",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputPowerD",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputSampleSizeD",title="Input must be an integer greater than zero.",placement="right",trigger="focus")
             )

)
  # })
)