library(shiny)

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
  navbarPage("SMART Sample Size Calculator", id="SMARTsize",

             ##### HOME TAB#####
             
             tabPanel("Home",
                      
                      sidebarPanel(h3("Introduction"),
                                   
                                   h4("Objective"),
                                   p("The goal of this application is to compute sample size (or power) for sequential multiple assignment
                                     randomized trials (SMARTs) with binary or continouous outcomes in which the primary objective is to make
                                     comparisons between two adaptive interventions (AIs, also known as dynamic treatment regimes or DTRs)."),
                                   br(),
                                   h4("Methods"),
                                   p("The application is run on R, version 3.1.0, and written using Shiny, an open-source web application framework
                                     for R produced by RStudio."),
                                   p("For a SMART with a binary outcome, the application computes the probability of 'success'
                                     (however success may be defined) for both adaptive interventions of interest, then performs a test of proportions
                                     to determine a sample size or power for just the two AIs under study. Depending on the adaptive interventions being 
                                     compared, as well as the design of the trial, the preliminary results are upweighted to determine sample size or
                                     power for the entire trial."),
                                   p("For a SMART with a continuous outcome, the application computes a standardized effect size, then performs a Z-test
                                     to determine a preliminary sample size or power, which is then upweighted as above, using the methods of Oetting, et. al."),
                                   br(),
                                   h4("Notation"),
                                   p("Throughout the application, we use the following notations:"),
                                   p("- ",img(src="images/randomize.gif",width=25),"refers to a randomization, with probability 0.5, of all available individuals into 
                                     the two subsequent treatments. For example, in design B at right, the rightmost ",img(src="images/randomize.gif",width=17),"indicates
                                     that non-responders to first-stage treatment are randomized equally between two available second-stage treatments."),
                                   p("- Adaptive Interventions (AI) are named by combining first stage treatment, then the second stage treatments for responders and non-responders,
                                     respectively. The AI 'Give A; then, if response, give C; if no response, give E' is named 'ArCnrE'.")
                      ),
                      
                      mainPanel(
                        h1("Sample Size Calculator for SMARTs with Binary or Continuous Outcomes"),
                        br(),
                        p("Choose a design by clicking the cooresponding tab at the top of the window, or the button below the corresponding diagram. 
                          For notational information, see the sidebar."),
                        br(),
                        fluidRow(
                          column(6,
                                 img(src="images/SMARTdesignA_0_0.gif"),
                                 actionButton("pickTabA","Design A"),
                                 helpText("8 embedded adaptive interventions: ArCnrE, ArCnrF, ArDnrE, ArDnrF, BrGnrI, BrGnrJ, BrHnrI, BrHnrJ")
                          ),
                          column(6,
                                 img(src="images/SMARTdesignB_0_0.gif"),
                                 actionButton("pickTabB","Design B"),
                                 helpText("4 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG, BrFnrH")
                          )
                        ),
                        fluidRow(
                          column(6,
                                 img(src="images/SMARTdesignC.gif"),
                                 actionButton("pickTabC","Design C"),
                                 helpText("3 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG")
                          ),
                          column(6,
                                 img(src="images/SMARTdesignD.gif"),
                                 actionButton("pickTabD","Design D"),
                                 helpText("4 embedded non-adaptive interventions: AC, AD, BE, BF")
                          )
                        ),
                        br(),
                        
                        h2("What is a SMART?"),
                        
                        p("To understand the concept of a SMART, we first focus on adaptive interventions. An",strong("adaptive intervention"), 
                          "(also known as a dynamic treatment regime, a treatment algorithm, an adaptive treatment strategy, etc.) is a sequence
                          of treatments adapted to an individual by means of decision rules which recommend subsequent stages of the intervention. Consider an example in the treatment of
                          breast cancer. An adaptive intervention (AI) for a 60-year-old, hormone-receptor-postive breast cancer patient may be 'Following surgery, treat with chemotherapy for 
                          six cycles. If there is no evidence of cancer and the lymph nodes are negative, treat with an aromatase inhibitor for five years. If there is evidence of cancer
                          following chemotherapy, continue chemotherapy for another six cycles. If the patient experiences a grade III or higher toxicity on the prescribed chemotherapy,
                          switch to another chemotherapy.' Note that this AI includes a decision point after six cycles of chemotherapy, at which point, depending on the patient's response
                          thus far, a secondary treatment is recommended. This secondary treatment is different for responders and non-responders."),
                        
                        p("In order to make comparisons of adaptive interventions, we utilize a ",strong("Sequential Multiple Assignment Randomized Trial (SMART)"), ". A SMART is an
                          experimental design in which individuals are randomized mutliple times and follow specific adaptive interventions.")
                      )
             ), 
             
             ##### DESIGN A #####
             
             tabPanel("Design A",
                      sidebarPanel(h3("About this design:"),
                                   p("Include information about the design, its requirements, and describe the input.")
                      ),
                      mainPanel(
                        
                        ##### A PAGE HEADER #####
                        
                        h1("Design A"),
                        tags$hr(),
                                
                        ##### A DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        # Currently the menus are not reactively-repopulating (making it possible to select the same DTR twice). Possible future improvement.
                        
                        p("Which two dynamic treatment regimes would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the DTRs you select."),
                        
                        fluidRow(
                          column(6,
                                 selectInput("firstDTRcompareA",label="Compare DTR",
                                             choices=list("[Choose First DTR]"=0, "ArCnrE"="ArCnrE", "ArCnrF"="ArCnrF", "ArDnrE"="ArDnrE", 
                                                          "ArDnrF"="ArDnrF", "BrGnrI"="BrGnrI", "BrGnrJ"="BrGnrJ", "BrHnrI"="BrHnrI", "BrHnrJ"="BrHnrJ"),
                                             selected=0)
                          ),
                          column(6,
                                 selectInput("secondDTRcompareA",label="to DTR",
                                             choices=list("[Choose Second DTR]"=0, "ArCnrE"="ArCnrE", "ArCnrF"="ArCnrF", "ArDnrE"="ArDnrE", 
                                                          "ArDnrF"="ArDnrF", "BrGnrI"="BrGnrI", "BrGnrJ"="BrGnrJ", "BrHnrI"="BrHnrI", "BrHnrJ"="BrHnrJ"),
                                             selected=0)  
                          )
                        ),
                        tags$hr(),
                        
                        
                        ##### A OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectOutcomeA", label="Is the outcome of interest binary or continuous?",
                                              choices=list("Binary"=1,"Continuous"=2),selected=1)
                          ),
                          column(6,
                                 numericInput("respA",
                                              label="What is the probability that a patient responds to first-stage treatment? If you are unsure, enter 0 for a conservative estimate.",
                                              value=0,min=0,max=1,step=0.01)
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### A IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designAimg",height="100%")
                                 ),
                          column(5, 
                                 conditionalPanel(condition="input.selectOutcomeA==1",
                                                  uiOutput("binaryDTR1probA"),
                                                  conditionalPanel(condition="input.cellOrConditionalA",
                                                                   fluidRow(column(1),
                                                                            column(4,uiOutput("cellProbsDTR1A"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probA"),
                                                  conditionalPanel(condition="input.cellOrConditionalA",
                                                                   fluidRow(column(1),
                                                                            column(4,uiOutput("cellProbsDTR2A"))
                                                                   )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeA==2",
                                                  uiOutput("continuousProbA")
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### A INPUT OPTIONS #####
                        # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                        # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                        # For continuous outcomes, option to input mean difference and standard-deviation
                        
                        fluidRow(
                          conditionalPanel(condition="input.selectOutcomeA==1",
                                           checkboxInput("cellOrConditionalA",label="Check this box to input cell-specific probabilities.",value=FALSE),
                                           checkboxInput("targetDiffCheckA",label="Check this box to input a target difference in probabilities.",value=FALSE),
                                           conditionalPanel(condition="input.targetDiffCheckA",
                                                            column(1), 
                                                            column(11,
                                                                   checkboxInput("targetOddsCheckA",label="Check this box to input a target odds-ratio instead of a target difference.",value=FALSE)
                                                            )
                                           )
                          ),
                          conditionalPanel(condition="input.selectOutcomeA==2",
                                           checkboxInput("meanSdCheckB",label="Check this box to input a difference in means and standard deviation.",value=FALSE)
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### A RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,radioButtons("selectResultsA",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample")),
                          column(6,
                                 numericInput("alphaA",label="Type I Error (Alpha):",value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsB=='sample'",
                                                  numericInput("inputPowerA",label="Power of Trial:",value=0.8, min=0, max=1,step=0.01)
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
                                         h4(textOutput("binarySampleSizeA"), style = "color:blue")
                        ),
                        conditionalPanel(condition="input.selectOutcomeA==1 & input.selectResultsA=='power'",
                                         h4(textOutput("binaryPowerA"),style="color:blue")
                        ),
                        conditionalPanel(condition="input.selectOutcomeA==2 & input.selectResultsA=='sample'",
                                         h4(textOutput("continuousSampleSizeA"),style="color:blue")
                        ),
                        conditionalPanel(condition="input.selectOutcomeA==2 & input.selectResultsA=='power'",
                                         h4(textOutput("continuousPowerA"),style="color:blue")
                        )
                        
                      )
             ),
             
             
             ########### DESIGN B ##########
             
             tabPanel("Design B",
                      
                      ##### B SIDEBAR #####
                      
                      sidebarPanel(h4("About this design:"),
                                   p("Design B is a SMART in which rerandomization to second-stage treatment options depends on response to the first-stage treatment.
                                     For example, individuals who do not respond to treatment A are randomized a second time and given either D or E as second-stage treatment,
                                     whereas individuals who do respond to treatment A are not rerandomized and are provided second-stage treatment C. Note that A, C, D, and E
                                     need not all be distinct: it is possible that C is a continuation of A, for example. It is required, however, that D and E be different."),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   p("- We assume that the probability of response is equal for both first-stage treatments. "),
                                   p("- For binary outcomes, the default input is probabilities of success for an individual consistent with each of the selected AI's.
                                     'Cell-specific probabilities' refer to the probability of success for an individual whose intervention ended with a particular cell."),
                                   p("- For continuous outcomes, the default input is the standardized effect size for making comparisons between the two selected AI's.
                                     Alterntively, you can provide mean outcomes for the two AI's of interest, along with the standard error of the difference between those means. "),
                                   h5("A Note on Input Formatting:"),
                                   p("All inputs must be given in decimal form (with leading zero), and can be precise to two decimal places.
                                     Fractional input is disallowed. For example, '0.07' is valid input; both '.07' and '7/100' are invalid."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("Consider a trial with a binary outcome in which the probability of response to first-stage treatment is 0.6, and that we are interested in comparing AI's ArCnrD to BrFnrG.
                                     Suppose that the probability of success for ArCnrD is 0.70, and 0.57 for BrFnrG. At 80% power and 5% type-I error rate, the sample size for this trial
                                     is 600.")
                      ),
                      
                      mainPanel(
                        
                        ##### B PAGE HEADER #####
                        
                        h1("Design B"),
                        tags$hr(),
                        
                        ##### B DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        # Currently the menus are not reactively-repopulating (making it possible to select the same DTR twice). Possible future improvement.
                        
                        p("Which two adaptive interventions would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the AIs you select."),
                        
                        fluidRow(
                          column(6,
                                 selectInput("firstDTRcompareB",label="Compare AI",
                                             choices=list("[Choose First AI]"=0, "ArCnrD"="ArCnrD", "ArCnrE"="ArCnrE", "BrFnrG"="BrFnrG", "BrFnrH"="BrFnrH"),
                                             selected=0)
                          ),
                          column(6,
                                 selectInput("secondDTRcompareB",label="to AI",
                                             choices=list("[Choose Second AI]"=0, "ArCnrD"="ArCnrD", "ArCnrE"="ArCnrE", "BrFnrG"="BrFnrG", "BrFnrH"="BrFnrH"),
                                             selected=0)  
                          )
                        ),
                        
                        # If the same DTR is selected twice, print an error message.
                        # Currently, UI and Server code conditions on non-unique DTR selection. If above dropdowns become dynamic, deprecate this conditioning.
                        
                        fluidRow(
                                 conditionalPanel(condition="input.firstDTRcompareB==input.secondDTRcompareB & input.firstDTRcompareB != 0",
                                           p("ERROR: Please select two different AIs to compare.",style="color:red")
                                 )
                        ),
                        tags$hr(),
                        
                        ##### B OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectOutcomeB", label="Is the outcome of interest binary or continuous?",
                                              choices=list("Binary"=1,"Continuous"=2),selected=1)
                                 ),
                          column(6,
                                 numericInput("respB",
                                              label="What is the probability that a patient responds to first-stage treatment? If you are unsure, enter 0 for a conservative estimate.",
                                              value=0,min=0,max=1,step=0.01)
                                 )
                          ),
                        
                        tags$hr(),

                        ##### B IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designBimg",height="100%")),
                          column(5, 
                                 conditionalPanel(condition="input.selectOutcomeB==1",
                                                  p("Please provide the probability of success for each of the AI's of interest."),
                                                  uiOutput("binaryDTR1probB"),
                                                  conditionalPanel(condition="input.cellOrConditionalB",
                                                                   fluidRow(column(1),
                                                                            column(4,uiOutput("cellProbsDTR1B"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probB"),
                                                  conditionalPanel(condition="input.cellOrConditionalB",
                                                                   fluidRow(column(1),
                                                                            column(4,uiOutput("cellProbsDTR2B"))
                                                                   )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeB==2",
                                                  uiOutput("continuousProbB"),
                                                  conditionalPanel(condition="input.meanSdCheckB",
                                                                   fluidRow(column(1),
                                                                            column(4,uiOutput("meanEstB"))
                                                                   )
                                                  )
                                 ),
                                 
                                 ##### B INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareB != 0 && input.secondDTRcompareB !=0",
                                                  br(),
                                                  helpText("If you prefer to provide different information, check the appropriate box below."),
                                                  conditionalPanel(condition="input.selectOutcomeB==1",
                                                                   checkboxInput("cellOrConditionalB",label="Check this box to input cell-specific probabilities.",value=FALSE),
                                                                   checkboxInput("targetDiffCheckB",label="Check this box to input a target difference in probabilities.",value=FALSE),
                                                                   conditionalPanel(condition="input.targetDiffCheckB",
                                                                                    column(1), 
                                                                                    column(11,
                                                                                           checkboxInput("targetOddsCheckB",label="Check this box to input a target odds-ratio instead of a target difference.",value=FALSE)
                                                                                    )
                                                                   )
                                                  ),
                                                  conditionalPanel(condition="input.selectOutcomeB==2",
                                                                   checkboxInput("meanSdCheckB",label="Check this box to input a difference in means and standard deviation.",value=FALSE)
                                                  )
                                 )
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### B RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,radioButtons("selectResultsB",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample")),
                          column(6,
                                 numericInput("alphaB",label="Type I Error (Alpha):",value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsB=='sample'",
                                                  numericInput("inputPowerB",label="Power of Trial:",value=0.8, min=0, max=1,step=0.01)
                                 ),
                                 conditionalPanel(condition="input.selectResultsB=='power'",
                                                  numericInput("inputSampleSizeB",label="Total Sample Size of Trial:",value=0, min=0)
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### B RESULTS #####
                        # Choose which result to display based on binary/continuous outcome and selected result option
                
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
                    )
             ),
  
             tabPanel("Design C",
                      sidebarPanel(h3("About this design:"),
                                   p("Include information about the design, its requirements, and describe the input.")
                      ),
                      mainPanel(h1("Design C"),
                                img(src="images/SMARTdesignC.gif")
                      )
             ),
             tabPanel("Design D",
                      sidebarPanel(h3("About this design:"),
                                   p("Include information about the design, its requirements, and describe the input.")
                      ),
                      mainPanel(h1("Design D"),
                                img(src="images/SMARTdesignD.gif")
                      )
             ),
collapsable=TRUE,
footer="Kidwell, Seewald, Almirall (2014)")
)