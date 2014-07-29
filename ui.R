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
                                   tags$ul(
                                     tags$li(img(src="images/randomize.gif",width=25),"refers to a randomization, with probability 0.5, of all available individuals into 
                                             the two subsequent treatments. For example, in design B at right, the rightmost ",img(src="images/randomize.gif",width=17),"indicates
                                             that non-responders to first-stage treatment are randomized equally between two available second-stage treatments."
                                       ),
                                     tags$li("Adaptive Interventions (AI) are named by combining first stage treatment, then the second stage treatments for responders and non-responders,
                                             respectively. The AI 'Give A; then, if response, give C; if no response, give E' is named 'ArCnrE'."
                                       )
                                   )
                      ),
                      
                      mainPanel(
                        h1("Sample Size Calculator for SMARTs with Binary or Continuous Outcomes"),
                        br(),
                        p("Choose a design by clicking the cooresponding tab at the top of the window, or the button below the corresponding diagram. 
                          For more information about SMARTs, see below the diagrams. Notation is established in the sidebar."),
                        br(),
                        fluidRow(
                          column(6,
                                 img(src="images/SMARTdesignA__.gif"),
                                 actionButton("pickTabA","Design A"),
                                 helpText("8 embedded adaptive interventions: ArCnrE, ArCnrF, ArDnrE, ArDnrF, BrGnrI, BrGnrJ, BrHnrI, BrHnrJ")
                          ),
                          column(6,
                                 img(src="images/SMARTdesignB__.gif"),
                                 actionButton("pickTabB","Design B"),
                                 helpText("4 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG, BrFnrH")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(6,
                                 img(src="images/SMARTdesignC__.gif"),
                                 actionButton("pickTabC","Design C"),
                                 helpText("3 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG")
                          ),
                          column(6,
                                 img(src="images/SMARTdesignD__.gif"),
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
                        
                        p("The notion of a 'tailoring variable' which we evaluate at each decision point is critical for an adaptive intervention. In fact, this variable is what makes the 
                          intervention",em("adaptive."), "Tailoring variables let us construct decision rules for assigning subsequent treatments, so they should be identified based solely
                          on practical, ethical, or scientific reasoning, and produce clear, specific, and objective decision rules. In the above example, and throughout this application, 
                          we use 'response to first-stage treatment' as the variable by which we determine the subsequent treatment an individual will receive. For more information about
                          tailoring variables, see Nahum-Shani, et al. (2012)."),
                        
                        tags$caption(h5("A note about 'response':")),
                        tags$blockquote("Depending on your area of research, 'response' may be classfied differently than in the diagrams above. For example, in Design B, if your trial 
                                        stipulates that individuals who respond to first stage treatment are rerandomized and non-responders are not, the application can accomodate your needs.
                                        To make this change, simply provide 'non-response probability' when prompted for 'response probability'. "),
                        
                        p("In order to make comparisons of adaptive interventions, we utilize a ",strong("Sequential Multiple Assignment Randomized Trial (SMART)"), ". A SMART is an
                          experimental design in which individuals are randomized mutliple times and follow specific intervention sequences. There are several advantages to using a SMART.
                          First, it allows investigators to detect 'delayed effects' in treatment; that is, the long-term effects of a treatment after a subsequent treatment is administered.
                          These delayed effects may be synergistic or antagonistic, and cannot be detected by separate two-arm trials for each stage. Along these same lines, a SMART may decrease
                          the likelihood of participant drop-out (due to the promise of subsequent treatment regardless of response), and, because of this, may reduce selection bias found in
                          a standard non-responder trial. Additionally, a SMART allows for the same analyses as standard trial designs, with the extra advantage of the ability to compare sequences
                          of treatments.")
                      )
             ), 
             
             ##### DESIGN A #####
             
             ##### A SIDEBAR #####
             
             tabPanel("Design A",
                      sidebarPanel(h4("About this design:"),
                                   p("This design is a SMART in which whether an individual is rerandomized does not depend on her response to first-stage treatment. For example, 
                                     a participant in the trial who 'responds' to treatment B is randomized to second-stage treatment G or H, whereas an individual who 'does not
                                     respond' to B is randomized to either I or J."),
                                   p("It is", em("not"), "necessary that all second-stage treatment options be distinct: one may wish
                                     to rerandomize all 'responders' between the same two treatments and similarly with 'non-responders', for example."),
                                   p("The key feature of Design A is that all individuals are rerandomized, regardless of response to first-stage treatment or the availability of
                                     second-stage treatments. There are",em("eight"),"embedded adaptive interventions."),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   tags$ul(
                                     tags$li("We assume that the probability of response is equal for both first-stage treatments."),
                                     tags$li("For binary outcomes, the default input is probabilities of success for an individual consistent with each of the selected AI's.
                                     'Cell-specific probabilities' refer to the probability of success for an individual whose intervention ended with a particular cell."),
                                     tags$li("For continuous outcomes, the default input is the standardized effect size for making comparisons between the two selected AI's.
                                     Alterntively, you can provide mean outcomes for the two AI's of interest, along with the standard error of the difference between those means.")
                                   ),
                                   h5("Input Formatting Rules:"),
                                   tags$blockquote("All inputs must be given in decimal form (with leading zero), and can be precise to two decimal places. Fractional input is
                                                   disallowed. For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("Consider a SMART modeled after design A which has a binary outcome and in which the probability of response to first stage treatment is 0.5. 
                                     Suppose also that we wish to compare AIs ArCnrE and BrGnrI, which have success probabilities 0.65 and 0.80, respectively. Given a 5% type-I 
                                     error rate, we require a sample size of 522 individuals in order to make this comparison with 80% power.")
                      ),
                      
                      mainPanel(
                        
                        ##### A PAGE HEADER #####
                        
                        h1("Design A"),
                        tags$hr(),
                        
                        ##### A DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        # Currently the menus are not reactively-repopulating (making it possible to select the same DTR twice). Possible future improvement.
                        
                        p("Which two adaptive interventions would you like to compare? Choose two from the menus below.",
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
                          column(7, imageOutput("designAimg",height="100%")),
                          column(5, 
                                 conditionalPanel(condition="input.selectOutcomeA==1",
                                                  p("Please provide the probability of success for each of the AI's of interest."),
                                                  uiOutput("binaryDTR1probA"),
                                                  conditionalPanel(condition="input.cellOrConditionalA",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("cellProbsDTR1A"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probA"),
                                                  
                                                  conditionalPanel(condition="input.cellOrConditionalA",
                                                                   fluidRow(column(1),
                                                                            column(11,
                                                                                   uiOutput("cellProbsDTR2A"))
                                                                   )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeA==2",
                                                  uiOutput("continuousProbA"),
                                                  conditionalPanel(condition="input.meanSdCheckA",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("meanEstA"))
                                                                   )
                                                  )
                                 ),
                                 
                                 ##### A INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareA != 0 && input.secondDTRcompareA != 0",
                                                  br(),
                                                  helpText("If you prefer to provide different information, check the appropriate box below."),
                                                  conditionalPanel(condition="input.selectOutcomeA==1",
                                                                   checkboxInput("cellOrConditionalA",label="Cell-Specific Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetDiffCheckA",label="Target Difference in Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetOddsCheckA",label="Target Odds Ratio",value=FALSE)
                                                  ),
                                                  conditionalPanel(condition="input.selectOutcomeA==2",
                                                                   checkboxInput("meanSdCheckA",label="Check this box to input a difference in means and standard deviation.",value=FALSE)
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
                                 radioButtons("selectSidedA", label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided" = 'one.sided', "Two-Sided"='two.sided'),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaA",label="Type I Error (Alpha):",value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsA=='sample'",
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
                      )
             ),
             
             
             ########### DESIGN B ##########
             
             tabPanel("Design B",
                      
                      ##### B SIDEBAR #####
                      
                      sidebarPanel(h4("About this design:"),
                                   p("Design B is a SMART in which rerandomization to second-stage treatment options depends on response to the first-stage treatment. Individuals who do not respond
                                     to treatment A or B are re-randomized between two options for second-stage treatment, whereas responders are all assigned to the same second-stage treatment."),
                                   p("It is",em("not"),"necessary that all possible second-stage treatments are distinct. We might, have, for example, that treatment C is the same as treatment A,
                                     or that F is the same as G. What",em("is"),"required, however, is that D and E are distinct, and that G and H are distinct."),
                                   p("There are",em("four"),"adaptive interventions in this design."),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   tags$ul(
                                     tags$li("We assume that the probability of response is equal for both first-stage treatments."),
                                     tags$li("For binary outcomes, the default input is probabilities of success for an individual consistent with each of the selected AI's.
                                     'Cell-specific probabilities' refer to the probability of success for an individual whose intervention ended with a particular cell."),
                                     tags$li("For continuous outcomes, the default input is the standardized effect size for making comparisons between the two selected AI's.
                                     Alterntively, you can provide mean outcomes for the two AI's of interest, along with the standard error of the difference between those means.")
                                   ),
                                   h5("Input Formatting Rules:"),
                                   tags$blockquote("All inputs must be given in decimal form (with leading zero), and can be precise to two decimal places. Fractional input is
                                                   disallowed. For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
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
                        # Dropdown menus provide options to select DTRs for comparison, rendered in server.R
                        
                        p("Which two adaptive interventions would you like to compare? Choose two from the menus below.",
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
                                                  uiOutput("continuousProbB"),
                                                  conditionalPanel(condition="input.meanSdCheckB",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("meanEstB"))
                                                                   )
                                                  )
                                 ),
                                 
                                 ##### B INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareB != 0 && input.secondDTRcompareB != 0",
                                                  br(),
                                                  helpText("If you prefer to provide different information, check the appropriate box below."),
                                                  conditionalPanel(condition="input.selectOutcomeB==1",
                                                                   checkboxInput("cellOrConditionalB",label="Cell-Specific Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetDiffCheckB",label="Target Difference in Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetOddsCheckB",label="Target Odds Ratio",value=FALSE)
                                                  ),
                                                  conditionalPanel(condition="input.selectOutcomeB==2",
                                                                   checkboxInput("meanSdCheckB",label="Check this box to input a difference in means and standard deviation.",value=FALSE)
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
                                 radioButtons("selectSidedB",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
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
                    )
             ),
  
             tabPanel("Design C",
                      sidebarPanel(h3("About this design:"),
                                   p("Design C is a SMART in which re-randomization depends on both first-stage treatment and response 
                                     to first-stage treatment. Only those individuals who did not respond to treatment A are re-randomized."),
                                   p("Treatments C, F, and G need",em("not"),"be distinct, but it is necessary that D and E are not the same."),
                                   p("There are",em("three"),"embedded adaptive interventions in this design."),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   tags$ul(
                                     tags$li("We assume that the probability of response is equal for both first-stage treatments."),
                                     tags$li("For binary outcomes, the default input is probabilities of success for an individual consistent with each of the selected AI's.
                                     'Cell-specific probabilities' refer to the probability of success for an individual whose intervention ended with a particular cell."),
                                     tags$li("For continuous outcomes, the default input is the standardized effect size for making comparisons between the two selected AI's.
                                     Alterntively, you can provide mean outcomes for the two AI's of interest, along with the standard error of the difference between those means.")
                                   ),
                                   h5("Input Formatting Rules:"),
                                   tags$blockquote("All inputs must be given in decimal form (with leading zero), and can be precise to two decimal places. Fractional input is
                                                   disallowed. For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("Consider a trial with a binary outcome in which the probability of response to first-stage treatment is 0.4, and that we are interested in comparing AI's ArCnrE to BrFnrG.
                                     Suppose that the probability of success for ArCnrE is 0.75, and 0.60 for BrFnrG. At 80% power and 5% type-I error rate, the sample size for this trial
                                     is 395.")
                      ),
                      mainPanel(
                        
                        ##### C PAGE HEADER #####
                        
                        h1("Design C"),
                        tags$hr(),
                        
                        ##### C DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        
                        p("Which two adaptive interventions would you like to compare? Choose two from the menus below.",
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
                        
                        ##### C OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectOutcomeC", label="Is the outcome of interest binary or continuous?",
                                              choices=list("Binary"=1,"Continuous"=2),selected=1)
                          ),
                          column(6,
                                 numericInput("respC",
                                              label="What is the probability that a patient responds to first-stage treatment? If you are unsure, enter 0 for a conservative estimate.",
                                              value=0,min=0,max=1,step=0.01)
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### C IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designCimg",height="100%")),
                          column(5, 
                                 conditionalPanel(condition="input.selectOutcomeC==1",
                                                  p("Please provide the probability of success for each of the AI's of interest."),
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
                                                  uiOutput("continuousProbC"),
                                                  conditionalPanel(condition="input.meanSdCheckC",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("meanEstC"))
                                                                   )
                                                  )
                                 ),
                                 
                                 ##### C INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareC != 0 && input.secondDTRcompareC != 0",
                                                  br(),
                                                  helpText("If you prefer to provide different information, check the appropriate box below."),
                                                  conditionalPanel(condition="input.selectOutcomeC==1",
                                                                   checkboxInput("cellOrConditionalC",label="Cell-Specific Probabilities",value=FALSE),
                                                                   checkboxInput("targetDiffCheckC",label="Target Difference in Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetOddsCheckC",label="Target Odds Ratio",value=FALSE)
                                                  ),
                                                  conditionalPanel(condition="input.selectOutcomeC==2",
                                                                   checkboxInput("meanSdCheckC",label="Check this box to input a difference in means and standard deviation.",value=FALSE)
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
                                 radioButtons("selectSidedC",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaC",label="Type I Error (Alpha):",value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsC=='sample'",
                                                  numericInput("inputPowerC",label="Power of Trial:",value=0.8, min=0, max=1,step=0.01)
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
                        conditionalPanel(condition="input.selectOutcomeC==1 & input.selectResultsA=='sample'",
                                         htmlOutput("binarySampleSizeC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==1 & input.selectResultsA=='power'",
                                         htmlOutput("binaryPowerC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==2 & input.selectResultsA=='sample'",
                                         htmlOutput("continuousSampleSizeC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==2 & input.selectResultsA=='power'",
                                         htmlOutput("continuousPowerC")
                        )
                      )
             ),
             tabPanel("Design D",
                      sidebarPanel(h3("About this design:"),
                                   p("Design D is a SMART in which all participants are re-randomized, and treatment options are",em("not"),"influenced by
                                     a tailoring variable. The interventions embedded in this SMART are thus non-adaptive, since information observed
                                     bfirst- and second-stage treatments do not impact decisions regarding subsequent treatments."),
                                   p("There are",em("zero"),"embedded adaptive interventions in this design, and",em("four"),"embedded non-adaptive
                                     'intervention paths'."),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   tags$ul(
                                     tags$li("For binary outcomes, the default input is probabilities of success for an individual consistent with each of the selected intervention paths.
                                             'Cell-specific probabilities' refer to the probability of success for an individual whose intervention ended with a particular cell."),
                                     tags$li("For continuous outcomes, the default input is the standardized effect size for making comparisons between the two selected intervention paths.
                                             Alterntively, you can provide mean outcomes for the two intervention paths of interest, along with the standard error of the difference between those means.")
                                     ),
                                   h5("Input Formatting Rules:"),
                                   tags$blockquote("All inputs must be given in decimal form (with leading zero), and can be precise to two decimal places. Fractional input is
                                                   disallowed. For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("Consider a trial with a binary outcome in which the probability of response to first-stage treatment is 0.4, and that we are interested in comparing 
                                     intervention paths AC to BE. Suppose that the probability of success for ArCnrE is 0.8, and 0.9 for BrFnrG. At 80% power and 5% type-I error rate, 
                                     the sample size for this trial is 796.")
                      ),
                      mainPanel(
                        
                        ##### D PAGE HEADER #####
                        
                        h1("Design D"),
                        tags$hr(),
                        
                        ##### D DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        
                        p("Which two intervention paths would you like to compare? Choose two from the menus below.",
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
                        
                        ##### D OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectOutcomeD", label="Is the outcome of interest binary or continuous?",
                                              choices=list("Binary"=1,"Continuous"=2),selected=1)
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
                                                  p("Please provide the probability of success for each of the interventions of interest."),
                                                  uiOutput("binaryDTR1probD"),
                                                  uiOutput("binaryDTR2probD")
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeD==2",
                                                  uiOutput("continuousProbD"),
                                                  conditionalPanel(condition="input.meanSdCheckD",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("meanEstD"))
                                                                   )
                                                  )
                                 ),
                                 
                                 ##### D INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareD != 0 && input.secondDTRcompareD != 0",
                                                  br(),
                                                  helpText("If you prefer to provide different information, check the appropriate box below."),
                                                  conditionalPanel(condition="input.selectOutcomeD==1",
                                                                   checkboxInput("targetDiffCheckD",label="Check this box to input a target difference in probabilities.",value=FALSE),
                                                                   checkboxInput("targetOddsCheckD",label="Check this box to input a target odds-ratio instead of a target difference.",value=FALSE)
                                                  ),
                                                  conditionalPanel(condition="input.selectOutcomeD==2",
                                                                   checkboxInput("meanSdCheckD",label="Check this box to input a difference in means and standard deviation.",value=FALSE)
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
                                 radioButtons("selectSidedD",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaD",label="Type I Error (Alpha):",value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsD=='sample'",
                                                  numericInput("inputPowerD",label="Power of Trial:",value=0.8, min=0, max=1,step=0.01)
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
                      )
             ),
collapsable=TRUE,
footer=HTML("<p> Kidwell, Seewald, Almirall (2014) </p>")))