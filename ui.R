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
                      
                      sidebarPanel(h3("Methods"),
                                   p("For binary outcomes, sample size is computed according to the methods presented in Kidwell, et al (in preparation). "),
                                   p("For continuous outcomes, sample size is computed according to the methods in Oetting, et al (2007)."),
                                   p("The application is run on", tags$a(href="http://www.r-project.org/", "R"), "version 3.1.0, and written using",
                                     tags$a(href="http://shiny.rstudio.com/","Shiny,"), "an open-source web application framework for R produced by",
                                     tags$a(href="http://www.rstudio.com", "RStudio.")),
                                   br(),
                                   h4("Notation and Assumptions"),
                                   tags$ul(
                                     tags$li(img(src="images/randomize.gif",width=25),"refers to randomization. We assume equal probability of randomization to two 
                                             intervention options, R=0.5."),
                                     tags$li("Adaptive interventions (AIs) are denoted by the combination of the letters of the interventions where the intervention
                                             following ‘r’ indicates the option for those who respond and the intervention following ‘nr’ indicates the option for those
                                             who do not respond. For example, ArCnrE denotes the AI which reads as ''First receive A and if there is response, receive C,
                                             if there is no response, receive E.''")
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
                        
                        h2("Background"),
                        
                        p("An",strong("adaptive intervention"), "AI, also known as a dynamic treatment regime, treatment algorithm, adaptive treatment strategy) is a sequence of treatments
                          or interventions tailored to an individual. For example, an AI in the treatment of breast cancer may be ''Following surgery, treat with chemotherapy for six cycles.
                          If there is no evidence of cancer and the lymph nodes are negative, treat with an aromatase inhibitor for five years. If there is evidence of cancer following chemotherapy,
                          continue chemotherapy for another six cycles. If the patient experiences a grade III or higher toxicity on the prescribed chemotherapy, switch to another chemotherapy.'
                          Note that this AI includes a decision point after six cycles of chemotherapy, at which point, depending on the patient's response thus far, a secondary treatment is recommended.
                          This secondary treatment is different for responders and non-responders."),
                        
                        p("The notion of a 'tailoring variable' which we evaluate at each decision point is critical for an adaptive intervention. In fact, this variable is what makes the 
                          intervention",em("adaptive."), "Tailoring variables let us construct decision rules for assigning subsequent treatments, so they should be identified based solely
                          on practical, ethical, or scientific reasoning, and produce clear, specific, and objective decision rules. In the above example, and throughout this application, 
                          we use 'response to first-stage treatment' as the variable by which we determine the subsequent treatment an individual will receive. For more information about
                          tailoring variables, see Nahum-Shani, et al. (2012). Depending on your area of research, 'response' may be classified differently than in the diagrams above. 
                          For example, in Design B, if your trial stipulates that individuals who respond to first stage treatment are rerandomized and non-responders are not, the application
                          can accommodate your needs.To make this change, simply provide 'non-response probability' when prompted for 'response probability'. "),
                        
                        p("A ",strong("Sequential Multiple Assignment Randomized Trial (SMART)"), " develops AIs. A SMART is an experimental design in which individuals are randomized multiple
                          times and follow specific intervention sequences. There are several advantages to using a SMART. First, it allows investigators to detect 'delayed effects' in treatment;
                          that is, the long-term effects of an intervention after subsequent treatment has been administered. These delayed effects may be synergistic or antagonistic, and cannot
                          be detected by separate trials for each stage. Additionally, a SMART may decrease the likelihood of participant drop-out (due to the promise of subsequent treatment 
                          regardless of response), and, because of this, may reduce selection bias found in a standard non-responder trial. Additionally, a SMART can address stage-specific research
                          questions, similar to standard trials, but with the extra advantage of developing and comparing embedded AIs which may more closely mimic clinical practice."),
                        
                        tags$hr(),
                        h4("References"),
                        tags$ol(
                          tags$li("Oetting, A., Levy, J., Weiss, R. and Murphy, S. (2007),",
                                  em("''Statistical methodology for a SMART design in the development of adaptive treatment strategies,'' in Causality and Psychopathology: Finding the Determinants of Disorders and their Cures,"),
                                  "Arlington: American Psychiatric Publishing, Inc."),
                          tags$li("Nahum-Shani, I., Qian, M., Almirall, D., Pelham, W. E., Gnagy, B., Fabiano, G. A., Waxmonsky, J. G., Yu, J. and Murphy, S. A. (2012), 
                                  ''Experimental design and primary data analysis methods for comparing adaptive interventions.''",em("Psychological methods,"), "17, 457.")
                        )
                      )
             ), 
             
             ##### DESIGN A #####
             
             ##### A SIDEBAR #####
             
             tabPanel("Design A",
                      sidebarPanel(h4("About this design:"),
                                   p("This SMART design re-randomizes all patients, but second-stage interventions may be dependent on response status. For example, a participant 
                                     in the trial who ''responds'' to treatment B is randomized to second-stage treatment G or H, whereas an individual who ''does not respond'' to
                                     B is randomized to either I or J."),
                                   p("It is", em("not"), "necessary that all second-stage treatment options be distinct. For example, treatments C and D may be the same as G and H."),
                                   p("There are",em("eight"),"embedded AIs."),
                                   tags$hr(),
                                   h4("Inputs and Assumptions:"),
                                   tags$ul(
                                     tags$li("The probability of response is equal for interventions A and B."),
                                     tags$li("For binary outcomes, the default inputs are the probabilities of success for each of the selected AIs. 'Cell-specific probabilities'
                                             refer to the probabilities of success for those consistent with a particular intervention pathway. Instead, a target difference in
                                             probabilities or target odds-ratio may be selected."),
                                     tags$li("For continuous outcomes, the default input is the standardized effect size for comparisons between the two selected AIs. 
                                             Alternatively, inputs may include the mean outcomes for the two AIs of interest and the standard error of the difference between means.")
                                   ),
                                   h5("Input Formatting Rules:"),
                                   tags$blockquote("All inputs must be given in decimal form with leading zero (no fractions, please) and may include up to two decimal places. For example,
                                                   ''0.07'' is valid input; both ''.07'' and ''7/100'' are invalid. Improperly-formatted input may result in unpredictable behavior or an error message."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("We wish to find the sample size for a SMART with a binary outcome where the probability of response to first stage interventions is 0.50. We estimate the overall 
                                     probabilities of success in the two AIs of interest, ArCnrE and BrGnrI,  are 0.65 and 0.80, respectively. Given a two-sided 5% type-I error, we require a sample size
                                     of 552 to make this comparison with 80% power.")
                      ),
                      
                      mainPanel(
                        
                        ##### A PAGE HEADER #####
                        
                        h1("Design A"),
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
                        
                        ##### A OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectOutcomeA", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                              choices=list("Binary"=1,"Continuous"=2),selected=1)
                          ),
                          column(6,
                                 numericInput("respA",
                                              label=HTML("What is the <strong> probability of response </strong> to the first-stage intervention? 
                                                         If you are unsure, leave as 0 for a conservative estimate."), 
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
                                                  p("Please provide the", strong("probability of success"), "for each of the AI's of interest."),
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
                      )
             ),
             
             
             ########### DESIGN B ##########
             
             tabPanel("Design B",
                      
                      ##### B SIDEBAR #####
                      
                      sidebarPanel(h4("About this design:"),
                                   p("This SMART design re-randomizes only those who do not respond to the initial intervention. "),
                                   p("It is",em("not"),"necessary that all possible second-stage treatments be distinct. For example, interventions D and E may be the same as
                                     G and H or C may be the same as B."),
                                   p("There are",em("four"),"embedded AIs."),
                                   tags$hr(),
                                   h4("Inputs and Assumptions:"),
                                   tags$ul(
                                     tags$li("The probability of response is equal for interventions A and B."),
                                     tags$li("For binary outcomes, the default inputs are the probabilities of success for each of the selected AIs. 'Cell-specific probabilities' refer to the probabilities of
                                             success for those consistent with a particular intervention pathway. Instead, a target difference in probabilities or target odds-ratio may be selected."),
                                     tags$li("For continuous outcomes, the default input is the standardized effect size for comparisons between the two selected AIs. Alternatively, inputs may include the mean
                                             outcomes for the two AIs of interest and the standard error of the difference between means.")
                                   ),
                                   h5("Input Formatting Rules:"),
                                   tags$blockquote("All inputs must be given in decimal form with leading zero (no fractions, please), and can be precise to two decimal places.
                                                   For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("We wish to find the sample size for a SMART with a binary outcome where the probability of response to first stage interventions is 0.60. We estimate the overall probabilities
                                     of success in the two AIs of interest, ArCnrD and BrFnrG, to be 0.70 and 0.50, respectively. Given a two-sided 5% type-I error, we require a sample size of 600 to make this
                                     comparison with 80% power.")
                      ),
                      
                      mainPanel(
                        
                        ##### B PAGE HEADER #####
                        
                        h1("Design B"),
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
                        
                        ##### B OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectOutcomeB", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                              choices=list("Binary"=1,"Continuous"=2),selected=1)
                                 ),
                          column(6,
                                 numericInput("respB",
                                              label=HTML("What is the <strong> probability of response </strong> to the first-stage intervention? 
                                                         If you are unsure, leave as 0 for a conservative estimate."),
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
                                                  p("Please provide the", strong("probability of success"), "for each of the AI's of interest."),
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
                    )
             ),
             
             ########### DESIGN C ##########
  
             tabPanel("Design C",
                      sidebarPanel(h3("About this design:"),
                                   p("In this SMART design, randomization at the second stage depends on the first-stage intervention and response this intervention.
                                     Only those individuals who did not respond to intervention A are re-randomized. This trial may be necessary due to lack of 
                                     second-stage options for particular interventions."),
                                   p("Treatments C and F",em("not"),"be distinct, but D and E must be distinct."),
                                   p("There are",em("three"),"embedded AIs."),
                                   tags$hr(),
                                   h4("Inputs and Assumptions:"),
                                   tags$ul(
                                     tags$li("The probability of response is equal for interventions A and B."),
                                     tags$li("For binary outcomes, the default inputs are the probabilities of success for each of the selected AIs. 'Cell-specific probabilities' refer to the probabilities of
                                             success for those consistent with a particular intervention pathway. Instead, a target difference in probabilities or target odds-ratio may be selected."),
                                     tags$li("For continuous outcomes, the default input is the standardized effect size for comparisons between the two selected AIs. Alternatively, inputs may include the mean
                                             outcomes for the two AIs of interest and the standard error of the difference between means.")
                                   ),
                                   h5("Input Formatting Rules:"),
                                   tags$blockquote("All inputs must be given in decimal form with leading zero (no fractions, please), and can be precise to two decimal places.
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
                        
                        ##### C OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectOutcomeC", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                              choices=list("Binary"=1,"Continuous"=2),selected=1)
                          ),
                          column(6,
                                 numericInput("respC",
                                              label=HTML("What is the <strong> probability of response </strong> to the first-stage intervention? 
                                                         If you are unsure, leave as 0 for a conservative estimate."),
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
                                                  p("Please provide the", strong("probability of success"), "for each of the AI's of interest."),
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
                      )
             ),
             
             ########### DESIGN D ##########
             
             tabPanel("Design D",
                      sidebarPanel(h3("About this design:"),
                                   p("In this design, all participants are re-randomized at the second-stage and treatment options are",em("not"),"influenced by
                                     a tailoring variable."),
                                   p("Second-stage interventions need", em("not"), "be distinct. For example, C and E may be the same or D and F may be the same."),
                                   p("The interventions embedded in this SMART are non-adaptive. There are four non-adaptive intervention paths."),
                                   tags$hr(),
                                   h4("Inputs:"),
                                   tags$ul(
                                     tags$li("For binary outcomes, the default input is probabilities of success for an individual consistent with each of the selected intervention paths. Instead, a target
                                             difference in probabilities or target odds-ratio may be selected."),
                                     tags$li("For continuous outcomes, the default input is the standardized effect size for making comparisons between the two selected intervention paths. Alternatively,
                                             you can provide mean outcomes for the two intervention paths of interest, along with the standard error of the difference between means.")
                                   ),
                                   h5("Input Formatting Rules:"),
                                   tags$blockquote("All inputs must be given in decimal form with leading zero (no fractions, please), and can be precise to two decimal places.
                                                   For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("We wish to find the sample size for a trial with a binary outcome where the probability of response to first stage interventions is 0.40. We estimate the overall
                                     probabilities of success in the two intervention paths of interest, AC and BF to be 0.75 and 0.60, respectively. Given a two-sided 5% type-I error, we require a 
                                     sample size of 608 to make this comparison with 80% power.")
                      ),
                      mainPanel(
                        
                        ##### D PAGE HEADER #####
                        
                        h1("Design D"),
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
                        
                        ##### D OUTCOME SELECT AND RESPONSE INPUT #####
                        # Provide options to select binary/continuous outcome
                        # User input to provide response probabilities
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectOutcomeD", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
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
                                                  p("Please provide the", strong("probability of success"), "for each of the intervention paths of interest."),
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
                      )
             ),
collapsable=TRUE,
footer=HTML("<p> Kidwell, Seewald, Almirall (in preparation). </p>")))