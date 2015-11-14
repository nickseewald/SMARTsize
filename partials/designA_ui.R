##### Define UI for Design A #####

##### A SIDEBAR #####

         sidebarPanel(
           includeHTML("www/html/sidebarA.html")
         )
         
         ##### A MAIN PANEL #####
         mainPanel(
           
           ##### A PAGE HEADER #####
           
           h1("Design I"),
           tags$hr(),
           
           ##### A OUTCOME SELECTION #####
           radioButtons("selectOutcomeA", label = HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                        choices = list("Binary" = 1,"Continuous" = 2), selected = 1),
           
           tags$hr(),
           
           ##### A DTR SELECTION #####
           # Dropdown menus provide options to select DTRs for comparison.
           # Currently the menus are not reactively-repopulating (making it possible to select the same DTR twice). Possible future improvement.
           
           p("Which two", strong("adaptive interventions"), "would you like to compare? Choose two from the menus below.",
             "The image below will change to highlight the AIs you select."),
           
           fluidRow(
             column(6, uiOutput("selectAI1A")),
             column(6, uiOutput("selectAI2A"))
           ),
           fluidRow(textOutput("printer")),
           
           tags$hr(),
           
           ##### A IMAGE AND PROBABILITY INPUTS #####
           # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
           # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
           
           fluidRow(
             column(7, imageOutput("designAimg", width = "100%", height = "100%")),
             column(5, 
                    numericInput("respA",
                                 label = HTML("<p>Concerning the tailoring variable, please provide the 
                                              <strong>probability of response</strong> to the 
                                              first-stage intervention. If you are unsure, leave as 
                                              0 for a conservative estimate.</p>"), 
                                 value = 0,min = 0,max = 1,step = 0.01),
                    conditionalPanel(condition = "input.selectOutcomeA == 1",
                                     p("Concerning the primary outcome, please provide the",
                                       strong("probability of success"), "for each of the AIs of interest."),
                                     uiOutput("binaryDTR1probA"),
                                     conditionalPanel(condition = "input.cellOrConditionalA",
                                                      fluidRow(column(11,offset = 1,
                                                                      uiOutput("cellProbsDTR1A"))
                                                      )
                                     ),
                                     uiOutput("binaryDTR2probA"),
                                     conditionalPanel(condition = "input.cellOrConditionalA",
                                                      fluidRow(column(11,offset = 1,
                                                                      uiOutput("cellProbsDTR2A"))
                                                      )
                                     )
                    ),
                    conditionalPanel(condition = "input.selectOutcomeA == 2",
                                     p("Concerning the primary outcome, please provide the", strong("standardized effect size"), "between the
                                       AIs of interest."),
                                     uiOutput("continuousProbA")
                                     ),
                    
                    ##### A INPUT OPTIONS #####
                    # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                    # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                    # For continuous outcomes, option to input mean difference and standard-deviation
                    
                    conditionalPanel(condition = "input.firstDTRcompareA != 0 && input.secondDTRcompareA != 0",
                                     br(),
                                     conditionalPanel(condition = "input.selectOutcomeA == 1",
                                                      helpText("If you prefer to provide different information, check the appropriate box below."),
                                                      checkboxInput("cellOrConditionalA",label = "Cell-Specific Success Probabilities",value = FALSE),
                                                      checkboxInput("targetDiffCheckA",label = "Target Difference in Success Probabilities",value=FALSE),
                                                      checkboxInput("targetOddsCheckA",label = "Target Odds Ratio",value = FALSE)
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
           conditionalPanel(condition = "input.selectOutcomeA == 1 & input.selectResultsA == 'sample'",
                            htmlOutput("binarySampleSizeA")
           ),
           conditionalPanel(condition = "input.selectOutcomeA == 1 & input.selectResultsA == 'power'",
                            htmlOutput("binaryPowerA")
           ),
           conditionalPanel(condition = "input.selectOutcomeA == 2 & input.selectResultsA == 'sample'",
                            htmlOutput("continuousSampleSizeA")
           ),
           conditionalPanel(condition = "input.selectOutcomeA == 2 & input.selectResultsA == 'power'",
                            htmlOutput("continuousPowerA")
           )
           )
         
         ##### A TOOLTIPS #####
         ### Add bootstrap-style tooltips to inputs coaching proper formatting
         ### Below are tooltips for inputs rendered statically in ui.R
         ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
         
         bsTooltip(id = "respA", title = "Input can range from 0-1 and must be in decimal form, up to two places.", 
                   placement = "right", trigger = "focus")      
         bsTooltip(id = "alphaA", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                   placement = "right", trigger = "focus")
         bsTooltip(id = "inputPowerA", title = "Input can range from 0-1 and must be in decimal form, up to two places.",
                   placement = "right", trigger = "focus")
         bsTooltip(id = "inputSampleSizeA", title = "Input must be an integer greater than zero.", 
                   placement = "right", trigger = "focus")                    
