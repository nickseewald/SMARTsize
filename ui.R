library(shiny)

shinyUI(
  navbarPage("SMART Sample Size Calculator",

             
             
             
             ##### HOME TAB#####
             
             tabPanel("Home",
                      sidebarPanel(h3("Sidebar information"),
                                   p("Welcome to the app. We'll put some instructions here, etc. etc."),
                                   p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
                                      Integer nec odio. Praesent libero. Sed cursus ante dapibus 
                                      diam. Sed nisi. Nulla quis sem at nibh elementum imperdiet. 
                                      Duis sagittis ipsum. Praesent mauris. Fusce nec tellus sed 
                                      augue semper porta. Mauris massa. Vestibulum lacinia arcu 
                                      eget nulla. Class aptent taciti sociosqu ad litora torquent 
                                      per conubia nostra, per inceptos himenaeos. Curabitur 
                                      sodales ligula in libero.")),
                      mainPanel(h1("Introduction"),br(),
                                p("Choose a design by clicking the cooresponding tab at the top of the window. If you are using a smartphone, 
                                  options are available via the menu button (three horizontal lines) at the top of the window."), br(),
                                fluidRow(column(6,img(src="images/SMARTdesignA.gif")), column(6,img(src="images/SMARTdesignB_0_0.gif"))),
                                fluidRow(column(6,helpText("Design A")),column(6,helpText("Design B"))),
                                fluidRow(column(6,img(src="images/SMARTdesignC.gif")), column(6,img(src="images/SMARTdesignD.gif"))),
                                fluidRow(column(6,helpText("Design C")),column(6,helpText("Design D"))),
                                h2("More Information"),
                                p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer nec odio. Praesent libero. Sed 
                                  cursus ante dapibus diam. Sed nisi. Nulla quis sem at nibh elementum imperdiet. Duis sagittis ipsum. 
                                  Praesent mauris. Fusce nec tellus sed augue semper porta. Mauris massa. Vestibulum lacinia arcu eget nulla. "),
                                p("Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Curabitur 
                                  sodales ligula in libero. Sed dignissim lacinia nunc. Curabitur tortor. Pellentesque nibh. Aenean quam.
                                  In scelerisque sem at dolor. Maecenas mattis. Sed convallis tristique sem. Proin ut ligula vel nunc 
                                  egestas porttitor. Morbi lectus risus, iaculis vel, suscipit quis, luctus non, massa. Fusce ac turpis 
                                  quis ligula lacinia aliquet. Mauris ipsum. "),
                                p("Nulla metus metus, ullamcorper vel, tincidunt sed, euismod in, nibh. Quisque volutpat condimentum velit.
                                  Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nam nec ante. 
                                  Sed lacinia, urna non tincidunt mattis, tortor neque adipiscing diam, a cursus ipsum ante quis turpis. 
                                  Nulla facilisi. Ut fringilla. Suspendisse potenti. Nunc feugiat mi a tellus consequat imperdiet. Vestibulum
                                  sapien. Proin quam. Etiam ultrices. Suspendisse in justo eu magna luctus suscipit.")
                      )
             ), 
             tabPanel("Design A",
                      sidebarPanel(h3("About this design:"),
                                   p("Include information about the design, its requirements, and describe the input.")
                      ),
                      mainPanel(h1("Design A"),
                                img(src="images/SMARTdesignA.gif"),
                                fluidRow(
                                  column(6,
                                         radioButtons("selectOutcomeA", label="Select a Type of Outcome:",
                                                      choices=list("Binary"=1,"Continuous"=2),selected=1)
                                  ),
                                  column(6,
                                         radioButtons("selectSuccessA",label="Select a method of providing Success Probabilities:",
                                                      choices=list("Overall DTR Success"=1, "Marginal Treatment Success"=2, "Target Difference"=3),
                                                      selected=1
                                         )
                                  )
                                ),
                                fluidRow(
                                  column(6,
                                         numericInput("respA.1",label="Probability of Response to First-Stage Treatment:",value=0,min=0,max=1,step=0.0001)
                                  ),
                                  column(6,
                                         conditionalPanel(condition="input.selectSuccessA==1",
                                                          numericInput("DTRsucc1A",label="Probability of Success for DTR ArCAnrE",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("DTRsucc2A",label="Probability of Success for DTR ArCAnrF",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("DTRsucc3A",label="Probability of Success for DTR ArDAnrE",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("DTRsucc4A",label="Probability of Success for DTR ArDAnrF",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("DTRsucc5A",label="Probability of Success for DTR BrHBnrJ",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("DTRsucc6A",label="Probability of Success for DTR BrHBnrK",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("DTRsucc7A",label="Probability of Success for DTR BrIBnrJ",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("DTRsucc8A",label="Probability of Success for DTR BrIBnrK",value=0,min=0,max=1,step=0.0001)
                                         ),
                                         conditionalPanel(condition="input.selectSuccessA==2",
                                                          numericInput("marSucc1A",label="P(S | A, r, C)",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("marSucc2A",label="P(S | A, r, D)",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("marSucc3A",label="P(S | A, nr, E)",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("marSucc4A",label="P(S | A, nr, F)",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("marSucc5A",label="P(S | B, r, H)",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("marSucc6A",label="P(S | B, r, I)",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("marSucc7A",label="P(S | B, nr, J)",value=0,min=0,max=1,step=0.0001),
                                                          numericInput("marSucc8A",label="P(S | B, nr, K)",value=0,min=0,max=1,step=0.0001)
                                         ),
                                         conditionalPanel(condition="input.selectSuccessA==3",
                                                          numericInput("effectSize",label="Target Difference in Effects:", value=0.5,min=0,max=1,step=0.00001)
                                         )
                                  )
                                ),
                                numericInput("sampleSize",label="Sample Size",value=0) 
                      )
                      ),
             
             ########### DESIGN B ##########
             
             tabPanel("Design B",
                      
                      ##### B SIDEBAR #####
                      
                      sidebarPanel(h3("About this design:"),
                                   p("Design B is a SMART in which randomization to second-stage treatment options depends on response to the first-stage treatment."),
                                   p(strong("Note:"), em("All inputs must be given in decimal form (with leading zero), and can be precise to four decimal places.
                                     Fractional input is disallowed."))
                      ),
                      mainPanel(
                        
                        ##### B PAGE HEADER #####
                        
                        h1("Design B"),
                        tags$hr(),
                        
                        ##### B DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        # Currently the menus are not reactively-repopulating (making it possible to select the same DTR twice). Possible future improvement.
                        
                        p("Which two dynamic treatment regimes would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the DTRs you select."),
                        
                        fluidRow(
                          column(6,
                                 selectInput("firstDTRcompareB",label="Compare DTR",
                                             choices=list("[Choose First DTR]"=0, "ArAnrC"="ArAnrC", "ArAnrD"="ArAnrD", "BrBnrE"="BrBnrE", "BrBnrF"="BrBnrF"),
                                             selected=0)
                          ),
                          column(6,
                                 selectInput("secondDTRcompareB",label="to DTR",
                                             choices=list("[Choose Second DTR]"=0, "ArAnrC"="ArAnrC", "ArAnrD"="ArAnrD", "BrBnrE"="BrBnrE", "BrBnrF"="BrBnrF"),
                                             selected=0)  
                          )
                        ),
                        
                        # If the same DTR is selected twice, print an error message.
                        # Currently, UI and Server code conditions on non-unique DTR selection. If above dropdowns become dynamic, deprecate this conditioning.
                        
                        fluidRow(
                                 conditionalPanel(condition="input.firstDTRcompareB==input.secondDTRcompareB & input.firstDTRcompareB != 0",
                                           p("ERROR: Please select two different DTRs to compare.",style="color:red")
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
                                              value=0,min=0,max=1,step=0.0001)
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
                                                  uiOutput("continuousProbB")
                                 )
                          )
                        ),
                        
                        tags$hr(),

                        ##### B INPUT OPTIONS #####
                        # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                          # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                          # For continuous outcomes, option to input mean difference and standard-deviation

                        fluidRow(
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
                        ),
                        
                        tags$hr(),
                        
                        ##### B RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,radioButtons("selectResultsB",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample")),
                          column(6,
                                 numericInput("alphaB",label="Type I Error (Alpha):",value=0.05,min=0,max=1,step=0.0001),
                                 conditionalPanel(condition="input.selectResultsB=='sample'",
                                                  numericInput("inputPowerB",label="Power of Trial:",value=0.8, min=0, max=1,step=0.0001)
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
                                           h4(textOutput("binarySampleSizeB"), style = "color:blue")
                                           ),
                          conditionalPanel(condition="input.selectOutcomeB==1 & input.selectResultsB=='power'",
                                           h4(textOutput("binaryPowerB"),style="color:blue")
                                           ),
                          conditionalPanel(condition="input.selectOutcomeB==2 & input.selectResultsB=='sample'",
                                           h4(textOutput("continuousSampleSizeB"),style="color:blue")
                                           ),
                          conditionalPanel(condition="input.selectOutcomeB==2 & input.selectResultsB=='power'",
                                           h4(textOutput("continuousPowerB"),style="color:blue")
                          )
                    )
             ),
             tabPanel("Design C",
                      sidebarPanel(h3("About this design:"),
                                   p("Include information about the design, its requirements, and describe the input.")
                      ),
                      mainPanel(h1("Design C"),
                                div(class='row-fluid',
                                    div(class='span4',img(src="images/SMARTdesignC.gif")),
                                    div(class='span3',numericInput("test",label="test",value=0)),
                                    div(class='span3',numericInput("othertest",label="othertest",value=0)))
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
collapsable=TRUE)
)
