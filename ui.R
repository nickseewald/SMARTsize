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
# along with SMARTsize.  If not, see# This file is part of SMARTsize.
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

### UI.R FOR SMART SAMPLE SIZE CALCULATOR ###

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
                                                 });")),
                                tags$script(HTML(paste("$(document).ready(function(){
                                                       $('.Rversion').text('", paste(R.Version()$major, R.Version()$minor, sep = "."),"');
                                });", sep = ""))),
                                # tags$script(src = "C:/Users/Nick/node_modules/svg-pan-zoom/dist/svg-pan-zoom.min.js"),
                                # tags$script(src = "www/js/mermaidAPI.js"),
                                # tags$script(HTML("var config = {
                                #                  startOnLoad:true,
                                #                  flowchart:{}
                                #                  };
                                #                  mermaid.initialize(config);")),
                                tags$script(type = "text/javascript", src = "js/svg-pan-zoom.min.js")
                                ),
                              
                              ### Home Sidebar Panel: Purpose, Notation, Assumptions, and Methods
                              sidebarPanel(
                                includeHTML("www/html/sidebarHome.html")
                              ),
                              
                              ### Main content of home page
                              mainPanel(
                                HTML("<div class='page-header'>
                                     <h1>Sample Size Calculator for SMARTs with Binary or Continuous Outcomes</h1></div>"),
                                p("Get started by choosing whether you'd like to design a SMART from scratch, or
                                  select one of three common designs."),
                                br(),
                                #                               HTML('<div class="btn-group" data-toggle="buttons">
                                # <label class="btn btn-primary active">
                                #                                    <input type="checkbox" autocomplete="off" checked> Checkbox 1 (pre-checked)
                                #                                    </label>
                                #                                    <label class="btn btn-primary">
                                #                                    <input type="checkbox" autocomplete="off"> Checkbox 2
                                #                                    </label>
                                #                                    <label class="btn btn-primary">
                                #                                    <input type="checkbox" autocomplete="off"> Checkbox 3
                                #                                    </label>
                                #                                    </div>'),
                                fluidRow(
                                  column(12, bsButton("DYO.startbutton", 
                                                      label = "Design Your Own SMART",
                                                      style = "primary", size = "large", type = "action",
                                                      class = "start-button center-block"))),
                                br(),
                                fluidRow(
                                  column(12, bsButton("premade.startbutton", 
                                                      label = "Choose from pre-existing designs",
                                                      style = "link", size = "small", type = "action", class = "center-block"))),
                                br(),
                                uiOutput('premadeDesigns'),
                                hr(),
                                includeHTML("www/html/homeBackground.html"),
                                
                                ##### References #####
                                
                                tags$hr(),
                                h4("References"),
                                tags$ol(
                                  tags$li(HTML("Oetting, A., Levy, J., Weiss, R. and Murphy, S. (2007)<I>, &quot;Statistical methodology for a SMART design in the development of adaptive treatment strategies
                                               ,&quot;</I><I> in <I>Causality and Psychopathology: Finding the Determinants of Disorders and their Cures (American Psychopathological Association)</I></I>, 
                                               Arlington, VA: American Psychiatric Publishing, Inc., pp. 179-205.")),
                                  tags$li("Nahum-Shani, I., Qian, M., Almirall, D., Pelham, W. E., Gnagy, B., Fabiano, G. A., Waxmonsky, J. G., Yu, J. and Murphy, S. A. (2012), 
                                          ''Experimental design and primary data analysis methods for comparing adaptive interventions.''", em("Psychological methods,"), "17, 457.")
                                  ),
                                
                                includeHTML("www/html/references.html") # Collapse for additional selected readigs
                                  )
                                ),
          
          ##### Design Your Own #####
          
          tabPanel("Design and Size",
                   sidebarPanel(includeHTML("www/html/sidebarDYO.html"),
                                hidden(bsButton(inputId = "unlockDYO",
                                                label = "Unlock Disabled Sections", style = "warning",
                                                icon = icon("unlock-alt")))),
                   mainPanel(
                     
                     ##### DYO Page Header #####
                     h1("Design Your Own SMART"),
                     h4("Follow the steps below to describe and size a custom SMART"),
                     tags$hr(),
                     
                     bsAlert('dyo-functional'),
                     
                     ##### DYO Collapse #####
                     h3("Describe Your Study"),
                     bsCollapse(
                       bsCollapsePanel(title = "1. Describe the Study Outcome",
                                       selectDTROutcomeUI("dyoOutcome"),
                                       fluidRow(continueButton("dyo.outcome.continue")),
                                       value = "dyo.outcome.describe"),
                       bsCollapsePanel(title = "2. Describe Your Desired Result", 
                                       resultOptionsUI("dyo.resultOptions"),
                                       fluidRow(backButton("dyo.resultOptions.back"),
                                                continueButton("dyo.resultOptions.continue")),
                                       value = "dyo.resultOptions.describe"),
                       bsCollapsePanel(title = "3. Describe the First Randomization", 
                                       bsAlert(anchorId = "premade-design-disabled-stage1-describe"),
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
                                       fluidRow(backButton("dyo.stage1.back"),
                                                continueButton("dyo.stage1.continue")),
                                       value = "dyo.stage1.describe"),
                       bsCollapsePanel(title = "4. Describe Response to First-Stage Treatment",
                                       bsAlert(anchorId = "premade-design-disabled-resp-describe"),
                                       fluidRow(
                                         column(6, radioButtons("dyo.stage1.resprob.eq",
                                                                "Are the response rates the same for all first-stage treatments?",
                                                                choices = c("Yes", "No"), selected = "Yes")),
                                         column(6, uiOutput("dyo.stage1.resprobUI"),
                                                checkboxInput("conservative", HTML("Compute <strong>conservative</strong> estimates of sample size.")))
                                       ),
                                       verbatimTextOutput("rprobs"),
                                       fluidRow(backButton("dyo.resp.back"),
                                                continueButton("dyo.resp.continue")),
                                       value = "dyo.resp.describe"),
                       bsCollapsePanel(title = "5. Describe the Second Randomization",
                                       bsAlert("premadeDesignInputsDisabled-stage2Alert"),
                                       fluidRow(column(6, h4("Responders"), tags$hr(),
                                                       radioButtons("dyo.rerand.resp",
                                                                    label = "Are responders to first-stage treatment re-randomized?",
                                                                    choices = list("Yes", "No"), selected = "Yes"),
                                                       conditionalPanel(condition = 'input["dyo.rerand.resp"] == "Yes"',
                                                                        list(uiOutput("dyo.rerand.respUI"),
                                                                             sliderInput("dyo.rerand.resp.ntxt",
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
                                                                        list(uiOutput("dyo.rerand.nrespUI"),
                                                                             sliderInput("dyo.rerand.nresp.ntxt",
                                                                                         label = "How many treatments are non-responders re-randomized between?",
                                                                                         min = 2, max = 5, value = 2, step = 1, ticks = FALSE, width = "80%"),
                                                                             radioButtons("dyo.rerand.nresp.eqrand", 
                                                                                          label = "Are non-responders re-randomized equally between treatments?",
                                                                                          choices = list("Yes", "No"), selected = "Yes"),
                                                                             conditionalPanel(condition = 'input["dyo.rerand.nresp.eqrand"] == "No"',
                                                                                              list(p("What are the allocation probabilities?"),
                                                                                                   fluidRow(column(11, uiOutput("dyo.rerand.nresp.rprobUI"), offset = 1)))))))),
                                       bsAlert("mustRerandomizeAlert"),
                                       fluidRow(backButton("dyo.rerand.back")),
                                       value = "dyo.rerand.describe"),
                       id = "dyo.setup.collapse",
                       open = "dyo.outcome.describe", multiple = TRUE
                     ),
                     
                     
                     ##### Aim Selection #####
                     primaryAimUI("dyo.primaryAim"),
                     conditionalPanel(
                       condition = "output.dyoprimaryAim == 'dtrs'",
                       eval(text.selectDTRcompare),
                       fluidRow(column(6, uiOutput("dyo.refdtrSelect")),
                                column(6, uiOutput("dyo.compdtrSelect")))
                     ),
                     conditionalPanel(
                       condition = "output.dyoprimaryAim == 'stage1' && input['dyo.stage1.ntxt'] > 2",
                       html.selectStage1Compare,
                       fluidRow(column(6, uiOutput("stage1Tx1Select")),
                                column(6, uiOutput("stage1Tx2Select"))),
                       fluidRow(column(6, checkboxInput("stage1omnibus", "Hi")))
                     ),
                     conditionalPanel(
                       condition = "output.dyoprimaryAim == 'stage2resp' && input['dyo.rerand.resp.ntxt'] > 2",
                       html.selectStage2RCompare,
                       fluidRow(column(6, uiOutput("stage2RTx1Select")),
                                column(6, uiOutput("stage2RTx2Select")))
                     ),
                     conditionalPanel(
                       condition = "output.dyoprimaryAim == 'stage2nresp' && input['dyo.rerand.nresp.ntxt'] > 2",
                       html.selectStage2NRCompare,
                       fluidRow(column(6, uiOutput("stage2NRTx1Select")),
                                column(6, uiOutput("stage2NRTx2Select")))
                     ),
                     tags$hr(),
                     
                     ##### Diagram and Probability Inputs #####
                     h3("Diagram and Outcome Information"),
                     fluidRow(
                       column(7, DiagrammeROutput("dyo.diagram", height = "550px")),
                       column(5, 
                              conditionalPanel(
                                condition = "output.dyooutcome == 'Binary' && output.dyoprimaryAim == 'dtrs'",
                                uiOutput("binaryRefInput"),
                                conditionalPanel(condition = "input.cellOrMarginal",
                                                 fluidRow(column(1),
                                                          column(
                                                            11, uiOutput("binaryRefCellProbs")
                                                          ))),
                                uiOutput("binaryCompInput"),
                                conditionalPanel(condition = "input.cellOrMarginal",
                                                 fluidRow(column(1),
                                                          column(
                                                            11, uiOutput("binaryCompCellProbs")
                                                          )))
                              ),
                              conditionalPanel(
                                condition = "output.dyooutcome == 'Binary' && output.dyoprimaryAim == 'stage1'",
                                html.stage1ProbsGuide,
                                uiOutput("binaryStage1Probs")
                              ),
                              conditionalPanel(
                                condition = "output.dyooutcome == 'Binary' && output.dyoprimaryAim == 'stage2resp'",
                                html.stage1ProbsGuide,
                                uiOutput("binaryStage2RProbs")
                              ),
                              conditionalPanel(
                                condition = "output.dyooutcome == 'Binary' && output.dyoprimaryAim == 'stage2nresp'",
                                html.stage1ProbsGuide,
                                uiOutput("binaryStage2NRProbs")
                              ),
                              conditionalPanel(condition = "output.dyooutcome == 'Continuous'",
                                               uiOutput('contDTRInput')),
                              conditionalPanel(
                                condition = "input.dyoprimaryAim == 'dtrs'",
                                conditionalPanel(
                                  condition = "input.dyoRefDTR != '' && input.dyoCompDTR != ''",
                                  br(),
                                  conditionalPanel(
                                    condition = "output.dyooutcome == 'Binary'",
                                    eval(text.altInputHelp),
                                    checkboxInput("cellOrMarginal",   label = text.cellSpecLabel, value = FALSE),
                                    bsAlert("cellOrMarginalDisabled"),
                                    checkboxInput("targetDifference", label = text.targDiffLabel, value = FALSE),
                                    checkboxInput("targetOddsRatio",  label = text.targORLabel,   value = FALSE)
                                  )
                                )
                              ))
                     ),
                     
                     tags$hr(),
                     
                     ##### Results #####
                     h3("Results"),
                     conditionalPanel(
                       condition = "output.dyooutcome == 'Continuous'",
                       uiOutput('continuousSampleSize')
                     ),
                     conditionalPanel(
                       condition = "output.dyooutcome == 'Binary'",
                       uiOutput('binarySampleSize')
                     ),
                     
                     # fluidRow(actionButton("zoom", "Zoom", icon = icon("zoom-in", lib = "glyphicon"))),
                     # verbatimTextOutput("graphstring"),
                     # tags$script(HTML("$(document).ready(function(){
                     #                  zoomDiagram = svgPanZoom('#mermaidChart0', {
                     #                  zoomEnabled: true,
                     #                  controlIconsEnabled: true,
                     #                  fit: true,
                     #                  center: true,});
                     #                  });")),
                     
                     tags$hr(),
                     bookmarkButton()
                     
                     # tags$script()
                   ) # end mainPanel
          ) # end tabPanel
          
                     )))