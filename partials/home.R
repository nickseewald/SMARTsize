tagList(
  
  tags$head(
    tags$style(HTML("img {
                    height=auto; max-width=100%;
                    }")),
                        tags$style(type='text/css',"input[type='number'] {width:60px}"), #set width of numericInputs
    tags$style(type='text/css',includeHTML("www/css/bootstrap-modal.css")), #responsive modals  
    tags$link(rel="stylesheet", href="//fonts.googleapis.com/css?family=Roboto|Roboto+Condensed"),
    tags$style("body {font-family: 'Roboto', sans-serif;} 
               h1 {font-family: 'Roboto Condensed', sans-serif;} 
               h2 {font-family: 'Roboto Condensed', sans-serif;}
               h3 {font-family: 'Roboto Condensed', sans-serif;} 
               h4 {font-family: 'Roboto Condensed', sans-serif;} 
               h5 {font-family: 'Roboto Condensed', sans-serif;} 
               h6 {font-family: 'Roboto Condensed', sans-serif;}") #apply font styles
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
             img(src="images/SMARTdesignA__.gif", class = "img-responsive"),
             actionButton("pickTabA","Design A"),
             bsButton(inputId = "launchModalA",label="Click here for an example",style="link"),
             bsModal(id="modalA",title="Modal A", trigger="launchModalA",source(file="./www/R/references.R",local=T,echo=F)$value,size='large')
             #                                  HTML("<p> 8 embedded adaptive interventions: ArCnrE, ArCnrF, ArDnrE, ArDnrF, BrGnrI, BrGnrJ, BrHnrI, BrHnrJ.
             #                                       <a data-toggle='modal' data-target='#exampleAmodal' style='color:#6b6b6b'>
             #                                       Click here for an example. </a> </p>"),
             #                                  includeHTML("www/html/exampleAmodal.html")
      ),
      column(6,
             img(src="images/SMARTdesignB__.gif", class = "img-responsive"),
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
             img(src="images/SMARTdesignC__.gif", class = "img-responsive"),
             actionButton("pickTabC","Design C"),
             HTML("<p> 3 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG.
                  <a data-toggle='modal' data-target='#exampleCmodal' style='color:#6b6b6b'>
                  Click here for an example. </a> </p>"),
             includeHTML("www/html/exampleCmodal.html")
             ),
      column(6,
             img(src="images/SMARTdesignD__.gif", class = "img-responsive"),
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
)