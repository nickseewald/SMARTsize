tagList(
  
  tags$head(
    tags$style(HTML("img {
                    height=auto; max-width=100%;
                    }")),
                        tags$style(type='text/css',"input[type='number'] {width:60px}"), #set width of numericInputs
    # tags$style(type='text/css',includeHTML("./www/css/bootstrap-modal.css")), #responsive modals  
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
    includeHTML("www/html/sidebarA.html")
  ),
  
  mainPanel(                        
    h1("Sample Size Calculator for SMARTs with Binary or Continuous Outcomes"),
    br(),
    p("Choose the SMART design of interest by clicking the corresponding tab at the top of the window, 
      or the button below the corresponding diagram. Notation is established in the sidebar. 
      See below for more background on SMARTs."),
    br()
  )
)