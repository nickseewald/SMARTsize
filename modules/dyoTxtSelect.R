dyoTxtSelectUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput("rerandUI"),
    
    fluidRow(column(6, sliderInput(ns("ntxt"), 
                                   label = "How many initial treatments do you have?",
                                   min = 2, max = 5, value = 2, step = 1, ticks = FALSE)),
             column(6, radioButtons(ns("eqrand"),
                                    label = "Are participants randomized equally between treatments?",
                                    choices = list("Yes", "No"), selected = "Yes"),
                    conditionalPanel(condition = 'input["dyo.stage1.eqrand"] == "No"',
                                     list(p("What are the allocation probabilities?"),
                                          fluidRow(column(1),
                                                   column(11, uiOutput("dyo.stage1.rprobUI")))))))
  )
}

dyoTxtSelect <- function(input, output, session, stage) {
  # output$stage <- renderText(stage)
  output$label <- renderText({
    switch(stage, 
           "stage1" = "How many initial treatments do you have?",
           "stage2resp" = "")
  })
  
  output$rerandUI <- renderUI({
    label <- switch(stage,
                    "stage1"      = "",
                    "stage2resp"  = "Are responders to first-stage treatment re-randomized?".
                    "stage2nresp" = "Are non-responders to first-stage treatment re-randomized?")
    if (stage != "stage1") {
      radioButtons(ns(paste0(stage, ".rerand")), 
                   label = label, choies = list("Yes", "No"), selected = "Yes")
    }
  })
  
  output$ntxtSelectUI <- renderUI({
    label <- switch(stage,
                    "stage1"      = "How many initial treatments do you have?",
                    "stage2resp"  = "How many treatments are responders re-randomized between?",
                    "stage2nresp" = "How many treatments are non-responders re-randomized between?")
    
  })
  
  output$rprobUI <- renderUI({})
}