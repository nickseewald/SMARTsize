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
# along with SMARTsize.  If not, see <http://www.gnu.org/licenses/>.

primaryComparisonUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("compare"))
    ) # end tagList
}

primaryComparison <- function(input, output, session, primaryAim, design) {
  
  output$compare <- renderUI({
    if (primaryAim() == "pairwise") {
      out <- fluidRow(
        column(6, 
               selectizeInput(ns("AI1"),
                              label = "Reference Adaptive Intervention:",
                              choices = eval(parse(text = paste0("design", design, ".AIs"))),
                              options = list(
                                placeholder = "Please select a Reference AI.",
                                onInitialize = I('function() { this.setValue(0); }')
                                )
                              )
               ),
        column(6, 
               selectizeInput(ns("AI2"),
                                 label = "Comparison Adaptive Intervention:",
                                 choices = secondAIlist(),
                                 options = list(
                                   placeholder = "Please select a Comparison AI.",
                                   onInitialize = I('function() { this.setValue(0); }')
                                   )
                              )
               )
        )
      return(list(out, tags$hr()))
    } else if (primaryAim() == "stage2") {
      if (design == 1) {
        radioButtons(ns("primaryAim"), )
      }
    }
  })
  
  secondAIlist <- reactive({
    AIs <- eval(parse(text = paste0("design", design, ".AIs")))
    return(AIs[AIs != input$AI1])
    
    
  })
}