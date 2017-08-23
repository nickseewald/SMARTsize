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

primaryAimUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("aim"))
}

primaryAim <- function(input, output, session, rerand) {
  choice <- reactive({
    choices <- list("Compare first-stage treatments as initial treatments in an adaptive intervention." = "stage1")
    if ("responders" %in% rerand())
      choices[["Compare second-stage treatments among responders as secondary treatments in an adaptive intervention."]] <- "stage2resp"
    if ("nonresponders" %in% rerand()) 
      choices[["Compare second-stage treatments among non-responders as secondary treatments in an adaptive intervention."]] <- "stage2nresp"
    if (is.null(rerand()))
      choices[["Compare second-stage treatments as secondary treatments in an adaptive intervention."]] <- "stage2"
    choices[["Compare two embedded adaptive interventions with different first-stage treatments."]] <- "dtrs"
    choices
  })
  
  output$aim <- renderUI({
    ns <- session$ns
    radioButtons(ns("primaryAim"), 
                 label = "What is your primary aim?",
                 choices = choice(),
                 width = "100%",
                 selected = "dtrs")
  })
  return(reactive({input$primaryAim}))
}