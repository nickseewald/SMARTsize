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

##### GLOBAL.R FOR SMART SAMPLE SIZE CALCULATOR #####
### NICK SEEWALD, 2014
### UNIVERSITY OF MICHIGAN
### DEPARTMENT OF BIOSTATISTICS

library(shiny)
# library(pwr)
library(shinyBS)
library(DiagrammeR)
# library(V8)
# library(htmlwidgets)
library(shinyjs)

### Enable bookmarkable state via URL (shinyapps.io doesn't yet
### support server-side saves)
enableBookmarking(store = "url")

### Function creates disabled (greyed-out) inputs
### https://groups.google.com/d/msg/shiny-discuss/uSetp4TtW-s/Jktu3fS60RAJ
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

### Custom "back" and "continue" buttons
backButton <- function(inputId) {
  bsButton(inputId,
           label = "Back",
           icon = icon("arrow-left", lib = "glyphicon"),
           style = "default", class = "btn-pull-left")
}
continueButton <- function(inputId) {
  bsButton(inputId, 
           label = "Continue",
           icon = icon("arrow-right", lib = "glyphicon"),
           style = "primary", class = "btn-pull-right")
}

### Compute A and B for sample size formula
ABcomp <- function(pi.stage1, resp, pi.stage2R, pi.stage2NR) {
  resp/pi.stage2R + (1 - resp) / pi.stage2NR
}

### Function evaluates full-DTR probabilities; not reactive
marginalizeDTRProbs <- function(cell1, resp, cell2){
  pDTR <- cell1 * resp + cell2 * (1 - resp)
  return(pDTR)
}

### Create operator to sequentially evaluate need() statements
`%then%` <- shiny:::`%OR%`

### Create vectors of all embedded DTRs for each design
designA.AIs <- list("{A, C, E}" = "ArCnrE", "{A, C, F}" = "ArCnrF", 
                     "{A, D, E}" = "ArDnrE", "{A, D, F}" = "ArDnrF",
                     "{B, G, I}" = "BrGnrI", "{B, G, J}" = "BrGnrJ",
                     "{B, H, I}" = "BrHnrI", "{B, H, J}" = "BrHnrJ")
designB.AIs <- list("{A, C, D}" = "ArCnrD", "{A, C, E}" = "ArCnrE",
                     "{B, F, G}" = "BrFnrG", "{B, F, H}" = "BrFnrH")
designC.AIs <- list("{A, C, D}" = "ArCnrD", "{A, C, E}" = "ArCnrE",
                     "{B, F, G}" = "BrFnrG")


### Source strings used throughout the app (labels, warnings, etc.)
source("www/R/strings.R")

### Source code for shiny modules
source("modules/selectDTROutcome.R")
source("modules/primaryAim.R")
source("modules/resultOptions.R")
source("modules/dyoDiagramStage2.R")