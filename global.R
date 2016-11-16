##### GLOBAL.R FOR SMART SAMPLE SIZE CALCULATOR #####
### NICK SEEWALD, 2014
### UNIVERSITY OF MICHIGAN
### DEPARTMENT OF BIOSTATISTICS

library(shiny)
# library(pwr)
library(shinyBS)
library(DiagrammeR)
library(V8)
library(htmlwidgets)
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

### Function evaluates full-DTR probabilities; not reactive
fullDTRprob <- function(cell1, resp, cell2){
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