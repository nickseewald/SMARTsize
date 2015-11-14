##### GLOBAL.R FOR SMART SAMPLE SIZE CALCULATOR #####
### NICK SEEWALD, 2014
### UNIVERSITY OF MICHIGAN
### DEPARTMENT OF BIOSTATISTICS

##### NON-REACTIVE FUNCTION DECLARATIONS #####

### Function creates disabled (greyed-out) inputs
### Taken from https://groups.google.com/d/msg/shiny-discuss/uSetp4TtW-s/Jktu3fS60RAJ
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
  pDTR <- cell1 * resp + cell2 * (1-resp)
  return(pDTR)
}

### Create operator to sequentially evaluate need() statements
`%then%` <- shiny:::`%OR%`

### Create vectors of all embedded DTRs for each design
designA.DTRs <- list("{A,C,E}" = "ArCnrE", "{A,C,F}" = "ArCnrF", "{A,D,E}" = "ArDnrE", "{A,D,F}" = "ArDnrF",
                     "{B,G,I}" = "BrGnrI", "{B,G,J}" = "BrGnrJ", "{B,H,I}" = "BrHnrI", "{B,H,J}" = "BrHnrJ")
designB.DTRs <- list("{A,C,D}" = "ArCnrD", "{A,C,E}" = "ArCnrE", "{B,F,G}" = "BrFnrG", "{B,F,H}" = "BrFnrH")
designC.DTRs <- list("{A,C,D}" = "ArCnrD", "{A,C,E}" = "ArCnrE", "{B,F,G}" = "BrFnrG")


##### ERROR MESSAGES #####

