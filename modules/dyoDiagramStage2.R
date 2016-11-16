dyoDiagramStage2UI <- function(id) {
  ns <- NS(id)
}

dyoDiagramStage2 <- function(input, output, session, RNR, rerand, ntxt.stage1, ntxt.stage2) {
  linkLabel <- switch(tolower(RNR), "r" = "Responders", "nr" = "Non-Responders")
  reactive({
    
    if (rerand() == "Yes") {
      stage1ToRand <- rep(NA, ntxt.stage1())
      randToStage2 <- matrix(rep(NA, ntxt.stage1() * ntxt.stage2()),
                             nrow = ntxt.stage1())
      for (i in 1:length(stage1ToRand)) {
        # For every stage 1 input, draw an arrow to a randomization (circled R)
        stage1ToRand[i] <- paste0(LETTERS[i], "-->|", linkLabel, "|R", LETTERS[i], RNR, "((R))")
        for (j in 1:ntxt.stage2()) {
          # For each randomization drawn above, draw arrows to stage 2 treatment options
          randToStage2[i, j] <- paste0("R", LETTERS[i], RNR, "-->", LETTERS[i], RNR, j, "[", LETTERS[i], RNR, j, "]")
        }
      }
      
      paste(paste(stage1ToRand, collapse = " \n "), paste(randToStage2, collapse = " \n "), sep = " \n ")
      
    } else {
      paste(sapply(1:ntxt.stage1(),
                   function(i) {
                     paste0(LETTERS[i], "-->|", linkLabel, "|", LETTERS[i], "R", RNR)
                   }), collapse = "\n")
    }
  })
}