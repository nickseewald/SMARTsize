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

dyoDiagramStage2UI <- function(id) {
  ns <- NS(id)
}

dyoDiagramStage2 <- function(input, output, session, RNR, rerand, ntxt.stage1,
                             ntxt.stage2, whichtxts, randprobs = reactive(NULL),
                             respprobs = reactive(NULL)) {
  RNR <- tolower(RNR)
  linkLabel <- switch(RNR, "r" = "Responders", "nr" = "Non-Responders")
  
  reactive({
    
    if (rerand() == "Yes") {
      stage1ToRand <- rep(NA, ntxt.stage1())
      randToStage2 <- matrix(rep(NA, length(whichtxts()) * ntxt.stage2()),
                             nrow = length(whichtxts()))
      for (i in 1:length(whichtxts())) {
        # For every stage 1 input, draw an arrow to a randomization (circled R)
        stage1ToRand[i] <- paste0(whichtxts()[i], "-->|", linkLabel, "|R", whichtxts()[i], RNR, "((R));\n ",
                                  "class R", whichtxts()[i], RNR, " randomize;")
        for (j in 1:ntxt.stage2()) {
          # For each randomization drawn above, draw arrows to stage 2 treatment options
          randToStage2[i, j] <- paste0("R", whichtxts()[i], RNR, "-->",
                                       whichtxts()[i], RNR, j, "[", whichtxts()[i],
                                       RNR, j, "]")
        }
      }
      
      if (length(whichtxts()) != ntxt.stage1()) {
        stage1.norand <- LETTERS[1:ntxt.stage1()][!(LETTERS[1:ntxt.stage1()] %in% whichtxts())]
        for (i in (length(whichtxts()) + 1):ntxt.stage1()) {
          stage1ToRand[i] <- paste0(stage1.norand[i - length(whichtxts())], "-->|", linkLabel,
                                    "|", stage1.norand[i - length(whichtxts())], RNR)
        }
      }
      
      paste(paste(stage1ToRand, collapse = " \n "), 
              paste(randToStage2, collapse = " \n "),
              sep = " \n ")
      
    } else {
      paste(sapply(1:ntxt.stage1(),
                   function(i) {
                     paste0(LETTERS[i], "-->|", linkLabel, "|", LETTERS[i], RNR)
                   }), collapse = "\n")
    }
  })
}