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

## 
selectDTROutcomeUI <- function(id) {
  # Namespace function
  ns <- NS(id)
  tagList(
    radioButtons(ns("selectOutcome"), 
                 label = HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                 choices = list("Binary", "Continuous"), selected = "Binary")
    )
}

selectDTROutcome <- function(input, output, session) {
  return(reactive(input$selectOutcome))
}