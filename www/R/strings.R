#################################
##### STRINGS for SMARTsize #####
#################################

##### Rendered Text #####
text.outcomeType <-
  HTML("Is the <strong> outcome </strong> of interest binary or continuous?")

text.selectDTRcompare <- 
  quote(p("Which two", strong("adaptive interventions"), "would you like to
          compare? Choose two from the menus below. The image below will
          change to highlight the AIs you select."))

text.refDTRLabel <- "Reference Adaptive Intervention:"
text.refDTRPlaceholder <- "Please select a Reference AI."

text.compDTRLabel <- "Comparison Adaptive Intervention:"
text.compDTRPlaceholder <- "Please select a Comparison AI."

text.responseProbLabel <-
  quote(HTML("<p>Concerning the tailoring variable, please provide the
             <strong>probability of response</strong> to the first-stage
             intervention. If you are unsure, leave as 0 for a conservative
             estimate.</p>"))

text.successProbLabel <-
  p("Concerning the primary outcome, please provide the",
    strong("probability of success"), "for each of the AIs of interest.")

text.stdEffectLabel <-
  p("Concerning the primary outcome, please provide the", 
    strong("standardized effect size"), "between the AIs of interest.")

text.altInputHelp <- 
  helpText("If you prefer to provide different information,
           check the appropriate box below.")

text.cellSpecLabel <- "Cell-Specific Success Probabilities"
text.targDiffLabel <- "Target Difference in Success Probabilities"
text.targORLabel   <- "Target Odds Ratio"

text.sampleSizeOrPower <- "Are you interested in finding sample size or power?"
text.oneOrTwoSidedTest <- "Do you want to perform a one- or two-sided test?"

##### Error Messages #####