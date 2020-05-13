#################################
##### STRINGS for SMARTsize #####
#################################

html.outcomeType <-
  HTML("Is the <strong> outcome </strong> of interest binary or continuous?")

html.selectStage1Compare <-
  HTML("Which two <strong>first-stage treatments</strong> would you like
       to compare?")

html.selectStage2RCompare <-
  HTML("Which two <strong>second-stage treatments</strong> for 
  <strong>responders</strong> would you like to compare?")

html.selectStage2NRCompare <-
  HTML("Which two <strong>second-stage treatments</strong> for 
  <strong>non-responders</strong> would you like to compare?")

html.stage1ProbsGuide <- 
  HTML("Please provide the probabilities of success for individuals who receive
       the given ")

text.selectDTRcompare <- 
  quote(p("Which two", strong("adaptive interventions"), "would you like to
          compare? Choose two from the menus below. The image below will
          change to highlight the AIs you select."))

text.refDTRLabel <- "Reference Adaptive Intervention:"
text.refDTRPlaceholder <- "Please select a Reference AI."

text.compDTRLabel <- "Comparison Adaptive Intervention:"
text.compDTRPlaceholder <- "Please select a Comparison AI."

html.refDTRSuccess <-
  HTML("Probability of Success for Reference AI &nbsp; <img src='images/blue_dash.gif'>")
html.compDTRSuccess <-
  HTML("Probability of Success for Comparison AI &nbsp; <img src='images/red_dash.gif'>")

text.tooltip <- "Input can range from 0-1 and must be in decimal form, up to two places."

text.responseProbLabel <-
  quote(HTML("<p>Please provide the
             <strong>probability of response</strong> to the first-stage
             intervention.</p>"))
text.noResponse <-
  "Please provide a response probability. If unknown, check the box for a conservative estimate."
text.invalidResponse <-
  "The provided response probability is not a valid probability. Please enter a value between 0 and 1."

text.successProbLabel <-
  p("Concerning the primary outcome, please provide the",
    strong("probability of success"), "for each of the AIs of interest.")
text.noSuccessProb <- "The success probability is missing for at least one AI. Please provide a numeric input."
text.sameSuccessProb <- "Please provide unique success probabilities for each AI. Sample size is indeterminate for equal AI probabilities."
text.invalidSuccessProb <- "The provided success probability for at least one AI is not a valid probability. Please enter values greater than 0 and less than 1."

text.invalidCellResult <- "The provided cell-specific probabilities yield identical overall AI-specific probabilities
of success. Sample size is indeterminate for equal AI probabilities. Please adjust your inputs."

text.invalidDiffResult <- "The target difference is too large to produce a valid probability between 0 and 1. Please adjust your inputs."
text.invalidDiff <- "The target difference must be greater than 0, since sample size is indeterminate for equal AI probabilities.
             Please adjust your inputs."

text.stdEffectLabel <-
  p("Concerning the primary outcome, please provide the", 
    strong("standardized effect size"), "between the AIs of interest.")

text.altInputHelp <- 
  helpText("If you prefer to provide different information,
           check the appropriate box below.")

text.noOddsRatio <- "Please enter an odds ratio."
text.invalidOR1 <- "Sample size is indeterminate for an odds ratio of 1. Please enter a different target odds ratio."
text.invalidOR0 <- "Sample size is indeterminate for an odds ratio of 0. Please enter a different target odds ratio."

text.cellSpecLabel <- "Cell-Specific Success Probabilities"
text.targDiffLabel <- "Target Difference in Success Probabilities"
text.targORLabel   <- "Target Odds Ratio"
text.diffDirection <- 
  "Is the probability of the Comparison AI smaller or larger than the probability of the Reference AI?"
text.targORInputLabel <- 
  "Target Odds Ratio of Success (with Reference AI in the denominator)"

text.pathSuccess <- "Probability of success for Path "

text.effectSize <- "Standardized Effect Size"
text.noEffectSize <- "The standardized effect size is missing. Please enter a value between 0 and 10."
text.invalidEffectSize <- "Sample size is indeterminate for an effect size of 0. Please enter a different target effect size."

text.sentenceOverallSuccess <- ", have overall success probabilities "

text.sentenceAIinterest <- "The two AIs of interest, "

text.sentenceDiff <- " and the difference in overall probabilities of success in the two AIs of interest, "
text.sentenceOR <- " and the odds ratio of success for the two AIs of interest, "
text.sentenceEffectSize <- " and the standardized effect size between the two AIs of interest, "

text.sampleSizeOrPower <- "Are you interested in finding sample size or power?"
text.oneOrTwoSidedTest <- "Do you want to perform a one- or two-sided test?"

text.noPower  <- "Please provide a target power above, in Part 2 of the study description section."
text.power0   <- "Sample size is indeterminate for 0% power. Please specify a power greater than zero."
text.power100 <- "Sample size is indeterminate for 100% power or greater. Please specify a power less than 1."

text.size0    <- "Power is indeterminate for a sample size of 0. Please provide a valid sample size."
html.alpha0   <- eval(HTML("Please enter a nonzero value for type-I error."))

text.lowSizeA <- paste("The provided sample size is not large enough to yield",
                       "a trial in which at least one person is consistent",
                       "with each AI. Sample size must be at least 8 to proceed.")

text.lowEffectSizeA <- paste("Given the provided effect size, fewer than 8 ",
                             "individuals are required to achieve the desired ",
                             "power. This is not enough individuals to run a ",
                             "SMART. You can test for a smaller effect size, ",
                             "or increase the desired power.")

text.mustRerandomize <- "Either responders or non-responders must be re-randomized. Change the study design so that at least one group is re-randomized."
text.rerandMismatchResp <- paste("You have indicated that some or all responders to first-stage treatments are re-randomized, but have not selected", 
                                 "any first-stage treatments for which the responders are re-randomized. Please correct this to continue.")
text.rerandMismatchNresp <- paste("You have indicated that some or all non-responders to first-stage treatments are re-randomized, but have not selected", 
                                  "any first-stage treatments for which the responders are re-randomized. Please correct this to continue.")

text.alert.premadeDisabled.title <- "This section has been disabled."