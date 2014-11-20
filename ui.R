##### UI.R FOR SMART SAMPLE SIZE CALCULATOR #####
### NICK SEEWALD, 2014
### UNIVERSITY OF MICHIGAN
### DEPARTMENT OF BIOSTATISTICS

library(shiny)
library(pwr)
library(shinyBS)
options(encoding='UTF-8')

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

shinyUI( 
  navbarPage("SMART Sample Size Calculator", id="SMARTsize",
             
             ##### HOME TAB#####
             
             tabPanel("Home",
                      
                     ### CSS HEADER ###
                      ### Apply style attributes across the application
                      tags$head(
                        tags$style(type='text/css',"input[type='number'] {width:60px}"), #set width of numericInputs
                        tags$style(type='text/css',includeHTML("www/css/bootstrap-modal.css")), #responsive modals  
                        tags$link(rel="stylesheet", href="//fonts.googleapis.com/css?family=Roboto|Roboto+Condensed"),
                        tags$style("body {font-family: 'Roboto', sans-serif;} 
                                    h1 {font-family: 'Roboto Condensed', sans-serif;} 
                                    h2 {font-family: 'Roboto Condensed', sans-serif;}
                                    h3 {font-family: 'Roboto Condensed', sans-serif;} 
                                    h4 {font-family: 'Roboto Condensed', sans-serif;} 
                                    h5 {font-family: 'Roboto Condensed', sans-serif;} 
                                    h6 {font-family: 'Roboto Condensed', sans-serif;}") #apply font styles
                        ),
                     
                      sidebarPanel(
                        h3("Purpose"),
                        p("The purpose of this applet is to provide the minimum required sample size or power of a SMART (Sequential Multiple 
                          Assignment Randomized Trial) with the goal of comparing two embedded adaptive interventions with continuous or binary outcomes."),
                        
                        h3("Notation and Assumptions"),
                        tags$ul(
                                tags$li(img(src="images/randomize.gif",width=25),"refers to randomization. We assume equal probability of randomization to two 
                                        intervention options, R=0.5."),
                                tags$li("Adaptive interventions (AIs) are denoted by the combination of the letters of the interventions where the intervention
                                        following 'r' indicates the the option for those who respond, and the intervention following 'nr' indicates the option for
                                        those who do not respond. For example, ArCnrE denotes the AI which reads as ''First receive A and if there is a response,
                                        receive C; if there is no response, receive E.''"),
                                tags$li("For more details on the assumptions behind the sample size calculations, please refer to Kidwell et al. (in preparation)
                                        and Oetting et al. (2007).")
                        ),
                        
                        h3("Methods"),
                        p("For binary outcomes, sample size is computed according to the methods presented in Kidwell et al. (in preparation). "),
                        p("For continuous outcomes, sample size is computed according to the methods in Oetting et al. (2007)."),
                        p("The application is run on", tags$a(href="http://www.r-project.org/", "R"), "version 3.1.0, and written using",
                          tags$a(href="http://shiny.rstudio.com/","Shiny,"), "an open-source web application framework for R produced by",
                          tags$a(href="http://www.rstudio.com", "RStudio."))
                        
                      ),
                      
                      mainPanel(                        
                        h1("Sample Size Calculator for SMARTs with Binary or Continuous Outcomes"),
                        br(),
                        p("Choose the SMART design of interest by clicking the corresponding tab at the top of the window, or the button below the corresponding diagram. 
                          Notation is established in the sidebar. See below for more background on SMARTs."),
                        br(),
                        fluidRow(
                          column(6,
                                 img(src="images/SMARTdesignA__.gif"),
                                 actionButton("pickTabA","Design A"),
                                 HTML("<p> 8 embedded adaptive interventions: ArCnrE, ArCnrF, ArDnrE, ArDnrF, BrGnrI, BrGnrJ, BrHnrI, BrHnrJ.
                                      <a data-toggle='modal' data-target='#exampleAmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleAmodal.html")
                          ),
                          column(6,
                                 img(src="images/SMARTdesignB__.gif"),
                                 actionButton("pickTabB","Design B"),
                                 HTML("<p> 4 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG, BrFnrH.
                                      <a data-toggle='modal' data-target='#exampleBmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleBmodal.html")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(6,
                                 img(src="images/SMARTdesignC__.gif"),
                                 actionButton("pickTabC","Design C"),
                                 HTML("<p> 3 embedded adaptive interventions: ArCnrD, ArCnrE, BrFnrG.
                                      <a data-toggle='modal' data-target='#exampleCmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleCmodal.html")
                          ),
                          column(6,
                                 img(src="images/SMARTdesignD__.gif"),
                                 actionButton("pickTabD","Design D"),
                                 HTML("<p> 4 embedded non-adaptive interventions: AC, AD, BE, BF.
                                      <a data-toggle='modal' data-target='#exampleDmodal' style='color:#6b6b6b'>
                                      Click here for an example. </a> </p>"),
                                 includeHTML("www/html/exampleDmodal.html")
                          )
                        ),
                        br(),
                        
                        h2("Background"),
                        
                        p("An",strong("adaptive intervention"), "(AI, also known as a dynamic treatment regime, treatment algorithm, adaptive treatment strategy) leads to a sequence of treatments
                          or interventions tailored to an individual. For example, an AI in the treatment of autism may be '''First treat the child using joint attention symbolic play engagement and 
                          regulation therapy along with enhanced milieu training for 12 weeks. If the child responds (25% or more improvement on 7 or more of 14 measures of communication) then continue
                          the same treatment for an additional 12 weeks. If the child does not respond at the end of 12 weeks, then continue the same treatment with the addition of a speech generating device.''"),
                        
                        p("AIs contain three pieces: critical decisions which include treatment options, tailoring variables to decide how to adapt the treatments, and decision rules which input
                          previous treatments and tailoring variables and output recommended subsequent treatment. The example AI in the treatment of autism includes decisions as to the best initial
                          and second-line therapy for children, with treatment options including types of therapy and a speech generating device, the tailoring variable includes response defined as 
                          meeting a threshold on various communication tests, and, depending on response, subsequent treatment differs."),
                        
                        p("A",strong("tailoring variable"), "is a personal characteristic (fixed or time-varying) that can be used to personalize treatment to an individual. For more information about tailoring
                          variables, see Nahum-Shani et al. (2012). In the SMART designs above, we assume the tailoring variable is response, but your definition may vary (for example, adherent/non-adherent,
                          engage/non-engage). Also note that in Designs B and C, non-responders are re-randomized, while responders are not. It is possible to switch this notation so that only responders
                          would be randomized and non-responders would not. If this is the case, please provide the ''non-response probability'' when prompted for ''response probability''."),
                        
                        p("A ",strong("Sequential Multiple Assignment Randomized Trial (SMART)"), "is used to develop effective AIs. A SMART is an experimental design in which some or all individuals are randomized
                          multiple times to follow specific intervention sequences. We depict the four most common SMART designs above. In the depictions above, each treatment at each stage is assigned a unique letter.
                          It is not, however, necessary for all treatments to be unique. For example, in Design A, treatments C and G may be the same and/or D and H may be the same."),
                        
                        p("The primary aim of a SMART may be the comparison of main effects (first or second-stage treatments), comparison of AIs, or further tailoring of AIs. This applet is designed with
                          the primary aim of comparing AIs. Specifically, this applet calculates the minimum required sample size or power for a SMART where the primary interest is in comparing two AIs 
                          where the primary outcome of the trial is continuous or binary. If instead interest is in computing the sample size for a SMART with a survival outcome, please see",
                          a(href="http://methodologymedia.psu.edu/logranktest/samplesize", "http://methodologymedia.psu.edu/logranktest/samplesize"),"or",
                          a(href="http://www.pitt.edu/~wahed/Research/Resources/", "http://www.pitt.edu/~wahed/Research/Resources/"),". For continuous outcomes where the goal is to identify the best AI,
                          please see", a(href="http://methodologymedia.psu.edu/smart/samplesize", "http://methodologymedia.psu.edu/smart/samplesize"), ". For pilot studies, please see",
                          a(href="http://methodology.psu.edu/downloads/rcodesmart", "http://methodology.psu.edu/downloads/rcodesmart"),"."),
                        
                        p("For more information on SMARTs, please refer to additional selected reading references."),
                        
                        ##### REFERENCES #####
                        
                        tags$hr(),
                        h4("References"),
                        tags$ol(
                          tags$li(HTML("Oetting, A., Levy, J., Weiss, R. and Murphy, S. (2007)<I>, &quot;Statistical methodology for a SMART design in the development of adaptive treatment strategies
                                       ,&quot;</I><I> in <I>Causality and Psychopathology: Finding the Determinants of Disorders and their Cures (American Psychopathological Association)</I></I>, 
                                       Arlington, VA: American Psychiatric Publishing, Inc., pp. 179-205.")),
                          tags$li("Nahum-Shani, I., Qian, M., Almirall, D., Pelham, W. E., Gnagy, B., Fabiano, G. A., Waxmonsky, J. G., Yu, J. and Murphy, S. A. (2012), 
                                  ''Experimental design and primary data analysis methods for comparing adaptive interventions.''",em("Psychological methods,"), "17, 457.")
                        ),
                        
                        bsCollapse(multiple = FALSE, id = "moreRefs",
                                   bsCollapsePanel("Additional Selected Readings",
                                                   HTML("<ol>
                                                        <li> Almirall, D., Compton, S. N., Gunlicks-Stoessel, M., Duan, N. and Murphy, S. A. (2012), &quot;Designing a pilot sequential multiple
                                                        assignment randomized trial for developing an adaptive treatment strategy,&quot;<I> Statistics in medicine </I>, 31, 1887-1902.
                                                        <li> Auyeung, S. F., Long, Q., Royster, E. B., Murthy, S., McNutt, M. D., Lawson, D., Miller, A., Manatunga, A. and Musselman, D. L. (2009),
                                                        &quot;Sequential multiple-assignment randomized trial design of neurobehavioral treatment for patients with metastatic malignant melanoma 
                                                        undergoing high-dose interferon-alpha therapy,&quot;<I> Clinical trials (London, England) </I>, 6, 480-490. 
                                                        <li> Bembom, O. and van der Laan, Mark J. (2008), &quot;Analyzing sequentially randomized trials based on causal effect models for realistic
                                                        individualized treatment rules,&quot;<I> Statistics in medicine </I>, 27, 3689-3716.
                                                        <li> Bembom, O. and van der Laan, M. J. (2007), &quot;Statistical methods for analyzing sequentially randomized trials,&quot;<I> Journal of
                                                        the National Cancer Institute </I>, 99, 1577-1582. 
                                                        <li> Buyze, J. and Goetghebeur, E. (2013), &quot;Evaluating dynamic treatment strategies: does it have to be more costly?&quot;<I> 
                                                        Pharmaceutical statistics </I>, 12, 35-42.
                                                        <li> Buyze, J., Van Rompaye, B. and Goetghebeur, E. (2010), &quot;Designing a sequentially randomized study with 
                                                        adherence enhancing interventions for diabetes patients,&quot;<I> Statistics in medicine </I>, 29, 1114-1126.
                                                        <li> Chakraborty, B. and Murphy, S. A. (2014), &quot;Dynamic treatment regimes,&quot;<I> Annual Review of Statistics and its Application
                                                             </I>, 1, 447-464.
                                                        <li> Chakraborty, B. (2011), &quot;Dynamic Treatment Regimes for Managing Chronic Health Conditions: A Statistical Perspective,&quot;
                                                             <I> Am J Public Health </I>, 101, 40-45.
                                                        <li> Cohen, J. (2013), <I> Statistical Power Analysis for the Behavioral Sciences,</I> 2nd ed., Hillsdale, NJ: Lawrence Erlbaum Associates, Inc.
                                                        <li> Dawson, R. and Lavori, P. W. (2004), &quot;Placebo-free designs for evaluating new mental health treatments: the use of adaptive treatment
                                                             strategies,&quot;<I> Statistics in medicine </I>, 23, 3249-3262.
                                                        <li> Dawson, R. and Lavori, P. W. (2010), &quot;Sample size calculations for evaluating treatment policies in multi-stage designs,&quot;<I>
                                                        Clinical trials (London, England) </I>, 7, 643-652.
                                                        <li> Feng, W. and Wahed, A. S. (2009), &quot;Sample size for two-stage studies with maintenance therapy,&quot;<I> Statistics in medicine </I>,
                                                        28, 2028-2041.
                                                        <li> Feng, W. and Wahed, A. S. (2008), &quot;Supremum weighted log-rank test and sample size for comparing two-stage adaptive treatment strategies,
                                                        &quot;<I> Biometrika </I>, 95, 695-707.
                                                        <li> Goldberg, Y. and Kosorok, M. R. (2012), &quot;Q-Learning with Censored Data,&quot;<I> Annals of statistics </I>, 40, 529-560.
                                                        <li> Guo, X. and Tsiatis, A. (2005), &quot;A weighted risk set estimator for survival distributions in two-stage randomization designs with censored
                                                        survival data,&quot;<I> The International Journal of Biostatistics </I>, 1, 1-15.
                                                        <li> Hern&#225;n, M. A., Lanoy, E., Costagliola, D. and Robins, J. M. (2006), &quot;Comparison of dynamic treatment regimes via inverse probability 
                                                        weighting,&quot;<I> Basic &amp; clinical pharmacology &amp; toxicology </I>, 98, 237-242.
                                                        <li> Kidwell, K. M. and Wahed, A. S. (2013), &quot;Weighted log-rank statistic to compare shared-path adaptive treatment strategies,&quot;
                                                        <I> Biostatistics (Oxford, England) </I>, 14, 299-312.
                                                        <li> Ko, J. H. and Wahed, A. S. (2012), &quot;Up-front versus sequential randomizations for inference on adaptive treatment strategies,&quot;
                                                        <I> Statistics in medicine </I>, 31, 812-830.
                                                        <li> Lavori, P. W., Dawson, R. and Rush, A. J. (2000), &quot;Flexible treatment strategies in chronic disease: clinical and research implications,
                                                        &quot;<I> Biological psychiatry </I>, 48, 605-614.
                                                        <li> Lavori, P. W. and Dawson, R. (2008), &quot;Adaptive treatment strategies in chronic disease,&quot;<I> Annual Review of Medicine </I>, 59, 443-453.
                                                        <li> Lavori, P. W. and Dawson, R. (2004), &quot;Dynamic treatment regimes: practical design considerations,&quot;<I> Clinical trials 
                                                        (London, England) </I>, 1, 9-20.
                                                        <li> Lei, H., Nahum-Shani, I., Lynch, K., Oslin, D. and Murphy, S. A. (2012), &quot;A &quot;SMART&quot; design for building individualized treatment 
                                                        sequences,&quot;<I> Annual review of clinical psychology </I>, 8, 21-48.
                                                        <li> Li, Z. and Murphy, S. A. (2011), &quot;Sample size formulae for two-stage randomized trials with survival outcomes,&quot;<I> Biometrika </I>, 98, 503-518.
                                                        <li> Li, Z., Valenstein, M., Pfeiffer, P. and Ganoczy, D. (2014), &quot;A global logrank test for adaptive treatment strategies based on observational studies,
                                                        &quot;<I> Statistics in medicine </I>, 33, 760-771.
                                                        <li> Lokhnygina, Y. and Helterbrand, J. D. (2007), &quot;Cox Regression Methods for Two-Stage Randomization Designs,&quot;<I> Biometrics </I>, 63, 422-428.
                                                        <li> Lunceford, J. K., Davidian, M. and Tsiatis, A. A. (2002), &quot;Estimation of Survival Distributions of Treatment Policies in Two-Stage Randomization Designs
                                                        in Clinical Trials,&quot;<I> Biometrics </I>, 58, 48-57.
                                                        <li> Miyahara, S. and Wahed, A. S. (2010), &quot;Weighted Kaplan-Meier estimators for two-stage treatment regimes,&quot; <I> Statistics in medicine </I>, 29, 2581-2591.
                                                        <li> Moodie, E. E., Richardson, T. S. and Stephens, D. A. (2007), &quot;Demystifying optimal dynamic treatment regimes,&quot;<I> Biometrics </I>, 63, 447-455.
                                                        <li> Murphy, S. A., Lynch, K. G., Oslin, D., McKay, J. R. and TenHave, T. (2007), &quot;Developing adaptive treatment strategies in substance abuse research,&quot;
                                                        <I> Drug and alcohol dependence </I>, 88, S24-S30.
                                                        <li> Murphy, S. A., Oslin, D. W., Rush, A. J. and Zhu, J. (2007), &quot;Methodological challenges in constructing effective treatment sequences for chronic 
                                                        psychiatric disorders,&quot;<I> Neuropsychopharmacology </I>, 32, 257-262.
                                                        <li> Nahum-Shani, I., Qian, M., Almirall, D., Pelham, W. E., Gnagy, B., Fabiano, G. A., Waxmonsky, J. G., Yu, J. and Murphy, S. A. (2012), &quot;Experimental design
                                                        and primary data analysis methods for comparing adaptive interventions.&quot;<I> Psychological methods </I>, 17, 457.
                                                        <li> Nahum-Shani, I., Qian, M., Almirall, D., Pelham, W. E., Gnagy, B., Fabiano, G. A., Waxmonsky, J. G., Yu, J. and Murphy, S. A. (2012), &quot;Q-learning: A data
                                                        analysis method for constructing adaptive interventions.&quot;<I> Psychological methods </I>, 17, 478.
                                                        <li> Oetting, A., Levy, J., Weiss, R. and Murphy, S. (2007)<I>, &quot;Statistical methodology for a SMART design in the development of adaptive treatment strategies,
                                                        &quot;</I><I> in <I>Causality and Psychopathology: Finding the Determinants of Disorders and their Cures (American Psychopathological Association)</I></I>, 
                                                        Arlington, VA: American Psychiatric Publishing, Inc., pp. 179-205.
                                                        <li> Orellana, L., Rotnitzky, A. and Robins, J. M. (2010), &quot;Dynamic regime marginal structural mean models for estimation of optimal dynamic treatment regimes, 
                                                        part I: main content,&quot;<I> The International Journal of Biostatistics </I>, 6.
                                                        <li> Robins, J. (1986), &quot;A new approach to causal inference in mortality studies with a sustained exposure period-application to control of the healthy worker 
                                                        survivor effect,&quot;<I> Mathematical Modelling </I>, 7, 1393-1512.
                                                        <li> Robins, J. M. (1997)<I>, &quot;Causal inference from complex longitudinal data,&quot;</I><I> in Latent variable modeling and applications to causality</I>, 
                                                        New York: Springer, pp. 69-117.
                                                        <li> Tang, X. and Wahed, A. S. (2011), &quot;Comparison of treatment regimes with adjustment for auxiliary variables,&quot;<I> Journal of Applied Statistics </I>,
                                                        38, 2925-2938.
                                                        <li> Thall, P. F. (2008), &quot;A review of phase 2-3 clinical trial designs,&quot;<I> Lifetime Data Analysis </I>, 14, 37-53.
                                                        <li> Thall, P. F., Millikan, R. E. and Sung, H. (2000), &quot;Evaluating multiple treatment courses in clinical trials,&quot;<I> Statistics in medicine </I>, 19, 1011-1028.
                                                        <li> Thall, P. F., Wooten, L. H., Logothetis, C. J., Millikan, R. E. and Tannir, N. M. (2007), &quot;Bayesian and frequentist two-stage treatment strategies based on
                                                        sequential failure times subject to interval censoring,&quot;<I> Statistics in medicine </I>, 26, 4687-4702.
                                                        <li> Thall, P. F., Logothetis, C., Pagliaro, L. C., Wen, S., Brown, M. A., Williams, D. and Millikan, R. E. (2007), &quot;Adaptive therapy for androgen-independent prostate
                                                        cancer: a randomized selection trial of four regimens,&quot;<I> Journal of the National Cancer Institute </I>, 99, 1613-1622.
                                                        <li> Wahed, A. S. and Thall, P. F. (2013), &quot;Evaluating joint effects of induction-salvage treatment regimes on overall survival in acute leukaemia,&quot;<I> Journal of 
                                                        the Royal Statistical Society: Series C (Applied Statistics) </I>, 62, 67-83.
                                                        <li> Wahed, A. S. and Tsiatis, A. A. (2006), &quot;Semiparametric efficient estimation of survival distributions in two-stage randomisation designs in clinical trials with 
                                                        censored data,&quot;<I> Biometrika </I>, 93, 163-177.
                                                        <li> Wahed, A. S. and Tsiatis, A. A. (2004), &quot;Optimal Estimator for the Survival Distribution and Related Quantities for Treatment Policies in Two-Stage Randomization 
                                                        Designs in Clinical Trials,&quot;<I> Biometrics </I>, 60, 124-133.
                                                        <li> Wang, L., Rotnitzky, A., Lin, X., Millikan, R. E. and Thall, P. F. (2012), &quot;Evaluation of viable dynamic treatment regimes in a sequentially randomized trial of
                                                        advanced prostate cancer,&quot;<I> Journal of the American Statistical Association </I>, 107, 493-508.
                                                        <li> Wolbers, M. and Helterbrand, J. D. (2008), &quot;Two-stage randomization designs in drug development,&quot;<I> Statistics in medicine </I>, 27, 4161-4174.
                                                        <li> Zhao, Y., Zeng, D., Rush, A. J. and Kosorok, M. R. (2012), &quot;Estimating individualized treatment rules using outcome weighted learning,&quot;<I> Journal
                                                        of the American Statistical Association </I>, 107, 1106-1118.
                                                        <li> Zhao, Y., Zeng, D., Socinski, M. A. and Kosorok, M. R. (2011), &quot;Reinforcement learning strategies for clinical trials in nonsmall cell lung cancer,
                                                        &quot;<I> Biometrics </I>, 67, 1422-1433.
                                                        </ol>"),
                                                   id="col1", value="test1")
                                   )
                        )
             ), 
             
             ##### DESIGN A #####
             
             ##### A SIDEBAR #####
             
             tabPanel("Design A",
                      sidebarPanel(h4("About this design:"),
                                   p("In this design, all participants are randomized twice. Initially, all participants are randomized to A vs. B. Participants identified as responders to A (B) are
                                     re-randomized to C vs. D (G vs. H). Participants identified as non-responders to A (B) are re-randomized to E vs. F (I vs. J). There are 8 embedded AIs in this
                                     SMART. They are ArCnrE, ArCnrF, ArDnrE, ArDnrF, BrGnrI, BrGnrJ, BrHnrI, and BrHnrJ."),
                                   tags$hr(),
                                   h4("Inputs Required:"),
                                   tags$ul(
                                     tags$li("Indication of continuous or binary outcome."),
                                     tags$li("The probability of response to the first-stage treatment. We assume this probability is the same for both initial treatments. If you cannot provide an
                                             estimate of this parameter, please indicate 0 to provide conservative output."),
                                     tags$li("Indication of a one- or two-sided test."),
                                     tags$li("Specification of a type-I error rate (level of significance). The default value is 0.05, but may range between 0 and 1."),
                                     tags$li("If interest is in sample size, specify the power of the study. The default power is 0.80, but may range between 0 and 1.
                                             If interest is in power, specify the sample size of the study. This must be input as an integer greater than zero."),
                                     tags$li("For binary outcomes:",
                                             tags$ul(
                                               tags$li("The default inputs are the probabilities of success for each of the selected AIs. These are the overall probabilities of success for those
                                                       participants following each of the selected embedded AIs. For example, the probability of success for ArCnrE."),
                                               tags$li("Alternatively, cell-specific probabilities may be specified. These refer to the probabilities of success for those consistent with a particular
                                                       intervention pathway. For example, the probability of success for ArC."),
                                               tags$li("Alternatively, if cell-specific or AI-specific probabilities cannot be specified, a target difference in probabilities (must be between 0 and 1)
                                                       or a target odds-ratio (must be positive and not equal to 1), may be selected and provided. With both of these selections, we assume one
                                                       of the AIs has probability of success equal to 0.5 to yield conservative results.")
                                     )),
                                     tags$li("For continuous outcomes:",
                                             tags$ul(
                                               tags$li("Specify the standardized effect size between the selected AIs. We use Cohen's definition for standardized effect size such that for the two selected
                                                       AIs it is the difference between the means of the two AIs divided by the square root of the pooled variance (square root of the average of the variances
                                                       of the two groups). This may range between 0 and 10.")
                                     ))
                                   ),
                                   h5("Input Formatting Rules:"),
                                   p("All inputs must be given in decimal form with leading zero (no fractions, please) and may include up to two decimal places. For example,
                                                   ''0.07'' is valid input; both ''.07'' and ''7/100'' are invalid. Improperly-formatted input may result in unpredictable behavior or an error message."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("We wish to find the sample size for a SMART with a binary outcome where the probability of response to first stage interventions is 0.50. We estimate the overall 
                                     probabilities of success in the two AIs of interest, ArCnrE and BrGnrI,  are 0.65 and 0.80, respectively. Given a two-sided 5% type-I error, we require a sample size
                                     of 552 to make this comparison with 80% power.")
                      ),
                      
                      mainPanel(
                        
                        ##### A PAGE HEADER #####
                        
                        h1("Design A"),
                        tags$hr(),
                        
                        ##### A OUTCOME SELECTION #####
                        radioButtons("selectOutcomeA", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                     choices=list("Binary"=1,"Continuous"=2),selected=1),
                        
                        tags$hr(),
                        
                        ##### A DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        # Currently the menus are not reactively-repopulating (making it possible to select the same DTR twice). Possible future improvement.
                                                
                        p("Which two", strong("adaptive interventions"), "would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the AIs you select."),
                        
                        fluidRow(
                          column(6,
                                 uiOutput("selectAI1A")
                          ),
                          column(6,
                                 uiOutput("selectAI2A")
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### A IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designAimg",height="100%")),
                          column(5, 
                                 numericInput("respA",
                                              label=HTML("Concerning the tailoring variable, please provide the <strong> probability of response </strong> to the 
                                                         first-stage intervention. If you are unsure, leave as 0 for a conservative estimate."), 
                                              value=0,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectOutcomeA==1",
                                                  p("Concerning the primary outcome, please provide the", strong("probability of success"), "for each of the
                                                    AIs of interest."),
                                                  uiOutput("binaryDTR1probA"),
                                                  conditionalPanel(condition="input.cellOrConditionalA",
                                                                   fluidRow(column(11,offset=1,
                                                                                   uiOutput("cellProbsDTR1A"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probA"),
                                                  
                                                  conditionalPanel(condition="input.cellOrConditionalA",
                                                                   fluidRow(column(11,offset=1,
                                                                                   uiOutput("cellProbsDTR2A"))
                                                                   )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeA==2",
                                                  p("Concerning the primary outcome, please provide the", strong("standardized effect size"), "between the
                                                    AIs of interest."),
                                                  uiOutput("continuousProbA")
                                 ),
                                 
                                 ##### A INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareA != 0 && input.secondDTRcompareA != 0",
                                                  br(),
                                                  conditionalPanel(condition="input.selectOutcomeA==1",
                                                                   helpText("If you prefer to provide different information, check the appropriate box below."),
                                                                   checkboxInput("cellOrConditionalA",label="Cell-Specific Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetDiffCheckA",label="Target Difference in Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetOddsCheckA",label="Target Odds Ratio",value=FALSE)
                                                  )
                                 )
                          )
                        ),
                        
                        tags$hr(),
                        
                        
                        ##### A RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,
                                 radioButtons("selectResultsA",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample"),
                                 radioButtons("selectAlternativeA",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaA",label=HTML("Type I Error (&alpha;):"),value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsA=='sample'",
                                                  numericInput("inputPowerA",label=HTML("Power of Trial (1-&beta;):"),value=0.8, min=0, max=1,step=0.01)
                                 ),
                                 conditionalPanel(condition="input.selectResultsA=='power'",
                                                  numericInput("inputSampleSizeA",label="Total Sample Size of Trial:",value=0, min=0)
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### A RESULTS #####
                        # Choose which result to display based on binary/continuous outcome and selected result option
                        
                        h3("Results"),
                        conditionalPanel(condition="input.selectOutcomeA==1 & input.selectResultsA=='sample'",
                                         htmlOutput("binarySampleSizeA")
                        ),
                        conditionalPanel(condition="input.selectOutcomeA==1 & input.selectResultsA=='power'",
                                         htmlOutput("binaryPowerA")
                        ),
                        conditionalPanel(condition="input.selectOutcomeA==2 & input.selectResultsA=='sample'",
                                         htmlOutput("continuousSampleSizeA")
                        ),
                        conditionalPanel(condition="input.selectOutcomeA==2 & input.selectResultsA=='power'",
                                         htmlOutput("continuousPowerA")
                        )
                      ),
                      
                      ##### A TOOLTIPS #####
                      ### Add bootstrap-style tooltips to inputs coaching proper formatting
                      ### Below are tooltips for inputs rendered statically in ui.R
                      ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                      
                      bsTooltip(id="respA",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),      
                      bsTooltip(id="alphaA",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputPowerA",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputSampleSizeA",title="Input must be an integer greater than zero.",placement="right",trigger="focus")                    
                      
             ),
             
             
             ########### DESIGN B ##########
             
             tabPanel("Design B",
                      
                      ##### B SIDEBAR #####
                      
                      sidebarPanel(h4("About this design:"),
                                   p("In this design, only participants who do not respond to first-stage treatment are randomized twice. Initially all participants are randomized to A vs. B. Participants
                                     identified as responders to A (B) are not re-randomized, and assigned to treatment C (F). Participants identified as non-responders to A (B) are re-randomized to D vs.
                                     E (G vs. H). There are 4 embedded AIs in this SMART. They are ArCnrD, ArCnrE, BrFnrG, and BrFnrH."),
                                   tags$hr(),
                                   h4("Inputs Required:"),
                                   tags$ul(
                                     tags$li("Indication of continuous or binary outcome."),
                                     tags$li("The probability of response to the first-stage treatment. We assume this probability is the same for both initial treatments. If you cannot provide an
                                             estimate of this parameter, please indicate 0 to provide conservative output."),
                                     tags$li("Indication of a one- or two-sided test."),
                                     tags$li("Specification of a type-I error rate (level of significance). The default value is 0.05, but may range between 0 and 1."),
                                     tags$li("If interest is in sample size, specify the power of the study. The default power is 0.80, but may range between 0 and 1.
                                             If interest is in power, specify the sample size of the study. This must be input as an integer greater than zero."),
                                     tags$li("For binary outcomes:",
                                             tags$ul(
                                               tags$li("The default inputs are the probabilities of success for each of the selected AIs. These are the overall probabilities of success for those
                                                       participants following each of the selected embedded AIs. For example, the probability of success for ArCnrE."),
                                               tags$li("Alternatively, cell-specific probabilities may be specified. These refer to the probabilities of success for those consistent with a particular
                                                       intervention pathway. For example, the probability of success for ArC."),
                                               tags$li("Alternatively, if cell-specific or AI-specific probabilities cannot be specified, a target difference in probabilities (must be between 0 and 1)
                                                       or a target odds-ratio (must be positive and not equal to 1), may be selected and provided. With both of these selections, we assume one
                                                       of the AIs has probability of success equal to 0.5 to yield conservative results.")
                                             )),
                                     tags$li("For continuous outcomes:",
                                             tags$ul(
                                               tags$li("Specify the standardized effect size between the selected AIs. We use Cohen's definition for standardized effect size
                                                       such that for the two selected AIs it is the difference between the means of the two AIs divided by the square root of the pooled variance
                                                       (square root of the average of the variances of the two groups). This may range between 0 and 10.")
                                             ))
                                      ),
                                   h5("Input Formatting Rules:"),
                                   p("All inputs must be given in decimal form with leading zero (no fractions, please), and can be precise to two decimal places.
                                                   For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("We wish to find the sample size for a SMART with a binary outcome where the probability of response to first stage interventions is 0.60. We estimate the overall probabilities
                                     of success in the two AIs of interest, ArCnrD and BrFnrG, to be 0.70 and 0.57, respectively. Given a two-sided 5% type-I error, we require a sample size of 600 to make this
                                     comparison with 80% power.")
                      ),
                      
                      mainPanel( div( tabPanel(
                        
                        ##### B PAGE HEADER #####
                        
                        h1("Design B"),
                        tags$hr(),
                        
                        ##### B OUTCOME SELECTION #####
                        
                        radioButtons("selectOutcomeB", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                     choices=list("Binary"=1,"Continuous"=2),selected=1),
                        
                        tags$hr(),                        
                        
                        ##### B DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison, rendered in server.R
                        
                        p("Which two", strong("adaptive interventions"), "would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the AIs you select."),
                        
                        fluidRow(
                          column(6,
                                 uiOutput("selectAI1B")
                          ),
                          column(6,
                                 uiOutput("selectAI2B")  
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### B IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designBimg",height="100%")),
                          column(5,
                                 numericInput("respB",
                                              label=HTML("Concerning the tailoring variable, please provide the <strong> probability of response
                                                         </strong> to the first-stage intervention. If you are unsure, leave as 0 for a 
                                                         conservative estimate."),
                                              value=0,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectOutcomeB==1",
                                                  p("Concerning the primary outcome, please provide the", strong("probability of success"), 
                                                    "for each of the AIs of interest."),
                                                  uiOutput("binaryDTR1probB"),
                                                  conditionalPanel(condition="input.cellOrConditionalB",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("cellProbsDTR1B"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probB"),
                                                  conditionalPanel(condition="input.cellOrConditionalB",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("cellProbsDTR2B"))
                                                                   )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeB==2",
                                                  p("Concerning the primary outcome, please provide the", strong("standardized effect size"), "between the
                                                    AIs of interest."),
                                                  uiOutput("continuousProbB")
                                 ),
                                 
                                 ##### B INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareB != 0 && input.secondDTRcompareB != 0",
                                                  br(),
                                                  conditionalPanel(condition="input.selectOutcomeB==1",
                                                                   helpText("If you prefer to provide different information, check the appropriate box below."),
                                                                   checkboxInput("cellOrConditionalB",label="Cell-Specific Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetDiffCheckB",label="Target Difference in Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetOddsCheckB",label="Target Odds Ratio",value=FALSE)
                                                  )
                                 )
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### B RESULT OPTIONS #####
                        ### Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,radioButtons("selectResultsB",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample"),
                                 radioButtons("selectAlternativeB",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaB",label=HTML("Type I Error (&alpha;):"),value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsB=='sample'",
                                                  numericInput("inputPowerB",label=HTML("Power of Trial (1-&beta;):"),value=0.8, min=0, max=1,step=0.01)
                                 ),
                                 conditionalPanel(condition="input.selectResultsB=='power'",
                                                  numericInput("inputSampleSizeB",label="Total Sample Size of Trial:",value=0, min=0)
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### B RESULTS #####
                        ### Choose which result to display based on binary/continuous outcome and selected result option
                
                          h3("Results"),
                          conditionalPanel(condition="input.selectOutcomeB==1 & input.selectResultsB=='sample'",
                                           htmlOutput("binarySampleSizeB")
                          ),
                          conditionalPanel(condition="input.selectOutcomeB==1 & input.selectResultsB=='power'",
                                          htmlOutput("binaryPowerB")
                          ),
                          conditionalPanel(condition="input.selectOutcomeB==2 & input.selectResultsB=='sample'",
                                           htmlOutput("continuousSampleSizeB")
                          ),
                          conditionalPanel(condition="input.selectOutcomeB==2 & input.selectResultsB=='power'",
                                           htmlOutput("continuousPowerB")
                          )
                    ),style='width:800px;'),
                    tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 12600px; /* or 950px */}"))),
                    
                    ##### B TOOLTIPS #####
                    ### Add bootstrap-style tooltips to inputs coaching proper formatting
                    ### Below are tooltips for inputs rendered statically in ui.R
                    ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                    
                    bsTooltip(id="respB",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),      
                    bsTooltip(id="alphaB",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                    bsTooltip(id="inputPowerB",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                    bsTooltip(id="inputSampleSizeB",title="Input must be an integer greater than zero.",placement="right",trigger="focus")                   
             ),
             
             ########### DESIGN C ##########
  
             tabPanel("Design C",
                      
                      ##### C SIDEBAR #####
                      
                      sidebarPanel(h4("About this design:"),
                                   p("In this design, only participants who do not respond to a particular first-stage treatment are randomized twice. Initially all participants are randomized to A vs. B.
                                     Participants identified as responders to A (B) are not re-randomized and are assigned to treatment C (F). Participants identified as non-responders to A are re-randomized 
                                     to D vs. E; those identified as non-responders to B are not re-randomized and are assigned to treatment G. There are 3 embedded AIs in this SMART. They are ArCnrD, 
                                     ArCnrE, and BrFnrG."),
                                   tags$hr(),
                                   h4("Inputs Required:"),
                                   tags$ul(
                                     tags$li("Indication of continuous or binary outcome."),
                                     tags$li("The probability of response to the first-stage treatment. We assume this probability is the same for both initial treatments. If you cannot provide an
                                             estimate of this parameter, please indicate 0 to provide conservative output."),
                                     tags$li("Indication of a one- or two-sided test."),
                                     tags$li("Specification of a type-I error rate (level of significance). The default value is 0.05, but may range between 0 and 1."),
                                     tags$li("If interest is in sample size, specify the power of the study. The default power is 0.80, but may range between 0 and 1.
                                             If interest is in power, specify the sample size of the study. This must be input as an integer greater than zero."),
                                     tags$li("For binary outcomes:",
                                             tags$ul(
                                               tags$li("The default inputs are the probabilities of success for each of the selected AIs. These are the overall probabilities of success for those
                                                       participants following each of the selected embedded AIs. For example, the probability of success for ArCnrE."),
                                               tags$li("Alternatively, cell-specific probabilities may be specified. These refer to the probabilities of success for those consistent with a particular
                                                       intervention pathway. For example, the probability of success for ArC."),
                                               tags$li("Alternatively, if cell-specific or AI-specific probabilities cannot be specified, a target difference in probabilities (must be between 0 and 1)
                                                       or a target odds-ratio (must be positive and not equal to 1), may be selected and provided. With both of these selections, we assume one
                                                       of the AIs has probability of success equal to 0.5 to yield conservative results.")
                                             )),
                                     tags$li("For continuous outcomes:",
                                             tags$ul(
                                               tags$li("Specify the standardized effect size between the selected AIs. We use Cohen's definition for standardized effect size such that for the two selected AIs
                                                       it is the difference between the means of the two AIs divided by the square root of the pooled variance (square root of the average of the variances of
                                                       the two groups). This may range between 0 and 10.")
                                             ))
                                     ),
                                   h5("Input Formatting Rules:"),
                                   p("All inputs must be given in decimal form with leading zero (no fractions, please), and can be precise to two decimal places.
                                                   For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("We wish to find the sample size for a SMART with a binary outcome where the probability of response to first stage interventions is 0.40. We estimate the overall probabilities
                                     of success in the two AIs of interest, ArCnrE and BrFnrG, to be 0.75 and 0.60, respectively. Given a two-sided 5% type-I error, we require a sample size of 395 to make this
                                     comparison with 80% power.")
                      ),
                      mainPanel(
                        
                        ##### C PAGE HEADER #####
                        
                        h1("Design C"),
                        tags$hr(),
                        
                        ##### C OUTCOME SELECTION #####
                        
                        radioButtons("selectOutcomeC", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                     choices=list("Binary"=1,"Continuous"=2),selected=1),
                        tags$hr(),
                        
                        ##### C DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        
                        p("Which two", strong("adaptive interventions"), "would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the AIs you select."),
                        
                        fluidRow(
                          column(6,
                                 uiOutput("selectAI1C")
                          ),
                          column(6,
                                 uiOutput("selectAI2C")
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### C IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designCimg",height="100%")),
                          column(5, 
                                 numericInput("respC",
                                              label=HTML("Concerning the tailoring variable, please provide the <strong> probability of response
                                                         </strong> to the first-stage intervention. If you are unsure, leave as 0 for a 
                                                         conservative estimate."),
                                              value=0,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectOutcomeC==1",
                                                  p("Concerning the primary outcome, please provide the", strong("probability of success"),
                                                    "for each of the AIs of interest."),
                                                  uiOutput("binaryDTR1probC"),
                                                  conditionalPanel(condition="input.cellOrConditionalC",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("cellProbsDTR1C"))
                                                                   )
                                                  ),
                                                  uiOutput("binaryDTR2probC"),
                                                  conditionalPanel(condition="input.cellOrConditionalC",
                                                                   fluidRow(column(1),
                                                                            column(11,uiOutput("cellProbsDTR2C"))
                                                                   )
                                                  )
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeC==2",
                                                  p("Concerning the primary outcome, please provide the", strong("standardized effect size"), "between the
                                                    AIs of interest."),
                                                  uiOutput("continuousProbC")
                                 ),
                                 
                                 ##### C INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareC != 0 && input.secondDTRcompareC != 0",
                                                  br(),
                                                  conditionalPanel(condition="input.selectOutcomeC==1",
                                                                   helpText("If you prefer to provide different information, check the appropriate box below."),
                                                                   checkboxInput("cellOrConditionalC",label="Cell-Specific Probabilities",value=FALSE),
                                                                   checkboxInput("targetDiffCheckC",label="Target Difference in Success Probabilities",value=FALSE),
                                                                   checkboxInput("targetOddsCheckC",label="Target Odds Ratio",value=FALSE)
                                                  )
                                 )
                          )
                        ),
                        
                        tags$hr(),
                        
                        
                        ##### C RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,radioButtons("selectResultsC",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample"),
                                 radioButtons("selectAlternativeC",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaC",label=HTML("Type I Error (&alpha;):"),value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsC=='sample'",
                                                  numericInput("inputPowerC",label=HTML("Power of Trial (1-&beta;):"),value=0.8, min=0, max=1,step=0.01)
                                 ),
                                 conditionalPanel(condition="input.selectResultsC=='power'",
                                                  numericInput("inputSampleSizeC",label="Total Sample Size of Trial:",value=0, min=0)
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### C RESULTS #####
                        # Choose which result to display based on binary/continuous outcome and selected result option
                        
                        h3("Results"),
                        conditionalPanel(condition="input.selectOutcomeC==1 & input.selectResultsC=='sample'",
                                         htmlOutput("binarySampleSizeC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==1 & input.selectResultsC=='power'",
                                         htmlOutput("binaryPowerC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==2 & input.selectResultsC=='sample'",
                                         htmlOutput("continuousSampleSizeC")
                        ),
                        conditionalPanel(condition="input.selectOutcomeC==2 & input.selectResultsC=='power'",
                                         htmlOutput("continuousPowerC")
                        )
                      ),
                      
                      ##### C TOOLTIPS #####
                      ### Add bootstrap-style tooltips to inputs coaching proper formatting
                      ### Below are tooltips for inputs rendered statically in ui.R
                      ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                      
                      bsTooltip(id="respC",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),      
                      bsTooltip(id="alphaC",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputPowerC",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputSampleSizeC",title="Input must be an integer greater than zero.",placement="right",trigger="focus")
             ),
             
             ########### DESIGN D ##########
             
             tabPanel("Design D",
                      
                      ##### D SIDEBAR #####
                      
                      sidebarPanel(h4("About this design:"),
                                   p("In this design, all participants are randomized twice, and treatment options are not influenced by a tailoring variable. There are 4 embedded non-adaptive intervention
                                     paths in this SMART. They are AC, AD, BE, and BF."),
                                   tags$hr(),
                                   h4("Inputs Required:"),
                                   tags$ul(tags$li("Indication of continuous or binary outcome."),
                                           tags$li("Indication of a one- or two-sided test."),
                                           tags$li("Specification of a type-I error rate (level of significance). The default value is 0.05, but may range between 0 and 1."),
                                           tags$li("If interest is in sample size, specify the power of the study. The default power is 0.80, but may range between 0 and 1.
                                             If interest is in power, specify the sample size of the study. This must be input as an integer greater than zero."),
                                           tags$li("For binary outcomes:",
                                                   tags$ul(
                                                     tags$li("The default inputs are the probabilities of success for each of the selected intervention paths. These are the overall probabilities of success for those
                                                       participants following each of the selected embedded intervention paths. For example, the probability of success for AE."),
                                                     tags$li("Alternatively, if intervention path-specific probabilities cannot be specified, a target difference in probabilities (must be between 0 and 1)
                                                       or a target odds-ratio (must be positive and not equal to 1), may be selected and provided. With both of these selections, we assume one
                                                       of the intervention paths has probability of success equal to 0.5 to yield conservative results.")
                                                   )),
                                           tags$li("For continuous outcomes:",
                                                   tags$ul(
                                                     tags$li("Specify the standardized effect size between the selected AIs. We use Cohen's definition for standardized effect size such that
                                                             for the two selected AIs it is the difference between the means of the two AIs divided by the square root of the pooled variance (square root of the 
                                                             average of the variances of the two groups). This may range between 0 and 10.")
                                                   ))
                                   ),
                                   h5("Input Formatting Rules:"),
                                   p("All inputs must be given in decimal form with leading zero (no fractions, please), and can be precise to two decimal places.
                                                   For example, '0.07' is valid input; both '.07' and '7/100' are invalid. Improperly-formatted input may result in 
                                                   unpredictable behavior."),
                                   tags$hr(),
                                   h4("Example:"),
                                   p("We wish to find the sample size for a trial with a binary outcome where the probability of response to first stage intervention is 0.40. We estimate the overall
                                     probabilities of success in the two intervention paths of interest, AC and BF to be 0.75 and 0.60, respectively. Given a two-sided 5% type-I error, we require a 
                                     sample size of 608 to make this comparison with 80% power.")
                      ),
                      mainPanel(
                        
                        ##### D PAGE HEADER #####
                        
                        h1("Design D"),
                        tags$hr(),
                        
                        ##### D OUTCOME SELECTION #####
                        
                        radioButtons("selectOutcomeD", label=HTML("Is the <strong> outcome </strong> of interest binary or continuous?"),
                                     choices=list("Binary"=1,"Continuous"=2),selected=1),
                        tags$hr(),
                        
                        ##### D DTR SELECTION #####
                        # Dropdown menus provide options to select DTRs for comparison.
                        
                        p("Which two", strong("non-adaptive intervention paths"), "would you like to compare? Choose two from the menus below.",
                          "The image below will change to highlight the intervention paths you select."),
                        
                        fluidRow(
                          column(6,
                                 uiOutput("selectAI1D")
                          ),
                          column(6,
                                 uiOutput("selectAI2D")
                          )
                        ),
                        
                        tags$hr(),
                        
                        ##### D IMAGE AND PROBABILITY INPUTS #####
                        # Call imageOutput, which reactively displays an image highlighting selected DTRs from image assets in /www/images (see /server.R)
                        # Call uiOutput, which reactively renders UI inputs according to selected DTRs and options selected below.
                        
                        fluidRow(
                          column(7, imageOutput("designDimg",height="100%")),
                          column(5, 
                                 conditionalPanel(condition="input.selectOutcomeD==1",
                                                  p("Concerning the primary outcome, please provide the", strong("probability of success"),
                                                    "for each of the intervention paths of interest."),
                                                  uiOutput("binaryDTR1probD"),
                                                  uiOutput("binaryDTR2probD")
                                 ),
                                 conditionalPanel(condition="input.selectOutcomeD==2",
                                                  p("Concerning the primary outcome, please provide the", strong("standardized effect size"), "between the
                                                    intervention paths of interest."),
                                                  uiOutput("continuousProbD")
                                 ),
                                 
                                 ##### D INPUT OPTIONS #####
                                 # Check for output selection (binary/continuous) then provide options for tailoring ways to input response
                                 # For binary outcomes, options for cell-specific probabilities, target difference, and target odds-ratio
                                 # For continuous outcomes, option to input mean difference and standard-deviation
                                 
                                 conditionalPanel(condition="input.firstDTRcompareD != 0 && input.secondDTRcompareD != 0",
                                                  br(),
                                                  conditionalPanel(condition="input.selectOutcomeD==1",
                                                                   helpText("If you prefer to provide different information, check the appropriate box below."),
                                                                   checkboxInput("targetDiffCheckD",label="Check this box to input a target difference in probabilities.",value=FALSE),
                                                                   checkboxInput("targetOddsCheckD",label="Check this box to input a target odds-ratio instead of a target difference.",value=FALSE)
                                                  )
                                 )
                          )
                        ),
                        
                        tags$hr(),
                        
                        
                        ##### D RESULT OPTIONS #####
                        # Provide options to tailor results: Choose sample size or power, provide alpha and 1-beta
                        
                        fluidRow(
                          column(6,radioButtons("selectResultsD",label="Are you interested in finding sample size or power?",
                                                choices=list("Sample Size" = "sample", "Power" = "power"),
                                                selected="sample"),
                                 radioButtons("selectAlternativeD",label="Do you want to perform a one- or two-sided test?",
                                              choices=list("One-Sided"="one.sided","Two-Sided"="two.sided"),
                                              selected="two.sided")
                                 ),
                          column(6,
                                 numericInput("alphaD",label=HTML("Type I Error (&alpha;):"),value=0.05,min=0,max=1,step=0.01),
                                 conditionalPanel(condition="input.selectResultsD=='sample'",
                                                  numericInput("inputPowerD",label=HTML("Power of Trial (1-&beta;):"),value=0.8, min=0, max=1,step=0.01)
                                 ),
                                 conditionalPanel(condition="input.selectResultsD=='power'",
                                                  numericInput("inputSampleSizeD",label="Total Sample Size of Trial:",value=0, min=0)
                                 )
                          )
                        ),
                        tags$hr(),
                        
                        ##### D RESULTS #####
                        # Choose which result to display based on binary/continuous outcome and selected result option
                        
                        h3("Results"),
                        conditionalPanel(condition="input.selectOutcomeD==1 & input.selectResultsD=='sample'",
                                         htmlOutput("binarySampleSizeD")
                        ),
                        conditionalPanel(condition="input.selectOutcomeD==1 & input.selectResultsD=='power'",
                                         htmlOutput("binaryPowerD")
                        ),
                        conditionalPanel(condition="input.selectOutcomeD==2 & input.selectResultsD=='sample'",
                                         htmlOutput("continuousSampleSizeD")
                        ),
                        conditionalPanel(condition="input.selectOutcomeD==2 & input.selectResultsD=='power'",
                                         htmlOutput("continuousPowerD")
                        )
                      ),
                      
                      ##### D TOOLTIPS #####
                      ### Add bootstrap-style tooltips to inputs coaching proper formatting
                      ### Below are tooltips for inputs rendered statically in ui.R
                      ### For inputs rendered dynamically, see the appropriate renderUI() in server.R
                      
                      bsTooltip(id="respD",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),      
                      bsTooltip(id="alphaD",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputPowerD",title="Input can range from 0-1 and must be in decimal form with a leading zero, up to two places.",placement="right",trigger="focus"),
                      bsTooltip(id="inputSampleSizeD",title="Input must be an integer greater than zero.",placement="right",trigger="focus")
             ),
collapsable=TRUE,
footer=HTML("<p> Kidwell, Seewald, Almirall (in preparation). </p>
            <p> Address correspondence to <a href='nseewald@umich.edu';>nseewald@umich.edu>nseewald@umich.edu</a></p>
            <div style='color:grey;font-size:8px'>  SMARTsize Application Version 1.0.0, last updated 19 November 2014 </div>")
))