
page_nonpara_bayes_start = fluidPage(
  titlePanel("Getting Started"),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 numericInput(inputId = "nonpara_bayes_seed",
                              label = "Please select a seed for the computations.",
                              value = 1),
                 selectInput(inputId = "nonpara_bayes_case1", 
                             label = "Please select whether the prevalence is known or unknown.",
                             c("The prevalence w is known" = 1, 
                               "The prevalence w is unknown" = 2),
                             selected = 1
                 ),
                 conditionalPanel(
                   condition = "input.nonpara_bayes_case1 == 1",
                   numericInput(inputId = "nonpara_bayes_prevalence_w",
                                'Please Input the Prevalence w.',
                                value = 0.65), #
                   p("The prevalence has been determined. Please proceed to the computations. The Prevalence section may be skipped,
                     as it is used to calculate the prevalence.")
                 ),
                 # Only show this panel if Custom is selected
                 conditionalPanel(
                   condition = "input.nonpara_bayes_case1 == 2",
                   p("Please select the beta prior parameters (alpha1w and alpha2w), and then the sampling regime."),
                   
                   numericInput(inputId = "nonpara_bayes_prevalence_alpha1w", 
                                label = 'alpha1w',
                                value = 391.72),
                   numericInput(inputId = "nonpara_bayes_prevalence_alpha2w", 
                                label = 'alpha2w',
                                value = 211.39),
                   selectInput(inputId = "nonpara_bayes_case2", 
                               label = "Please select the sampling regime.",
                               c("A sample of n_D from diseased and n_ND from non diseased." = "A", 
                                 "A sample of n from population, observe n_D diseased and n_ND nondiseased." = "B"),
                               selected = "case_a_opt"
                   ),
                   
                   conditionalPanel(
                     condition ="input.nonpara_bayes_case2 == 'A'",
                     p("The sampling regime has been chosen. You may observe The Prevalence section to see the prior. 
                       Since we do not have data on the posterior, we cannot make more estimates for the prevalence w.")
                   ),
                   conditionalPanel(
                     condition ="input.nonpara_bayes_case2 == 'B'",
                     p("The sampling regime has been chosen. You may observe The Prevalence section to see the estimation 
                       for the prevalence.")
                   )
                 ),
    ),
    mainPanel(
      p("Insert a description here.")
    )
  ),
  br(style = "line-height:7;")
)