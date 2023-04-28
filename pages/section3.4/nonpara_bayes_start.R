
page_nonpara_bayes_start = fluidPage(
  titlePanel("Getting Started"),
  sidebarLayout(
    sidebarPanel(width = 4, 
      numericInput(inputId = "nonpara_bayes_seed",
                   label = "Please select a seed for the computations.",
                   value = 1),
      selectInput(inputId = "nonpara_bayes_case1", 
                  label = "Please select whether the prevalence $\\omega$ is known or unknown.",
                  choices = c("The prevalence w is known" = 1, 
                              "The prevalence w is unknown" = 2),
                  selected = 1
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_case1 == 1",
        numericInput(inputId = "nonpara_bayes_prevalence_w",
                     label = 'Please Input the Prevalence w.',
                     value = 0.65), #
        p("The prevalence $\\omega$ has been determined. Please proceed to the computations. 
        The Prevalence section may be skipped, as it is used to estimate the prevalence $\\omega$.")
      ),

      conditionalPanel(
        condition = "input.nonpara_bayes_case1 == 2",
        p("Please select the beta prior parameters ($\\alpha_{1\\omega}$ and $\\alpha_{2\\omega}$), 
          and then the sampling regime."),
        numericInput(inputId = "nonpara_bayes_prevalence_alpha1w", 
                     label = '$\\alpha_{1\\omega}$',
                     value = 391.72),
        numericInput(inputId = "nonpara_bayes_prevalence_alpha2w", 
                     label = '$\\alpha_{2\\omega}$',
                     value = 211.39),
        selectInput(inputId = "nonpara_bayes_case2", 
                    label = "Please select the sampling regime.",
                    choices = c("A sample of n_D from diseased and n_ND from non diseased." = "A", 
                                "A sample of n from population, observe n_D diseased and n_ND nondiseased." = "B"),
                    selected = "case_a_opt"
        ),
                   
        conditionalPanel(
          condition ="input.nonpara_bayes_case2 == 'A'",
          p("The sampling regime has been chosen. You may observe The Prevalence section to see the prior. 
          Since we do not have data on the posterior, we cannot make more estimates for the prevalence $\\omega$.")
        ),
        conditionalPanel(
          condition ="input.nonpara_bayes_case2 == 'B'",
          p("The sampling regime has been chosen. You may observe The Prevalence section to see the estimation 
          for the prevalence $\\omega$.")
        )
      ),
    ),
    mainPanel(
      getting_started_default_description_3
    )
  ),
  #br(style = "line-height:7;")
)