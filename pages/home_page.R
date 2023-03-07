
home_page = fluidPage(
  
  titlePanel("Getting Started"),
  sidebarLayout(
    sidebarPanel(width = 4, 
      selectInput(inputId = "pick_case_1", "Please select whether the prevalence is known or unknown.",
                  c("The prevalence w is known" = "case_1_opt", 
                    "The prevalence w is unknown" = "case_2_opt"),
                  selected = "case_1_opt"
      ),
      conditionalPanel(
        condition = "input.pick_case_1 == 'case_1_opt'",
        numericInput(inputId = "global_prevalence_w",
                     tags$p('Please Input the Prevalence w.', style = "font-size: 90%;"),
                     value = 0.65), #
        p("The prevalence has been determined. Please proceed to section 3.2. The Prevalence section may be skipped,
          as it is used to calculate the prevalence.")
      ),
        # Only show this panel if Custom is selected
      conditionalPanel(
        condition = "input.pick_case_1 == 'case_2_opt'",
        p("Please select the beta prior parameters (alpha1w and alpha2w), and then the
          sampling regime."),
        
        numericInput(inputId = "prevalence_setup_alpha1w", 
                     label = 'alpha1w',
                     value = 391.72),
        numericInput(inputId = "prevalence_setup_alpha2w", 
                     label = 'alpha2w',
                     value = 211.39),
        selectInput(inputId = "pick_case_2", "Please select the sampling regime.",
                    c("A sample of n_D from diseased and n_ND from non diseased." = "case_a_opt", 
                      "A sample of n from population, observe n_D diseased and n_ND nondiseased." = "case_b_opt"),
                    selected = "case_a_opt"
        ),
        
        conditionalPanel(
          condition ="input.pick_case_2 == 'case_a_opt'",
          p("The sampling regime has been chosen. You may observe The Prevalence section to see the prior,
            although since we do not have data on the posterior, we cannot make more estimates for the prevalence w.")
          ),
        conditionalPanel(
          condition ="input.pick_case_2 == 'case_b_opt'",
          p("The sampling regime has been chosen. You may observe The Prevalence section to see the estimation
            for the prevalence.")
        )
      ),
    ),
    mainPanel(
      p("Hello and welcome! This is the home page. Please select one of the cases you are dealing with
        before proceeding with the calculator."),
      p("Note that the relevant prevalence w is the proportion of this subpopulation who are diseased."),
      p("Some of the computations take a long time to run, hence, switching between 
        pages may take awhile."),
      p("The calculator is based on the works of Al Labadi, L., Evans, M. and Liang, Q. (2022).
        Specifically, this website is currently based on Section 3.2."),
      p("We plan to implement more sections (section 3.3, section 3.4) in the future. The links currently have no useful information."),
      tags$div(
        tags$a(href="https://www.mdpi.com/1099-4300/24/12/1710", 
               "The paper is found here."),
        "It is fortunately open access! :)"
      )
    )
  )
)