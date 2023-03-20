################################################################
# DEFINITIONS                                                  #
################################################################

output$formulas = renderImage({
  list(src = "./pages/section3.2/formulas.png",
       width = "70%", height = "100%")
}, deleteFile = FALSE
)


# Defining the inputs
input_var_name <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                    "Total Non-Diseased", 
                    "Total Diseased",
                    "Monte Carlo (Simulation) Sample Size",
                    "Relevant Prevalence w", 
                    "alphaND1, ..., alphaNDm",
                    "alphaD1, ..., alphaDm",
                    "fND",
                    "fD",
                    "Delta",
                    "Gamma",
                    "Hypothesized AUC (greater than)",
                    "Input File Name")
input_var_description <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                           "  The amount of \"non-diseased\" individuals from the total sample size.",
                           "  The total amount of \"diseased\" individuals from the total sample size.",
                           "  The sample size to use the Monte Carlo method to simulate the prior and the posterior. 
                     When computations are taking too long, it is recommended to lower the sample size.",
                           "  TODO",
                           "  The parameters of the Dirichlet distribution of the non-diseased sample.",
                           "  The parameters of the Dirichlet distribution of the diseased sample.",
                           "  TODO",
                           "  TODO",
                           "  Also known as the distance that matters. It is the distance between any two points on the grid.",
                           "  A value that's less than the posterior content of the plausible region. It is used to 
                     determine the credible region to test the validity of where the maximum of the relative belief 
                     ratio is located.",
                           "  Input what the user hypothesizes AUC to be greater than.",
                           "  The name of the file you want to download. The .csv file will include the grid points, the prior, 
                     the posterior, and the relative belief ratio.")
input_test_df <- data.frame(input_var_description)
rownames(input_test_df) = input_var_name
colnames(input_test_df) = "Description of the Inputs"


# Defining the outputs
output_var_name <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                     "Plausible Region", 
                     "Posterior Content",
                     "Credible Region",
                     "Area Under The Line Plot",
                     "Prior copt",
                     "Post copt",
                     "copt Estimate",
                     "Error Characteristics",
                     "FPRest copt Estimate",
                     "FNRest copt Estimate",
                     "ERRORwest copt Estimate",
                     "FDRest copt Estimate",
                     "FNDRest copt Estimate",
                     "PPVest copt Estimate",
                     "Relative Belief Ratio of Event AUC > XX%",
                     "Posterior Probability of Event AUC > XX%")
output_var_description <- c("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
                            "This interval consists of all the values for which there is evidence in favor.", 
                            "This interval measures how strongly it is believed the true value lies in this set.",
                            "Provides the exact area under the line plot for the prior, posterior, instead of 
                            the histograms to show how accurate it is.", 
                            "TODO",
                            "TODO",
                            "TODO",
                            "TODO",
                            "TODO",
                            "TODO",
                            "See next tab.", 
                            "See next tab.",
                            "See next tab.",
                            "See next tab.",
                            "See next tab.",
                            "Provides the relative belief ratio given the hypothesized AUC.",
                            "Provides the posterior content given the hypothesized AUC.")
output_test_df <- data.frame(output_var_description)
rownames(output_test_df) = output_var_name
colnames(output_test_df) = "Description of the Outputs"



output$input_table_description <- render_tableHTML({
  #mtcars %>%
  input_test_df %>%
    tableHTML(border = 2) %>%
    #tableHTML(widths = c(140, rep(45, 11))) %>%
    add_css_header(css = list(c('height', 'background-color', 'text-align'),
                              c('30px', '#def1fd', 'left')),
                   headers = 1:12) %>%
    add_css_row(css = list('background-color', '#eaf6fe'),
                rows = even(1:12)) %>%
    add_css_row(css = list('background-color', '#ffffff'),
                rows = odd(1:12))
  #            rows = odd(1:12)) %>%
  #add_css_column(css = list(c('text-align', 'background-color'),
  #                          c('center', '#def1fd')),
  #               columns = c('rownames'))
})

output$output_table_description <- render_tableHTML({
  #mtcars %>%
  output_test_df %>%
    tableHTML(border = 2) %>%
    #tableHTML(widths = c(140, rep(45, 11))) %>%
    add_css_header(css = list(c('height', 'background-color', 'text-align'),
                              c('30px', '#def1fd', 'left')),
                   headers = 1:12) %>%
    add_css_row(css = list('background-color', '#eaf6fe'),
                rows = even(1:12)) %>%
    add_css_row(css = list('background-color', '#ffffff'),
                rows = odd(1:12))
  #            rows = odd(1:12)) %>%
  #add_css_column(css = list(c('text-align', 'background-color'),
  #                          c('center', '#def1fd')),
  #               columns = c('rownames'))
})
