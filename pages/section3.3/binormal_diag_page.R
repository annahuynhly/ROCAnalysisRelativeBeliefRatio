################################################################
# SETUP VARIABLES & PICKING SAMPLING REGIME                    #
################################################################

binormal_diag_setup_variables_alt = div( 
  titlePanel("Setup Variables - NEED TO MODIFY"),
  sidebarLayout(
    sidebarPanel(width = 3, 
                 selectInput(inputId = "binormal_case", 
                             label = "Please select the scenario..",
                             c("Assume variances are equal" = "equal_var", 
                               "Assume variances are unequal" = "unequal_var"),
                             selected = "equal_var"
                 ),
    ),
    mainPanel(
      fluidPage(
        
        fluidRow(
          column(3, h4("Simulation Sizes:")),
          column(3, numericInput(inputId = "binormal_diag_nMonteCarlo", 
                                 label = 'Simulation Sample Size',
                                 value = 100000, min = 0)),
          column(3, numericInput(inputId = "binormal_diag_delta", 
                                 label = 'Delta',
                                 value = 0.01)),
        ),
        
        fluidRow(
          column(3, h4("Hyperparameters 1:")),
          column(3, numericInput(inputId = "binormal_diag_mu0", 
                                 label = 'mu0',
                                 value = 0)),
          column(3, numericInput(inputId = "binormal_diag_tau0", 
                                 label = 'tau0',
                                 value = 0.5)),
        ),
        
        fluidRow(
          column(3, h4("Hyperparameters 2:")),
          column(3, numericInput(inputId = "binormal_diag_lambda2", 
                                 label = 'lambda2',
                                 value = 1.056)),
          column(3, numericInput(inputId = "binormal_diag_lambda1", 
                                 label = 'lambda1',
                                 value = 1.787)),
        ),
        
        fluidRow(
          column(3, h4("Data Sample Size:")),
          column(3, numericInput(inputId = "binormal_diag_nND",
                                 label = 'nND',
                                 value = 25)),
          column(3, numericInput(inputId = "binormal_diag_nD", 
                                 label = 'nD',
                                 value = 20)),
        ),
        
        fluidRow(
          column(3, h4("Data Means:")),
          column(3, numericInput(inputId = "binormal_diag_meanND", 
                                 label = 'meanND',
                                 value = -0.072)),
          column(3, numericInput(inputId = "binormal_diag_meanD", 
                                 label = 'meanD',
                                 value = 0.976)),
        ),
        
        fluidRow(
          column(3, h4("Data Sum of Squares:")),
          column(3, numericInput(inputId = "binormal_diag_sND_squared", 
                                 label = 'sND squared',
                                 value = 19.638)),
          column(3, numericInput(inputId = "binormal_diag_sD_squared", 
                                 label = 'sD squared',
                                 value = 16.778)),
        ),
        
      )
    )
  ),
  br(style = "line-height:2;"),
)


################################################################
# OUTPUT 1 PAGE                                                #
################################################################

binormal_diag_plausible_region = div( 
  titlePanel("Inferences for Optimal Cutoff"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      selectInput(inputId = "binormal_diag_inferences", 
                  label = "Please select what you would like to view.",
                  choices = c("View Results" = "results", 
                               "View Plots" = "plots"),
                  selected = "results"),
      
      conditionalPanel(
        condition = "input.binormal_diag_inferences == 'plots'",
        selectInput(inputId = "binormal_diag_inferences_plot_type",
                    label = 'Select which plot to view.',
                    choices = list("FNR" = 'FNR',
                                   "FPR" = 'FPR',
                                   "Error" = 'Error',
                                   "FDR" = 'FDR',
                                   "FNDR" = 'FNDR'),
                    selected = "FNR")
      ),
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.binormal_diag_inferences == 'results'",
        tabPanel("Inferences for Optimal Cutoff", 
                 withSpinner(verbatimTextOutput("binormal_diag_inf_opt_cutoff"))),
      ),
      conditionalPanel(
        condition = "input.binormal_diag_inferences == 'plots'",
        tabPanel("Inferences for Optimal Cutoff",
                 fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                      withSpinner(plotOutput("binormal_diag_inf_opt_cutoff_plot1")), 
                                      withSpinner(plotOutput("binormal_diag_inf_opt_cutoff_plot2"))))),
      ),
      
    )
  )
)

################################################################
# GRAPH 1 PAGE (HISTOGRAM)                                     #
################################################################

binormal_diag_plots = div( 
  titlePanel("Plots for the AUC"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "binormal_diag_hist_visual", 
                  label = "Choose Visual:",
                  choices = list("With Bars" = "binormal_diag_withbars",
                                 "Without Bars" = "binormal_diag_withoutbars"),
                  selected = "binormal_diag_withoutbars"),
                 
      selectInput(inputId = "binormal_diag_colour", 
                  label = 'Select a colour', 
                  choices = list("Default Theme 1" = 'default1',
                                 "Default Theme 2" = 'default2',
                                 "Manually Insert" = 'manual'), 
                  selected = 'default'),
                 
      conditionalPanel(
        condition = "input.binormal_diag_colour == 'manual'",
        selectInput(inputId = "binormal_diag_modify_colour",
                    label = 'Select line to modify',
                    choices = list("Prior" = 'prior',
                                   "Posterior" = 'post',
                                   "Relative Belief Ratio" = 'rbr',
                                   "Plausible Region" = 'pr',
                                   "Line of y = 1" = 'line_1',
                                   "Credible Region" = 'cr'),
                    selected = 'prior'), 
      ),
                 
      conditionalPanel(
        condition = "input.binormal_diag_modify_colour == 'prior'",
        textInput(inputId = "binormal_diag_colour_prior",
                  label = 'Input the colour of the prior',
                  value = "FF007F"), 
      ),
      conditionalPanel(
        condition = "input.binormal_diag_modify_colour == 'post'",
        textInput(inputId = "binormal_diag_colour_post",
                  label = 'Input the colour of the posterior',
                  value = "FF00FF"), 
      ),
      conditionalPanel(
        condition = "input.binormal_diag_modify_colour == 'rbr'",
        textInput(inputId = "binormal_diag_colour_rbr",
                  label = 'Input the colour of the relative belief ratio',
                  value = "7F00FF"), 
      ),
      conditionalPanel(
        condition = "input.binormal_diag_modify_colour == 'pr'",
        textInput(inputId = "binormal_diag_colour_pr",
                  label = 'Input the colour of the plausible region',
                  value = "A717DB"), 
      ),
      conditionalPanel(
        condition = "input.binormal_diag_modify_colour == 'line_1'",
        textInput(inputId = "binormal_diag_colour_line_1",
                  label = 'Input the colour of the y = 1 line',
                  value = "5327E4"), 
      ),
      conditionalPanel(
        condition = "input.binormal_diag_modify_colour == 'cr'",
        textInput(inputId = "binormal_diag_colour_cr",
                  label = 'Input the colour of the credible region',
                  value = "650d84"), 
      ),
      conditionalPanel(
        condition = "input.binormal_diag_hist_visual == 'binormal_diag_withbars'",
        sliderInput(inputId = "binormal_diag_col_transparency", 
                    label = "Scale for colour transparency",
                    min = 0, max = 1, value = 0.2), 
      )
    ),
    mainPanel(
      tabPanel("Plots",
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    withSpinner(plotOutput("binormal_diag_postprior_graph")), 
                                    withSpinner(plotOutput("binormal_diag_RB_graph"))))),
    )
  )
)

################################################################
# GRAPH 2 PAGE                                                 #
################################################################

default_lty_list = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6)

binormal_diag_copt_plots = div( 
  titlePanel("Plots for the Optimal Cutoff"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "binormal_diag_c_opt_carry_colour",
                  label = "Select a colour theme",
                  choices = list("Default Theme 1" = 'default1',
                                 "Default Theme 2" = 'default2',
                                 "Custom Theme from AUC Plots" = 'custom',
                                 "Manually Insert" = 'manual'),
                  selected = 'default'),      
      selectInput(inputId = "binormal_diag_c_opt_modify",
                  label = "Select which object to modify",
                  choices = list("Prior" = 'prior',
                                 "Posterior" = 'post',
                                 "Relative Belief Ratio" = 'rbr'),
                 selected = 'prior'),
                 
      conditionalPanel(
        condition = "input.binormal_diag_c_opt_modify == 'prior'",
        selectInput(inputId = "binormal_diag_priorc_opt_label", 
                    label = "Line Type for the Prior",
                    choices = default_lty_list,
                    selected = 1),
                   
        conditionalPanel(
          condition = "input.binormal_diag_c_opt_carry_colour == 'manual'",
          textInput(inputId = "binormal_diag_priorc_opt_colour",
                    label = 'Hex Colour for the Prior',
                    value = "065143"),
          )
        ),
        conditionalPanel(
          condition = "input.binormal_diag_c_opt_modify == 'post'",
          selectInput(inputId = "binormal_diag_postc_opt_label", 
                      label = "Line Type for the Posterior",
                      choices = default_lty_list,
                      selected = 2),
          conditionalPanel(
            condition = "input.binormal_diag_c_opt_carry_colour == 'manual'",
            textInput(inputId = "binormal_diag_postc_opt_colour",
                      label = 'Hex Colour for the Posterior',
                      value = "70B77E"), 
          )
        ),
        conditionalPanel(
          condition = "input.binormal_diag_c_opt_modify == 'rbr'",
          selectInput(inputId = "binormal_diag_rbc_opt_label", 
                      label = "Line Type for the RB Ratio",
                      choices = default_lty_list,
                      selected = 6),
          conditionalPanel(
            condition = "input.binormal_diag_c_opt_carry_colour == 'manual'",
            textInput(inputId = "binormal_diag_rbrc_opt_colour",
                      label = 'Hex Colour for the RB Ratio',
                      value = "CE1483"),
          )
        ),
    ),
    mainPanel(
      tabPanel("Plots",
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    withSpinner(plotOutput("binormal_diag_postprior_copt_graph")), 
                                    withSpinner(plotOutput("binormal_diag_RB_copt_graph"))))),
      
    )
  )
)

################################################################
# HYPOTHESIS TESTING                                           #
################################################################

binormal_diag_hypothesizedAUC = div( 
  titlePanel("Inferences for the AUC"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "binormal_diag_hypoAUC",
                   label = 'Hypothesized AUC (greater than)',
                   value = 0.5),
      textInput(inputId = "binormal_diag_gamma", 
                label = tags$p("Gamma (must be less than posterior content)", 
                        style = "font-size: 95%"), 
                value = "NA")
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0", 
               withSpinner(verbatimTextOutput("binormal_diag_hypoAUC_value")))
    )
  )
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

binormal_diag_download_1 = div( 
  titlePanel("Download Prior & Posterior"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
                 textInput(inputId = "binormal_diag_filename", 
                           label = "Input File Name", 
                           value = "AUC Values"),
                 downloadButton("binormal_diag_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
               withSpinner(dataTableOutput("binormal_diag_dataframe"))
      )
    )
  )
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_binormal_diag = div(
  titlePanel("Binormal Diagnostic"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Setup Variables", binormal_diag_setup_variables_alt),
              #tabPanel("Description", binormal_diag_description),
              tabPanel("Inferences for the AUC", binormal_diag_hypothesizedAUC),
              tabPanel("Plots for the AUC", binormal_diag_plots),
              tabPanel("Inferences for Optimal Cutoff", binormal_diag_plausible_region),
              tabPanel("Plots for the Optimal Cutoff", binormal_diag_copt_plots),
              tabPanel("Download Prior & Posterior", binormal_diag_download_1),
  )
)