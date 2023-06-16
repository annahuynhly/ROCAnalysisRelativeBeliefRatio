################################################################
# SETUP VARIABLES & PICKING SAMPLING REGIME                    #
################################################################

file_upload_example = div(
  p("When uploading a .csv file, the file must have a header and two columns:"),
  p(HTML("<ul>
            <li> The first column must include data for the nondiseased case, </li>
            <li> The second column must include data for the diseased case. </li>
         </ul>")),
  p("Please leave NA/NaN values as blank. You can download an example below of the format:"),
)

binormal_diag_setup_variables_1 = div( 
  titlePanel("Setup Values"),
  sidebarLayout(
    sidebarPanel(width = 4, 
      selectInput(inputId = "binormal_case", 
                  label = "Please select the scenario.",
                  choices = c("Assume variances are equal" = "equal_var", 
                            "Assume variances are unequal" = "unequal_var"),
                  selected = "equal_var"
      ),
      selectInput(inputId = "binormal_diag_condition",
                  label = "Select whether to use the conditional or non conditional prior.",
                  choices = list("Conditional" = 'cond',
                                 "Unconditional" = 'uncond'),
                  selected = 'uncond'
      ),
      selectInput(inputId = "binormal_diag_data_method",
                  label = "Insert the data for the diseased and non-diseased groups,
                  or use the minimal sufficient statistics instead?",
                  choices = c("Use the minimal sufficient statistics" = 1,
                              "Use the raw data" = 2),
                  selected = 1
      ),
      
      conditionalPanel(
        condition = "input.binormal_diag_data_method == 2",
        fileInput(inputId = "binormal_diag_csv", 
                  label = "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
      ),
      
    ),
    mainPanel(
      fluidPage(
        fluidRow(
          column(3, h4("Simulation Sizes:")),
          column(3, numericInput(inputId = "binormal_diag_nMonteCarlo", 
                                 label = '$\\text{Simulation Sample Size}$',
                                 value = 100000, min = 1)),
          column(3, numericInput(inputId = "binormal_diag_delta", 
                                 label = '$\\text{Delta } (\\delta)$',
                                 value = 0.005)),
        ),
        fluidRow(
          column(3, h4("Hyperparameters 1:")),
          column(3, numericInput(inputId = "binormal_diag_mu0", 
                                 label = '$\\mu_{0}$',
                                 value = 0)),
          column(3, numericInput(inputId = "binormal_diag_tau0", 
                                 label = '$\\tau_0$',
                                 value = 0.5)),
        ),
        fluidRow(
          column(3, h4("Hyperparameters 2:")),
          column(3, numericInput(inputId = "binormal_diag_lambda1", 
                                 label = '$\\lambda_{1}$',
                                 value = 1.787)),
          column(3, numericInput(inputId = "binormal_diag_lambda2", 
                                 label = '$\\lambda_{2}$',
                                 value = 1.056)),
        ),
        
        conditionalPanel(
          condition = "input.binormal_diag_data_method == 1",
          fluidRow(
            column(3, h4("Data Sample Size:")),
            column(3, numericInput(inputId = "binormal_diag_nND",
                                   label = '$n_{ND}$',
                                   value = 25)),
            column(3, numericInput(inputId = "binormal_diag_nD", 
                                   label = '$n_{D}$',
                                   value = 20)),
          ),
          fluidRow(
            column(3, h4("Data Means:")),
            column(3, numericInput(inputId = "binormal_diag_meanND", 
                                   label = '$\\bar{x}_{ND}$',
                                   value = -0.072)),
            column(3, numericInput(inputId = "binormal_diag_meanD", 
                                   label = '$\\bar{x}_{D}$',
                                   value = 0.976)),
          ),
          fluidRow(
            column(3, h4("Data Sum of Squares:")),
            column(3, numericInput(inputId = "binormal_diag_sND_squared", 
                                   label = '$s^{2}_{ND}$',
                                   value = 19.638)),
            column(3, numericInput(inputId = "binormal_diag_sD_squared", 
                                   label = '$s^{2}_{D}$',
                                   value = 16.778)),
          ),
        ), # end of conditional panel
        conditionalPanel(
          condition = "input.binormal_diag_data_method == 2",
          file_upload_example,
          downloadButton(outputId = "binormal_diag_sample", 
                         label = "Download Sample")
        ),
      )
    )
  )
)

binormal_diag_setup_variables_2 = div( 
  titlePanel("Setup Values"),
  sidebarLayout(
    sidebarPanel(width = 4, 
      selectInput(inputId = "binormal_optimal_cutoff_denote_variables", 
                  label = "Do you want to use the same values that was used for the inferences 
                  for the AUC?",
                  choices = c("Yes" = "yes", 
                              "No" = "no"),
                  selected = "yes"
      ),
      numericInput(inputId = "binormal_diag_lambda", 
                   label = 'Degrees of Freedom for transforming c values to {0,1] using 
                   Student (lambda) cdf',
                   value = 1
      ),
      conditionalPanel(
        condition = "input.binormal_optimal_cutoff_denote_variables == 'no'",
        selectInput(inputId = "binormal_case_alt", 
                    label = "Please select the scenario.",
                    choices = c("Assume variances are equal" = "equal_var", 
                                "Assume variances are unequal" = "unequal_var"),
                    selected = "equal_var"),
        
        selectInput(inputId = "binormal_diag_data_method_alt",
                    label = "Insert the data for the diseased and non-diseased groups,
                    or use the minimal sufficient statistics instead?",
                    choices = c("Use the minimal sufficient statistics" = 1,
                                "Use the raw data" = 2),
                    selected = 1
        ),
        
        conditionalPanel(
          condition = "input.binormal_diag_data_method_alt == 2",
          fileInput(inputId = "binormal_diag_csv_alt", 
                    label = "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
          ),
        ),
        
      ),
      selectInput(inputId = "binormal_optimal_cutoff_denote_copt",
                  label = "Would you like to hardcode the copt estimate?",
                  choice = c("Yes" = "yes",
                             "No" = "no"),
                  selected = "no"
      ),
      conditionalPanel(
        condition = "input.binormal_optimal_cutoff_denote_copt == 'yes'",
        numericInput(inputId = "binormal_diag_optimal_cutoff_copt",
                     label = 'Input the copt estimate.',
                     value = 0.715), 
      ),
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.binormal_optimal_cutoff_denote_variables == 'no'",
        fluidPage(
          fluidRow(
            column(3, h4("Simulation Sizes:")),
            column(3, numericInput(inputId = "binormal_diag_nMonteCarlo_alt", 
                                   label = '$\\text{Simulation Sample Size}$',
                                   value = 100000, min = 1)),
            column(3, numericInput(inputId = "binormal_diag_delta_alt", 
                                   label = '$\\text{Delta } (\\delta)$',
                                   value = 0.005)),
          ),
          fluidRow(
            column(3, h4("Hyperparameters 1:")),
            column(3, numericInput(inputId = "binormal_diag_mu0_alt", 
                                   label = '$\\mu_{0}$',
                                   value = 0)),
            column(3, numericInput(inputId = "binormal_diag_tau0_alt", 
                                   label = '$\\tau_0$',
                                   value = 0.5)),
          ),
          fluidRow(
            column(3, h4("Hyperparameters 2:")),
            column(3, numericInput(inputId = "binormal_diag_lambda1_alt", 
                                   label = '$\\lambda_{1}$',
                                   value = 1.787)),
            column(3, numericInput(inputId = "binormal_diag_lambda2_alt", 
                                   label = '$\\lambda_{2}$',
                                   value = 1.056)),
          ),
          
          conditionalPanel(
            condition = "input.binormal_diag_data_method_alt == 1",
            fluidRow(
              column(3, h4("Data Sample Size:")),
              column(3, numericInput(inputId = "binormal_diag_nND_alt",
                                     label = '$n_{ND}$',
                                     value = 25)),
              column(3, numericInput(inputId = "binormal_diag_nD_alt", 
                                     label = '$n_{D}$',
                                     value = 20)),
            ),
            fluidRow(
              column(3, h4("Data Means:")),
              column(3, numericInput(inputId = "binormal_diag_meanND_alt", 
                                     label = '$\\bar{x}_{ND}$',
                                     value = -0.072)),
              column(3, numericInput(inputId = "binormal_diag_meanD_alt", 
                                     label = '$\\bar{x}_{D}$',
                                     value = 0.976)),
            ),
            fluidRow(
              column(3, h4("Data Sum of Squares:")),
              column(3, numericInput(inputId = "binormal_diag_sND_squared_alt", 
                                     label = '$s^{2}_{ND}$',
                                     value = 19.638)),
              column(3, numericInput(inputId = "binormal_diag_sD_squared_alt", 
                                     label = '$s^{2}_{D}$',
                                     value = 16.778)),
            ),
          ), # end of conditional Panel
          conditionalPanel(
            condition = "input.binormal_diag_data_method_alt == 2",
            file_upload_example,
            downloadButton(outputId = "binormal_diag_sample_2", 
                           label = "Download Sample")
          ),
        )
      ),
      conditionalPanel(
        condition = "input.binormal_optimal_cutoff_denote_variables == 'yes'",
        p("Currently the variables will be used from the previous section instead.")
      ),
    )
  ),
  #br(style = "line-height:2;"),
)

################################################################
# PAGE FOR INFERENCES FOR THE AUC                              #
################################################################

binormal_diag_hypothesizedAUC = div( 
  titlePanel("Inferences for the AUC"),
  sidebarLayout(
    sidebarPanel(width = 3,
      numericInput(inputId = "binormal_diag_hypoAUC",
                   label = 'Hypothesized AUC (greater than)',
                   value = 0.5
      ),
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0", 
               withSpinner(verbatimTextOutput("binormal_diag_hypoAUC_value"))
      )
    )
  )
)

################################################################
# HISTOGRAM FOR THE AUC                                        #
################################################################

binormal_diag_plots = div( 
  titlePanel("Plots for the AUC"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      textInput(inputId = "binormal_diag_gamma", 
                label = tags$p("Gamma (must be less than posterior content)", 
                               style = "font-size: 95%"), 
                value = "NA"
      ),           
      sliderInput(inputId = "binormal_diag_smoother", 
                  label = "Number of Average Points (Smoother)", 
                  min = 1, max = 49, value = 3, step = 2
      ),
      selectInput(inputId = "binormal_diag_colour", 
                  label = 'Select a colour', 
                  choices = colour_theme_list, 
                  selected = 'default'
      ),
      conditionalPanel(
        condition = "input.binormal_diag_colour == 'manual'",
        selectInput(inputId = "binormal_diag_modify_colour",
                    label = 'Select line to modify',
                    choices = output_line_list,
                    selected = 'prior'
        ), 
        conditionalPanel(
          condition = "input.binormal_diag_modify_colour == 'prior'",
          textInput(inputId = "binormal_diag_colour_prior",
                    label = 'Input the colour of the prior',
                    value = "FF007F"
          ), 
          selectInput(inputId = "binormal_diag_lty_prior", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_modify_colour == 'post'",
          textInput(inputId = "binormal_diag_colour_post",
                    label = 'Input the colour of the posterior',
                    value = "FF00FF"
          ),
          selectInput(inputId = "binormal_diag_lty_post", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_modify_colour == 'rbr'",
          textInput(inputId = "binormal_diag_colour_rbr",
                    label = 'Input the colour of the relative belief ratio',
                    value = "7F00FF"
          ), 
          selectInput(inputId = "binormal_diag_lty_rbr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_modify_colour == 'line_1'",
          textInput(inputId = "binormal_diag_colour_line_1",
                    label = 'Input the colour of the y = 1 line',
                    value = "5327E4"
          ), 
          selectInput(inputId = "binormal_diag_lty_line_1", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_modify_colour == 'cr'",
          textInput(inputId = "binormal_diag_colour_cr",
                    label = 'Input the colour of the credible region',
                    value = "650d84"), 
          selectInput(inputId = "binormal_diag_lty_cr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
      ),
      sliderInput(inputId = "binormal_diag_col_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ), 
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            withSpinner(plotOutput("binormal_diag_postprior_graph")), 
            withSpinner(plotOutput("binormal_diag_RB_graph"))
          )
        )
      ),
    )
  )
)

################################################################
# PAGE FOR DOWNLOADING VALUES                                  #
################################################################

binormal_diag_download_1 = div( 
  titlePanel("Download"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "binormal_diag_filename", 
                label = "Input File Name", 
                value = "AUC Values"
      ),
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
# PAGE FOR INFERENCES OF THE OPTIMAL CUTOFF                    #
################################################################

binormal_diag_AUC_inferences = div( 
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
        sliderInput(inputId = "binormal_diag_smoother_inferences", 
                    label = "Number of Average Points (Smoother)", 
                    min = 1, max = 49, value = 3, step = 2
        ),
        selectInput(inputId = "binormal_diag_inferences_plot_type",
                    label = 'Select which plot to view.',
                    choices = list("FNR" = 'FNR',
                                   "FPR" = 'FPR',
                                   "Error" = 'Error',
                                   "FDR" = 'FDR',
                                   "FNDR" = 'FNDR'),
                    selected = "FNR"
        ),
        selectInput(inputId = "binormal_diag_inferences_colour", 
                    label = 'Select a colour', 
                    choices = colour_theme_list_custom, 
                    selected = 'default'
        ),
        conditionalPanel(
          condition = "input.binormal_diag_inferences_colour == 'manual'",
          selectInput(inputId = "binormal_diag_inferences_modify_colour",
                      label = 'Select line to modify',
                      choices = output_line_list[1:3],
                      selected = 'prior'), 
          
          conditionalPanel(
            condition = "input.binormal_diag_inferences_modify_colour == 'prior'",
            textInput(inputId = "binormal_diag_colour_inferences_prior",
                      label = 'Input the colour of the prior',
                      value = "FF007F"), 
            selectInput(inputId = "binormal_diag_lty_inferences_prior", 
                        label = 'Select a line type', 
                        choices = default_lty_list, 
                        selected = 2
            ),
          ),
          conditionalPanel(
            condition = "input.binormal_diag_inferences_modify_colour == 'post'",
            textInput(inputId = "binormal_diag_colour_inferences_post",
                      label = 'Input the colour of the posterior',
                      value = "FF00FF"), 
            selectInput(inputId = "binormal_diag_lty_inferences_post", 
                        label = 'Select a line type', 
                        choices = default_lty_list, 
                        selected = 2
            ),
          ),
          conditionalPanel(
            condition = "input.binormal_diag_inferences_modify_colour == 'rbr'",
            textInput(inputId = "binormal_diag_colour_inferences_rbr",
                      label = 'Input the colour of the relative belief ratio',
                      value = "7F00FF"), 
            selectInput(inputId = "binormal_diag_lty_inferences_rbr", 
                        label = 'Select a line type', 
                        choices = default_lty_list, 
                        selected = 2
            ),
          ),
        ),
        sliderInput(inputId = "binormal_diag_inferences_col_transparency", 
                    label = "Scale for colour transparency",
                    min = 0, max = 1, value = 0.1
          ), 
        )
        ## END COPY
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.binormal_diag_inferences == 'results'",
        tabPanel("Inferences for Optimal Cutoff", 
                 withSpinner(verbatimTextOutput("binormal_diag_inf_opt_cutoff"))
        ),
      ),
      conditionalPanel(
        condition = "input.binormal_diag_inferences == 'plots'",
        tabPanel("Inferences for Optimal Cutoff",
          fluidRow(
            splitLayout(
              cellWidths = c("50%", "50%"), 
              withSpinner(plotOutput("binormal_diag_inf_opt_cutoff_plot1")), 
              withSpinner(plotOutput("binormal_diag_inf_opt_cutoff_plot2"))
            )
          )
        ),
      )
    )
  )
)

################################################################
# GRAPHS FOR CMOD DENSITY                                      #
################################################################

binormal_diag_copt_plots = div( 
  titlePanel("Plots for the Optimal Cutoff"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      conditionalPanel(
        condition = "input.binormal_diag_inferences == 'results'",
        textInput(inputId = "binormal_diag_gamma_copt", 
                  label = tags$p("Gamma (must be less than posterior content)", 
                                 style = "font-size: 95%"), 
                  value = "NA"),
      ),
      sliderInput(inputId = "binormal_diag_smoother_copt", 
                  label = "Number of Average Points (Smoother)", 
                  min = 1, max = 49, value = 3, step = 2
      ),
      selectInput(inputId = "binormal_diag_c_opt_carry_colour",
                  label = "Select a colour theme",
                  choices = colour_theme_list_custom,
                  selected = 'default'
      ),      
      selectInput(inputId = "binormal_diag_c_opt_modify",
                  label = "Select which object to modify",
                  choices = output_line_list,
                 selected = 'prior'
      ),
      conditionalPanel(
        condition = "input.binormal_diag_c_opt_modify == 'prior'",
        selectInput(inputId = "binormal_diag_priorc_opt_label", 
                    label = "Line Type for the Prior",
                    choices = default_lty_list,
                    selected = 1
        ),
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
                      selected = 2
          ),
          conditionalPanel(
            condition = "input.binormal_diag_c_opt_carry_colour == 'manual'",
            textInput(inputId = "binormal_diag_postc_opt_colour",
                      label = 'Hex Colour for the Posterior',
                      value = "70B77E"), 
          )
        ),
        conditionalPanel(
          condition = "input.binormal_diag_c_opt_modify == 'rbr'",
          selectInput(inputId = "binormal_diag_rbrc_opt_label", 
                      label = "Line Type for the RB Ratio",
                      choices = default_lty_list,
                      selected = 6
          ),
          conditionalPanel(
            condition = "input.binormal_diag_c_opt_carry_colour == 'manual'",
            textInput(inputId = "binormal_diag_rbrc_opt_colour",
                      label = 'Hex Colour for the RB Ratio',
                      value = "CE1483"),
          )
        ),
      
        conditionalPanel(
          condition = "input.binormal_diag_c_opt_modify == 'line_1'",
          selectInput(inputId = "binormal_diag_line_1c_opt_label", 
                      label = "Line Type for the y = 1 Line",
                      choices = default_lty_list,
                      selected = 2
          ),
          conditionalPanel(
            condition = "input.binormal_diag_c_opt_carry_colour == 'manual'",
            textInput(inputId = "binormal_diag_line_1c_opt_colour",
                      label = 'Hex Colour for the y = 1 Line',
                      value = "73C1E7"),
          )
        ),
        conditionalPanel(
          condition = "input.binormal_diag_c_opt_modify == 'cr'",
          selectInput(inputId = "binormal_diag_crc_opt_label", 
                      label = "Line Type for the Credible Region",
                      choices = default_lty_list,
                      selected = 3
          ),
          conditionalPanel(
            condition = "input.binormal_diag_c_opt_carry_colour == 'manual'",
            textInput(inputId = "binormal_diag_crc_opt_colour",
                      label = 'Hex Colour for the Credible Region',
                      value = "9878C3"),
          )
        ),
        # end copy
      
      sliderInput(inputId = "binormal_diag_c_opt_col_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ), 
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            withSpinner(plotOutput("binormal_diag_postprior_cmod_graph")), 
            withSpinner(plotOutput("binormal_diag_RB_cmod_graph"))
          )
        )
      ),
    )
  )
)



################################################################
# PAGE LOGIC                                                   #
################################################################

page_binormal_diag_inference1 = div(
  titlePanel("Binormal Diagnostic"),
  tabsetPanel(type = "tabs",
              tabPanel("Setup Values", binormal_diag_setup_variables_1),
              tabPanel("Plots for the AUC", binormal_diag_plots),
              tabPanel("Inferences for the AUC", binormal_diag_hypothesizedAUC),
              tabPanel("Download Prior & Posterior", binormal_diag_download_1),
  )
)

page_binormal_diag_inference2 = div(
  titlePanel("Binormal Diagnostic"),
  tabsetPanel(type = "tabs",
              tabPanel("Setup Values", binormal_diag_setup_variables_2),
              tabPanel("Inferences for Optimal Cutoff", binormal_diag_AUC_inferences),
              tabPanel("Plots for the Optimal Cutoff", binormal_diag_copt_plots)
  )
)
