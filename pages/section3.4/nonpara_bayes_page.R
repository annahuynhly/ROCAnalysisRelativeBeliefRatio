################################################################
# DESCRIPTION                                                  #
################################################################

nonpara_bayes_setup_variables_1 = div(
  titlePanel("Setup Values"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
      selectInput(inputId = "nonpara_bayes_condition",
                  label = "Select whether to use the conditional or non conditional prior.",
                  choices = list("Conditional" = 'cond',
                                 "Unconditional" = 'uncond'),
                  selected = 'cond'
      ),
      selectInput(inputId = "nonpara_bayes_DP_method", 
                  label = "Manually input $a_{D}$ or input $\\epsilon$ to generate $a_{D}$?",
                  choices = c("Choose a_D" = "aD", 
                              "Choose epsilon" = "epsilon"),
                  selected = "epsilon"
      ),
      selectInput(inputId = "nonpara_bayes_data_method",
                  label = "Insert the data for the diseased and non-diseased groups,
                  or input the descriptive statistics instead?",
                  choices = c("Insert descriptive statistics" = 1,
                              "Insert the raw data" = 2),
                  selected = 1
      ),
      
      conditionalPanel(
        condition = "input.nonpara_bayes_data_method == 2",
        
        textInput(inputId = "nonpara_bayes_data_values", 
                  label = "Please enter in new values.", 
                  value = "-0.012920,  0.033001, -0.31001"),
        p("If you are adding multiple values, please separate them by a comma. Note that the values
          of the groups won't show if you are currently running another simulation."),
        h4("Diseased Group"),
        actionButton(inputId = "nonpara_bayes_add_values_diseased", 
                     label = "Add Values"),
        actionButton(inputId = "nonpara_bayes_reset_last_diseased",
                     label = "Remove Last Entry"),
        actionButton(inputId = "nonpara_bayes_reset_diseased",
                     label = "Reset"),
        verbatimTextOutput("nonpara_bayes_show_diseased_vec"),
        br(),
        h4("Non-diseased Group"),
        actionButton(inputId = "nonpara_bayes_add_values_nondiseased",
                     label = "Add Values"),
        actionButton(inputId = "nonpara_bayes_reset_last_nondiseased",
                     label = "Remove Last Entry"),
        actionButton(inputId = "nonpara_bayes_reset_nondiseased",
                     label = "Reset"),
        verbatimTextOutput("nonpara_bayes_show_nondiseased_vec"),
      ),
      
      
    ),
    mainPanel(
      fluidPage( 
        #fluidRow(
        #  column(3, h4("Hyperparameters of $H_{D}$:")),
        #  column(3, numericInput(inputId = "nonpara_bayes_muD",
        #                         label = '$\\mu_{D}$',
        #                         value = 45)),
        #  column(3, numericInput(inputId = "nonpara_bayes_sigma_squared", 
        #                         label = '$\\sigma^{2}$',
        #                         value = 0.5)),
        #),
        fluidRow(
          column(3, h4("Hyperparameters of DP:")),
          conditionalPanel(
            condition = "input.nonpara_bayes_DP_method == 'aD'",
            column(3, numericInput(inputId = "nonpara_bayes_aD", 
                                   label = "$a_{D}$",
                                   value = 20),),
          ),
          conditionalPanel(
            condition = "input.nonpara_bayes_DP_method == 'epsilon'",
            column(3, numericInput(inputId = "nonpara_bayes_epsilon", 
                                   label = "$\\text{Epsilon } (\\epsilon)$",
                                   value = 0.25),),
          ),
          column(3, numericInput(inputId = "nonpara_bayes_delta", 
                                 label = '$\\text{Delta } (\\delta)$',
                                 value = 0.005)),
        ),
        
        fluidRow(
          column(3, h4("Simulation Sizes:")),
          column(3, numericInput(inputId = "nonpara_bayes_nMonteCarlo", 
                                 label = '$\\text{Simulation Sample Size}$',
                                 value = 100000, min = 1)),
          column(3, numericInput(inputId = "nonpara_bayes_nstar", # CHANGE THIS
                    label = "$n^{*}$",
                    value = 200)),
        ),
        
        fluidRow(
          column(3, h4("Hyperparameters 1:")),
          column(3, numericInput(inputId = "nonpara_bayes_mu0", 
                                 label = '$\\mu_{0}$',
                                 value = 0)),
          column(3, numericInput(inputId = "nonpara_bayes_tau0", 
                                 label = '$\\tau_0$',
                                 value = 0.5)),
        ),
        fluidRow(
          column(3, h4("Hyperparameters 2:")),
          column(3, numericInput(inputId = "nonpara_bayes_lambda1", 
                                 label = '$\\lambda_{1}$',
                                 value = 1.787)),
          column(3, numericInput(inputId = "nonpara_bayes_lambda2", 
                                 label = '$\\lambda_{2}$',
                                 value = 1.056)),
        ),
        
        conditionalPanel(
          condition = "input.nonpara_bayes_data_method == 1",
          fluidRow(
            column(3, h4("Data Sample Size:")),
            column(3, numericInput(inputId = "nonpara_bayes_nND",
                                   label = '$n_{ND}$',
                                   value = 25)),
            column(3, numericInput(inputId = "nonpara_bayes_nD", 
                                   label = '$n_{D}$',
                                   value = 20)),
          ),
          fluidRow(
            
            column(3, h4("Data Means:")),
            column(3, numericInput(inputId = "nonpara_bayes_meanND", 
                                   label = '$\\bar{x}_{ND}$',
                                   value = -0.072)),
            column(3, numericInput(inputId = "nonpara_bayes_meanD", 
                                   label = '$\\bar{x}_{D}$',
                                   value = 0.976)),
          ),
          fluidRow(
            column(3, h4("Data Sum of Squares:")),
            column(3, numericInput(inputId = "nonpara_bayes_sND_squared", 
                                   label = '$s^{2}_{ND}$',
                                   value = 19.638)),
            column(3, numericInput(inputId = "nonpara_bayes_sD_squared", 
                                   label = '$s^{2}_{D}$',
                                   value = 16.778)),
          ),
          
        ),
      ),
    ),
  ),
)

########## ALT VERSION


nonpara_bayes_setup_variables_2 = div(
  titlePanel("Setup Values"),
  sidebarLayout(
    sidebarPanel(width = 4, 
      selectInput(inputId = "nonpara_bayes_optimal_cutoff_denote_variables", 
                  label = "Do you want to use the same values that was used for the inferences 
                  for the AUC?",
                  choices = c("Yes" = "yes", 
                              "No" = "no"),
                  selected = "yes"
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_optimal_cutoff_denote_variables == 'no'",
        selectInput(inputId = "nonpara_bayes_DP_method_alt", 
                    label = "Manually input $a_{D}$ or input $\\epsilon$ to generate $a_{D}$?",
                    choices = c("Choose a_D" = "aD", 
                                "Choose epsilon" = "epsilon"),
                    selected = "epsilon"
        ),
        selectInput(inputId = "nonpara_bayes_data_method_alt",
                    label = "Insert the data for the diseased and non-diseased groups,
                    or input the descriptive statistics instead?",
                    choices = c("Insert descriptive statistics" = 1,
                                "Insert the raw data" = 2),
                    selected = 1
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_data_method_alt == 2",
          textInput(inputId = "nonpara_bayes_data_values_alt", 
                    label = "Please enter in new values.", 
                    value = "-0.012920,  0.033001, -0.31001"),
          p("If you are adding multiple values, please separate them by a comma. Note that the values
          of the groups won't show if you are currently running another simulation."),
          h4("Diseased Group"),
          actionButton(inputId = "nonpara_bayes_add_values_diseased_alt", 
                       label = "Add Values"),
          actionButton(inputId = "nonpara_bayes_reset_last_diseased_alt",
                       label = "Remove Last Entry"),
          actionButton(inputId = "nonpara_bayes_reset_diseased_alt",
                       label = "Reset"),
          verbatimTextOutput("nonpara_bayes_show_diseased_vec_alt"),
          br(),
          h4("Non-diseased Group"),
          actionButton(inputId = "nonpara_bayes_add_values_nondiseased_alt",
                       label = "Add Values"),
          actionButton(inputId = "nonpara_bayes_reset_last_nondiseased_alt",
                       label = "Remove Last Entry"),
          actionButton(inputId = "nonpara_bayes_reset_nondiseased_alt",
                       label = "Reset"),
          verbatimTextOutput("nonpara_bayes_show_nondiseased_vec_alt"),
        ),
      ), # end for conditional panel where user puts in new inputs
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.nonpara_bayes_optimal_cutoff_denote_variables == 'yes'",
        p("Currently the variables will be used from the previous section instead."),
        br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
        br(),br(),br(),
      ),
      
      conditionalPanel(
        condition = "input.nonpara_bayes_optimal_cutoff_denote_variables == 'no'",
        fluidPage( # CHANGE THIS
          #fluidRow(
          #  column(3, h4("Hyperparameters of $H_{D}$:")),
          #  column(3, numericInput(inputId = "nonpara_bayes_muD_alt",
          #                         label = '$\\mu_{D}$',
          #                         value = 45)),
          #  column(3, numericInput(inputId = "nonpara_bayes_sigma_squared_alt", 
          #                         label = '$\\sigma^{2}$',
          #                         value = 0.5)),
          #),
          fluidRow(
            column(3, h4("Hyperparameters of DP:")),
            conditionalPanel(
              condition = "input.nonpara_bayes_DP_method_alt == 'aD'",
              column(3, numericInput(inputId = "nonpara_bayes_aD_alt", 
                                     label = "$a_{D}$",
                                     value = 20),),
            ),
            conditionalPanel(
              condition = "input.nonpara_bayes_DP_method_alt == 'epsilon'",
              column(3, numericInput(inputId = "nonpara_bayes_epsilon_alt", 
                                     label = "$\\text{Epsilon } (\\epsilon)$",
                                     value = 0.25),),
            ),
            column(3, numericInput(inputId = "nonpara_bayes_delta_alt", 
                                   label = '$\\text{Delta } (\\delta)$',
                                   value = 0.005)),
          ),
          
          fluidRow(
            column(3, h4("Simulation Sizes:")),
            column(3, numericInput(inputId = "nonpara_bayes_nMonteCarlo_alt", 
                                   label = '$\\text{Simulation Sample Size}$',
                                   value = 100000, min = 1)),
            column(3, numericInput(inputId = "nonpara_bayes_nstar_alt", # CHANGE THIS
                                   label = "$n^{*}$",
                                   value = 100)),
          ),
          
          fluidRow(
            column(3, h4("Hyperparameters 1:")),
            column(3, numericInput(inputId = "nonpara_bayes_mu0_alt", 
                                   label = '$\\mu_{0}$',
                                   value = 0)),
            column(3, numericInput(inputId = "nonpara_bayes_tau0_alt", 
                                   label = '$\\tau_0$',
                                   value = 0.5)),
          ),
          fluidRow(
            column(3, h4("Hyperparameters 2:")),
            column(3, numericInput(inputId = "nonpara_bayes_lambda1_alt", 
                                   label = '$\\lambda_{1}$',
                                   value = 1.787)),
            column(3, numericInput(inputId = "nonpara_bayes_lambda2_alt", 
                                   label = '$\\lambda_{2}$',
                                   value = 1.056)),
          ),
          conditionalPanel(
            condition = "input.nonpara_bayes_data_method_alt == 1",
            fluidRow(
              column(3, h4("Data Sample Size:")),
              column(3, numericInput(inputId = "nonpara_bayes_nND_alt",
                                     label = '$n_{ND}$',
                                     value = 25)),
              column(3, numericInput(inputId = "nonpara_bayes_nD_alt", 
                                     label = '$n_{D}$',
                                     value = 20)),
            ),
            fluidRow(
              column(3, h4("Data Means:")),
              column(3, numericInput(inputId = "nonpara_bayes_meanND_alt", 
                                     label = '$\\bar{x}_{ND}$',
                                     value = -0.072)),
              column(3, numericInput(inputId = "nonpara_bayes_meanD_alt", 
                                     label = '$\\bar{x}_{D}$',
                                     value = 0.976)),
            ),
            fluidRow(
              column(3, h4("Data Sum of Squares:")),
              column(3, numericInput(inputId = "nonpara_bayes_sND_squared_alt", 
                                     label = '$s^{2}_{ND}$',
                                     value = 19.638)),
              column(3, numericInput(inputId = "nonpara_bayes_sD_squared_alt", 
                                     label = '$s^{2}_{D}$',
                                     value = 16.778)),
            ),
          ),
        ),
      ),
    ) # end of mainPanel
  )
)

################################################################
# HYPOTHESIS TESTING                                           #
################################################################

nonpara_bayes_hypothesizedAUC = div( 
  titlePanel("Inferences for the AUC"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "nonpara_bayes_gamma", 
                label = tags$p("Gamma (must be less than posterior content)", 
                               style = "font-size: 95%"), 
                value = "NA"
      )
    ),
    mainPanel(
      tabPanel("test title", 
               withSpinner(verbatimTextOutput("nonpara_bayes_hypoAUC_value"))))
  )
)

################################################################
# INFERENCES FOR COPT                                          #
################################################################

nonpara_bayes_inferences_for_copt = div( 
  titlePanel("Inferences for Optimal Cutoff"),
  mainPanel(
    #p("This is currently in progress. Come back later!")
    tabPanel("test", 
             withSpinner(verbatimTextOutput("nonpara_bayes_inf_opt_cutoff"))),
  ),
  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
  br(),br(),br(),
)

################################################################
# HISTOGRAM FOR THE AUC                                        #
################################################################

nonpara_bayes_plots = div( 
  titlePanel("Plots for the AUC"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      sliderInput(inputId = "nonpara_bayes_smoother", 
                  label = "Number of Average Points (Smoother)", 
                  min = 1, max = 49, value = 3, step = 2),
      selectInput(inputId = "nonpara_bayes_colour", 
                  label = 'Select a colour', 
                  choices = colour_theme_list, 
                  selected = 'default'
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_colour == 'manual'",
        selectInput(inputId = "nonpara_bayes_modify_colour",
                    label = 'Select line to modify',
                    choices = output_line_list,
                    selected = 'prior'
        ), 
        conditionalPanel(
          condition = "input.nonpara_bayes_modify_colour == 'prior'",
            textInput(inputId = "nonpara_bayes_colour_prior",
                      label = 'Input the colour of the prior',
                      value = "FF007F"
          ), 
          selectInput(inputId = "nonpara_bayes_lty_prior", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_modify_colour == 'post'",
          textInput(inputId = "nonpara_bayes_colour_post",
                    label = 'Input the colour of the posterior',
                    value = "FF00FF"
          ),
          selectInput(inputId = "nonpara_bayes_lty_post", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_modify_colour == 'rbr'",
          textInput(inputId = "nonpara_bayes_colour_rbr",
                    label = 'Input the colour of the relative belief ratio',
                    value = "7F00FF"
          ), 
          selectInput(inputId = "nonpara_bayes_lty_rbr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_modify_colour == 'pr'",
          textInput(inputId = "nonpara_bayes_colour_pr",
                    label = 'Input the colour of the plausible region',
                    value = "A717DB"
          ), 
          selectInput(inputId = "nonpara_bayes_lty_pr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_modify_colour == 'line_1'",
          textInput(inputId = "nonpara_bayes_colour_line_1",
                    label = 'Input the colour of the y = 1 line',
                    value = "5327E4"
          ), 
          selectInput(inputId = "nonpara_bayes_lty_line_1", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_modify_colour == 'cr'",
          textInput(inputId = "nonpara_bayes_colour_cr",
                    label = 'Input the colour of the credible region',
                    value = "650d84"), 
          selectInput(inputId = "nonpara_bayes_lty_cr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
      ),
      sliderInput(inputId = "nonpara_bayes_col_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ), 
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
              withSpinner(plotOutput("nonpara_bayes_postprior_graph")), 
              withSpinner(plotOutput("nonpara_bayes_RB_graph"))
          )
        )
      ),
    )
  )
)

################################################################
# PLOTS FOR COPT                                               #
################################################################


nonpara_bayes_copt_plots = div( 
  titlePanel("Plots for the Optimal Cutoff"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "nonpara_bayes_plot_type",
                  label = "Select the type of plot.",
                  choices = list("Copt" = "copt",
                                 "Cmod" = "cmod"),
                  selected = 'copt'
      ),
      sliderInput(inputId = "nonpara_bayes_smoother_copt", 
                  label = "Number of Average Points (Smoother)", 
                  min = 1, max = 49, value = 3, step = 2
      ),
      selectInput(inputId = "nonpara_bayes_c_opt_carry_colour",
                  label = "Select a colour theme",
                  choices = colour_theme_list_custom,
                  selected = 'default'
      ),      
      selectInput(inputId = "nonpara_bayes_c_opt_modify",
                  label = "Select which object to modify",
                  choices = output_line_list,
                  selected = 'prior'
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_c_opt_modify == 'prior'",
        selectInput(inputId = "nonpara_bayes_priorc_opt_label", 
                    label = "Line Type for the Prior",
                    choices = default_lty_list,
                    selected = 1
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_c_opt_carry_colour == 'manual'",
          textInput(inputId = "nonpara_bayes_priorc_opt_colour",
                    label = 'Hex Colour for the Prior',
                    value = "065143"),
        )
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_c_opt_modify == 'post'",
        selectInput(inputId = "nonpara_bayes_postc_opt_label", 
                    label = "Line Type for the Posterior",
                    choices = default_lty_list,
                    selected = 2
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_c_opt_carry_colour == 'manual'",
          textInput(inputId = "nonpara_bayes_postc_opt_colour",
                    label = 'Hex Colour for the Posterior',
                    value = "70B77E"), 
        )
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_c_opt_modify == 'rbr'",
        selectInput(inputId = "nonpara_bayes_rbrc_opt_label", 
                    label = "Line Type for the RB Ratio",
                    choices = default_lty_list,
                    selected = 6
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_c_opt_carry_colour == 'manual'",
          textInput(inputId = "nonpara_bayes_rbrc_opt_colour",
                    label = 'Hex Colour for the RB Ratio',
                    value = "CE1483"),
        )
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_c_opt_modify == 'pr'",
        selectInput(inputId = "nonpara_bayes_prc_opt_label", 
                    label = "Line Type for the Plausible Region",
                    choices = default_lty_list,
                    selected = 2
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_c_opt_carry_colour == 'manual'",
          textInput(inputId = "nonpara_bayes_prc_opt_colour",
                    label = 'Hex Colour for the Plausible Region',
                    value = "73C1E7"),
        )
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_c_opt_modify == 'line_1'",
        selectInput(inputId = "nonpara_bayes_line_1c_opt_label", 
                    label = "Line Type for the y = 1 Line",
                    choices = default_lty_list,
                    selected = 2
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_c_opt_carry_colour == 'manual'",
          textInput(inputId = "nonpara_bayes_line_1c_opt_colour",
                    label = 'Hex Colour for the y = 1 Line',
                    value = "73C1E7"),
        )
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_c_opt_modify == 'cr'",
        selectInput(inputId = "nonpara_bayes_crc_opt_label", 
                    label = "Line Type for the Credible Region",
                    choices = default_lty_list,
                    selected = 3
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_c_opt_carry_colour == 'manual'",
          textInput(inputId = "nonpara_bayes_crc_opt_colour",
                    label = 'Hex Colour for the Credible Region',
                    value = "9878C3"),
        )
      ),
      sliderInput(inputId = "nonpara_bayes_c_opt_col_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ), 
    ),
    
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"),
            withSpinner(plotOutput("nonpara_bayes_postprior_cmod_graph")), 
            withSpinner(plotOutput("nonpara_bayes_RB_cmod_graph"))
          )
        )
      ),
    )
  )
)


################################################################
# DOWNLOAD PAGE                                                #
################################################################

nonpara_bayes_download_1 = div( 
  titlePanel("Download Prior & Posterior"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
                 textInput(inputId = "nonpara_bayes_filename", 
                           label = "Input File Name", 
                           value = "AUC Values"),
                 downloadButton("nonpara_bayes_downloadData", "Download"),
    ),
    mainPanel(
      #p("This is a work in progress. Please come back later!")
      tabPanel("Download Output", dataTableOutput("nonpara_bayes_dataframe"))
    )
  )
)

nonpara_bayes_generate_dataframe = function(delta, AUC_prior, AUC_post, AUC_RBR){
  grid_pts = nonpara_bayes_grid(delta)
  #TEMPORARILY CHANGE THE AUC_prior_pts
  #AUC_prior_pts = c(0, grab_AUC_densities_breaks(delta, AUC_prior)*delta)
  #AUC_post_pts = c(0, grab_AUC_densities_breaks(delta, AUC_post)*delta)
  AUC_prior_pts = c(0, grab_AUC_densities_breaks(delta, AUC_prior)$density)
  AUC_post_pts = c(0, grab_AUC_densities_breaks(delta, AUC_post)$density)
  #print(c(length(grid_pts), length(AUC_prior_pts), length(AUC_post_pts), length(AUC_RBR)))
  df = data.frame(grid_pts, AUC_prior_pts, AUC_post_pts, AUC_RBR)
  colnames(df) = c("Grid point", "Prior of the AUC", "Posterior of the AUC", 
                   "Relative Belief Ratio of the AUC")
  return(df)
}

################################################################
# PAGE LOGIC                                                   #
################################################################

page_nonpara_bayes1 = div(
  titlePanel("Nonparametric Bayes Model"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Setup Variables", nonpara_bayes_setup_variables_1),
              tabPanel("Plots for the AUC", nonpara_bayes_plots),
              tabPanel("Inferences for the AUC", nonpara_bayes_hypothesizedAUC),
              tabPanel("Download Prior & Posterior", nonpara_bayes_download_1),
  )
)

page_nonpara_bayes2 = div(
  titlePanel("Nonparametric Bayes Model"),
  tabsetPanel(type = "tabs",
              tabPanel("Setup Values", nonpara_bayes_setup_variables_2),
              tabPanel("Inferences for Optimal Cutoff", nonpara_bayes_inferences_for_copt),
              tabPanel("Plots for the Optimal Cutoff", nonpara_bayes_copt_plots),
  )
)


