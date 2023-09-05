################################################################
# DESCRIPTION                                                  #
################################################################

nonpara_bayes_setup_variables_1 = div(
  titlePanel("Setup Values"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput(inputId = "nonpara_bayes_seed",
                   label = "Please select a seed for the computations",
                   value = 1),
      selectInput(inputId = "nonpara_bayes_condition",
                  label = "Select whether to use the conditional (given AUC>1/2) 
                  or unconditional prior",
                  choices = list("Conditional" = 'cond',
                                 "Unconditional" = 'uncond'),
                  selected = 'cond'
      ),
      selectizeInput(
        inputId = "nonpara_bayes_DP_method", 
        label = "Manually input $a_{D}$ or input $\\epsilon$ to generate $a_{D}$?", 
        choices = NULL,
        options = list(
          options = list(
            list(value = "aD", head = "Choose ", latex = "a_{D}"),
            list(value = "epsilon", head = "Choose ", latex = "\\epsilon")
          ),
          valueField = "value",
          render = I("{
        item: function(item, escape) { 
                var html = katex.renderToString(item.latex);
                return '<div>' + item.head + html + '</div>'; 
              },
        option: function(item, escape) { 
                  var html = katex.renderToString(item.latex);
                  return '<div>' + item.head + html + '</div>';; 
                }
      }")
        ),
        selected = "aD"
      ), # end
      
      # THIS WAS THE PREVIOUS METHOD FOR SELECTING
      #selectInput(inputId = "nonpara_bayes_data_method",
      #            label = "Insert the data for the diseased and non-diseased groups,
      #            or use the minimal sufficient statistics instead?",
      #            choices = c("Use the minimal sufficient statistics" = 1,
      #                        "Use the raw data" = 2),
      #            selected = 1
      #),
      
      fileInput(inputId = "nonpara_bayes_csv", 
                label = "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
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
                                 value = 0.05)),
        ),
        
        fluidRow(
          column(3, h4("Simulation Sizes:")),
          column(3, numericInput(inputId = "nonpara_bayes_nMonteCarlo", 
                                 label = '$\\text{Simulation Sample Size}$',
                                 value = 5000, min = 1)),
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

        file_upload_example,
        downloadButton(outputId = "nonpara_bayes_sample", 
                       label = "Download Sample")
      ), # end of fluid page
    ),
  ),
)

########## ALT VERSION

nonpara_bayes_setup_variables_2 = div(
  titlePanel("Setup Values"),
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      
      selectInput(inputId = "nonpara_bayes_optimal_cutoff_denote_copt",
                  label = "How would you like to select the cutoff?",
                  choice = c("Find the cutoff copt minimizing Error(c)" = "no", 
                             "Find the cutoff maximizing Youden's index" = "youden",
                             #"Find the cutoff minimizing distance to (0, 1)" = "distance", 
                             "Specify the cutoff" = "yes"),
                  selected = "no"
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_optimal_cutoff_denote_copt == 'yes'",
        numericInput(inputId = "nonpara_bayes_optimal_cutoff_copt",
                     label = 'Input the copt estimate',
                     value = 0.15), 
      ),
      
      selectInput(inputId = "nonpara_bayes_optimal_cutoff_denote_variables", 
                  label = "Do you want to use the same values that was used for the inferences 
                  for the AUC?",
                  choices = c("Yes" = "yes", 
                              "No" = "no"),
                  selected = "no"
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_optimal_cutoff_denote_variables == 'no'",
        
        numericInput(inputId = "nonpara_bayes_seed_copt",
                     label = "Please select a seed for the computations",
                     value = 1),
        
        selectizeInput(
          inputId = "nonpara_bayes_DP_method_alt", 
          label = "Manually input $a_{D}$ or input $\\epsilon$ to generate $a_{D}$?", 
          choices = NULL,
          options = list(
            options = list(
              list(value = "aD", head = "Choose ", latex = "a_{D}"),
              list(value = "epsilon", head = "Choose ", latex = "\\epsilon")
            ),
            valueField = "value",
            render = I("{
        item: function(item, escape) { 
                var html = katex.renderToString(item.latex);
                return '<div>' + item.head + html + '</div>'; 
              },
        option: function(item, escape) { 
                  var html = katex.renderToString(item.latex);
                  return '<div>' + item.head + html + '</div>';; 
                }
      }")
          ),
          selected = "aD"
        ), # end
        
        fileInput(inputId = "nonpara_bayes_csv_alt", 
                  label = "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        ),
          
      ),
    ), # end for conditional panel where user puts in new inputs
    mainPanel(
      conditionalPanel(
        condition = "input.nonpara_bayes_optimal_cutoff_denote_variables == 'yes'",
        p("Currently the same variables will be used from the inferences for the AUC."),
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
                                   value = 5000, min = 1)),
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
          file_upload_example,
          downloadButton(outputId = "nonpara_bayes_sample_2", 
                         label = "Download Sample")
        ),
      ),
    ) # end of mainPanel
  )
)

################################################################
# HYPOTHESIS TESTING                                           #
################################################################

nonpara_bayes_AUCinferences = div( 
  titlePanel("Inferences for the AUC"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "nonpara_bayes_gamma", 
                label = tags$p("Gamma for the credible region 
                               (must be less than the posterior content of the plausible region)", 
                               style = "font-size: 95%"), 
                value = "NA"
      )
    ),
    mainPanel(
      tabPanel("inferennces for the AUC", 
               withSpinner(verbatimTextOutput("nonpara_bayes_hypoAUC_value"))))
  ),
  br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
  br(),br(),br(),br(),br(),br(),
)

################################################################
# INFERENCES FOR COPT                                          #
################################################################

nonpara_bayes_inferences_for_copt = div( 
  titlePanel("Inferences for the Optimal Cutoff"),
  sidebarLayout(
    sidebarPanel(width = 3,
      textInput(inputId = "nonpara_bayes_gamma_alt", 
                label = tags$p("Gamma for the credible region 
                               (must be less than the posterior content of the plausible region)", 
                               style = "font-size: 95%"), 
                value = "NA"
      )
    ),
    mainPanel(
      tabPanel("inferences for the optimal cutoff", 
               withSpinner(verbatimTextOutput("nonpara_bayes_inf_opt_cutoff"))),
    ),
  ),
  br(),br(),br(),br(),br(),br(),br(),br(),br(),
)

################################################################
# HISTOGRAM FOR THE AUC                                        #
################################################################

nonpara_bayes_plots = div( 
  titlePanel("Plots for the AUC"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      sliderInput(inputId = "nonpara_bayes_smoother", 
                  label = "Number of Successive Points Averaged (Smoother)", 
                  min = 1, max = 49, value = 3, step = 2),
      selectInput(inputId = "nonpara_bayes_colour", 
                  label = 'Select a colour', 
                  choices = colour_theme_list, 
                  selected = 'default'
      ),
      selectInput(inputId = "nonpara_bayes_AUC_legend_position",
                  label = "Select the position of the legends",
                  choices = default_legend_position_list,
                  selected = "topleft"),
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
    sidebarPanel(
      width = 3,
      selectInput(inputId = "nonpara_bayes_plot_type",
                  label = "Select the type of plot",
                  choices = list("Optimal Cutoff" = "copt",
                                 "Optimal Cutoff (Modified)" = "cmod"),
                  selected = 'copt'
      ),
      sliderInput(inputId = "nonpara_bayes_smoother_copt", 
                  label = "Number of Successive Points Averaged (Smoother)", 
                  min = 1, max = 49, value = 3, step = 2
      ),
      selectInput(inputId = "nonpara_bayes_c_opt_carry_colour",
                  label = "Select a colour theme",
                  choices = colour_theme_list_custom,
                  selected = 'default'
      ),      
      selectInput(inputId = "nonpara_bayes_c_opt_legend_position",
                  label = "Select the position of the legends",
                  choices = default_legend_position_list,
                  selected = "topleft"),
      conditionalPanel(
        condition = "input.nonpara_bayes_c_opt_carry_colour == 'manual'",
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
      htmlOutput("nonpara_bayes_optimal_cutoff_type_description"),
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
                           label = "Input file name", 
                           value = "AUC Values"),
                 downloadButton("nonpara_bayes_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
               withSpinner(dataTableOutput("nonpara_bayes_dataframe")))
    )
  )
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_nonpara_bayes1 = div(
  titlePanel("Nonparametric Bayes Model"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Setup Variables", nonpara_bayes_setup_variables_1),
              tabPanel("Inferences for the AUC", nonpara_bayes_AUCinferences),
              tabPanel("Plots for the AUC", nonpara_bayes_plots),
              tabPanel("Download Prior & Posterior", nonpara_bayes_download_1),
  )
)

page_nonpara_bayes2 = div(
  titlePanel("Nonparametric Bayes Model"),
  tabsetPanel(type = "tabs",
              tabPanel("Setup Values", nonpara_bayes_setup_variables_2),
              tabPanel("Inferences for the Optimal Cutoff", nonpara_bayes_inferences_for_copt),
              tabPanel("Plots for the Optimal Cutoff", nonpara_bayes_copt_plots),
  )
)
