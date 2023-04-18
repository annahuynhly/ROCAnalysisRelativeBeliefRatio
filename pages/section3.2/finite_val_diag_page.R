################################################################
# DESCRIPTION                                                  #
################################################################

finite_val_setup_variables_1 = fluidPage(
  titlePanel("Setup Values"),
  br(),
  
  fluidRow(
    column(3, h3("Simulation Sizes:")),
    column(3, numericInput(inputId = "finite_val_nMonteCarlo", 
                           label = 'Monte Carlo (Simulation) Sample Size',
                           value = 100000, min = 0)),
    column(3, numericInput(inputId = "finite_val_delta", 
                           label = "Delta", 
                           value = 0.04, min = 0, max = 1)),
  ),
  fluidRow(
    column(3, h3("Hyperparameters:")),
    column(3, textInput(inputId = "finite_val_alpha_ND",
                        label = 'alphaND1, ..., alphaNDm',
                        value = "1, 1, 1, 1, 1")),
    column(3, textInput(inputId = "finite_val_alpha_D",
                        label = 'alphaD1, ..., alphaDm',
                        value = "1, 1, 1, 1, 1")),
  ),
  fluidRow(
    column(3, h3("Data (Sample Count):")),
    column(3, numericInput(inputId = "finite_val_nND",
                           label = 'Total Non-Diseased',
                           value = 50, min = 1)),
    column(3, numericInput(inputId = "finite_val_nD", 
                           label = 'Total Diseased',
                           value = 100, min = 1)),
  ),
  fluidRow(
    column(3, h3("Data (Distribution)")),
    column(3, textInput(inputId = "finite_val_fND",
                        label = 'fNd',
                        value = "29, 7, 4, 5, 5")),
    column(3, textInput(inputId = "finite_val_fD",
                        label = 'fD',
                        value = "14, 7, 25, 33, 21")),
  ),
  
  br(style = "line-height:10;"),
)

finite_val_setup_variables_2 = fluidPage(
  titlePanel("Setup Values"),
  br(),
  
  sidebarLayout(
    sidebarPanel(width = 3, 
      selectInput(inputId = "finite_val_optimal_cutoff_denote_variables", 
                  label = "Do you want to use the same values that was used for the inferences 
                  for the AUC?",
                  choices = c("Yes" = "yes", 
                              "No" = "no"),
                  selected = "yes"
      ),
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.finite_val_optimal_cutoff_denote_variables == 'no'",
        fluidPage(
          fluidRow(
            column(3, h3("Simulation Sizes:")),
            column(3, numericInput(inputId = "finite_val_nMonteCarlo_alt", 
                                   label = 'Monte Carlo (Simulation) Sample Size',
                                   value = 100000, min = 0)),
            column(3, numericInput(inputId = "finite_val_delta_alt", 
                                   label = "Delta", 
                                   value = 0.04, min = 0, max = 1)),
          ),
          #fluidRow(
          #  column(3, h3("Hyperparameters:")),
          #  column(3, textInput(inputId = "finite_val_alpha_ND_alt",
          #                      label = 'alphaND1, ..., alphaNDm',
          #                      value = "1, 1, 1, 1, 1")),
          #  column(3, textInput(inputId = "finite_val_alpha_D_alt",
          #                      label = 'alphaD1, ..., alphaDm',
          #                      value = "1, 1, 1, 1, 1")),
          #),
          #fluidRow(
          #  column(3, h3("Data (Sample Count):")),
          #  column(3, numericInput(inputId = "finite_val_nND_alt",
          #                         label = 'Total Non-Diseased',
          #                         value = 50, min = 1)),
          #  column(3, numericInput(inputId = "finite_val_nD_alt", 
          #                         label = 'Total Diseased',
          #                         value = 100, min = 1)),
          #),
          #fluidRow(
          #  column(3, h3("Data (Distribution)")),
          #  column(3, textInput(inputId = "finite_val_fND_alt",
          #                      label = 'fNd',
          #                      value = "29, 7, 4, 5, 5")),
          #  column(3, textInput(inputId = "finite_val_fD_alt",
          #                      label = 'fD',
          #                      value = "14, 7, 25, 33, 21")),
          #),
        )
      ),
      conditionalPanel(
        condition = "input.finite_val_optimal_cutoff_denote_variables == 'yes'",
        p("Currently the variables will be used from the previous section instead.")
      ),
    )
  ),
  
  br(style = "line-height:10;"),
)


################################################################
# OUTPUT 1 PAGE                                                #
################################################################

finite_val_hypothesizedAUC = div( 
  titlePanel("Inferences for the AUC"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "finite_val_hypoAUC",
                   label = 'Hypothesized AUC (greater than)',
                   value = 0.5
      ),
      textInput(inputId = "finite_val_gamma", 
                label = tags$p("Gamma (must be less than posterior content)", 
                               style = "font-size: 95%"), 
                value = "NA"
      )
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0", 
        withSpinner(verbatimTextOutput("finite_val_hypoAUC_value"))
      )
    )
  ),
  br(style = "line-height:2;"),
)


################################################################
# OUTPUT 2 PAGE                                                #
################################################################

finite_val_plausible_region = div( 
  titlePanel("Inferences for Optimal Cutoff"),
  
  mainPanel(
    tabPanel("Inferences for Optimal Cutoff", 
      withSpinner(verbatimTextOutput("finite_val_output1"))
    ),
  ),
  br(style = "line-height:28;")
)

################################################################
# GRAPH 1 PAGE (HISTOGRAM)                                     #
################################################################

finite_val_plots = div( 
  titlePanel("Plots for the AUC"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "finite_val_hist_visual", label = "Choose Visual:",
                   choices = c("With Bars" = "finite_val_withbars",
                               "Without Bars" = "finite_val_withoutbars"),
                   selected = "finite_val_withoutbars"
      ),
      selectInput(inputId = "finite_val_colour", 
                  label = 'Select a colour', 
                  choices = colour_theme_list, 
                  selected = 'default'
      ),
      conditionalPanel(
        condition = "input.finite_val_colour == 'manual'",
        selectInput(inputId = "finite_val_modify_colour",
                    label = 'Select line to modify',
                    choices = output_line_list,
                    selected = 'prior'), 
        
        conditionalPanel(
          condition = "input.finite_val_modify_colour == 'prior'",
          textInput(inputId = "finite_val_colour_prior",
                    label = 'Input the colour of the prior',
                    value = "FF007F"
          ), 
          selectInput(inputId = "finite_val_lty_prior", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_modify_colour == 'post'",
          textInput(inputId = "finite_val_colour_post",
                    label = 'Input the colour of the posterior',
                    value = "FF00FF"
          ), 
          selectInput(inputId = "finite_val_lty_post", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_modify_colour == 'rbr'",
          textInput(inputId = "finite_val_colour_rbr",
                    label = 'Input the colour of the relative belief ratio',
                    value = "7F00FF"
          ), 
          selectInput(inputId = "finite_val_lty_rbr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_modify_colour == 'pr'",
          textInput(inputId = "finite_val_colour_pr",
                    label = 'Input the colour of the plausible region',
                    value = "A717DB"
          ), 
          selectInput(inputId = "finite_val_lty_pr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_modify_colour == 'line_1'",
          textInput(inputId = "finite_val_colour_line_1",
                    label = 'Input the colour of the y = 1 line',
                    value = "5327E4"
          ), 
          selectInput(inputId = "finite_val_lty_line_1", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_modify_colour == 'cr'",
          textInput(inputId = "finite_val_colour_cr",
                    label = 'Input the colour of the credible region',
                    value = "650d84"
          ), 
          selectInput(inputId = "finite_val_lty_cr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
      ),
      conditionalPanel(
        condition = "input.finite_val_hist_visual == 'finite_val_withbars'",
        sliderInput(inputId = "finite_val_col_transparency", 
                    label = "Scale for colour transparency",
                    min = 0, max = 1, value = 0.2
        ), 
      )
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            withSpinner(plotOutput("finite_val_postprior_graph")), 
            withSpinner(plotOutput("finite_val_RB_graph"))
          )
        )
      ),
    )
  ),
  
  br(style = "line-height:1;"),
)

################################################################
# GRAPH 2 PAGE                                                 #
################################################################

finite_val_copt_plots = div( 
  titlePanel("Plots for the Optimal Cutoff"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "finite_val_c_opt_carry_colour",
                  label = "Select a colour theme",
                  choices = colour_theme_list_custom,
                  selected = 'default'
      ),      
      selectInput(inputId = "finite_val_c_opt_modify",
                  label = "Select which object to modify",
                  choices = list("Prior" = 'prior',
                                 "Posterior" = 'post',
                                 "Relative Belief Ratio" = 'rbr'),
                  selected = 'prior'
      ),
      conditionalPanel(
        condition = "input.finite_val_c_opt_modify == 'prior'",
        selectInput(inputId = "finite_val_priorc_opt_label", 
                    label = "Plot Symbol for Prior",
                    choices = default_copt_list,
                    selected = 3
        ),
        conditionalPanel(
          condition = "input.finite_val_c_opt_carry_colour == 'manual'",
          textInput(inputId = "finite_val_priorc_opt_colour",
                    label = 'Hex Colour for the Prior',
                    value = "065143"
          ), 
        )
      ),
      conditionalPanel(
        condition = "input.finite_val_c_opt_modify == 'post'",
        selectInput(inputId = "finite_val_postc_opt_label", 
                    label = "Plot Symbol for Posterior",
                    default_copt_list,
                    selected = 4
        ),
        conditionalPanel(
          condition = "input.finite_val_c_opt_carry_colour == 'manual'",
          textInput(inputId = "finite_val_postc_opt_colour",
                    label = 'Hex Colour for the Posterior',
                    value = "70B77E"
          ), 
        )
      ),
      conditionalPanel(
        condition = "input.finite_val_c_opt_modify == 'rbr'",
        selectInput(inputId = "finite_val_rbc_opt_label", 
                    label = "Plot Symbol for RB Ratio",
                    choices = default_copt_list,
                    selected = 8
        ),
        conditionalPanel(
          condition = "input.finite_val_c_opt_carry_colour == 'manual'",
          textInput(inputId = "finite_val_rbrc_opt_colour",
                    label = 'Hex Colour for the RB Ratio',
                    value = "CE1483"
          ),
        )
      ),
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            withSpinner(plotOutput("finite_val_postprior_copt_graph")), 
            withSpinner(plotOutput("finite_val_RB_copt_graph"))
          )
        )
      ),
    )
  ),
  
  br(style = "line-height:2;"),
)


################################################################
# DOWNLOAD PAGE                                                #
################################################################

finite_val_download_1 = div( 
  titlePanel("Download Prior & Posterior"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "finite_val_filename", 
                label = "Input File Name", 
                value = "Finite Value Diag Values"),
      #radioButtons(inputId = "finite_val_choosefile", "Choose Which Data to Download",
      #             choices = list("Prior" = 1, "Posterior" = 2),
      #             selected = 1),
      #actionButton('finite_val_prev_five', 'Previous Cols'),
      #actionButton('finite_val_next_five', 'Next Cols'),
      downloadButton("finite_val_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
               withSpinner(dataTableOutput("finite_val_dataframe"))
      )
    )
  )
)

finite_val_generate_dataframe = function(delta, AUC_prior, AUC_post, AUC_RBR){
  grid_pts = closed_bracket_grid(delta)
  AUC_prior_pts = c(0, grab_AUC_densities_breaks(delta, AUC_prior)$density)
  AUC_post_pts = c(0, grab_AUC_densities_breaks(delta, AUC_post)$density)
  df = data.frame(grid_pts, AUC_prior_pts, AUC_post_pts, AUC_RBR)
  colnames(df) = c("Grid point", "Prior of the AUC", "Posterior of the AUC", 
                   "Relative Belief Ratio of the AUC")
  return(df)
}

################################################################
# PAGE LOGIC                                                   #
################################################################

page_finite_val_inference1 = div(
  titlePanel("Finite-valued Diagnostic"),
  tabsetPanel(type = "tabs",
              tabPanel("Setup Values", finite_val_setup_variables_1),
              tabPanel("Inferences for the AUC", finite_val_hypothesizedAUC),
              tabPanel("Plots for the AUC", finite_val_plots),
              tabPanel("Download Prior & Posterior", finite_val_download_1),
  )
)

page_finite_val_inference2 = div(
  titlePanel("Finite-valued Diagnostic"),
  tabsetPanel(type = "tabs",
              #tabPanel("Setup Values", finite_val_setup_variables_2),
              tabPanel("Inferences for Optimal Cutoff", finite_val_plausible_region),
              tabPanel("Plots for the Optimal Cutoff", finite_val_copt_plots),
  )
)
