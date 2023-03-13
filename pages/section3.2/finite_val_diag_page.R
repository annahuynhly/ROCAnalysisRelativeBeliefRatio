################################################################
# DESCRIPTION                                                  #
################################################################

finite_val_setup_variables = fluidPage(
  titlePanel("Setup Variables"),
  
  fluidRow(
    column(3, numericInput(inputId = "finite_val_nND",
                           label = 'Total Non-Diseased',
                           value = 50, min = 1)),
    column(3, numericInput(inputId = "finite_val_nD", 
                           label = 'Total Diseased',
                           value = 100, min = 1)),
    column(3, numericInput(inputId = "finite_val_nMonteCarlo", 
                           label = 'Monte Carlo (Simulation) Sample Size',
                           value = 100000, min = 0)),
  ),
  
  fluidRow(
    column(3, textInput(inputId = "finite_val_alpha_ND",
                        label = 'alphaND1, ..., alphaNDm',
                        value = "1, 1, 1, 1, 1")),
    column(3, textInput(inputId = "finite_val_alpha_D",
                        label = 'alphaD1, ..., alphaDm',
                        value = "1, 1, 1, 1, 1")),
    column(3, textInput(inputId = "finite_val_fND",
                        label = 'fNd',
                        value = "29, 7, 4, 5, 5")),
  ),
  
  fluidRow(
    column(3, textInput(inputId = "finite_val_fD",
                        label = 'fD',
                        value = "14, 7, 25, 33, 21")),
    column(3, numericInput(inputId = "finite_val_delta", 
                           label = "Delta", 
                           value = 0.04, min = 0, max = 1)),
    column(3, textInput(inputId = "finite_val_gamma", 
                        label = tags$p("Gamma (must be less than posterior content)", 
                                style = "font-size: 95%"), 
                        value = "NA")),
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
                              value = 0.5),
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
                   c("With Bars" = "finite_val_withbars",
                     "Without Bars" = "finite_val_withoutbars"),
                   selected = "finite_val_withoutbars"),
      
      selectInput(inputId = "finite_val_colour", 
                  label = 'Select a colour', 
                  choices = list("Default Theme 1" = 'default1',
                                 "Default Theme 2" = 'default2',
                                 "Manually Insert" = 'manual'), 
                  selected = 'default'),
      
      conditionalPanel(
        condition = "input.finite_val_colour == 'manual'",
        selectInput(inputId = "finite_val_modify_colour",
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
        condition = "input.finite_val_modify_colour == 'prior'",
        textInput(inputId = "finite_val_colour_prior",
                  label = 'Input the colour of the prior',
                  value = "FF007F"), 
      ),
      conditionalPanel(
        condition = "input.finite_val_modify_colour == 'post'",
        textInput(inputId = "finite_val_colour_post",
                  label = 'Input the colour of the posterior',
                  value = "FF00FF"), 
      ),
      conditionalPanel(
        condition = "input.finite_val_modify_colour == 'rbr'",
        textInput(inputId = "finite_val_colour_rbr",
                  label = 'Input the colour of the relative belief ratio',
                  value = "7F00FF"), 
      ),
      conditionalPanel(
        condition = "input.finite_val_modify_colour == 'pr'",
        textInput(inputId = "finite_val_colour_pr",
                  label = 'Input the colour of the plausible region',
                  value = "A717DB"), 
      ),
      conditionalPanel(
        condition = "input.finite_val_modify_colour == 'line_1'",
        textInput(inputId = "finite_val_colour_line_1",
                  label = 'Input the colour of the y = 1 line',
                  value = "5327E4"), 
      ),
      conditionalPanel(
        condition = "input.finite_val_modify_colour == 'cr'",
        textInput(inputId = "finite_val_colour_cr",
                  label = 'Input the colour of the credible region',
                  value = "650d84"), 
      ),
      
      conditionalPanel(
        condition = "input.finite_val_hist_visual == 'finite_val_withbars'",
        sliderInput(inputId = "finite_val_col_transparency", 
                    label = "Scale for colour transparency",
                    min = 0, max = 1, value = 0.2), 
      )
    ),
    mainPanel(
      tabPanel("Plots",
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    withSpinner(plotOutput("finite_val_postprior_graph")), 
                                    withSpinner(plotOutput("finite_val_RB_graph"))))),
    )
  ),
  br(style = "line-height:1;"),
)

################################################################
# GRAPH 2 PAGE                                                 #
################################################################

# Note: base R : pch values go from 1 to 25
default_copt_list = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                      "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10,
                      "11" = 11, "12" = 12, "13" = 13, "14" = 14, "15" = 15,
                      "16" = 16, "17" = 17, "18" = 18, "19" = 19, "20" = 20,
                      "21" = 21, "22" = 22, "23" = 23, "24" = 24, "25" = 25)

finite_val_copt_plots = div( 
  titlePanel("Plots for the Optimal Cutoff"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "finite_val_c_opt_carry_colour",
                  label = "Select a colour theme",
                  list("Default Theme 1" = 'default1',
                       "Default Theme 2" = 'default2',
                       "Custom Theme from AUC Plots" = 'custom',
                       "Manually Insert" = 'manual'),
                       selected = 'default'),      
      selectInput(inputId = "finite_val_c_opt_modify",
                  label = "Select which object to modify",
                  list("Prior" = 'prior',
                       "Posterior" = 'post',
                       "Relative Belief Ratio" = 'rbr'),
                  selected = 'prior'
                  ),
                 
      conditionalPanel(
        condition = "input.finite_val_c_opt_modify == 'prior'",
        selectInput(inputId = "finite_val_priorc_opt_label", 
                    label = "Plot Symbol for Prior",
                    default_copt_list,
                    selected = 3),
        
        conditionalPanel(
          condition = "input.finite_val_c_opt_carry_colour == 'manual'",
          textInput(inputId = "finite_val_priorc_opt_colour",
                    label = 'Hex Colour for the Prior',
                    value = "065143"), 
        )
      ),
      conditionalPanel(
        condition = "input.finite_val_c_opt_modify == 'post'",
        selectInput(inputId = "finite_val_postc_opt_label", 
                    label = "Plot Symbol for Posterior",
                    default_copt_list,
                    selected = 4),
        conditionalPanel(
          condition = "input.finite_val_c_opt_carry_colour == 'manual'",
          textInput(inputId = "finite_val_postc_opt_colour",
                    label = 'Hex Colour for the Posterior',
                    value = "70B77E"), 
        )
      ),
      conditionalPanel(
        condition = "input.finite_val_c_opt_modify == 'rbr'",
        selectInput(inputId = "finite_val_rbc_opt_label", 
                    label = "Plot Symbol for RB Ratio",
                    default_copt_list,
                    selected = 8),
        
        conditionalPanel(
          condition = "input.finite_val_c_opt_carry_colour == 'manual'",
          textInput(inputId = "finite_val_rbrc_opt_colour",
                    label = 'Hex Colour for the RB Ratio',
                    value = "CE1483"),
        )
      ),
  
    ),
    mainPanel(
      tabPanel("Plots",
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    withSpinner(plotOutput("finite_val_postprior_copt_graph")), 
                                    withSpinner(plotOutput("finite_val_RB_copt_graph"))))),
      
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

page_finite_val = div(
  titlePanel("Finite-valued Diagnostic"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Setup Variables", finite_val_setup_variables),
              #tabPanel("Description", finite_val_description),
              tabPanel("Inferences for the AUC", finite_val_hypothesizedAUC),
              tabPanel("Plots for the AUC", finite_val_plots),
              tabPanel("Inferences for Optimal Cutoff", finite_val_plausible_region),
              tabPanel("Plots for the Optimal Cutoff", finite_val_copt_plots),
              tabPanel("Download Prior & Posterior", finite_val_download_1),
  )
)
