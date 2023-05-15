################################################################
# DESCRIPTION                                                  #
################################################################

nonpara_bayes_setup_variables_1 = div(
  titlePanel("Setup Values"),
  
  sidebarLayout(
    sidebarPanel(width = 4, 
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
        p("If you are adding multiple values, please separate them by a comma."),
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
      fluidPage( # CHANGE THIS
        
        fluidRow(
          column(3, h4("Hyperparameters of $H_{D}$:")),
          column(3, numericInput(inputId = "nonpara_bayes_muD",
                                 label = '$\\mu_{D}$',
                                 value = 45)),
          column(3, numericInput(inputId = "nonpara_bayes_sigma_squared", 
                                 label = '$\\sigma^{2}$',
                                 value = 0.5)),
        ),
        
        fluidRow(
          column(3, h4("Hyperparameters of DP:")),
          conditionalPanel(
            condition = "input.nonpara_bayes_DP_method == 'aD'",
            column(3, numericInput(inputId = "nonpara_bayes_aD", 
                                   label = "$a_{D}$",
                                   value = 9),),
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


################################################################
# OUTPUT 1 PAGE                                                #
################################################################

nonpara_bayes_plausible_region = div( 
  titlePanel("Inferences for Optimal Cutoff"),
  mainPanel(
    p("This is currently in progress. Come back later!")
    #tabPanel("Inferences for Optimal Cutoff", verbatimTextOutput("nonpara_bayes_output1")),
  )
)

################################################################
# GRAPH 1 PAGE (HISTOGRAM)                                     #
################################################################

nonpara_bayes_plots = div( 
  titlePanel("Plots for the AUC"), 
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput(inputId = "nonpara_bayes_hist_visual", label = "Choose Visual:",
                             c("With Bars" = "nonpara_bayes_withbars",
                               "Without Bars" = "nonpara_bayes_withoutbars"),
                             selected = "nonpara_bayes_withoutbars"),
                 
                 selectInput(inputId = "nonpara_bayes_colour", 
                             label = 'Select a colour', 
                             choices = list("Default Theme" = 'default', 
                                            "Manually Insert" = 'manual'), 
                             selected = 'default'),
                 
                 conditionalPanel(
                   condition = "input.nonpara_bayes_colour == 'manual'",
                   selectInput(inputId = "nonpara_bayes_modify_colour",
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
                   condition = "input.nonpara_bayes_modify_colour == 'prior'",
                   textInput(inputId = "nonpara_bayes_colour_prior",
                             label = 'Input the colour of the prior',
                             value = "FF007F"), 
                 ),
                 conditionalPanel(
                   condition = "input.nonpara_bayes_modify_colour == 'post'",
                   textInput(inputId = "nonpara_bayes_colour_post",
                             label = 'Input the colour of the posterior',
                             value = "FF00FF"), 
                 ),
                 conditionalPanel(
                   condition = "input.nonpara_bayes_modify_colour == 'rbr'",
                   textInput(inputId = "nonpara_bayes_colour_rbr",
                             label = 'Input the colour of the relative belief ratio',
                             value = "7F00FF"), 
                 ),
                 conditionalPanel(
                   condition = "input.nonpara_bayes_modify_colour == 'pr'",
                   textInput(inputId = "nonpara_bayes_colour_pr",
                             label = 'Input the colour of the plausible region',
                             value = "A717DB"), 
                 ),
                 conditionalPanel(
                   condition = "input.nonpara_bayes_modify_colour == 'line_1'",
                   textInput(inputId = "nonpara_bayes_colour_line_1",
                             label = 'Input the colour of the y = 1 line',
                             value = "5327E4"), 
                 ),
                 conditionalPanel(
                   condition = "input.nonpara_bayes_modify_colour == 'cr'",
                   textInput(inputId = "nonpara_bayes_colour_cr",
                             label = 'Input the colour of the credible region',
                             value = "650d84"), 
                 ),
                 
                 conditionalPanel(
                   condition = "input.nonpara_bayes_hist_visual == 'nonpara_bayes_withbars'",
                   sliderInput(inputId = "nonpara_bayes_col_transparency", 
                               label = "Scale for colour transparency",
                               min = 0, max = 1, value = 0.2), 
                 )
    ),
    mainPanel(
      p("This is currently being worked on. Come back later!"),
      #tabPanel("Plots",
      #         fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
      #                              plotOutput("nonpara_bayes_postprior_graph"), 
      #                              plotOutput("nonpara_bayes_RB_graph")))),
    )
  )
)

################################################################
# GRAPH 2 PAGE                                                 #
################################################################

default_copt_list = list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
                         "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10,
                         "11" = 11, "12" = 12, "13" = 13, "14" = 14, "15" = 15,
                         "16" = 16, "17" = 17, "18" = 18, "19" = 19, "20" = 20,
                         "21" = 21, "22" = 22, "23" = 23, "24" = 24, "25" = 25)

nonpara_bayes_copt_plots = div( 
  titlePanel("Plots for the Optimal Cutoff"), 
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput(inputId = "nonpara_bayes_c_opt_carry_colour",
                             label = "Select a colour theme",
                             list("Default Theme" = 'default',
                                  "Custom Theme from AUC Plots" = 'custom',
                                  "Manually Insert" = 'manual'),
                             selected = 'default'),      
                 selectInput(inputId = "nonpara_bayes_c_opt_modify",
                             label = "Select which object to modify",
                             list("Prior" = 'prior',
                                  "Posterior" = 'post',
                                  "Relative Belief Ratio" = 'rbr'),
                             selected = 'prior'
                 ),
                 
                 conditionalPanel(
                   condition = "input.nonpara_bayes_c_opt_modify == 'prior'",
                   selectInput(inputId = "nonpara_bayes_priorc_opt_label", 
                               label = "Plot Symbol for Prior",
                               default_copt_list,
                               selected = 3),
                   
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
                               label = "Plot Symbol for Posterior",
                               default_copt_list,
                               selected = 4),
                   conditionalPanel(
                     condition = "input.nonpara_bayes_c_opt_carry_colour == 'manual'",
                     textInput(inputId = "nonpara_bayes_postc_opt_colour",
                               label = 'Hex Colour for the Posterior',
                               value = "70B77E"), 
                   )
                 ),
                 conditionalPanel(
                   condition = "input.nonpara_bayes_c_opt_modify == 'rbr'",
                   selectInput(inputId = "nonpara_bayes_rbc_opt_label", 
                               label = "Plot Symbol for RB Ratio",
                               default_copt_list,
                               selected = 8),
                   
                   conditionalPanel(
                     condition = "input.nonpara_bayes_c_opt_carry_colour == 'manual'",
                     textInput(inputId = "nonpara_bayes_rbrc_opt_colour",
                               label = 'Hex Colour for the RB Ratio',
                               value = "CE1483"),
                   )
                 ),
                 
    ),
    mainPanel(
      p("This is currently being worked on. Come back later!"),
      #tabPanel("Plots",
      #         fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
      #                              plotOutput("nonpara_bayes_postprior_copt_graph"), 
      #                              plotOutput("nonpara_bayes_RB_copt_graph")))),
      
    )
  )
)

################################################################
# HYPOTHESIS TESTING                                           #
################################################################

nonpara_bayes_hypothesizedAUC = div( 
  titlePanel("Inferences for the AUC"),
  sidebarLayout(
    sidebarPanel(width = 3, 
                 numericInput(inputId = "nonpara_bayes_hypoAUC",
                              tags$p('Hypothesized AUC (greater than)', style = "font-size: 90%;"),value = 0.5),
    ),
    p("This is currently being worked on. Come back later!"),
    #mainPanel(
    #  tabPanel("Relative Belief Plot of w0", verbatimTextOutput("nonpara_bayes_hypoAUC_value")))
  )
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

nonpara_bayes_download_1 = div( 
  titlePanel("Download Prior & Posterior"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
                 textInput(inputId = "nonpara_bayes_filename", "Input File Name", value = "AUC Values"),
                 downloadButton("nonpara_bayes_downloadData", "Download"),
    ),
    mainPanel(
      p("This is a work in progress. Please come back later!")
      #tabPanel("Download Output", dataTableOutput("nonpara_bayes_dataframe"))
      
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
  colnames(df) = c("Grid point", "Prior of the AUC", "Posterior of the AUC", "Relative Belief Ratio of the AUC")
  return(df)
}

################################################################
# PAGE LOGIC                                                   #
################################################################

page_nonpara_bayes = div(
  titlePanel("Nonparametric Bayes Model"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Setup Variables", nonpara_bayes_setup_variables_1),
              #tabPanel("Description", nonpara_bayes_description),
              tabPanel("Inferences for the AUC", nonpara_bayes_hypothesizedAUC),
              tabPanel("Plots for the AUC", nonpara_bayes_plots),
              tabPanel("Inferences for Optimal Cutoff", nonpara_bayes_plausible_region),
              tabPanel("Plots for the Optimal Cutoff", nonpara_bayes_copt_plots),
              tabPanel("Download Prior & Posterior", nonpara_bayes_download_1),
  )
)