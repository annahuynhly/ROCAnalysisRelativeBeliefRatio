################################################################
# SETUP VARIABLES & PICKING SAMPLING REGIME                    #
################################################################

binormal_val_setup_variables_alt = div( 
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
          column(3, numericInput(inputId = "binormal_val_nMonteCarlo", 
                                 label = 'Simulation Sample Size',
                                 value = 100000, min = 0)),
          column(3, numericInput(inputId = "binormal_val_delta", 
                                 label = 'Delta',
                                 value = 0.01)),
          column(3, numericInput(inputId = "binormal_val_lambda1", 
                                 label = 'lambda1',
                                 value = 1.787)),
        ),
        
        fluidRow(
          column(3, numericInput(inputId = "binormal_val_mu0", 
                                 label = 'mu0',
                                 value = 0)),
          column(3, numericInput(inputId = "binormal_val_tau0", 
                                 label = 'tau0',
                                 value = 0.5)),
          column(3, numericInput(inputId = "binormal_val_lambda2", 
                                 label = 'lambda2',
                                 value = 1.056)),
        ),
        
        fluidRow(
          column(3, numericInput(inputId = "binormal_val_nND",
                                 label = 'nND',
                                 value = 25)),
          column(3, numericInput(inputId = "binormal_val_meanND", 
                                 label = 'meanND',
                                 value = -0.072)),
          column(3, numericInput(inputId = "binormal_val_sND_squared", 
                                 label = 'sND squared',
                                 value = 19.638)),
        ),
        
        fluidRow(
          column(3, numericInput(inputId = "binormal_val_nD", 
                                 label = 'nD',
                                 value = 20)),
          column(3, numericInput(inputId = "binormal_val_meanD", 
                                 label = 'meanD',
                                 value = 0.976)),
          column(3, numericInput(inputId = "binormal_val_sD_squared", 
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

binormal_val_plausible_region = div( 
  titlePanel("Inferences for Optimal Cutoff"),
  mainPanel(
    p("This is currently in progress. Come back later!")
    #tabPanel("Inferences for Optimal Cutoff", verbatimTextOutput("binormal_val_output1")),
  )
)

################################################################
# GRAPH 1 PAGE (HISTOGRAM)                                     #
################################################################

binormal_val_plots = div( 
  titlePanel("Plots for the AUC"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "binormal_val_hist_visual", 
                  label = "Choose Visual:",
                  choices = list("With Bars" = "binormal_val_withbars",
                                 "Without Bars" = "binormal_val_withoutbars"),
                  selected = "binormal_val_withoutbars"),
                 
      selectInput(inputId = "binormal_val_colour", 
                  label = 'Select a colour', 
                  choices = list("Default Theme 1" = 'default1',
                                 "Default Theme 2" = 'default2',
                                 "Manually Insert" = 'manual'), 
                  selected = 'default'),
                 
      conditionalPanel(
        condition = "input.binormal_val_colour == 'manual'",
        selectInput(inputId = "binormal_val_modify_colour",
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
        condition = "input.binormal_val_modify_colour == 'prior'",
        textInput(inputId = "binormal_val_colour_prior",
                  label = 'Input the colour of the prior',
                  value = "FF007F"), 
      ),
      conditionalPanel(
        condition = "input.binormal_val_modify_colour == 'post'",
        textInput(inputId = "binormal_val_colour_post",
                  label = 'Input the colour of the posterior',
                  value = "FF00FF"), 
      ),
      conditionalPanel(
        condition = "input.binormal_val_modify_colour == 'rbr'",
        textInput(inputId = "binormal_val_colour_rbr",
                  label = 'Input the colour of the relative belief ratio',
                  value = "7F00FF"), 
      ),
      conditionalPanel(
        condition = "input.binormal_val_modify_colour == 'pr'",
        textInput(inputId = "binormal_val_colour_pr",
                  label = 'Input the colour of the plausible region',
                  value = "A717DB"), 
      ),
      conditionalPanel(
        condition = "input.binormal_val_modify_colour == 'line_1'",
        textInput(inputId = "binormal_val_colour_line_1",
                  label = 'Input the colour of the y = 1 line',
                  value = "5327E4"), 
      ),
      conditionalPanel(
        condition = "input.binormal_val_modify_colour == 'cr'",
        textInput(inputId = "binormal_val_colour_cr",
                  label = 'Input the colour of the credible region',
                  value = "650d84"), 
      ),
      conditionalPanel(
        condition = "input.binormal_val_hist_visual == 'binormal_val_withbars'",
        sliderInput(inputId = "binormal_val_col_transparency", 
                    label = "Scale for colour transparency",
                    min = 0, max = 1, value = 0.2), 
      )
    ),
    mainPanel(
      tabPanel("Plots",
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    withSpinner(plotOutput("binormal_val_postprior_graph")), 
                                    withSpinner(plotOutput("binormal_val_RB_graph"))))),
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

binormal_val_copt_plots = div( 
  titlePanel("Plots for the Optimal Cutoff"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "binormal_val_c_opt_carry_colour",
                  label = "Select a colour theme",
                  choices = list("Default Theme 1" = 'default1',
                                 "Default Theme 2" = 'default2',
                                 "Custom Theme from AUC Plots" = 'custom',
                                 "Manually Insert" = 'manual'),
                  selected = 'default'),      
      selectInput(inputId = "binormal_val_c_opt_modify",
                  label = "Select which object to modify",
                  choices = list("Prior" = 'prior',
                                 "Posterior" = 'post',
                                 "Relative Belief Ratio" = 'rbr'),
                 selected = 'prior'),
                 
      conditionalPanel(
        condition = "input.binormal_val_c_opt_modify == 'prior'",
        selectInput(inputId = "binormal_val_priorc_opt_label", 
                    label = "Plot Symbol for Prior",
                    choices = default_copt_list,
                    selected = 3),
                   
        conditionalPanel(
          condition = "input.binormal_val_c_opt_carry_colour == 'manual'",
          textInput(inputId = "binormal_val_priorc_opt_colour",
                    label = 'Hex Colour for the Prior',
                    value = "065143"),
          )
        ),
        conditionalPanel(
          condition = "input.binormal_val_c_opt_modify == 'post'",
          selectInput(inputId = "binormal_val_postc_opt_label", 
                      label = "Plot Symbol for Posterior",
                      choices = default_copt_list,
                      selected = 4),
          conditionalPanel(
            condition = "input.binormal_val_c_opt_carry_colour == 'manual'",
            textInput(inputId = "binormal_val_postc_opt_colour",
                      label = 'Hex Colour for the Posterior',
                      value = "70B77E"), 
          )
        ),
        conditionalPanel(
          condition = "input.binormal_val_c_opt_modify == 'rbr'",
          selectInput(inputId = "binormal_val_rbc_opt_label", 
                      label = "Plot Symbol for RB Ratio",
                      choices = default_copt_list,
                      selected = 8),
          conditionalPanel(
            condition = "input.binormal_val_c_opt_carry_colour == 'manual'",
            textInput(inputId = "binormal_val_rbrc_opt_colour",
                      label = 'Hex Colour for the RB Ratio',
                      value = "CE1483"),
          )
        ),
    ),
    mainPanel(
      p("This is currently being worked on. Come back later!"),
      #tabPanel("Plots",
      #         fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
      #                              plotOutput("binormal_val_postprior_copt_graph"), 
      #                              plotOutput("binormal_val_RB_copt_graph")))),
      
    )
  )
)

################################################################
# HYPOTHESIS TESTING                                           #
################################################################

binormal_val_hypothesizedAUC = div( 
  titlePanel("Inferences for the AUC"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "binormal_val_hypoAUC",
                   label = 'Hypothesized AUC (greater than)',
                   value = 0.5),
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0", 
               withSpinner(verbatimTextOutput("binormal_val_hypoAUC_value")))
    )
  )
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

binormal_val_download_1 = div( 
  titlePanel("Download Prior & Posterior"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
                 textInput(inputId = "binormal_val_filename", 
                           label = "Input File Name", 
                           value = "AUC Values"),
                 downloadButton("binormal_val_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
               withSpinner(dataTableOutput("binormal_val_dataframe"))
      )
    )
  )
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_binormal_val = div(
  titlePanel("Binormal-valued Diagnostic"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Setup Variables", binormal_val_setup_variables_alt),
              #tabPanel("Description", binormal_val_description),
              tabPanel("Inferences for the AUC", binormal_val_hypothesizedAUC),
              tabPanel("Plots for the AUC", binormal_val_plots),
              tabPanel("Inferences for Optimal Cutoff", binormal_val_plausible_region),
              tabPanel("Plots for the Optimal Cutoff", binormal_val_copt_plots),
              tabPanel("Download Prior & Posterior", binormal_val_download_1),
  )
)