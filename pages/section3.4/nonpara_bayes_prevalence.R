################################################################
# INPUT VALUES PAGE                                            #
################################################################

nonpara_bayes_prevalence_plausible_region = div( 
  titlePanel("Relative Belief Estimate of Prevalence w & Plausible Region"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "nonpara_bayes_prevalence_delta", 
                   label = tags$p('Delta (the meaningful difference for the prevalence)', 
                           style = "font-size: 90%;"),
                   value = 0.001, min = 0, max = 1
      ),
      textInput(inputId = "nonpara_bayes_prevalence_gamma", 
                label = "Gamma (must be less than posterior content)", 
                value = "NA"
      ),
    ),
    mainPanel(
      tabPanel("Plausible Region & Max w", 
               withSpinner((verbatimTextOutput("nonpara_bayes_prevalence_values1")))
      ),
    )
  )
)

################################################################
# PREVALENCE PLOTS PAGE                                        #
################################################################

nonpara_bayes_prevalence_plots = div( 
  titlePanel("Plots"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "nonpara_bayes_prevalence_colour", 
                  label = 'Select colour theme', 
                  choices = colour_theme_list, 
                  selected = 'default1'
      ),
      selectInput(inputId = "nonpara_bayes_prevalence_legend_position",
                  label = "Select the position of the legends",
                  choices = default_legend_position_list,
                  selected = "topleft"),
      conditionalPanel(
        condition = "input.nonpara_bayes_prevalence_colour == 'manual'",
        selectInput(inputId = "nonpara_bayes_prevalence_modify_colour",
                    label = 'Select line to modify',
                    choices = output_line_list,
                    selected = 'prior'), 
        
        # Below consists of all of the different types of colour inputs! 
        conditionalPanel(
          condition = "input.nonpara_bayes_prevalence_modify_colour == 'prior'",
          textInput(inputId = "nonpara_bayes_prevalence_colour_prior",
                    label = 'Input the hex colour of the prior',
                    value = "FF007F"
          ), 
          selectInput(inputId = "nonpara_bayes_prevalence_lty_prior", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_prevalence_modify_colour == 'post'",
          textInput(inputId = "nonpara_bayes_prevalence_colour_post",
                    label = 'Input the hex colour of the posterior',
                    value = "FF00FF"
          ), 
          selectInput(inputId = "nonpara_bayes_prevalence_lty_post", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_prevalence_modify_colour == 'rbr'",
          textInput(inputId = "nonpara_bayes_prevalence_colour_rbr",
                    label = 'Input the hex colour of the relative belief ratio',
                    value = "7F00FF"
          ), 
          selectInput(inputId = "nonpara_bayes_prevalence_lty_rbr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_prevalence_modify_colour == 'line_1'",
          textInput(inputId = "nonpara_bayes_prevalence_colour_line_1",
                    label = 'Input the hex colour of the y = 1 line',
                    value = "5327E4"
          ), 
          selectInput(inputId = "nonpara_bayes_prevalence_lty_line_1", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_prevalence_modify_colour == 'cr'",
          textInput(inputId = "nonpara_bayes_prevalence_colour_cr",
                    label = 'Input the hex colour of the credible region',
                    value = "650d84"
          ), 
          selectInput(inputId = "nonpara_bayes_prevalence_lty_cr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
        
      ),
      
      # Switching back to modifying transparency
      sliderInput(inputId = "nonpara_bayes_prevalence_col_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ),
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            withSpinner(plotOutput("nonpara_bayes_prevalence_postprior_graph")), 
            withSpinner(plotOutput("nonpara_bayes_prevalence_RB_graph"))
          )
        ),
      ),
    )
  )
)

nonpara_bayes_prevalence_plot_alt = div(
  titlePanel("Plot of the Prior of w"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "nonpara_bayes_prevalence_delta_alt", 
                   label = tags$p('Delta (the meaningful difference for the prevalence)', 
                           style = "font-size: 87%;"),
                   value = 0.001, min = 0, max = 1
      ),
      selectInput(inputId = "nonpara_bayes_prevalence_colour_1", 
                  label = 'Select a colour', 
                  choices = basic_colour_list, 
                  selected = 'red'
      ),
      selectInput(inputId = "nonpara_bayes_prevalence_lty_1", 
                  label = 'Select a line type', 
                  choices = default_lty_list, 
                  selected = 2
      ),
      sliderInput(inputId = "nonpara_bayes_prevalence_col_transparency_1", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ),
      conditionalPanel(
        condition = "input.nonpara_bayes_prevalence_colour_1 == 'manual'",
        textInput(inputId = "nonpara_bayes_prevalence_colour_2",
                  label = 'Input the hex colour code.',
                  value = "AC2DE2"
        ),
      ),
    ),
    mainPanel(
      withSpinner(plotOutput("nonpara_bayes_prevalence_post_graph_alt"))
    ),
  )
)


################################################################
# PLOT ILLUSTRATING TEST OF W = W0                             #
################################################################

nonpara_bayes_prevalence_relative_belief_plot_of_w0 = div( 
  titlePanel("Test of w = w0"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "nonpara_bayes_prevalence_w0",
                   label = 'Hypothesis w = w0',
                   value = 0.6
      ),
      ############################################################## START
      selectInput(inputId = "nonpara_bayes_diag_prevalence_colour_w0", 
                  label = 'Select colour theme', 
                  choices = colour_theme_list, 
                  selected = 'default1'),
      selectInput(inputId = "nonpara_bayes_prevalence_legend_position_w0",
                  label = "Select the position of the legends",
                  choices = default_legend_position_list,
                  selected = "topleft"),
      conditionalPanel(
        condition = "input.nonpara_bayes_diag_prevalence_colour_w0 == 'manual'",
        selectInput(inputId = "nonpara_bayes_diag_prevalence_modify_colour_w0",
                    label = 'Select object to modify',
                    choices = colour_theme_w0,
                    selected = 'rbr'), 
        
        conditionalPanel(
          condition = "input.nonpara_bayes_diag_prevalence_modify_colour_w0 == 'rbr'",
          textInput(inputId = "nonpara_bayes_diag_prevalence_colour_rbr_w0",
                    label = 'Input the hex colour of the relative belief ratio',
                    value = "FF0000"
          ),
          selectInput(inputId = "nonpara_bayes_diag_prevalence_lty_rbr_w0", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_diag_prevalence_modify_colour_w0 == 'rbr_w0'",
          textInput(inputId = "nonpara_bayes_diag_prevalence_colour_rbr_at_w0",
                    label = 'Input the hex colour of the relative belief ratio at w0',
                    value = "000080"
          ),
          selectInput(inputId = "nonpara_bayes_diag_prevalence_lty_rbr_at_w0", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_diag_prevalence_modify_colour_w0 == 'interval'",
          textInput(inputId = "nonpara_bayes_diag_prevalence_colour_interval",
                    label = 'Input the hex colour of the boundaries of the interval',
                    value = "7c83e8"
          ), 
          selectInput(inputId = "nonpara_bayes_diag_prevalence_lty_interval", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
        conditionalPanel(
          condition = "input.nonpara_bayes_diag_prevalence_modify_colour_w0 == 'str'",
          textInput(inputId = "nonpara_bayes_diag_prevalence_colour_str",
                    label = 'Input the hex colour of the strength',
                    value = "996DEC"
          ), 
        ),
      ),
      # Below consists of all of the different types of colour inputs! 
      # Switching back to modifying transparency
      sliderInput(inputId = "nonpara_bayes_diag_prevalence_col_transparency_w0", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.3
      ),
      
      ############################################################# END
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0",
        fluidRow(
          splitLayout(
            cellWidths = c("60%", "35%"), 
            withSpinner(plotOutput(outputId = "nonpara_bayes_prevalence_w0_graph")), 
            withSpinner(verbatimTextOutput("nonpara_bayes_prevalence_values2")),
          )
        )
      )
    )
  )
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

nonpara_bayes_prevalence_download = div( 
  titlePanel("Download Output"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "nonpara_bayes_prevalence_filename", 
                label = "Input file name", 
                value = "Prior Post RBR of W"
      ),
      downloadButton("nonpara_bayes_prevalence_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
               withSpinner(dataTableOutput("nonpara_bayes_prevalence_dataframe"))
      )
    )
  )
)

nonpara_bayes_prevalence_download_alt = div( 
  titlePanel("Download Output"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "nonpara_bayes_prevalence_filename_alt", 
                label = "Input File Name", 
                value = "Prior Of W"),
      downloadButton("nonpara_bayes_prevalence_downloadData_alt", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
               withSpinner(dataTableOutput("nonpara_bayes_prevalence_dataframe_alt"))
      )
    )
  )
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_nonpara_bayes_prevalence = div( 
  titlePanel("The Prevalence"), 
  conditionalPanel(
    condition = "input.nonpara_bayes_case1 == 1",
    
    fluidRow(
      align = "center",
      tags$script(src = "https://kit.fontawesome.com/5e940c3ade.js"),
      tags$div(id = "nonpara_bayes_prevalence_arrow",
               tags$i(class = "fa-solid fa-circle-right", style = "font-size: 16rem;"),
               style = "padding:10rem;",
      ),
      h4("The prevalence is already known, so there is nothing to do here. Please proceed
      to inferences for the optimal cutoff.")
    ),
    
  ),
  conditionalPanel(
    condition = "input.nonpara_bayes_case1 == 2",
    
    conditionalPanel(
      condition = "input.nonpara_bayes_case2 == 'A'",
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", nonpara_bayes_prevalence_plot_alt),
                  tabPanel("Download Output", nonpara_bayes_prevalence_download_alt)
      )
    ),
    
    conditionalPanel(
      condition = "input.nonpara_bayes_case2 == 'B'",
      tabsetPanel(type = "tabs",
                  #tabPanel("Description", nonpara_bayes_prevalence_description), 
                  tabPanel("Relative Belief Estimate of w", nonpara_bayes_prevalence_plausible_region),
                  tabPanel("Plots", nonpara_bayes_prevalence_plots),
                  #tabPanel("Strength of w0", nonpara_bayes_prevalence_Strength_of_w0),
                  tabPanel("Test of w = w0", nonpara_bayes_prevalence_relative_belief_plot_of_w0),
                  tabPanel("Download Output", nonpara_bayes_prevalence_download)
      )
    )
  )
)
