################################################################
# OUTPUT 1 PAGE                                                #
################################################################

binormal_diag_prevalence_plausible_region = div( 
  titlePanel("Relative Belief Estimate of Prevalence w & Plausible Region"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "binormal_diag_prevalence_gamma", 
                label = "Gamma for the credible region 
                (must be less than the posterior content of the plausible region)", 
                value = "NA"
      ),
      numericInput(inputId = "binormal_diag_prevalence_delta", 
                   label = tags$p('Delta (the meaningful difference for the prevalence)', 
                           style = "font-size: 90%;"),
                   value = 0.001, min = 0, max = 1
      ),
    ),
    mainPanel(
      tabPanel("Plausible Region & Max w", 
               withSpinner((verbatimTextOutput("binormal_diag_prevalence_values1")))
      ),
    )
  )
)

################################################################
# GRAPH 1 PAGE                                                 #
################################################################

binormal_diag_prevalence_plots = div( 
  titlePanel("Plots"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(inputId = "binormal_diag_prevalence_colour", 
                  label = 'Select colour theme', 
                  choices = colour_theme_list, 
                  selected = 'default1'
      ),
      conditionalPanel(
        condition = "input.binormal_diag_prevalence_colour == 'manual'",
        selectInput(inputId = "binormal_diag_prevalence_modify_colour",
                    label = 'Select line to modify',
                    choices = output_line_list,
                    selected = 'prior'
        ),
        
        # Below consists of all of the different types of colour inputs! 
        conditionalPanel(
          condition = "input.binormal_diag_prevalence_modify_colour == 'prior'",
          textInput(inputId = "binormal_diag_prevalence_colour_prior",
                    label = 'Input the hex colour of the prior',
                    value = "FF007F"
          ), 
          selectInput(inputId = "binormal_diag_prevalence_lty_prior", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_prevalence_modify_colour == 'post'",
          textInput(inputId = "binormal_diag_prevalence_colour_post",
                    label = 'Input the hex colour of the posterior',
                    value = "FF00FF"
          ),
          selectInput(inputId = "binormal_diag_prevalence_lty_post", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_prevalence_modify_colour == 'rbr'",
          textInput(inputId = "binormal_diag_prevalence_colour_rbr",
                    label = 'Input the hex colour of the relative belief ratio',
                    value = "7F00FF"
          ), 
          selectInput(inputId = "binormal_diag_prevalence_lty_rbr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_prevalence_modify_colour == 'line_1'",
          textInput(inputId = "binormal_diag_prevalence_colour_line_1",
                    label = 'Input the hex colour of the y = 1 line',
                    value = "5327E4"
          ),
          selectInput(inputId = "binormal_diag_prevalence_lty_line_1", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_prevalence_modify_colour == 'cr'",
          textInput(inputId = "binormal_diag_prevalence_colour_cr",
                    label = 'Input the hex colour of the credible region',
                    value = "650d84"), 
          selectInput(inputId = "binormal_diag_prevalence_lty_cr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
        
      ),
      # Switching back to modifying transparency
      sliderInput(inputId = "binormal_diag_prevalence_col_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ),
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
            withSpinner(plotOutput("binormal_diag_prevalence_postprior_graph")), 
            withSpinner(plotOutput("binormal_diag_prevalence_RB_graph"))
          )
        ),
      ),
    )
  )
)

binormal_diag_prevalence_plot_alt = div(
  titlePanel("Plot of the Prior of w"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "binormal_diag_prevalence_delta_alt", 
                   label = tags$p('Delta (the meaningful difference for the prevalence)', 
                           style = "font-size: 87%;"),
                   value = 0.001, min = 0, max = 1
      ),
      selectInput(inputId = "binormal_diag_prevalence_colour_1", 
                  label = 'Select a colour', 
                  choices = basic_colour_list, 
                 selected = 'red'
      ),
      selectInput(inputId = "binormal_diag_prevalence_lty_1", 
                  label = 'Select a line type', 
                  choices = default_lty_list, 
                  selected = 2
      ),
      sliderInput(inputId = "binormal_diag_prevalence_col_transparency_1", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ),
      conditionalPanel(
        condition = "input.binormal_diag_prevalence_colour_1 == 'manual'",
        textInput(inputId = "binormal_diag_prevalence_colour_2",
                  label = 'Input the hex colour code.',
                  value = "AC2DE2"
        ),
      ),
    ),
    mainPanel(
      withSpinner(plotOutput("binormal_diag_prevalence_post_graph_alt"))
    ),
  )
)


################################################################
# OUTPUT + GRAPH 2 PAGE                                        #
################################################################

binormal_diag_prevalence_relative_belief_plot_of_w0 = div( 
  titlePanel("Test of w = w0"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "binormal_diag_prevalence_w0",
                   label = 'Hypothesis w = w0',
                   value = 0.6
      ),
      ############################################################## START
      selectInput(inputId = "binormal_diag_diag_prevalence_colour_w0", 
                  label = 'Select colour theme', 
                  choices = colour_theme_list, 
                  selected = 'default1'),
      conditionalPanel(
        condition = "input.binormal_diag_diag_prevalence_colour_w0 == 'manual'",
        selectInput(inputId = "binormal_diag_diag_prevalence_modify_colour_w0",
                    label = 'Select object to modify',
                    choices = colour_theme_w0,
                    selected = 'rbr'), 
        
        conditionalPanel(
          condition = "input.binormal_diag_diag_prevalence_modify_colour_w0 == 'rbr'",
          textInput(inputId = "binormal_diag_diag_prevalence_colour_rbr_w0",
                    label = 'Input the hex colour of the relative belief ratio',
                    value = "FF0000"
          ),
          selectInput(inputId = "binormal_diag_diag_prevalence_lty_rbr_w0", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_diag_prevalence_modify_colour_w0 == 'rbr_w0'",
          textInput(inputId = "binormal_diag_diag_prevalence_colour_rbr_at_w0",
                    label = 'Input the hex colour of the relative belief ratio at w0',
                    value = "000080"
          ),
          selectInput(inputId = "binormal_diag_diag_prevalence_lty_rbr_at_w0", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_diag_prevalence_modify_colour_w0 == 'interval'",
          textInput(inputId = "binormal_diag_diag_prevalence_colour_interval",
                    label = 'Input the hex colour of the boundaries of the interval',
                    value = "7c83e8"
          ), 
          selectInput(inputId = "binormal_diag_diag_prevalence_lty_interval", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
        conditionalPanel(
          condition = "input.binormal_diag_diag_prevalence_modify_colour_w0 == 'str'",
          textInput(inputId = "binormal_diag_diag_prevalence_colour_str",
                    label = 'Input the hex colour of the strength',
                    value = "996DEC"
          ), 
        ),
      ),
      # Below consists of all of the different types of colour inputs! 
      # Switching back to modifying transparency
      sliderInput(inputId = "binormal_diag_diag_prevalence_col_transparency_w0", 
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
            withSpinner(plotOutput(outputId = "binormal_diag_prevalence_w0_graph")), 
            withSpinner(verbatimTextOutput("binormal_diag_prevalence_values2")),
          )
        )
      )
    )
  )
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

binormal_diag_prevalence_download = div( 
  titlePanel("Download Output"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "binormal_diag_prevalence_filename", 
                label = "Input File Name", 
                value = "Prior Post RBR of W"
      ),
      downloadButton("binormal_diag_prevalence_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
               withSpinner(dataTableOutput("binormal_diag_prevalence_dataframe"))
      )
    )
  )
)

binormal_diag_prevalence_download_alt = div( 
  titlePanel("Download Output"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "binormal_diag_prevalence_filename_alt", 
                label = "Input File Name", 
                value = "Prior Of W"
      ),
      downloadButton("binormal_diag_prevalence_downloadData_alt", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
               withSpinner(dataTableOutput("binormal_diag_prevalence_dataframe_alt"))
      )
    )
  )
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_binormal_diag_prevalence = div( 
  # This is the page that that connects to app.R
  titlePanel("The Prevalence"), 
  conditionalPanel(
    condition = "input.binormal_diag_case1 == 1",
    
    fluidRow(
      align = "center",
      tags$script(src = "https://kit.fontawesome.com/5e940c3ade.js"),
      tags$div(id = "binormal_diag_prevalence_arrow",
               tags$i(class = "fa-solid fa-circle-right", style = "font-size: 16rem;"),
               style = "padding:10rem;",
      ),
      h4("The prevalence is already known, so there is nothing to do here. Please proceed
      to inferences for the optimal cutoff.")
    ),
    
  ),
  conditionalPanel(
    condition = "input.binormal_diag_case1 == 2",
    
    conditionalPanel(
      condition = "input.binormal_diag_case2 == 'A'",
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", binormal_diag_prevalence_plot_alt),
                  tabPanel("Download Output", binormal_diag_prevalence_download_alt)
      )
    ),
    
    conditionalPanel(
      condition = "input.binormal_diag_case2 == 'B'",
      tabsetPanel(type = "tabs",
                  #tabPanel("Description", binormal_diag_prevalence_description), 
                  tabPanel("Relative Belief Estimate of w", binormal_diag_prevalence_plausible_region),
                  tabPanel("Plots", binormal_diag_prevalence_plots),
                  #tabPanel("Strength of w0", binormal_diag_prevalence_Strength_of_w0),
                  tabPanel("Test of w = w0", binormal_diag_prevalence_relative_belief_plot_of_w0),
                  tabPanel("Download Output", binormal_diag_prevalence_download)
      )
    )
  )
)
