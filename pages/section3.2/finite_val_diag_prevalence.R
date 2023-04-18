################################################################
# DESCRIPTION PAGE                                             #
################################################################

finite_val_diag_prevalence_description = div( 
  titlePanel("Page Description"),
  p("The goal of this section is for the user to compute the prior of w, posterior of w, and the relative belief 
    ratio of w to estimate the prevalence w. The user is free to download any results."),
  p("Plot images can be saved by right clicking them."),
  hr(style = "border-top: 1px solid #363e4f;"),
  
  h4("Inputs and their Meanings"),
  p(HTML("<ul>
            <li><b>alpha1w:</b> The first parameter for beta. </li>
            <li><b>alpha2w:</b> The second parameter for beta. </li>
            <li><b>Total Sample Size:</b> The amount of  </li>
            <li><b>Total Diseased:</b> the total amount of \"diseased\" individuals from the
                   total sample size. </li>
            <li><b>Delta:</b> The distance that matters. It is the distance between any
                              two points on the grid. </li>
            <li><b>Gamma:</b> A value that's supposed to be less than the posterior content
                              of the plausible region.</li>
            <li><b>Hypothesis w = w0:</b> input what the user hypothesizes w to be. </li>
            <li><b>Input File Name:</b> The name of the file you want to download. The .csv file
                                        will include the grid points, the prior, the posterior,
                                        and the relative belief ratio. </li>
         </ul>")),
  
  h4("Outputs and their Meanings"),
  p(HTML("<ul>
            <li><b>plausible_region:</b> plausible region. PR = { w : RB(w | ... ) > 1 }</li>
            <li><b>RB_estimate_of_prevalence_w:</b> the chosen value for w (when RB(w | ... ) is maximized. 
            It is actually just n/nD.)</li>
            <li><b>relative_belief_ratio_at_w0:</b> directly calculated from RB(w0 | ... )</li>
            <li><b>w0_interval:</b> the region once RB(w | ... ) > RB(w0 | ... )</li>
            <li><b>strength:</b> given as π({w : RB(w | ... ) <= RB(w0 | ... )| ... }) 
            It is highlighted in the second graph.</li>
         </ul>")),
)

################################################################
# OUTPUT 1 PAGE                                                #
################################################################

finite_val_diag_prevalence_plausible_region = div( 
  titlePanel("Relative Belief Estimate of Prevalence w & Plausible Region"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "finite_val_diag_prevalence_n", 
                   label = 'Total Sample Size',
                   value = 100, min = 1
      ),
      numericInput(inputId = "finite_val_diag_prevalence_nD", 
                   label = 'Total Diseased',
                   value = 68, min = 0
      ),
      numericInput(inputId = "finite_val_diag_prevalence_delta", 
                   label = tags$p('Delta (the meaningful difference for the prevalence)', 
                           style = "font-size: 90%;"),
                   value = 0.001, min = 0, max = 1
      ),
    ),
    mainPanel(
      tabPanel("Plausible Region & Max w", 
               withSpinner((verbatimTextOutput("finite_val_diag_prevalence_values1")))
      ),
    )
  )
)

################################################################
# GRAPH 1 PAGE                                                 #
################################################################

finite_val_diag_prevalence_plots = div( 
  titlePanel("Plots"), 
  sidebarLayout(
    sidebarPanel(width = 3,
      textInput(inputId = "finite_val_diag_prevalence_gamma", 
                label = "Gamma (must be less than posterior content)", 
                value = "NA"),
      selectInput(inputId = "finite_val_diag_prevalence_colour", 
                  label = 'Select colour theme', 
                  choices = colour_theme_list, 
                  selected = 'default1'),
      conditionalPanel(
        condition = "input.finite_val_diag_prevalence_colour == 'manual'",
        selectInput(inputId = "finite_val_diag_prevalence_modify_colour",
                    label = 'Select line to modify',
                    choices = output_line_list,
                    selected = 'prior'), 
        
        conditionalPanel(
          condition = "input.finite_val_diag_prevalence_modify_colour == 'prior'",
          textInput(inputId = "finite_val_diag_prevalence_colour_prior",
                    label = 'Input the hex colour of the prior',
                    value = "FF007F"
          ),
          selectInput(inputId = "finite_val_diag_prevalence_lty_prior", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_diag_prevalence_modify_colour == 'post'",
          textInput(inputId = "finite_val_diag_prevalence_colour_post",
                    label = 'Input the hex colour of the posterior',
                    value = "FF00FF"
          ),
          selectInput(inputId = "finite_val_diag_prevalence_lty_post", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_diag_prevalence_modify_colour == 'rbr'",
          textInput(inputId = "finite_val_diag_prevalence_colour_rbr",
                    label = 'Input the hex colour of the relative belief ratio',
                    value = "7F00FF"
          ),
          selectInput(inputId = "finite_val_diag_prevalence_lty_rbr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_diag_prevalence_modify_colour == 'pr'",
          textInput(inputId = "finite_val_diag_prevalence_colour_pr",
                    label = 'Input the hex colour of the plausible region',
                    value = "A717DB"
          ),
          selectInput(inputId = "finite_val_diag_prevalence_lty_pr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_diag_prevalence_modify_colour == 'line_1'",
          textInput(inputId = "finite_val_diag_prevalence_colour_line_1",
                    label = 'Input the hex colour of the y = 1 line',
                    value = "5327E4"
          ), 
          selectInput(inputId = "finite_val_diag_prevalence_lty_line_1", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 2
          ),
        ),
        conditionalPanel(
          condition = "input.finite_val_diag_prevalence_modify_colour == 'cr'",
          textInput(inputId = "finite_val_diag_prevalence_colour_cr",
                    label = 'Input the hex colour of the credible region',
                    value = "650d84"
          ), 
          selectInput(inputId = "finite_val_diag_prevalence_lty_cr", 
                      label = 'Select a line type', 
                      choices = default_lty_list, 
                      selected = 3
          ),
        ),
      ),
      # Below consists of all of the different types of colour inputs! 
      # Switching back to modifying transparency
      sliderInput(inputId = "finite_val_diag_prevalence_col_transparency", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ),
    ),
    mainPanel(
      tabPanel("Plots",
        fluidRow(
          splitLayout(
            cellWidths = c("50%", "50%"), 
              withSpinner(plotOutput("finite_val_diag_prevalence_postprior_graph")), 
              withSpinner(plotOutput("finite_val_diag_prevalence_RB_graph"))
          )
        )
      ),
    )
  )
)

finite_val_diag_prevalence_plot_alt = div(
  titlePanel("Plot of the Prior of w"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "finite_val_diag_prevalence_delta_alt", 
                   label = tags$p('Delta (the meaningful difference for the prevalence)', 
                           style = "font-size: 87%;"),
                   value = 0.001, min = 0, max = 1
      ),
      selectInput(inputId = "finite_val_diag_prevalence_colour_1", 
                  label = 'Select a colour', 
                  choices = basic_colour_list, 
                  selected = 'red'
      ),
      selectInput(inputId = "finite_val_diag_prevalence_lty_1", 
                  label = 'Select a line type', 
                  choices = default_lty_list, 
                  selected = 2
      ),
      sliderInput(inputId = "finite_val_diag_prevalence_col_transparency_1", 
                  label = "Scale for colour transparency",
                  min = 0, max = 1, value = 0.1
      ),
      conditionalPanel(
        condition = "input.finite_val_diag_prevalence_colour_1 == 'manual'",
        textInput(inputId = "finite_val_diag_prevalence_colour_2",
                  label = 'Input the hex colour code.',
                  value = "AC2DE2"
        ),
      ),
    ),
    mainPanel(
      withSpinner(plotOutput("finite_val_diag_prevalence_post_graph_alt"))
    ),
  )
)


################################################################
# OUTPUT + GRAPH 2 PAGE                                        #
################################################################

finite_val_diag_prevalence_relative_belief_plot_of_w0 = div( 
  titlePanel("Test of w = w0"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "finite_val_diag_prevalence_w0",
                   label = 'Hypothesis w = w0',
                   value = 0.6)
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0",
        fluidRow(
          splitLayout(
            cellWidths = c("60%", "35%"), 
              withSpinner(plotOutput(outputId = "finite_val_diag_prevalence_w0_graph")), 
              withSpinner(verbatimTextOutput("finite_val_diag_prevalence_values2")),
          )
        )
      )
    )
  )
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

finite_val_diag_prevalence_download = div( 
  titlePanel("Download Output"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "finite_val_diag_prevalence_filename", 
                label = "Input File Name", 
                value = "Prior Post RBR of W"
      ),
      downloadButton("finite_val_diag_prevalence_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
        withSpinner(dataTableOutput("finite_val_diag_prevalence_dataframe"))
      )
    )
  )
)

finite_val_diag_prevalence_download_alt = div( 
  titlePanel("Download Output"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
      textInput(inputId = "finite_val_diag_prevalence_filename_alt", 
                label = "Input File Name", 
                value = "Prior Of W"
      ),
      downloadButton("finite_val_diag_prevalence_downloadData_alt", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", 
               withSpinner(dataTableOutput("finite_val_diag_prevalence_dataframe_alt"))
      )
    )
  )
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_finite_val_prevalence = div( 
  # This is the page that that connects to app.R
  titlePanel("The Prevalence"), 
  conditionalPanel(
    condition = "input.finite_val_diag_case1 == 1",
    
    fluidRow(
      align = "center",
      tags$script(src = "https://kit.fontawesome.com/5e940c3ade.js"),
      tags$div(id = "finite_val_diag_prevalence_arrow",
               tags$i(class = "fa-solid fa-circle-right", style = "font-size: 16rem;"),
               style = "padding:10rem;",
      ),
      h4("The prevalence is already known, so there is nothing to do here. Please proceed
      to inferences of the AUC (currently labelled as Section 3.2).")
    ),
    
  ),
  conditionalPanel(
    condition = "input.finite_val_diag_case1 == 2",
    
    conditionalPanel(
      condition = "input.finite_val_diag_case2 == 'A'",
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", finite_val_diag_prevalence_plot_alt),
                  tabPanel("Download Output", finite_val_diag_prevalence_download_alt)
      )
    ),
    
    conditionalPanel(
      condition = "input.finite_val_diag_case2 == 'B'",
      tabsetPanel(type = "tabs",
                  tabPanel("Description", finite_val_diag_prevalence_description), 
                  tabPanel("Relative Belief Estimate of w", finite_val_diag_prevalence_plausible_region),
                  tabPanel("Plots", finite_val_diag_prevalence_plots),
                  #tabPanel("Strength of w0", finite_val_diag_prevalence_Strength_of_w0),
                  tabPanel("Test of w = w0", finite_val_diag_prevalence_relative_belief_plot_of_w0),
                  tabPanel("Download Output", finite_val_diag_prevalence_download)
      )
    )
  )
)
