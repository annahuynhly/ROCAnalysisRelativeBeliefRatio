################################################################
# DESCRIPTION PAGE                                             #
################################################################

prevalence_setup_description = div( 
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

prevalence_setup_description_alt = div(
  titlePanel("Page Description"),
  
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "prevalence_setup_alpha1w", 
                   label = 'alpha1w',
                   value = 391.72),
      numericInput(inputId = "prevalence_setup_alpha2w", 
                   label = 'alpha2w',
                   value = 211.39),
    ),
    mainPanel(
      p("Here, since the posterior is not is not known, we can only show the graph of the prior."),
      hr(style = "border-top: 1px solid #363e4f;"),
      
      h4("Inputs and their Meanings"),
      p(HTML("<ul>
            <li><b>alpha1w:</b> The first parameter for beta. </li>
            <li><b>alpha2w:</b> The second parameter for beta. </li>
          </ul>"))
    ),
  )
)

################################################################
# OUTPUT 1 PAGE                                                #
################################################################

prevalence_setup_plausible_region = div( 
  titlePanel("Relative Belief Estimate of Prevalence w & Plausible Region"),
  sidebarLayout(
    sidebarPanel(width = 3, 
      numericInput(inputId = "prevalence_setup_n", 
                   label = 'Total Sample Size',
                   value = 100, min = 1),
      numericInput(inputId = "prevalence_setup_nD", 
                   label = 'Total Diseased',
                   value = 68, min = 0),
      numericInput(inputId = "prevalence_setup_delta", 
                   label = tags$p('Delta (the meaningful difference for the prevalence)', 
                                  style = "font-size: 90%;"),
                   value = 0.001, min = 0, max = 1),
    ),
    mainPanel(
      tabPanel("Plausible Region & Max w", verbatimTextOutput("prevalence_setup_values1")),
    )
  )
)

################################################################
# GRAPH 1 PAGE                                                 #
################################################################

prevalence_setup_plots = div( 
  titlePanel("Plots"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
                 textInput(inputId = "prevalence_setup_gamma", 
                           label = "Gamma (must be less than posterior content)", 
                           value = "NA"),
                 selectInput(inputId = "prevalence_setup_colour", 
                             label = 'Select colour theme', 
                             choices = list("Default Theme" = 'default',
                                            "Dull Life" = 'dull',
                                            "Lovely Mei" = 'lovelymei',
                                            "Manually Insert" = 'manual'), # TO ADD: let the user pick
                             selected = 'default'),
                 
                 conditionalPanel(
                   condition = "input.prevalence_setup_colour == 'manual'",
                   selectInput(inputId = "prevalence_setup_modify_colour",
                               label = 'Select line to modify',
                               choices = list("Prior" = 'prior',
                                              "Posterior" = 'post',
                                              "Relative Belief Ratio" = 'rbr',
                                              "Plausible Region" = 'pr',
                                              "Line of y = 1" = 'line_1',
                                              "Credible Region" = 'cr'),
                               selected = 'prior'), 
                 ),
                # These consist of all of the colour inputs
                 conditionalPanel(
                   condition = "input.prevalence_setup_modify_colour == 'prior'",
                   textInput(inputId = "prevalence_setup_colour_prior",
                             label = 'Input the hex colour of the prior',
                             value = "FF007F"), 
                 ),
                 conditionalPanel(
                   condition = "input.prevalence_setup_modify_colour == 'post'",
                   textInput(inputId = "prevalence_setup_colour_post",
                             label = 'Input the hex colour of the posterior',
                             value = "FF00FF"), 
                 ),
                 conditionalPanel(
                   condition = "input.prevalence_setup_modify_colour == 'rbr'",
                   textInput(inputId = "prevalence_setup_colour_rbr",
                             label = 'Input the hex colour of the relative belief ratio',
                             value = "7F00FF"), 
                 ),
                 conditionalPanel(
                   condition = "input.prevalence_setup_modify_colour == 'pr'",
                   textInput(inputId = "prevalence_setup_colour_pr",
                             label = 'Input the hex colour of the plausible region',
                             value = "A717DB"), 
                 ),
                 conditionalPanel(
                   condition = "input.prevalence_setup_modify_colour == 'line_1'",
                   textInput(inputId = "prevalence_setup_colour_line_1",
                             label = 'Input the hex colour of the y = 1 line',
                             value = "5327E4"), 
                 ),
                 conditionalPanel(
                   condition = "input.prevalence_setup_modify_colour == 'cr'",
                   textInput(inputId = "prevalence_setup_colour_cr",
                             label = 'Input the hex colour of the credible region',
                             value = "650d84"), 
                 ),
                # Switching back to modifying transparency
                 sliderInput(inputId = "prevalence_setup_col_transparency", 
                             label = "Scale for colour transparency",
                             min = 0, max = 1, value = 0.1),
    ),
    mainPanel(
      tabPanel("Plots",
               fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                    plotOutput("prevalence_setup_postprior_graph"), 
                                    plotOutput("prevalence_setup_RB_graph")))),
    )
  )
)

prevalence_setup_plot_alt = div(
  titlePanel("Plot of the Prior of w"),
  sidebarLayout(
    sidebarPanel(width = 3, 
                 numericInput(inputId = "prevalence_setup_delta", 
                              label = tags$p('Delta (the meaningful difference for the prevalence)', 
                                             style = "font-size: 87%;"),
                              #label = "Delta = the meaningful difference for the prevalence", 
                              value = 0.001, min = 0, max = 1),
                 selectInput(inputId = "prevalence_setup_colour_1", 
                             label = 'Select a colour', 
                             choices = list("Red" = 'red', 
                                            "Blue" = 'blue', 
                                            "Green" = 'green',
                                            "Manually Insert" = 'manual'), # TO ADD: let the user pick their own 
                             selected = 'red'),
                 sliderInput(inputId = "prevalence_setup_col_transparency_1", 
                             label = "Scale for colour transparency",
                             min = 0, max = 1, value = 0.1),
                 conditionalPanel(
                   condition = "input.prevalence_setup_colour_1 == 'manual'",
                   textInput(inputId = "prevalence_setup_colour_2",
                             label = 'Input the hex colour code.',
                             value = "AC2DE2"), 
                   
                 ),
    ),
    mainPanel(
      plotOutput("prevalence_setup_post_graph_alt")
      #fluidRow(splitLayout(cellWidths = c("70%", "30%"), 
      #                     plotOutput("prevalence_setup_post_graph_alt"), 
      #                     verbatimTextOutput("prevalence_setup_prior_values"))),
    ),
  )
)


################################################################
# OUTPUT + GRAPH 2 PAGE                                        #
################################################################

prevalence_setup_relative_belief_plot_of_w0 = div( 
  titlePanel("Test of w = w0"),
  sidebarLayout(
    sidebarPanel(width = 3, 
                 numericInput(inputId = "prevalence_setup_w0",
                              label = 'Hypothesis w = w0',
                              value = 0.6)
    ),
    mainPanel(
      tabPanel("Relative Belief Plot of w0",
               fluidRow(splitLayout(cellWidths = c("60%", "35%"), 
                                    plotOutput(outputId = "prevalence_setup_w0_graph"), 
                                    verbatimTextOutput("prevalence_setup_values2")))),
    )
  )
)

################################################################
# DOWNLOAD PAGE                                                #
################################################################

page_prevalence_download = div( 
  titlePanel("Download Output"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
                 textInput(inputId = "RB_filename", 
                           label = "Input File Name", 
                           value = "PriorPostRelativeBeliefRatio"),
                 downloadButton("RB_downloadData", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", dataTableOutput("RB_dataframe"))
    )
  )
)

page_prevalence_download_alt = div( 
  titlePanel("Download Output"), 
  sidebarLayout(
    sidebarPanel(width = 3, 
                 textInput(inputId = "RB_filename_alt", 
                           label = "Input File Name", 
                           value = "PriorOfW"),
                 downloadButton("RB_downloadData_alt", "Download"),
    ),
    mainPanel(
      tabPanel("Download Output", dataTableOutput("RB_dataframe_alt"))
    )
  )
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_prevalence_setup = div( 
  # This is the page that that connects to app.R
  titlePanel("The Prevalence"), 
  conditionalPanel(
    condition = "input.pick_case_1 == 'case_1_opt'",
    p("The prevalence is already known, so there is nothing to do here. Please proceed
      to inferences of the AUC (currently labelled as Section 3.2).")
  ),
  conditionalPanel(
    condition = "input.pick_case_1 == 'case_2_opt'",
    
    conditionalPanel(
      condition = "input.pick_case_2 == 'case_b_opt'",
      tabsetPanel(type = "tabs",
                  tabPanel("Description", prevalence_setup_description), 
                  tabPanel("Relative Belief Estimate of w", prevalence_setup_plausible_region),
                  tabPanel("Plots", prevalence_setup_plots),
                  #tabPanel("Strength of w0", prevalence_setup_Strength_of_w0),
                  tabPanel("Test of w = w0", prevalence_setup_relative_belief_plot_of_w0),
                  tabPanel("Download Output", page_prevalence_download)
      )
    ),
    conditionalPanel(
      condition = "input.pick_case_2 == 'case_a_opt'",
      tabsetPanel(type = "tabs",
                  #tabPanel("Description", prevalence_setup_description_alt),
                  tabPanel("Plot", prevalence_setup_plot_alt),
                  tabPanel("Download Output", page_prevalence_download_alt)
      )
    )
  )
)

