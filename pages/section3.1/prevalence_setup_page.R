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
  p("Here, since the posterior is not is not known, we can only show the graph of the prior."),
  hr(style = "border-top: 1px solid #363e4f;"),
  
  h4("Inputs and their Meanings"),
  p(HTML("<ul>
            <li><b>alpha1w:</b> The first parameter for beta. </li>
            <li><b>alpha2w:</b> The second parameter for beta. </li>
          </ul>")
  ),
  
)

################################################################
# OUTPUT 1 PAGE                                                #
################################################################

prevalence_setup_plausible_region = div( 
  titlePanel("Relative Belief Estimate of Prevalence w & Plausible Region"),
  sidebarLayout(
    sidebarPanel(width = 3, 
                 numericInput(inputId = "prevalence_setup_alpha1w", # CHANGE THIS
                              tags$p('alpha1w', style = "font-size: 90%;"),value = 391.72),
                 numericInput(inputId = "prevalence_setup_alpha2w", # CHANGE THIS
                              tags$p('alpha2w', style = "font-size: 90%;"),value = 211.39),
                 numericInput(inputId = "prevalence_setup_n", # CHANGE THIS
                              tags$p('Total Sample Size', style = "font-size: 90%;"),value = 100, min = 1),
                 numericInput(inputId = "prevalence_setup_nD", # CHANGE THIS
                              tags$p('Total Diseased', style = "font-size: 90%;"),value = 68, min = 0),
                 numericInput(inputId = "prevalence_setup_delta", 
                              tags$p("Delta"), value = 0.001, min = 0, max = 1),
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
                 textInput(inputId = "prevalence_setup_gamma", label = "Gamma (must be less than posterior content)", 
                           value = "NA")
                 #numericInput(inputId = "prevalence_setup_gamma", # Changed to text to deal with edge case where gamma is not chosen
                 #             tags$p('Gamma', style = "font-size: 90%;"),value = 0.8), # need to change value
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
                 numericInput(inputId = "prevalence_setup_delta", label = "Delta", 
                              value = 0.001, min = 0, max = 1),
                 numericInput(inputId = "prevalence_setup_alpha1w", # CHANGE THIS
                              tags$p('alpha1w', style = "font-size: 90%;"),value = 391.72),
                 numericInput(inputId = "prevalence_setup_alpha2w", # CHANGE THIS
                              tags$p('alpha2w', style = "font-size: 90%;"),value = 211.39),
    ),
    mainPanel(
      fluidRow(splitLayout(cellWidths = c("70%", "30%"), 
                           plotOutput("prevalence_setup_post_graph_alt"), 
                           verbatimTextOutput("prevalence_setup_prior_values"))),
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
                              tags$p('Hypothesis w = w0', style = "font-size: 90%;"),value = 0.6)
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
                 textInput(inputId = "RB_filename", "Input File Name", 
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
                 textInput(inputId = "RB_filename_alt", "Input File Name", 
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
      to section 3.2.")
  ),
  conditionalPanel(
    condition = "input.pick_case_1 == 'case_2_opt'",
    
    conditionalPanel(
      condition = "input.pick_case_2 == 'case_b_opt'",
      tabsetPanel(type = "tabs",
                  tabPanel("Description", prevalence_setup_description), 
                  tabPanel("Relative Estimate of w", prevalence_setup_plausible_region),
                  tabPanel("Plots", prevalence_setup_plots),
                  #tabPanel("Strength of w0", prevalence_setup_Strength_of_w0),
                  tabPanel("Test of w = w0", prevalence_setup_relative_belief_plot_of_w0),
                  tabPanel("Download Output", page_prevalence_download)
      )
    ),
    conditionalPanel(
      condition = "input.pick_case_2 == 'case_a_opt'",
      tabsetPanel(type = "tabs",
                  tabPanel("Description", prevalence_setup_description_alt),
                  tabPanel("Plot", prevalence_setup_plot_alt),
                  tabPanel("Download Output", page_prevalence_download_alt)
      )
    )
  )
)

################################################################
# MAIN FUNCTIONS                                               #
################################################################
