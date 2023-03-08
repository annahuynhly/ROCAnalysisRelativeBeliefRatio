################################################################
# DESCRIPTION                                                  #
################################################################

sect3.2_def_description = div(
  titlePanel("Page Description"),
  mainPanel(
      p("This tab lists the meaning behind the inputs and the outputs listed
        in the AUC page under section 3.2."),
  ),
  br(style = "line-height:22;")
)

################################################################
# DESCRIPTION OF INPUTS                                        #
################################################################

sect3.2_def_inputs = div(
  titlePanel("Inputs and their Meanings"),
  mainPanel(
    p("Inputs refer to the users inputs found on the AUC page from section 3.2."),
    
    tableHTML_output("input_table_description"),
    
    #h4("Inputs and their Meanings"),
    #p(HTML("<ul>
    #        <li><b>Total Non-Diseased:</b> The amount of \"non-diseased\" individuals from
    #               the total sample size. </li>
    #        <li><b>Total Diseased:</b> the total amount of \"diseased\" individuals from the
    #               total sample size. </li>
    #        <li><b>Monte Carlo (Simulation) Sample Size:</b> the sample size to use the Monte Carlo method
    #               to simulate the prior and the posterior. When computations are taking too long,
    #               it is recommended to lower the sample size. </li>
    #        
    #        <li><b>Relevant Prevalence w:</b> TODO </li>
    #        <li><b>alphaND1, ..., alphaNDm:</b> The parameters of the Dirichlet distribution of the
    #               non-diseased sample. </li>
    #        <li><b>alphaD1, ..., alphaDm:</b> The parameters of the Dirichlet distribution of the
    #               diseased sample. </li>
    #        <li><b>fND:</b> TO WRITE </li>
    #        <li><b>fD:</b> TO WRITE </li>
    #        
    #        <li><b>Delta:</b> The distance that matters. It is the distance between any
    #               two points on the grid. TODO: might change. </li>
    #        <li><b>Gamma:</b> A value that's less than the posterior content of the plausible region. 
    #               It is used to determine the credible region to test the validity of where the
    #               maximum of the relative belief ratio is located.</li>
    #                          
    #        <li><b>Hypothesized AUC (greater than):</b> input what the user hypothesizes AUC to be
    #               greater than. </li>
    #        <li><b>Input File Name:</b> The name of the file you want to download. The .csv file
    #               will include the grid points, the prior, the posterior,
    #               and the relative belief ratio. </li>
    #     </ul>")),
  ),
  br(style = "line-height:22;")
)

################################################################
# DESCRIPTION OF OUTPUTS                                       #
################################################################

sect3.2_def_outputs = div(
  titlePanel("Inputs and their Meanings"),
  mainPanel(
    p("Outputs refer to the outputs (results) found on the AUC page from section 3.2."),
    
    tableHTML_output("output_table_description"),
    
    #h4("Outputs and their Meanings"),
    #p(HTML("<ul>
    #        <li><b>Plausible Region:</b> TODO </li>
    #        <li><b>Posterior Content:</b> TODO </li>
    #        <li><b>Credible Region:</b> TODO </li>
    #        <li><b>Area Under The Line Plot:</b> provides the exact area under the line plot for the prior, posterior, instead of 
    #        the histograms to show how accurate it is.</li>
    #        <li><b>Prior copt:</b> TODO </li>
    #        <li><b>Post copt:</b> TODO </li>
    #        <li><b>copt Estimate:</b> TODO </li>
    #        <li><b>Error Characteristics:</b> TODO </li>
    #        <li><b>FPRest copt Estimate:</b> TODO </li>
    #        <li><b>FNRest copt Estimate:</b> TODO </li>
    #        <li><b>ERRORwest copt Estimate:</b> TODO </li>
    #        <li><b>FDRest copt Estimate:</b> TODO </li>
    #        <li><b>FNDRest copt Estimate:</b> TODO </li>
    #        <li><b>PPVest copt Estimate:</b> TODO </li>
    #        <li><b>Relative Belief Ratio of Event AUC > XX%:</b> Provides the relative belief ratio given the hypothesized
    #               AUC.</li>
    #        <li><b>Posterior Probability of Event AUC > XX%:</b> Provides the posterior content given the hypothesized
    #               AUC. </li>
     #    </ul>"))
  ),
  br(style = "line-height:22;")
)

################################################################
# ELABORATION OF STATISTICAL TERMS                             #
################################################################

sect3.2_def_tables = div(
  titlePanel("Elaboration of Statistical Terms"),
  mainPanel(
    p("In this section, we refer to some uncommonly known statistical definitions that were used."),
    
    imageOutput("formulas"),
  ),
  br(style = "line-height:22;")
)


################################################################
# PAGE LOGIC                                                   #
################################################################

page_sect3.2_def = div(
  titlePanel("Section 3.2: Definitions"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Definitions", sect3.2_def_description),
              tabPanel("Inputs", sect3.2_def_inputs),
              tabPanel("Outputs", sect3.2_def_outputs),
              tabPanel("Elaboration of Statistical Terms", sect3.2_def_tables)
  )
)