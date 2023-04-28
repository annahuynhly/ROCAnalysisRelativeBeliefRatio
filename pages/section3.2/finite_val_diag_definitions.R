################################################################
# DESCRIPTION                                                  #
################################################################

table_CSS = "
table, th, td {
  border: 1px solid #2c3e50 !important;
  border-collapse: collapse !important;
}
"

sect3.2_def_description = div(
  titlePanel("Page Description"),
  mainPanel(
    p("This tab lists the meaning behind the inputs and the outputs listed
      in the AUC page under section 3.2."),
    #div("more math here $$\\sqrt{2}$$"),
    #div("$$X$$ : $$\\Omega \\mapsto \\mathbb{R}^{1}$$ is defined on a population 
    #  $$\\Omega = \\Omega_{D} \\cup \\Omega_{ND}$$  with 
    #  $$\\Omega_{D} \\cap \\Omega_{ND} = \\emptyset$$."),
    
    tags$head(
      tags$link(rel="stylesheet", 
                href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
                integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                crossorigin="anonymous"),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
      HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
    ),
    
    withMathJax(),
    p("Latex test below:"),
    p("Let $\\Omega_{D}$ represent the population of those who are diseased, and $\\Omega_{ND}$ 
    represent the population who are not diseased. There is a measurement $X$ : $\\Omega \\mapsto 
    \\mathbb{R}^{1}$ is defined on a population $\\Omega = \\Omega_{D} \\cup \\Omega_{ND}$ with 
    $\\Omega_{D} \\cap \\Omega_{ND} = \\emptyset$."),
    p("Here, we say that $F_{ND}(c) = $ # $(\\{w \\in \\Omega_{ND}:X(\\omega) \\leq c\\}) / $ # 
    $(\\Omega_{ND})$ is the conditional cumulative distribution function (cdf) of X in the non 
    diseased population. 
    Similarly, $F_{D}(c) = $ # $(\\{w \\in \\Omega_{D}: X(\\omega) \\leq c\\}) 
    / $ # $(\\Omega_{D})$ is the conditional cdf of X in the diseased population."),
    
    div(
      tags$head(
        tags$style(HTML(table_CSS))
      ),
      tags$table(
        style = "width: 755px; height: 175px;",
        tags$tr(
          tags$th(""),
          tags$th(style = "text-align: center", 
                  "$\\Omega_{D}$"),
          tags$th(style = "text-align: center",
                  "$\\Omega_{ND}$"),
        ),
        tags$tr(
          tags$td(style = "text-align: center",
                  "$X > c$"),
          tags$td(style = "text-align: center",
                  "$\\text{TPR}(c) = 1 - F_{D}(c)$", br(),
                  em("Sensitivity (recall)"), " or", br(),
                  "True Positive Rate"),
          tags$td(style = "text-align: center",
                  "$\\text{FPR}(c) = 1 - F_{ND}(c)$", br(),
                  "False Positive Rate"),
        ),
        tags$tr(
          tags$td(style = "text-align: center",
                  "$X \\leq c$"),
          tags$td(style = "text-align: center",
                  "$\\text{FNR}(c) = F_{D}(c)$", br(),
                  "False Negative Rate"),
          tags$td(style = "text-align: center",
                  "$\\text{TPR}(c) = F_{ND}(c)$", br(),
                  em("Specificity"), " or", br(),
                  "True Negative Rate"),
        )
      ),
    ), # end of table
    
    p("In this case, $\\omega = $#$(\\Omega_{D})$/$(\\Omega)$ of the disease in $\\Omega$."),
    p("Furthermore, the following error characteristics depend on $\\omega$:"),
    
    p("$\\text{Error}(c) = $ Misclassification Rate $ = w \\text{FNR}(c) + 
      (1 - w)\\text{FPR}(c)$"),
    
    HTML("<p> $\\text{FDR}(c) = $ False Discovery Rate
            <span style = 'font-size:20px;'> $ = \\frac{(1-w)\\text{FPR}(c)}{w
            (1 - \\text{FNR}(c)) + (1 - w)\\text{FPR}(c)}$ </span>
         </p>"
    ),
    
    HTML("<p> $\\text{FNDR}(c) = $ False Nondiscovery Rate
            <span style = 'font-size:20px;'> $ = \\frac{w \\text{FNR}(c)}{w 
            \\text{FNR}(c) + (1 - w)(1 - \\text{FPR}(c))}$ </span>
         </p>"
    ),
    
    HTML("<p> $\\text{PPV}(c) = $ Positive Predictive Value
            <span style = 'font-size:20px;'> $ = \\frac{w \\text{TPR}(c)}{w 
            \\text{TPR}(c) + (1 - w){FPR}(c)}$
         </p>"
    ),
    
    HTML("<p> <span style = 'font-size:16px;'>
         $c_{opt} = $ optimal cutoff $ = $ arg inf Error$(c)$ 
         </span> </p>"
    ),
    
    
  ),
  br(style = "line-height:40;")
)

################################################################
# DESCRIPTION OF INPUTS                                        #
################################################################

sect3.2_def_inputs = div(
  titlePanel("Inputs and their Meanings"),
  mainPanel(
    p("Inputs refer to the users inputs found on the AUC page from section 3.2."),
    
    p("$X$ : $\\Omega \\mapsto \\mathbb{R}^{1}$ is defined on a population 
    $\\Omega = \\Omega_{D} \\cup \\Omega_{ND}$  with 
    $\\Omega_{D} \\cap \\Omega_{ND} = \\emptyset$."),
    
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