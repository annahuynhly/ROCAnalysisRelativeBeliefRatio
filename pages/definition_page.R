
table_CSS = "
table, th, td {
  border: 1px solid #2c3e50 !important;
  border-collapse: collapse !important;
}
"

def_problem = div(
  
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

  mainPanel(
    br(),
    p("Let $\\Omega_{D}$ represent the population of those who are diseased, and $\\Omega_{ND}$ 
      represent the population who are not diseased."),
    p("There is a measurement $X$ : $\\Omega \\mapsto 
      \\mathbb{R}^{1}$ is defined on a population $\\Omega = \\Omega_{D} \\cup \\Omega_{ND}$ with 
      $\\Omega_{D} \\cap \\Omega_{ND} = \\emptyset$."),
    p("Here, we say that $F_{ND}(c) = $ # $(\\{w \\in \\Omega_{ND}:X(\\omega) \\leq c\\}) / $ # 
      $(\\Omega_{ND})$ is the conditional cumulative distribution function (cdf) of X in the non 
      diseased population."),
    p("Similarly, $F_{D}(c) = $ # $(\\{w \\in \\Omega_{D}: X(\\omega) \\leq c\\}) 
      / $ # $(\\Omega_{D})$ is the conditional cdf of X in the diseased population."),
    p("There are two was to sample from $\\Omega$, either:"),
    HTML("<ol>
            <li>Take samples from either $\\Omega_{D}$ and $\\Omega_{ND}$ separately 
              (relevant in case-control studies.)
            </li>
            <li>Take a sample from $\\Omega$ (relevant in cross-sectional studies.)</li>
    </ol>"
    ),
    p("In the confusion matrix below, we provide the relevant probabilities for classification into
      $\\Omega_{D}$ and $\\Omega_{ND}$."),
    br(),
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
    br(),
    
    p("In this case, $\\omega = $#$(\\Omega_{D})$/$(\\Omega)$ of the disease in $\\Omega$."),
    p("Furthermore, the following error characteristics depend on $\\omega$:"),
    
    p("$\\text{Error}(c) = $ Misclassification Rate $ = w \\text{FNR}(c) + 
        (1 - w)\\text{FPR}(c)$"),
    p("Where $\\text{Error}(c)$ is the probability of misclassifying a subject."),
    
    HTML("<p> $\\text{FDR}(c) = $ False Discovery Rate
              <span style = 'font-size:20px;'> $ = \\frac{(1-w)\\text{FPR}(c)}{w
              (1 - \\text{FNR}(c)) + (1 - w)\\text{FPR}(c)}$ </span>
           </p>"
    ),
    p("In other words, $\\text{FDR}(c)$ is the conditional probability of someone being mistaken
      as having the disease when they do not have it."),
    
    HTML("<p> $\\text{FNDR}(c) = $ False Nondiscovery Rate
              <span style = 'font-size:20px;'> $ = \\frac{w \\text{FNR}(c)}{w 
              \\text{FNR}(c) + (1 - w)(1 - \\text{FPR}(c))}$ </span>
           </p>"
    ),
    p("In simpler terms, $\\text{FNDR}(c)$ is the conditional probability of someone being 
    mistaken as not being diseased when they have the disease."),
    
    HTML("<p> $\\text{PPV}(c) = $ Positive Predictive Value
              <span style = 'font-size:20px;'> $ = \\frac{w \\text{TPR}(c)}{w 
              \\text{TPR}(c) + (1 - w){FPR}(c)}$
           </p>"
    ),
    p("In plain English, $\\text{PPV}(c)$ is the conditional probability that given we claim that a subject 
      is tested as positive, the subject is actually positive."),
    
    HTML("<p> <span style = 'font-size:16px;'>
           $c_{opt} = $ optimal cutoff $ =  \\text{arg inf Error}(c)$ 
           </span> </p>"
    ),
    p("This represents the value that minimizes the probability of misclassifying someone (making 
      an error.) We plan to find $c_{opt}$ through bayesian statistics."),
    
  ),
  br(style = "line-height:55;"),
)

def_AUC_ROC = div(
  br(),
  mainPanel(
    p("ROC (Receiver Operating Characteristic) and AUC (Area Under the Curve) are statistical 
      concepts used to evaluate the performance of a model that performs binary classifcation 
      (where there are two possible states). For example, in our problem, we are dealing with 
      diseased and nondiseased groups."),
    p("ROC is a probability curve that plots the True Positive Rate (TPR) against the False 
      Positive Rate (FPR). AUC tells how much the model is capable of distinguishing between 
      two states (in our case, the diseased and non-diseased.). Higher the AUC, the better the 
      model at prediction."),
    p("If $F_{ND}, F_{D}$ are both discrete, then suppose we can claim that these distributions are
      concentrated on a set of points $c_{1} < c_{2} < ... < c_{m}$. When $\\omega_{D}$,
      $\\omega_{ND}$ are selected using the first sampling regime (taking samples from either 
      $\\Omega_{D}$ and $\\Omega_{ND}$ separately), then the probability that a higher score 
      is received on diagnostic $X$ by a diseased individual than a nondiseased individual is:"),
    HTML("<p> <span style = 'font-size:16px;'>
          $AUC = \\sum_{i=1}^{m} (1 - F_{D}(c_{i}))(F_{ND}(c_{i}) - F_{ND}(c_{i-1}))$ 
          </span> </p>"
    ),
    p("Otherwise, if $F_{ND}, F_{D}$ are both absolutely continuous, then the AUC is:"),
    HTML("<p> <span style = 'font-size:16px;'>
          $AUC = \\int_{-\\infty}^{\\infty} (1-F_{D}(c)) f_{ND}(c) dc$ 
          </span> </p>"
    ),
    p("If we assume that $F_{D}(c)$ is a constant on $\\{c : F_{ND}(c) = p\\}$ for every $p \\in 
      [0, 1]$, then there is a receiver operating characterstic (ROC) such that:"),
    HTML("<p> <span style = 'font-size:16px;'>
          $ROC(1-F_{ND}(c)) = 1 - F_{D}(c)$ 
          </span> </p>"
    ),
    p("Hence it is the case that we can re-write AUC as:"),
    HTML("<p> <span style = 'font-size:16px;'>
          $AUC = \\int_{-\\infty}^{\\infty} ROC(1-F_{ND}(c)) F_{ND}(c) dc$ 
          </span> </p>"
    ),
    p("If the AUC of $X$ is close to 1, then we have a good diagnostic test. However, if the AUC is close
      to $1/2$, then we have a poor diagnostic test. However, this isn't guaranteed since it depends on the
      cutoff $c$ that was used - it is only one part of the analysis. The error characteristics are required to
      assess the performance of the diagnostic.")
    
  ),
  br(style = "line-height:30;"),
)

def_RBR = div(
  br(),
  mainPanel(
    p("In statistics, the prior probability represents what we percieve the probability of an event 
    to be before we run a test or collect evidence. The probability after evidence is collected 
    refers to the posterior probability. We carry out the revision of the prior probability using
    baye's rule."),
    p("Let $\\{f_{\\theta} : \\theta \\in \\ \\Theta \\}$ be a model for $x$. We have a prior probability 
    measure $\\Pi$ with density $\\pi$ on $\\Theta$."),
    p("Similarly, we have the posterior probability measure $\\Pi(\\cdot | x)$ with density 
    $\\pi(\\cdot | x)$."),
    hr(),
    p("Now suppose we are interested in $\\psi = \\Psi(\\theta)$ where $\\Psi : \\Theta \\mapsto \\Psi$,
      then we have a model $\\{m_{\\psi} : \\psi \\in \\Psi \\}$ where:"),
    HTML("<p> <span style = 'font-size:16px;'>
          $m_{\\psi}(x) = \\int_{\\Psi^{-1}\\{\\psi \\}} f_{\\theta}(x) \\pi(\\theta | \\psi) d\\theta$
          </span> </p>"
    ),
    p("Which gives us the marginal prior:"),
    HTML("<p> <span style = 'font-size:16px;'>
          $\\pi_{\\Psi}(\\psi) = \\int_{\\Psi^{-1}\\{\\psi \\}} \\pi(\\theta) d\\theta$
          </span> </p>"
    ),
    p("And we have the marginal posterior $\\Pi_{\\Psi}(\\cdot | x)$ with density 
      $\\pi_{\\Psi}(\\cdot | x)$"),
    p("If we have that all of these distributions are discrete, then according to the principle of 
      evidence:"),
    HTML("<ul>
            <li>
              If $\\pi_{\\Psi}(\\psi | x) > \\pi_{\\Psi}(\\psi)$, there is evidence in favour 
              of $\\psi$.
            </li>
            <li>
              If $\\pi_{\\Psi}(\\psi | x) < \\pi_{\\Psi}(\\psi)$, there is evidence in against 
              $\\psi$.
            </li>
            <li>
              If $\\pi_{\\Psi}(\\psi | x) = \\pi_{\\Psi}(\\psi)$, there is no evidence for or 
              against.
            </li>
    </ul>"
    ),
    p("To order the possible values with respect to the evidence, we use the relative belief 
      ratio:"),
    HTML("<p> <span style = 'font-size:16px;'>
          $RB_{\\Psi}(\\psi | x) = $ </span>
          <span style = 'font-size:22px;'>
          $\\frac{\\pi_{\\Psi}(\\psi | x)}{\\pi_{\\Psi}(\\psi)}$
          </span> 
         </p>"
    ),
    p("Where when $RB_{\\Psi}(\\psi | x) > 1$ there is evidence in favour of $\\psi$. Similarly, 
      when $RB_{\\Psi}(\\psi | x) < 1$ there is evidence against $\\psi$."),
    hr(),
    p("Now suppose we have the continuous case, which is more complicated. We must consider a 
      sequence of neighbourhoods, $\\{N_{\\epsilon}(\\psi)\\} \\downarrow \\{\\psi \\}$ as 
      $\\epsilon \\rightarrow 0$. Then, under the condition that $\\pi_{\\Psi}(\\psi) > 0$ 
      and $\\pi_{\\Psi}$ is continuous at $\\psi$, we see that the relative belief ratio of these 
      neighbourhoods to be:"),
    HTML("<p> <span style = 'font-size:16px;'>
          $RB_{\\Psi}(N_{\\epsilon}(\\psi) | x) = $
          </span>
          <span style = 'font-size:22px;'>
          $\\frac{\\Pi_{\\Psi}(N_{\\epsilon}(\\psi) | x)}{\\Pi_{\\Psi}(N_{\\epsilon}(\\psi))}
          \\rightarrow \\frac{\\pi_{\\Psi}(\\psi | x)}{\\pi_{\\Psi}(\\psi)}$
          </span> </p>"
    ),
    p("However, these quantities are hard to compute in theory. We typically use the relative belief
      estimate:"),
    p("$\\psi(x) = \\text{arg } \\text{sup}_{\\psi \\in \\Psi}(\\psi | x)$"),
    p("The accuracy of this estimate is determined by its plausible region:"),
    p("$Pl_{\\Psi}(x) = \\{\\psi : RB_{\\Psi}(\\psi | x) > 1 \\}$"),
    p("Here, the plausible region is an interval that consists of all values where there is
      evidence in favour for $\\psi$."),
    p("Another way to access the accuracy of $\\psi(x)$, we use the relative belief credible 
      region:"),
    p("$C_{\\Psi, \\gamma}(x) = \\{\\psi : RB_{\\Psi}(\\psi | x) > c_{\\gamma}\\}$, where 
      $c_{\\gamma}$ is the largest value such that $\\Pi_{\\psi}(C_{\\Psi, \\gamma} | x) \\geq \\gamma$"),
    p("Furthermore, $\\gamma \\leq \\Pi_{\\Psi}(Pl_{\\Psi}(x) | x)$ to ensure that we always have values of $\\psi$
      that has evidence in favour."),
  ),
  br(style = "line-height:55;"),
)


def_page = div(
  titlePanel("General Definitions"),
  # OUTPUTTING THE VALUES
  tabsetPanel(type = "tabs",
              tabPanel("Outline of the Problem", def_problem),
              tabPanel("AUC and ROC", def_AUC_ROC),
              tabPanel("Relative Belief Instances", def_RBR),
  )
)