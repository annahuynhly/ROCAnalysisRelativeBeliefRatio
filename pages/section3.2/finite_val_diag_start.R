
page_finite_val_start = fluidPage(
  titlePanel("Getting Started"),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 
      #added this for latex support
      tags$head(
        tags$link(rel = "stylesheet", 
                  href = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", 
                  crossorigin="anonymous"),
        tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", 
                  crossorigin="anonymous")
      ),
      
      numericInput(inputId = "finite_val_diag_seed",
                   label = "Please select a seed for the computations.",
                   value = 1
      ),
      selectizeInput(
        inputId = "finite_val_diag_case1", 
        label = "Please select whether the prevalence $\\omega$ is known or unknown.",
        choices = c("\\text{The prevalence is known}" = 1, 
                    "\\text{The prevalence } \\omega \\text{ is unknown}" = 2),
        options = list(render = I("
      {
        item: function(item, escape) { 
                var html = katex.renderToString(item.label);
                return '<div>' + html + '</div>'; 
              },
        option: function(item, escape) { 
                  var html = katex.renderToString(item.label);
                  return '<div>' + html + '</div>'; 
                }
      }")
        ),
      ),
      #selectInput(inputId = "finite_val_diag_case1", 
      #            label = "Please select whether the prevalence $\\omega$ is known or unknown.",
      #            choices = c("The prevalence is known" = 1, 
      #                        "The prevalence w is unknown" = 2),
      #            selected = 1
      #),
      conditionalPanel(
        condition = "input.finite_val_diag_case1 == 1",
        numericInput(inputId = "finite_val_diag_prevalence_w",
                     label = 'Please input the prevalence $\\omega$.',
                     value = 0.65), #
        p("The prevalence $\\omega$ has been determined. Please proceed to the computations. 
        The Prevalence section may be skipped, as it is used to estimate the prevalence $\\omega$.")
      ),
                 
      conditionalPanel(
        condition = "input.finite_val_diag_case1 == 2",
        p("Please select the beta prior parameters ($\\alpha_{1\\omega}$ and $\\alpha_{2\\omega}$), 
          and then the sampling regime."),
        numericInput(inputId = "finite_val_diag_prevalence_alpha1w", 
                     label = '$\\alpha_{1\\omega}$',
                     value = 391.72),
        numericInput(inputId = "finite_val_diag_prevalence_alpha2w", 
                     label = '$\\alpha_{2\\omega}$',
                     value = 211.39),
        
#        selectizeInput(
#          inputId = "finite_val_diag_case2", 
#          label = "Please select the sampling regime.",
#          choices = c("\\text{A sample of } n_{D} \\text{ from diseased and } n_{ND}
#                      \\text{ from non diseased.}" = "A", 
#                      "\\text{A sample of } n \\text{ from population, observe } n_{D} 
#                      \\text{ diseased and } n_{ND} \\text{ nondiseased.}" = "B"),
#          options = list(render = I("
#      {
#        item: function(item, escape) { 
#                var html = katex.renderToString(item.label);
#                return '<div>' + html + '</div>'; 
#              },
#        option: function(item, escape) { 
#                  var html = katex.renderToString(item.label);
#                  return '<div>' + html + '</div>'; 
#                }
#      }")
#          ),
#        ),
        selectInput(inputId = "finite_val_diag_case2", 
                    label = "Please select the sampling regime.",
                    choices = c("A sample of n_D from diseased and n_ND from non diseased." = "A", 
                    "A sample of n from population, observe n_D diseased and n_ND nondiseased." = "B"),
                    selected = "case_a_opt"),
        conditionalPanel(
          condition ="input.finite_val_diag_case2 == 'A'",
          p("The sampling regime has been chosen. You may observe The Prevalence section to see the prior. 
            Since we do not have data on the posterior, we cannot make more estimates for the prevalence $\\omega$.")
        ),
        conditionalPanel(
          condition ="input.finite_val_diag_case2 == 'B'",
          p("The sampling regime has been chosen. You may observe The Prevalence section to see the estimation 
            for the prevalence $\\omega$.")
        )
      ),
    ),
    mainPanel(
      getting_started_default_description_1
    )
  ),
  #br(style = "line-height:7;")
)