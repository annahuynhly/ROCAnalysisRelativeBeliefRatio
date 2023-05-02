
page_finite_val_start = fluidPage(
  titlePanel("Getting Started"),
  sidebarLayout(
    sidebarPanel(width = 4, 
                 
      #added this for latex support - probs not needed for the future
      tags$head(
        tags$link(rel = "stylesheet", 
                  href = "https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", 
                  integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", 
                  crossorigin="anonymous"),
        tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", 
                    integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", 
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
        
        selectizeInput(
          inputId = "finite_val_diag_case2", 
          label = "Please select the sampling regime.", 
          choices = NULL,
          options = list(
            options = list(
              list(
                value = "A",
                head = "A sample of ",
                latex = "n_{D}",
                tail = " from diseased and ",
                latex2 = "n_{ND}",
                end = " from non-diseased."
              ),
              list(
                value = "B",
                head = "A sample of n from the population, observe ",
                latex = "n_{ND}",
                tail = " diseased and ",
                latex2 = "n_{ND}",
                end = " from non-diseased."
              )
            ),
            valueField = "value",
            render = I("{
        item: function(item, escape) { 
                var html = katex.renderToString(item.latex);
                var html2 = katex.renderToString(item.latex2);
                return '<div>' + item.head + html + item.tail + html2 + item.end + '</div>'; 
              },
        option: function(item, escape) { 
                  var html = katex.renderToString(item.latex);
                  var html2 = katex.renderToString(item.latex2);
                  return '<div>' + item.head + html + item.tail + html2 + item.end + '</div>';; 
                }
      }")
          ),
          selected = "A"
        ), # end
        
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