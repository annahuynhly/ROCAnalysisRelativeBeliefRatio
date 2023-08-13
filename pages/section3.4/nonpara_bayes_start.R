
page_nonpara_bayes_start = fluidPage(
  titlePanel("Getting Started"),
  sidebarLayout(
    sidebarPanel(width = 4, 
      
      selectizeInput(
        inputId = "nonpara_bayes_case1", 
        label = "Please select whether the prevalence $w$ is known or unknown.",
        choices = c("\\text{The prevalence is known}" = 1, 
                    "\\text{The prevalence } w \\text{ is unknown}" = 2),
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
        condition = "input.nonpara_bayes_case1 == 1",
        numericInput(inputId = "nonpara_bayes_prevalence_w",
                     label = 'Please input the prevalence.',
                     value = 0.65), #
        p("The prevalence $w$ has been determined. Please proceed to the computations. 
        The Prevalence section may be skipped, as it is used to estimate the prevalence $w$, when 
        it is unknown.")
      ),

      
      conditionalPanel(
        condition = "input.nonpara_bayes_case1 == 2",
        p("Please select the beta prior parameters ($\\alpha_{1w}$ and $\\alpha_{2w}$), 
          and then the sampling regime."),
        numericInput(inputId = "nonpara_bayes_prevalence_alpha1w", 
                     label = '$\\alpha_{1w}$',
                     value = 15.3589),
        numericInput(inputId = "nonpara_bayes_prevalence_alpha2w", 
                     label = '$\\alpha_{2w}$',
                     value = 22.53835),
        
        selectizeInput(
          inputId = "nonpara_bayes_case2", 
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
          )
        ), # end
        
        conditionalPanel(
          condition ="input.nonpara_bayes_case2 == 'A'",
          p("The sampling regime has been chosen. You may observe The Prevalence section to see the prior. 
          Since we do not have data on the posterior, we cannot make more estimates for the prevalence $w$.")
        ),
        conditionalPanel(
          condition ="input.nonpara_bayes_case2 == 'B'",
          p("The sampling regime has been chosen. You may observe The Prevalence section to see the estimation 
          for the prevalence $w$.")
        )
      ),
    ),
    mainPanel(
      p("WARNING: the following section will not work unless you upload the information for the 'Inferences for 
    the AUC' page."),
      getting_started_default_description_3
    )
  ),
  #br(style = "line-height:7;")
)