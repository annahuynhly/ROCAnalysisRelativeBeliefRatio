
################################################################
# DESCRIPTION                                                  #
################################################################

plotROC_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_plotROC = div( # CHANGE THIS
  titlePanel("Section 3.3: plotROC"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "plotROC_muD", # CHANGE THIS
                   tags$p('muD', style = "font-size: 90%;"), value = 1),
      numericInput(inputId = "plotROC_sigmaD", # CHANGE THIS
                   tags$p('sigmaD', style = "font-size: 90%;"), value = 1.5),
      numericInput(inputId = "plotROC_muND", # CHANGE THIS
                   tags$p('muND', style = "font-size: 90%;"), value = 0),
      numericInput(inputId = "plotROC_sigmaND", # CHANGE THIS
                   tags$p('sigmaND', style = "font-size: 90%;"), value = 1),
      numericInput(inputId = "plotROC_w", # CHANGE THIS
                   tags$p('w', style = "font-size: 90%;"), value = 0.5),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", plotROC_description), # CHANGE THIS
                  tabPanel("Calculator", verbatimTextOutput("plotROC_calculator")),
                  tabPanel("Plot", plotOutput(outputId = "plotROC_plot")),
      )
    )
  )
)
################################################################
# MAIN FUNCTIONS                                               #
################################################################


#muD=1
#sigmaD=1.5
#muND=0
#sigmaND=1
#w=.5

#plotROC_crit_values(muD, sigmaD, muND, sigmaND, w)

plotROC_crit_values = function(muD, sigmaD, muND, sigmaND, w){
  diff=(muND-muD)/sigmaD
  ratio=sigmaND/sigmaD
  crit=(muND-muD)**2 -2*log((1-w)/(w*ratio))
  # WARNING: this 'p' is hard coded!!
  p = c(1:1000)/1001
  ROC = 1- pnorm(diff+ratio*qnorm(1-p))
  newlist = list("diff" = diff, "ratio" = ratio, "crit" = crit, "p" = p, 
                 "ROC" = ROC)
  return(newlist)
}



