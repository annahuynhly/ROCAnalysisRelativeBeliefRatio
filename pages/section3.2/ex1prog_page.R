
################################################################
# DESCRIPTION                                                  #
################################################################

ex1prog_description = div(
  titlePanel("Page Description"),
  p("Insert a description here!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_ex1prog = div(
  titlePanel("Section 3.2: ex1prog"),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      #nMonteprior, fND, fD, p
      numericInput(inputId = "w",
                   tags$p('w', style = "font-size: 90%;"),value = 0.25, min = 0),
      numericInput(inputId = "q",
                   tags$p('q', style = "font-size: 90%;"), value = 2),
      numericInput(inputId = "n_size",
                   tags$p('Vector Size (For Graphs)', style = "font-size: 90%;"), value = 1000),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", ex1prog_description),
                  tabPanel("Calculator", verbatimTextOutput("ex1_prog_values")),
                  tabPanel("Plot", plotOutput(outputId = "Test_Plot")),
      )
    )
  )
)

################################################################
# MAIN FUNCTIONS                                               #
################################################################

ex1prog = function(w, q){
  w = as.numeric(w) # these lines are to ensure that the values are numeric...
  q = as.numeric(q)
  r = (1-w)/w
  AUC = 1-1/(1+q)
  popt = 1-(r/q)**(1/(q-1))
  FPR = popt
  FNR = (1-popt)**q
  ERROR = w*FNR+(1-w)*FPR
  FDR = (1-w)*FPR/(w*(1-FNR)+(1-w)*(1-FPR))
  FNDR = w*FNR/(w*FNR+(1-w)*(1-FPR))
  values = list("AUC" = AUC, "FPR" = FPR, "FNR" = FNR,
                "ERROR" = ERROR, "FDR" = FDR, "FNDR" = FNDR)
  return(values)
}

ex1prog_graph = function(n_size){
  ex1prog_p = c(0:n_size)/n_size
  ex1prog_ROC1 = 1-(1-ex1prog_p)**99
  ex1prog_ROC2 = 1-(1-ex1prog_p)**2
  values = list("p" = ex1prog_p, "ROC1" = ex1prog_ROC1, 
                "ROC2" = ex1prog_ROC2)
  return(values)
}
