
library(rBeta2009)

################################################################
# DESCRIPTION                                                  #
################################################################

conditionalAUCbig_description = div(
  titlePanel("Page Description"),
  p("Insert a description here!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_conditionalAUCbig = div(
  titlePanel("Section 3.2: conditionalROCbig"),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "conditionalAUCbig_nMonte",
                   tags$p('The sample size used for Monte Carlo', 
                          style = "font-size: 90%;"),value = 100000, min = 1),
      textInput(inputId = "conditionalAUCbig_fND",
                tags$p('fNd', style = "font-size: 90%;"),
                value = "29, 7, 4, 5, 5"),
      textInput(inputId = "conditionalAUCbig_fD",
                tags$p('fD', style = "font-size: 90%;"),
                value = "14, 7, 25, 33, 21"),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", conditionalAUCbig_description),
                  tabPanel("Calculator", verbatimTextOutput("conditionalAUCbig_values")),
                  tabPanel("Plot", plotOutput(outputId = "conditionalAUCbig_hist")),
      )
    )
  )
)

################################################################
# MAIN FUNCTIONS                                               #
################################################################

conditionalAUCbig = function(nmonte, fND, fD){
  # NOTE: making the assumption that this isn't going to be a re-used helper function
  fND = convert_char_to_vector(fND)
  fD = convert_char_to_vector(fD)
  if (!(valid_vector(fND) == TRUE & valid_vector(fD) == TRUE)) {
    return("Error: Either fND or fD provide invalid responses.")
  }
  if (length(fND) == length(fD)) {
    m = length(fND)
  } else {
    return("Invalid fND and fD; their lengths must be equal.") }
  ifpost = 1
  alpha_postND = rep(1, m) + fND
  alpha_postD = rep(1, m) + fD
  pNDarray = array(0*c(1:nmonte*m),dim=c(nmonte,m))
  pDarray = array(0*c(1:nmonte*m),dim=c(nmonte,m))
  crit = 0*c(1:nmonte)
  inc = 0
  eventcount = 0
  for (i in 1:nmonte){
    if (ifpost==0) {
      pND = rdirichlet(1,alpha_priorND)
      pD = rdirichlet(1,alpha_priorD)
    } else {
      pND = rdirichlet(1,alpha_postND)
      pD = rdirichlet(1,alpha_postD)}
    crit[i] = sum(cumsum(pD)*pND)
    if(crit[i] <= 0.5) {
      eventcount = eventcount+1
      inc = inc+1
      pNDarray[inc,] = c(pND)
      pDarray[inc,] = c(pD)
    }
  }
  pNDarray = pNDarray[1:inc,]
  pDarray = pDarray[1:inc,]
  probevent = eventcount/nmonte
  newlist = list("probevent" = probevent, "inc" = inc, "crit" = crit)
  return(newlist)
}
