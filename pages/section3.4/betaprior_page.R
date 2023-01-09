
################################################################
# DESCRIPTION                                                  #
################################################################

betaprior_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_betaprior = div( # CHANGE THIS
  titlePanel("Section 3.4: betaprior"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "betaprior_l", # CHANGE THIS
                   tags$p('l', style = "font-size: 90%;"),value = 0.0),
      numericInput(inputId = "betaprior_u", # CHANGE THIS
                   tags$p('u', style = "font-size: 90%;"),value = 0.15),
      numericInput(inputId = "betaprior_gamma", # CHANGE THIS
                   tags$p('gamma', style = "font-size: 90%;"),value = 0.99),
      numericInput(inputId = "betaprior_error", # CHANGE THIS
                   tags$p('error', style = "font-size: 90%;"),value = 0.000001),
      numericInput(inputId = "betaprior_nmax", # CHANGE THIS
                   tags$p('nmax', style = "font-size: 90%;"),value = 100),
      numericInput(inputId = "betaprior_taulow", # CHANGE THIS
                   tags$p('taulow', style = "font-size: 90%;"),value = 0),
      numericInput(inputId = "betaprior_tauup", # CHANGE THIS
                   tags$p('tauup', style = "font-size: 90%;"),value = 3000),
      numericInput(inputId = "betaprior_n", # CHANGE THIS
                   tags$p('n', style = "font-size: 90%;"),value = 45),
      numericInput(inputId = "betaprior_nD", # CHANGE THIS
                   tags$p('nD', style = "font-size: 90%;"),value = 20),
      numericInput(inputId = "betaprior_low", # CHANGE THIS
                   tags$p('low', style = "font-size: 90%;"),value = 0.374),
      numericInput(inputId = "betaprior_up", # CHANGE THIS
                   tags$p('up', style = "font-size: 90%;"),value = 0.516),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", betaprior_description), # CHANGE THIS
                  tabPanel("Calculator", verbatimTextOutput("betaprior_output")),
                  tabPanel("Plot", plotOutput(outputId = "betaprior_plot")),
      )
    )
  )
)
################################################################
# MAIN FUNCTIONS                                               #
################################################################


calculate_betaprior_values = function(l, u, gamma, error, nmax, taulow, tauup, n, nD, low, up){
  # calculate the mode
  phai = (l+u)/2 
  # input potential starting values taulow and tauup for tau for the iteration
  a1 = 1 + phai*tauup
  a2 = 1 + (1 - phai)*tauup
  content = pbeta(u,a1,a2) - pbeta(l,a1,a2)
  if (content < gamma) {
    cat(gamma,">",content,"\n")
    stop("starting value of tauup too low")
  } else { 
    # starting value is okay
    for (i in 1:nmax){
      tau = (taulow + tauup)/2
      a1 = 1 + phai*tau 
      a2 = 1 + (1-phai)*tau
      content = pbeta(u,a1,a2)-pbeta(l,a1,a2)
      if (content < gamma) {taulow=tau}
      if (content > gamma) {tauup=tau}
      if (abs(content- gamma)<= error) {
        #cat (i,content,a1,a2,"\n")
        break}
    }
  }
  a1post=nD+1+phai*tau 
  a2post=n-nD+1+(1-phai)*tau
  w=c(0:1000)/1000 # NOTE: this is hard coded
  # WHY ARE THERE DUPLICATES?
  rb=log(dbeta(w,a1post,a2post))-log(dbeta(w,a1,a2))
  rb=exp(rb) # why is there both?
  cont=pbeta(up,a1post,a2post)-pbeta(low,a1post,a2post)
  
  newlist = list("phai" = phai, "a1" = a1, "a2" = a2, "tau" = tau, "a1post" = a1post,
                 "a2post" = a2post, "rb" = rb, "cont" = cont, "w" = w)
  return(newlist)
}



