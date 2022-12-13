# File is realdataROC_1

page_realdataROC_1 = div(
  titlePanel("Section 3.2: realdataROC"),
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      #nMonteprior, fND, fD, p
      numericInput(inputId = "nMonteprior",
                   tags$p('The sample size used for the MC Estimation of the Prior', 
                          style = "font-size: 90%;"),value = 100000, min = 1),
      textInput(inputId = "fND",
                tags$p('fNd', style = "font-size: 90%;"),
                value = "25, 16, 18, 21, 20"),
      textInput(inputId = "fD",
                tags$p('fD', style = "font-size: 90%;"),
                value = "14, 11, 29, 28, 18"),
      numericInput(inputId = "p",
                   tags$p('P', style = "font-size: 90%;"),
                   value = 0.5)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      verbatimTextOutput("values")
    )
  )
)

# functions associated with this page

prior_distribution_c_opt = function(nMonteprior, fND, fD, p){
  fND = convert_char_to_vector(fND)
  fD = convert_char_to_vector(fD)
  if (!(valid_vector(fND) == TRUE & valid_vector(fD) == TRUE)) {
    return("Error: Either fND or fD provide invalid responses.")
  }
  #fND = as.integer(strsplit(fND, ", ")[[1]])
  #fD = as.integer(strsplit(fD, ", ")[[1]])
  if (length(fND) == length(fD)) {
    m = length(fND)
  } else {
    return("Error: Invalid fND and fD; their lengths must be equal.")}
  alpha_priorND = rep(1,m)
  alpha_priorD = rep(1,m)
  priorc_opt = rep(0,m)
  p = 0.5
  FN = rep(0,m)
  FP = rep(0,m)
  ERROR = rep(0,m)
  FNR = rep(0,m)
  FPR = rep(0,m)
  for (iMontepost in 1:nMonteprior) {
    #generate Dirichlet values for pND and pD
    pND_prior = rdirichlet(1,alpha_priorND)
    pD_prior = rdirichlet(1,alpha_priorD)
    # compute FN_prior, FP_prior and ERROR_prior  
    for(i in 1:m){
      FN[i] = sum(pD_prior[1:i])
      FP[i] = 1-sum(pND_prior[1:i])
      ERROR[i] = p*FN[i] + (1-p)*FP[i]
    }
    # locate the minimum value index (i.e. c opt)
    c_opt_value = which.min(ERROR)
    
    # update the prior distribution of c_opt
    priorc_opt[c_opt_value] <- priorc_opt[c_opt_value]+1
  }
  priorc_opt = priorc_opt/nMonteprior
  newlist = list("FN" = FN, "FP" = FP, "ERROR" = ERROR, "FNR" = FNR,
                 "FPR" = FPR, "c_opt_value" = c_opt_value, "priorc_opt" = priorc_opt)
  return(newlist)
}