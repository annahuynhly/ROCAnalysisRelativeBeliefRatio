# File is realdataROC_1

library(rBeta2009)
source("ShinyHelperFunctions.R")

# NOTE: will give same results in the beginning, but will change overtime.
addTaskCallback(function(...) {set.seed(0);TRUE})

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
      numericInput(inputId = "nMontepost",
                   tags$p('The sample size used for the MC Estimation of the Posterior', 
                          style = "font-size: 90%;"),value = 100000, min = 1),
      textInput(inputId = "fND",
                tags$p('fNd', style = "font-size: 90%;"),
                value = "25, 16, 18, 21, 20"),
      textInput(inputId = "fD",
                tags$p('fD', style = "font-size: 90%;"),
                value = "14, 11, 29, 28, 18"),
      numericInput(inputId = "realdataROC_p",
                   tags$p('P', style = "font-size: 90%;"),
                   value = 0.5),
      numericInput(inputId = "realdataROC_ngrid",
                   tags$p('The size of the grid', style = "font-size: 90%;"),
                   value = 100, min = 1),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Calculator_1", verbatimTextOutput("realdataROC_value_1")),
                  tabPanel("Calculator_2", verbatimTextOutput("realdataROC_value_2")),
                  tabPanel("Calculator_3", verbatimTextOutput("realdataROC_value_3")),
                  tabPanel("Calculator_4", verbatimTextOutput("realdataROC_value_4")),
      )
    )
  )
)

prior_distribution_c_opt = function(nMonteprior, fND, fD, p){
  if (!is.double(fND) & !is.double(fD)) {
    # The case where it's inputted as a string
    fND = convert_char_to_vector(fND)
    fD = convert_char_to_vector(fD)
    if (!(valid_vector(fND) == TRUE & valid_vector(fD) == TRUE)) {
      return("Error: Either fND or fD provide invalid responses.")
    }
  }
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

post_distribution_c_opt = function(nMontepost, fND, fD, p){
  if (!is.double(fND) & !is.double(fD)) {
    fND = convert_char_to_vector(fND)
    fD = convert_char_to_vector(fD)
    if (!(valid_vector(fND) == TRUE & valid_vector(fD) == TRUE)) {
      return("Error: Either fND or fD provide invalid responses.")
    }
  }
  if (length(fND) == length(fD)) {
    m = length(fND)
  } else {
    return("Error: Invalid fND and fD; their lengths must be equal.")}
  alpha_postND = rep(1,m)+fND
  alpha_postD = rep(1,m)+fD
  postc_opt = rep(0,m)
  p = 0.5
  FN = rep(0,m)
  FP = rep(0,m)
  ERROR = rep(0,m)
  FNR = rep(0,m)
  FPR = rep(0,m)
  for (iMontepost in 1:nMontepost) {
    #generate Dirichlet values for pND and pD
    pND_post = rdirichlet(1,alpha_postND)
    pD_post = rdirichlet(1,alpha_postD)
    # compute FN_prior, FP_prior and ERROR-Prior  
    for(i in 1:m){
      FN[i] = sum(pD_post[1:i])
      FP[i] = 1-sum(pND_post[1:i])
      ERROR[i] = p*FN[i] + (1-p)*FP[i]
    }
    # locate the minimum value index (i.e. c opt)
    c_opt_value = which.min(ERROR)
    # update the posterior distribution of c_opt
    postc_opt[c_opt_value] = postc_opt[c_opt_value]+1
  }
  postc_opt = postc_opt/nMontepost
  newlist = list("FN" = FN, "FP" = FP, "ERROR" = ERROR, "FNR" = FNR,
                 "FPR" = FPR, "c_opt_value" = c_opt_value, "postc_opt" = postc_opt)
  return(newlist)
}

#6. this part of the program computes RB(i|fD, fND) i=1,...,m, c_opt(D, fND), Pl(fD, fND) and 
# posterior content of Pl(D, fND) printing out results as well

# need to change the placeholder name... lol
realdataROC_placeholder_1 = function(nMonteprior, nMontepost, fND, fD, p){
  if (!is.double(fND) & !is.double(fD)) {
    fND = convert_char_to_vector(fND)
    fD = convert_char_to_vector(fD)
    if (!(valid_vector(fND) == TRUE & valid_vector(fD) == TRUE)) {
      return("Error: Either fND or fD provide invalid responses.")
    }
  }
  if (length(fND) == length(fD)) {
    m = length(fND)
  } else {
    return("Invalid fND and fD; their lengths must be equal.")}
  priorc_opt = prior_distribution_c_opt(nMonteprior, fND, fD, p)$priorc_opt
  postc_opt = post_distribution_c_opt(nMontepost, fND, fD, p)$postc_opt
  RB = postc_opt/priorc_opt
  c_optfDfND = which.max(RB)
  PlfDfND = rep(0,m)
  for (i in 1:m) {
    if (RB[i] > 1 ) { PlfDfND[i] <- 1}
  }
  postPlfDfND = sum(postc_opt*PlfDfND)
  newlist = list("c_optfDfND" = c_optfDfND, "PlfDfND" = PlfDfND,
                 "postPlfDfND" = postPlfDfND)
  return(newlist)
}

#7. estimtating FN(c_opt(f_ND,f_D)), FP(c_opt(f_ND,f_D)) and ERROR(c_opt(f_ND,f_D))

#NOTE TO SELF: I have a feeling these variables can be changed depending on the research

#fND = "25, 16, 18, 21, 20"
#fD = "14, 11, 29, 28, 18"
#nMonteprior=100000
#nMontepost=100000 # used later I think
#p = 0.5
#ngrid = 100

#test = realdataROC_placeholder_2(ngrid, nMonteprior, nMontepost, fND, fD, p)
#test

realdataROC_placeholder_2 = function(ngrid, nMonteprior, nMontepost, fND, fD, p){
  grid = (c(1:(ngrid+1))-1)/ngrid
  # making the assumption this function isn't dependent on anything else -- MIGHT BE SOURCE OF FUTURE BUGS
  fND = convert_char_to_vector(fND)
  fD = convert_char_to_vector(fD)
  if (length(fND) == length(fD)){
    m = length(fND)
    results = realdataROC_placeholder_1(nMonteprior, nMontepost, fND, fD, p)
    #note: these two are from monte carlo; the variable names were becoming too long
    prior_result = prior_monte_carlo(m, ngrid, results, nMonteprior, p)
    post_result = post_monte_carlo(m, ngrid, results, nMontepost, fND, fD, p)
    
    RBFNc_opt = post_result$FNc_optpost/prior_result$FNc_optprior
    index = which.max(RBFNc_opt) # do we need this?
    FNc_optest = (grid[index]+grid[index+1])/2
    FPc_optpost = post_result$FPc_optpost/nMontepost
    #sum(FPc_optpost)
    RBFPc_opt = post_result$FPc_optpost/prior_result$FPc_optprior
    index = which.max(RBFPc_opt) # do we need this?
    FPc_optest = (grid[index]+grid[index+1])/2
    ERRORc_optpost = post_result$ERRORc_optpost/nMontepost
    #sum(ERRORc_optpost)
    RBERRORc_opt = ERRORc_optpost/prior_result$ERRORc_optprior
    index = which.max(RBERRORc_opt) # do we need this?
    ERRORc_optest = (grid[index]+grid[index+1])/2
    #outputs
    newlist = list("c_optfDfND" = results$c_optfDfND, "PlfDfND" = results$PlfDfND, 
                   "postPlfDfND" = results$postPlfDfND, "FNc_optest" = FNc_optest,
                   "FPc_optest" = FPc_optest, "ERRORc_optest" = ERRORc_optest)
    return(newlist)
  } else {
    return("Invalid fND and fD; their lengths must be equal.")
  }
}





