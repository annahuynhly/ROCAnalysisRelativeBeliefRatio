
################################################################
# DESCRIPTION                                                  #
################################################################

binormalcoptequalvariance_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later! Seeing if changes exist!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_binormalcoptequalvariance = div( # CHANGE THIS
  titlePanel("Section 3.3: binormalcoptequalvariance"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "binormalcoptequalvariance_L", # CHANGE THIS
                   tags$p('Number of subintervals of [0,1] for estimating density of copt', 
                          style = "font-size: 90%;"),value = 200),
      numericInput(inputId = "binormalcoptequalvariance_nMonteprior", # CHANGE THIS
                   tags$p('The sample size used for the MC Estimation of the Prior', 
                          style = "font-size: 90%;"),value = 3000000),
      numericInput(inputId = "binormalcoptequalvariance_nMontepost", # CHANGE THIS
                   tags$p('The sample size used for the MC Estimation of the Posterior', 
                          style = "font-size: 90%;"),value = 3000000),
      numericInput(inputId = "binormalcoptequalvariance_coptest", # CHANGE THIS
                   tags$p('copt estimate', 
                          style = "font-size: 90%;"),value = 0.715)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", binormalcoptequalvariance_description), # CHANGE THIS
                  tabPanel("Calculator_1", verbatimTextOutput("binormalcoptequalvariance_value_1")),
                  tabPanel("Plot_1", plotOutput(outputId = "binormalcoptequalvariance_plot_1")),
                  tabPanel("Calculator_2", verbatimTextOutput("binormalcoptequalvariance_value_2")),
                  tabPanel("Plot_2", plotOutput(outputId = "binormalcoptequalvariance_plot_2")),
                  tabPanel("Calculator_3", verbatimTextOutput("binormalcoptequalvariance_value_3")),
                  tabPanel("Plot_3", plotOutput(outputId = "binormalcoptequalvariance_plot_3")),
                  #tabPanel("Plot_3", # PLACEHOLDER FLUIDROW EXAMPLE
                  #         fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                  #                  plotOutput("binormalcoptequalvariance_plot_3_1"), 
                  #                  plotOutput("binormalcoptequalvariance_plot_3_2")))),
                  #tabPanel("Calculator_4", verbatimTextOutput("binormalcoptequalvariance_value_4")),
      )
    )
  )
)
################################################################
# MAIN FUNCTIONS                                               #
################################################################

# this function is for debugging purposes
test_function = function(x){
  return(x)
}

prior_dist_copt = function(nMonteprior, L, mu0, tau0, alpha0, beta0, a1, a2,
                           nND, xND, sND2, nD, xD, sD2){
  # samples from (muD,sigmaD,muND,sigmaND) and prevalence w
  values = generate_A_and_grid(L)
  A = values$A
  grid = values$grid
  
  sigmaD = sqrt(1/rgamma(nMonteprior,alpha0,beta0))
  sigmaND = sigmaD
  muD = rnorm(nMonteprior,mu0,(tau0*sigmaD))
  priorimpwt = pnorm((muD-mu0)/(tau0*sigmaND))
  U = rbeta(nMonteprior,1,1)
  muND = mu0+tau0*sigmaND*qnorm(priorimpwt*U)
  # prevalence
  w = rbeta(nMonteprior,a1,a2)
  c=0.5*(muD+muND)+(sigmaD**2)*(log((1-w)/w))/(muD-muND)
  cmod=(pi/2+atan(c))/pi
  cmodmax=max(cmod)
  cmodmin=min(cmod)
  priorcmod <- rep(0,L)
  
  for (iMonteprior in 1:nMonteprior) {
    for (igrid in 1:L){
      if ( (A[igrid] < cmod[iMonteprior]) & (cmod[iMonteprior] <= A[igrid+1]) ) {
        priorcmod[igrid]=priorcmod[igrid]+priorimpwt[iMonteprior]
      }
    }
  }
  priorcmod=priorcmod/sum(priorcmod)
  priorcmoddensity=L*priorcmod
  
  newlist = list("cmodmax" = cmodmax, "cmodmin" = cmodmin, "priorcmod" = priorcmod,
                 "priorcmoddensity" = priorcmoddensity, "grid" = grid)
  return(newlist)
}


post_dist_copt = function(nMontepost, L, mu0, tau0, alpha0, beta0, a1, a2,
                          nND, xND, sND2, nD, xD, sD2){
  
  values = generate_A_and_grid(L)
  A = values$A
  grid = values$grid
  post_val = compute_hyperparameters(mu0, tau0, alpha0, beta0, a1, a2, 
                                     nND, xND, sND2, nD, xD, sD2)
  sigmaDpost = sqrt(1/rgamma(nMontepost, post_val$alpha0post, post_val$beta0post))
  sigmaNDpost = sigmaDpost
  muDpost = post_val$mu0Dpost + (post_val$tau0D)*(sigmaDpost)*rnorm(nMontepost,0,1)
  
  postimpwt = pnorm((muDpost-post_val$mu0NDpost)/(post_val$tau0ND*sigmaNDpost))
  U = rbeta(nMontepost,1,1)
  muNDpost = post_val$mu0NDpost + post_val$tau0ND*sigmaNDpost*qnorm(postimpwt*U)
  wpost = rbeta(nMontepost, post_val$a1post, post_val$a2post)
  
  c = 0.5*(muDpost+muNDpost)+(sigmaDpost**2)*(log((1-wpost)/wpost))/(muDpost-muNDpost)
  #print(c)
  cmod = (pi/2+atan(c))/pi
  cmodmax = max(cmod)
  cmodmin = min(cmod)
  
  postcmod = rep(0,L)
  
  # this is the loop for the Monte Carlo for the posterior
  for (iMontepost in 1:nMontepost) { 
    for (igrid in 1:L){
      if ( (A[igrid] < cmod[iMontepost]) & (cmod[iMontepost] <= A[igrid+1])) {
        postcmod[igrid]=postcmod[igrid]+postimpwt[iMontepost] 
      }
    }
  }
  
  postcmod=postcmod/sum(postcmod)
  postcmoddensity=L*postcmod
  newlist = list("cmodmax" = cmodmax, "cmodmin" = cmodmin, "postcmod" = postcmod,
                 "postcmoddensity" = postcmoddensity, "grid" = grid)
  return(newlist)
}


relative_belief_ratio_inferences = function(nMonteprior, nMontepost, L, mu0, tau0, alpha0, beta0, 
                                            a1, a2, nND, xND, sND2, nD, xD, sD2){
  values = generate_A_and_grid(L)
  A = values$A
  grid = values$grid
  
  prior_results = prior_dist_copt(nMonteprior, L, mu0, tau0, alpha0, beta0, a1, a2, nND, xND, sND2, nD, xD, sD2)
  post_results = post_dist_copt(nMontepost, L, mu0, tau0, alpha0, beta0, a1, a2, nND, xND, sND2, nD, xD, sD2)
  postcmod = post_results$postcmod
  priorcmod = prior_results$priorcmod
  
  RBcmod <- postcmod/priorcmod
  
  postPlcmod=0
  #print("The values in the plausible region for cmod, copt, the RB and the post. prob. at those values")
  #for (i in 1:L) {
  #  if (priorcmod[i] > 0 & RBcmod[i] > 1 ) {
  #    postPlcmod=postPlcmod+postcmod[i]
  #    cat(grid[i],tan(pi*grid[i]-pi/2),RBcmod[i],postcmod[i],"\n")}
  #}
  #cat("The posterior content of the plausible region = ",postPlcmod,"\n")
  
  imax=1
  for (i in 1:L) {
    if (priorcmod[i] >0 & RBcmod[i] > RBcmod[imax] ) {imax=i }
  }
  cmodest=grid[imax]
  coptest=tan(pi*cmodest-pi/2)
  
  newlist = list("RBcmod" = RBcmod, "cmodest" = cmodest, "coptest" = coptest, "grid" = grid)
  return(newlist)
}


prior_obtain_error_characteristics_copt_estimate = function(coptest, nMonteprior, L, mu0, tau0, alpha0, beta0, 
                                                            a1, a2, nND, xND, sND2, nD, xD, sD2){
  values = generate_A_and_grid(L)
  A = values$A
  grid = values$grid
  
  # samples from (muD,sigmaD,muND,sigmaND) and prevalence w
  sigmaD = sqrt(1/rgamma(nMonteprior,alpha0,beta0))
  sigmaND = sigmaD
  muD = rnorm(nMonteprior,mu0,(tau0*sigmaD))
  priorimpwt = pnorm((muD-mu0)/(tau0*sigmaND))
  U = rbeta(nMonteprior,1,1)
  muND = mu0+tau0*sigmaND*qnorm(priorimpwt*U)
  # prevalence
  w = rbeta(nMonteprior,a1,a2)
  
  FNR=pnorm((coptest-muD)/sigmaD)
  FPR=1-pnorm((coptest-muND)/sigmaND)
  Error=w*FNR+(1-w)*FPR
  FDR=(1-w)*FPR/((1-w)*FPR+w*(1-FNR))
  FNDR=w*FNR/(w*FNR+(1-w)*(1-FPR))
  
  priorFNR = rep(0,L)
  priorFPR = rep(0,L)
  priorError = rep(0,L)
  priorFDR = rep(0,L)
  priorFNDR = rep(0,L)
  
  for (iMonteprior in 1:nMonteprior) {
    for (igrid in 1:L){
      if ( (A[igrid] < FNR[iMonteprior]) & (FNR[iMonteprior] <= A[igrid+1]) ) {
        priorFNR[igrid]=priorFNR[igrid]+priorimpwt[iMonteprior]}
      if ( (A[igrid] < FPR[iMonteprior]) & (FPR[iMonteprior] <= A[igrid+1]) ) {
        priorFPR[igrid]=priorFPR[igrid]+priorimpwt[iMonteprior]}
      if ( (A[igrid] < Error[iMonteprior]) & (Error[iMonteprior] <= A[igrid+1]) ) {
        priorError[igrid]=priorError[igrid]+priorimpwt[iMonteprior]}
      if ( (A[igrid] < FDR[iMonteprior]) & (FDR[iMonteprior] <= A[igrid+1]) ) {
        priorFDR[igrid]=priorFDR[igrid]+priorimpwt[iMonteprior]}
      if ( (A[igrid] < FNDR[iMonteprior]) & (FNDR[iMonteprior] <= A[igrid+1]) ) {
        priorFNDR[igrid]=priorFNDR[igrid]+priorimpwt[iMonteprior]}
    }
  }
  priorFNR=priorFNR/sum(priorFNR)
  priorFNRdensity=L*priorFNR
  
  priorFPR=priorFPR/sum(priorFPR)
  priorFPRdensity=L*priorFPR
  
  priorError=priorError/sum(priorError)
  priorErrordensity=L*priorError
  
  priorFDR=priorFDR/sum(priorFDR)
  priorFDRdensity=L*priorFDR
  
  priorFNDR=priorFNDR/sum(priorFNDR)
  priorFNDRdensity=L*priorFNDR
  
  newlist = list("priorFNRdensity" = priorFNRdensity, "priorFPRdensity" = priorFPRdensity, 
                 "priorErrordensity" = priorErrordensity, "priorFDRdensity" = priorFDRdensity,
                 "priorFNDRdensity" = priorFNDRdensity)
  return(newlist)
}

post_obtain_error_characteristics_copt_estimate = function(coptest, nMontepost, L, mu0, tau0, alpha0, beta0, 
                                                           a1, a2, nND, xND, sND2, nD, xD, sD2){
  values = generate_A_and_grid(L)
  A = values$A
  grid = values$grid
  post_val = compute_hyperparameters(mu0, tau0, alpha0, beta0, a1, a2, 
                                     nND, xND, sND2, nD, xD, sD2)
  
  # samples from (muD,sigmaD,muND,sigmaND) and prevalence w
  sigmaDpost <- sqrt(1/rgamma(nMontepost, post_val$alpha0post, post_val$beta0post))
  sigmaNDpost = sigmaDpost
  muDpost = rnorm(nMontepost, post_val$mu0Dpost, (post_val$tau0D*sigmaDpost))
  postimpwt=pnorm((muDpost-post_val$mu0NDpost)/(post_val$tau0ND*sigmaNDpost))
  U=rbeta(nMontepost,1,1)
  muNDpost = post_val$mu0NDpost + post_val$tau0ND*sigmaNDpost*qnorm(postimpwt*U)
  # prevalence
  wpost=rbeta(nMontepost,post_val$a1post,post_val$a2post)
  
  FNRpost=pnorm((coptest-muDpost)/sigmaDpost)
  FPRpost=1-pnorm((coptest-muNDpost)/sigmaNDpost)
  Errorpost=wpost*FNRpost+(1-wpost)*FPRpost
  FDRpost=(1-wpost)*FPRpost/((1-wpost)*FPRpost+wpost*(1-FNRpost))
  FNDRpost=wpost*FNRpost/(wpost*FNRpost+(1-wpost)*(1-FPRpost))
  
  postFNR <- rep(0,L)
  postFPR <- rep(0,L)
  postError <- rep(0,L)
  postFDR <- rep(0,L)
  postFNDR <- rep(0,L)
  
  for (iMontepost in 1:nMontepost) {
    for (igrid in 1:L){
      if ( (A[igrid] < FNRpost[iMontepost]) & (FNRpost[iMontepost] <= A[igrid+1]) ) {
        postFNR[igrid]=postFNR[igrid]+postimpwt[iMontepost]}
      if ( (A[igrid] < FPRpost[iMontepost]) & (FPRpost[iMontepost] <= A[igrid+1]) ) {
        postFPR[igrid]=postFPR[igrid]+postimpwt[iMontepost]}
      if ( (A[igrid] < Errorpost[iMontepost]) & (Errorpost[iMontepost] <= A[igrid+1]) ) {
        postError[igrid]=postError[igrid]+postimpwt[iMontepost]}
      if ( (A[igrid] < FDRpost[iMontepost]) & (FDRpost[iMontepost] <= A[igrid+1]) ) {
        postFDR[igrid]=postFDR[igrid]+postimpwt[iMontepost]}
      if ( (A[igrid] < FNDRpost[iMontepost]) & (FNDRpost[iMontepost] <= A[igrid+1]) ) {
        postFNDR[igrid]=postFNDR[igrid]+postimpwt[iMontepost]}
    }
  }
  
  postFNR = postFNR/sum(postFNR)
  postFPR = postFPR/sum(postFPR)
  postError = postError/sum(postError)
  postFDR = postFDR/sum(postFDR)
  postFNDR = postFNDR/sum(postFNDR)
  
  newlist = list("postFNR" = postFNR, "postFPR" = postFPR, "postError" = postError,
                 "postFDR" = postFDR, "postFNDR" = postFNDR)
  return(newlist)
}


obtain_relative_belief_ratio_and_inferences = function(coptest, nMonteprior, nMontepost, L, mu0, tau0, 
                                                       alpha0, beta0, a1, a2, nND, xND, sND2, nD, xD, sD2){
  values = generate_A_and_grid(L)
  A = values$A
  grid = values$grid
  
  prior_results = prior_obtain_error_characteristics_copt_estimate(coptest, nMonteprior, L, mu0, tau0, alpha0, beta0, 
                                                                   a1, a2, nND, xND, sND2, nD, xD, sD2)
  post_results = post_obtain_error_characteristics_copt_estimate(coptest, nMontepost, L, mu0, tau0, alpha0, beta0, 
                                                                 a1, a2, nND, xND, sND2, nD, xD, sD2)
  # obtain relative belief ratio and inferences
  
  RBFNR <- post_results$postFNR/prior_results$priorFNR
  RBFPR <- post_results$postFPR/prior_results$priorFPR
  RBError <- post_results$postError/prior_results$priorError
  RBFDR <- post_results$postFDR/prior_results$priorFDR
  RBFNDR <- post_results$postFNDR/prior_results$priorFNDR
  
  # to get a starting value for imax
  for (i in 1:L) {
    if (prior_results$priorFNR[i]>0) {imaxFNR=i}
    if (prior_results$priorFPR[i]>0) {imaxFPR=i}
    if (prior_results$priorError[i]>0) {imaxError=i}
    if (prior_results$priorFDR[i]>0) {imaxFDR=i}
    if (prior_results$priorFNDR[i]>0) {imaxFNDR=i}
  }
  imaxFNR
  imaxFPR
  imaxError
  imaxFDR
  imaxFNDR
  for (i in 1:L) {
    if (prior_results$priorFNR[i] >0 & RBFNR[i] > RBFNR[imaxFNR] ) {imaxFNR=i }
    if (prior_results$priorFPR[i] >0 & RBFPR[i] > RBFPR[imaxFPR] ) {imaxFPR=i }
    if (prior_results$priorError[i] >0 & RBError[i] > RBError[imaxError] ) {imaxError=i }
    if (prior_results$priorFDR[i] >0 & RBFDR[i] > RBFDR[imaxFDR] ) {imaxFDR=i }
    if (prior_results$priorFNDR[i] >0 & RBFNDR[i] > RBFNDR[imaxFNDR] ) {imaxFNDR=i }
  }
  
  FNRest = grid[imaxFNR]
  FPRest = grid[imaxFPR]
  Errorest = grid[imaxError]
  FDRest = grid[imaxFDR]
  FNDRest = grid[imaxFNDR]
  
  postFNRdensity = L*post_results$postFNR
  postFPRdensity = L*post_results$postFPR
  postErrordensity = L*post_results$postError
  postFDRdensity = L*post_results$postFDR
  postFNDRdensity = L*post_results$postFNDR
  
  newlist = list("FNRest" = FNRest, "FPRest" = FPRest, "Errorest" = Errorest,
                 "FDRest" = FDRest, "FNDRest" = FNDRest, "postFNRdensity" = postFNRdensity,
                 "postFPRdensity" = postFPRdensity, "postErrordensity" = postErrordensity,
                 "postFDRdensity" = postFDRdensity, "postFNDRdensity" = postFNDRdensity)
  return(newlist)
  
}