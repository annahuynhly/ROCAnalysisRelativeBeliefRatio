
################################################################
# DESCRIPTION                                                  #
################################################################

binormalcoptunequalvariance_description = div( # CHANGE THIS
  titlePanel("Page Description"),
  p("Need to work on this section. Come back later!"),
)

################################################################
# PAGE LOGIC                                                   #
################################################################

page_binormalcoptunequalvariance = div( # CHANGE THIS
  titlePanel("Section 3.3: binormalcoptunequalvariance"), # CHANGE THIS
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      numericInput(inputId = "binormalcoptunequalvariance_L", # CHANGE THIS
                   tags$p('Number of subintervals of [0,1] for estimating density of copt', 
                          style = "font-size: 90%;"),value = 200),
      numericInput(inputId = "binormalcoptunequalvariance_nMonteprior", # CHANGE THIS
                   tags$p('The sample size used for the MC Estimation of the Prior', 
                          style = "font-size: 90%;"),value = 2000000),
      numericInput(inputId = "binormalcoptunequalvariance_nMontepost", # CHANGE THIS
                   tags$p('The sample size used for the MC Estimation of the Posterior', 
                          style = "font-size: 90%;"),value = 3000000),
      numericInput(inputId = "binormalcoptunequalvariance_coptest", # CHANGE THIS
                   tags$p('copt estimate', style = "font-size: 90%;"),value = 0.7386115),
      numericInput(inputId = "binormalcoptunequalvariance_lambda", # CHANGE THIS
                   tags$p('Lambda', style = "font-size: 90%;"),value = 1)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # OUTPUTTING THE VALUES
      tabsetPanel(type = "tabs",
                  tabPanel("Description", binormalcoptunequalvariance_description), # CHANGE THIS
                  tabPanel("Calculator_1", verbatimTextOutput("binormalcoptunequalvariance_value_1")),
                  tabPanel("Plot_1", plotOutput(outputId = "binormalcoptunequalvariance_plot_1")),
                  tabPanel("Calculator_2", verbatimTextOutput("binormalcoptunequalvariance_value_2")),
                  tabPanel("Plot_2", plotOutput(outputId = "binormalcoptunequalvariance_plot_2")),
                  tabPanel("Calculator_3", verbatimTextOutput("binormalcoptunequalvariance_value_3")),
                  tabPanel("Plot_3", plotOutput(outputId = "binormalcoptunequalvariance_plot_3")),
      )
    )
  )
)
################################################################
# MAIN FUNCTIONS                                               #
################################################################

prior_dist_copt_unequalvar = function(lambda, nMonteprior, L, mu0, tau0, alpha0, beta0, a1, a2,
                                      nND, xND, sND2, nD, xD, sD2){
  values = generate_A_and_grid(L)
  A = values$A
  grid = values$grid
  # samples from (muD,sigmaD,muND,sigmaND) and prevalence w
  sigmaD2 = 1/rgamma(nMonteprior,alpha0,beta0)
  sigmaND2 = 1/rgamma(nMonteprior,alpha0,beta0)
  sigmaD = sqrt(sigmaD2)
  sigmaND = sqrt(sigmaND2)
  # prevalence
  w = rbeta(nMonteprior,a1,a2)
  muD = rnorm(nMonteprior,mu0,(tau0*sigmaD))
  priordeltaTmp = 2*(sigmaD2-sigmaND2)*log(((1-w)/w)*(sigmaD/sigmaND))
  priordeltaTmp[priordeltaTmp>0] = 0
  priordeltaTmp = sqrt(-priordeltaTmp)
  priorimpwt = pnorm((muD-priordeltaTmp-mu0)/(tau0*sigmaND))
  U = rbeta(nMonteprior,1,1)
  muND = mu0 + tau0*sigmaND*qnorm(priorimpwt*U)
  
  countinfsprior=0
  for (i in 1:nMonteprior){
    if (muND[i]==-Inf | muND[i]==Inf){ countinfsprior = countinfsprior + 1}
  }
  #countinfsprior/nMonteprior # Unsure what this is for
  
  cond2 = (muND-muD)**2 + 2*(sigmaD2-sigmaND2)*log(((1-w)/w)*(sigmaD/sigmaND))
  check1 = min(muD-priordeltaTmp-muND) 
  check2 = min(cond2)
  
  c=0*muND
  for (i in 1:nMonteprior){
    if ( muND[i]==-Inf | muND[i]==Inf){ c[i]=-Inf } 
    else
    {c[i]=(sigmaND2[i]*muD[i]-sigmaD2[i]*muND[i])-(sigmaD[i]*sigmaND[i])*sqrt(cond2[i])}
  }
  c = c/(sigmaND2-sigmaD2)
  
  cmod = pt(c,lambda)
  cmodmax = max(cmod)
  cmodmin = min(cmod)
  
  priorcmod <- rep(0,L)
  for (iMonteprior in 1:nMonteprior){
    for (igrid in 1:L){
      if ( (A[igrid] < cmod[iMonteprior]) & (cmod[iMonteprior] <= A[igrid+1]) ) {
        priorcmod[igrid]=priorcmod[igrid]+priorimpwt[iMonteprior]
      }
    }
  }
  
  priorcmod=priorcmod/sum(priorcmod)
  priorcmoddensity=L*priorcmod
  
  # Might want to display more values..?
  newlist = list("priorcmoddensity" = priorcmoddensity, "priorcmod" = priorcmod, 
                 "cmodmin" = cmodmin, "cmodmax" = cmodmax, "grid" = grid)
  return(newlist)
}


post_dist_copt_unequalvar = function(lambda, nMonteprior, L, mu0, tau0, alpha0, beta0, a1, a2,
                                     nND, xND, sND2, nD, xD, sD2){
  
  values = generate_A_and_grid(L)
  A = values$A
  grid = values$grid
  post_val = compute_hyperparameters(mu0, tau0, alpha0, beta0, a1, a2, 
                                     nND, xND, sND2, nD, xD, sD2)
  
  sigmaD2post = 1/rgamma(nMontepost, post_val$alpha0Dpost, post_val$beta0Dpost)
  sigmaND2post = 1/rgamma(nMontepost, post_val$alpha0NDpost, post_val$beta0NDpost)
  sigmaDpost = sqrt(sigmaD2post)
  sigmaNDpost = sqrt(sigmaND2post)
  wpost = rbeta(nMontepost, post_val$a1post, post_val$a2post)
  muDpost = post_val$mu0Dpost + post_val$tau0D*sigmaDpost*rnorm(nMontepost,0,1)
  postdeltaTmp = 2*(sigmaD2post- sigmaND2post)*log(((1-wpost)/wpost)*(sigmaDpost/sigmaNDpost))
  postdeltaTmp[postdeltaTmp>0] = 0
  postdeltaTmp = sqrt(-postdeltaTmp)
  postimpwt = pnorm((muDpost-postdeltaTmp-post_val$mu0NDpost)/(post_val$tau0ND*sigmaNDpost))
  U = rbeta(nMontepost,1,1)
  muNDpost = post_val$mu0NDpost + post_val$tau0ND*sigmaNDpost*qnorm(postimpwt*U)
  
  countinfspost=0
  for (i in 1:nMontepost){
    if (muNDpost[i]== -Inf | muNDpost[i]==Inf){ countinfspost=countinfspost+1}
  }
  #countinfspost/nMontepost
  
  cond2 = (muNDpost-muDpost)**2 + 2*(sigmaD2post-sigmaND2post)*log(((1-wpost)/wpost)*(sigmaDpost/sigmaNDpost))
  check1 = min(muDpost-postdeltaTmp-muNDpost) 
  check2 = min(cond2)
  
  c=0*muNDpost
  for (i in 1:nMontepost){
    if ( muNDpost[i]==-Inf | muNDpost[i]==Inf){ c[i]=-Inf } 
    else
    {c[i]=(sigmaND2post[i]*muDpost[i]-sigmaD2post[i]*muNDpost[i])-(sigmaDpost[i]*sigmaNDpost[i])*sqrt(cond2[i])}
  }
  c = c/(sigmaND2post-sigmaD2post)
  
  #cmod=(pi/2+atan(c))/pi
  cmod = pt(c,lambda)
  cmodmax = max(cmod)
  cmodmin = min(cmod)
  
  postcmod <- rep(0,L)
  for (iMontepost in 1:nMontepost){
    for (igrid in 1:L){
      if ( (A[igrid] < cmod[iMontepost]) & (cmod[iMontepost] <= A[igrid+1]) ) {
        postcmod[igrid]=postcmod[igrid]+postimpwt[iMontepost]
      }
    }
  }
  
  postcmod = postcmod/sum(postcmod)
  postcmoddensity = L*postcmod
  
  # Might want to display more values..?
  newlist = list("postcmoddensity" = postcmoddensity, "postcmod" = postcmod, "cmodmax" = cmodmax,
                 "cmodmin" = cmodmin, "grid" = grid)
  return(newlist)
}


relative_belief_ratio_inferences_unequalvar = function(lambda, nMonteprior, nMontepost, L, mu0, tau0, 
                                                       alpha0, beta0, a1, a2, nND, xND, sND2, nD, xD, sD2){
  values = generate_A_and_grid(L)
  A = values$A
  grid = values$grid
  
  prior_results = prior_dist_copt_unequalvar(lambda, nMonteprior, L, mu0, tau0, alpha0, beta0, a1, a2, 
                                             nND, xND, sND2, nD, xD, sD2)
  post_results = post_dist_copt_unequalvar(lambda, nMontepost, L, mu0, tau0, alpha0, beta0, a1, a2, 
                                           nND, xND, sND2, nD, xD, sD2)
  
  RBcmod = post_results$postcmod/prior_results$priorcmod
  
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
    if (prior_results$priorcmod[i] >0) {imax=i}
  }
  for (i in 1:L) {
    if (prior_results$priorcmod[i] >0 && RBcmod[i] > RBcmod[imax] ) {imax=i }
  }
  cmodest = grid[imax]
  coptest = tan(pi*cmodest-pi/2)
  
  newlist = list("coptest" = coptest, "cmodest" = cmodest, "RBcmod" = RBcmod, "grid" = grid)
  return(newlist)
}

