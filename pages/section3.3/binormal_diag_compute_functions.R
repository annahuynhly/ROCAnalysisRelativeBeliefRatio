################################################################
# HELPER FUNCTIONS                                             #
################################################################

#source("C:\\Users\\AnnaH\\OneDrive\\Desktop\\Stats RA\\ShinyWebpage\\pages\\helper_functions.R")

# Note: this doesn't seem to be working that well.
fcnAUC <- function(z){
  return (dnorm(z)*pnorm((muDi-muNDi)/sigmaDi+sigmaNDi/sigmaDi*z))
}

################################################################
# FUNCTIONS FOR COMPUTATIONS                                   #
################################################################

binormal_compute_post_hyperpara = function(mu0, tau0, lambda1, lambda2, nND, meanND, 
                                           sND_squared, nD, meanD, sD_squared){
  lambda1post = lambda1 + (nD+nND)/2
  tau0D = 1/sqrt(nD+1/tau0^2)
  tau0ND = 1/sqrt(nND+1/tau0^2)
  lambda2post = (lambda2 + (sD_squared+sND_squared)/2 + (tau0D**2)*(nD/tau0^2)*(meanD - mu0)^2/2 + 
                   (tau0ND**2)*(nND/tau0^2)*(meanND - mu0)^2/2)
  mu0Dpost = (tau0D**2)*(nD*meanD + mu0/tau0**2)
  mu0NDpost = (tau0ND**2)*(nND*meanND + mu0/tau0**2)
  
  newlist = list("lambda1post" = lambda1post, "tau0D" = tau0D, "tau0ND" = tau0ND,
                 "lambda2post" = lambda2post, "mu0Dpost" = mu0Dpost, "mu0NDpost" = mu0NDpost)
}

binormal_diag_prior = function(w = FALSE, alpha1w = NA, alpha2w = NA, 
                               nMonteprior, delta, lambda1, lambda2, mu0, tau0){
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  priorAUC = rep(0, L)
  probAUCprior = 0
  sigmaD = sqrt(1/rgamma(nMonteprior,lambda1,lambda2))
  sigmaND = sigmaD
  muD = rnorm(nMonteprior, mu0, (tau0*sigmaD))
  muND = rnorm(nMonteprior, mu0, (tau0*sigmaND))
  
  pre_w = rep(0, nMonteprior)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, version = "prior")
  }
    
  priorimpwt = pnorm((muD - mu0)/(tau0*sigmaND)) # ADDED FOR COPT
  U = rbeta(nMonteprior,1,1) # ADDED FOR COPT
  muND_copt = mu0 + tau0*sigmaND*qnorm(priorimpwt*U) # ADDED FOR COPT
  c = 0.5*(muD + muND_copt)+(sigmaD**2)*(log((1 - pre_w)/pre_w))/(muD - muND_copt) # ADDED FOR COPT
  cmod = (pi/2 + atan(c))/pi # ADDED FOR COPT
  cmodmax = max(cmod) # ADDED FOR COPT
  cmodmin = min(cmod) # ADDED FOR COPT
  priorcmod = rep(0,L) # ADDED FOR COPT
  
  for (iMonteprior in 1:nMonteprior) {
    muDi = muD[iMonteprior]
    muNDi = muND[iMonteprior]
    if (muDi > muNDi){probAUCprior = probAUCprior+1}
    sigmaDi = sigmaD[iMonteprior]
    sigmaNDi = sigmaDi
    # Need to create the function here to use for integration
    fcnAUC <- function(z){return (dnorm(z)*pnorm((muDi-muNDi)/sigmaDi+sigmaNDi/sigmaDi*z))}
    AUC = integrate(fcnAUC, -Inf, Inf, abs.tol = 0.001) 
    
    for (igrid in 1:L){
      if ( (A[igrid] < as.numeric(AUC[1])) & (as.numeric(AUC[1]) <= A[igrid+1]) ){
        priorAUC[igrid] = priorAUC[igrid] + 1 
      }
      if ( (A[igrid] < cmod[iMonteprior]) & (cmod[iMonteprior] <= A[igrid+1]) ) { # THIS IF STAT. IS ADDED FOR COPT
        priorcmod[igrid] = priorcmod[igrid] + priorimpwt[iMonteprior]
      }
    }
  }
  priorAUC = average_vector_values(priorAUC) # APPLYING SMOOTHER
  priorAUC = priorAUC/nMonteprior
  probAUCprior = probAUCprior/nMonteprior
  priorAUCdensity = L*priorAUC
  
  priorcmod = priorcmod/sum(priorcmod) # ADDED FOR COPT
  priorcmoddensity = L*priorcmod # ADDED FOR COPT
  
  newlist = list("priorAUC" = priorAUC, "probAUCprior" = probAUCprior,
                 "priorAUCdensity" = priorAUCdensity,
                 "priorcmod" = priorcmod, "priorcmoddensity" = priorcmoddensity)
  return(newlist)
}

binormal_diag_post = function(w = FALSE, alpha1w = NA, alpha2w = NA, nND = NA, nD = NA, version,
                              nMontepost, delta, lambda1post, lambda2post, 
                              mu0Dpost, mu0NDpost, tau0D, tau0ND){
  A = closed_bracket_grid(delta) # this is technically their grid
  L = (1/delta) # length
  postAUC = rep(0,L)
  probAUCpost = 0
  sigmaDpost = sqrt(1/rgamma(nMontepost, lambda1post, lambda2post))
  sigmaNDpost = sigmaDpost
  muDpost = mu0Dpost+tau0D*sigmaDpost*rnorm(nMontepost,0,1)
  muNDpost = mu0NDpost+tau0ND*sigmaNDpost*rnorm(nMontepost,0,1)
  
  pre_w = rep(0, nMonteprior)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, nD, nND, version) # ADDED FOR COPT
  }
  
  postimpwt = pnorm((muDpost - mu0NDpost)/(tau0ND*sigmaNDpost)) # ADDED FOR COPT
  U = rbeta(nMontepost,1,1) # ADDED FOR COPT
  muNDpost_copt = mu0NDpost + tau0ND*sigmaNDpost*qnorm(postimpwt*U) # ADDED FOR COPT
  
  c = 0.5*(muDpost + muNDpost_copt)+(sigmaDpost**2)*(log((1-pre_w)/pre_w))/(muDpost-muNDpost_copt) # ADDED FOR COPT
  cmod = (pi/2+atan(c))/pi # ADDED FOR COPT
  cmodmax = max(cmod) # ADDED FOR COPT
  cmodmin = min(cmod) # ADDED FOR COPT
  
  postcmod = rep(0,L)

  # this is the loop for the Monte Carlo for the posterior
  for (iMontepost in 1:nMontepost) {
    muDi = muDpost[iMontepost]
    muNDi = muNDpost[iMontepost]
    if (muDi > muNDi){probAUCpost = probAUCpost+1}
    sigmaDi = sigmaDpost[iMontepost]
    sigmaNDi = sigmaDi
    # Need to create the function here to use for integration
    fcnAUC <- function(z){return (dnorm(z)*pnorm((muDi-muNDi)/sigmaDi+sigmaNDi/sigmaDi*z))}
    AUC <- integrate(fcnAUC, -Inf, Inf, abs.tol = 0.001) 
    for (igrid in 1:L){
      if ( (A[igrid] < as.numeric(AUC[1])) & (as.numeric(AUC[1]) <= A[igrid+1]) ) {
        postAUC[igrid] = postAUC[igrid] + 1 
      }
      if ( (A[igrid] < cmod[iMontepost]) & (cmod[iMontepost] <= A[igrid+1]) ) {
        postcmod[igrid] = postcmod[igrid] + postimpwt[iMontepost] 
      }
    }
  }
  postAUC = average_vector_values(postAUC) # applying a smoother
  postAUC = postAUC/nMontepost
  postAUCdensity = L*postAUC
  probAUCpost = probAUCpost/nMontepost
  
  postcmod = postcmod/sum(postcmod)
  postcmoddensity = L*postcmod
  
  newlist = list("postAUC" = postAUC, "postAUCdensity" = postAUCdensity,
                 "probAUCpost" = probAUCpost,
                 "postcmod" = postcmod, "postcmoddensity" = postcmoddensity)
  return(newlist)
}

binormal_diag_RBR = function(delta, probAUCprior, probAUCpost, priorAUC, postAUC,
                             priorcmod, postcmod){
  grid = open_bracket_grid(delta)
  L = ((1/delta) - 1)
  # Note: rel. belief ratio of AUC > 1/2 = RBprobAUC
  # Strength of evidence = probAUCpost
  RBprobAUC = probAUCpost/probAUCprior
  
  RBcmod = postcmod/priorcmod # ADDED FOR COPT
  
  RB_AUC = rep(0, length(priorAUC)) # Assuming priorAUC and postAUC are of same length
  for (i in 1:length(RB_AUC)){
    if (priorAUC[i] != 0){
      RB_AUC[i] = postAUC[i]/priorAUC[i]
    } else {
      RB_AUC[i] = NA
    }
  }

  AUCest = grid[which.max(RB_AUC)]
  
  # this is for testing for the plausible region
  RB_pl_test = RB_AUC
  RB_pl_test[is.na(RB_pl_test)] = 0
  
  # Getting the plausible region - assumes no breaks
  plausible_region = c()
  for (i in 1:length(grid)){
    if (RB_pl_test[i] > 1){
      plausible_region = c(plausible_region, as.numeric(grid[i]))
    }
  }
  plausible_region = c(plausible_region[1], 
                       plausible_region[length(plausible_region)])
  
  postPl_AUC=0 # posterior content
  temp_RB_AUC =  NA_to_0(RB_AUC)
  for (i in 1:L) {
    # reason: need prior to be non negative
    if (priorAUC[i] > 0 & temp_RB_AUC[i] > 1) {postPl_AUC = postPl_AUC + postAUC[i]}
  }
  
  imax = 1
  temp_RBcmod = NA_to_0(RBcmod)
  for (i in 1:L) {
    if (priorcmod[i] > 0 & temp_RBcmod[i] > temp_RBcmod[imax] ){imax = i}
  }
  cmodest = grid[imax]
  coptest = tan(pi*cmodest-pi/2)
  
  newlist = list("RB_AUC" = RB_AUC, "RBprobAUC" = RBprobAUC,
                 "AUCest" = AUCest, "postPl_AUC" = postPl_AUC,
                 "plausible_region" = plausible_region, "RBcmod" = RBcmod,
                 "cmodest" = cmodest, "coptest" = coptest)
}


binormal_diag_compute_credible_region = function(gamma, delta, AUC_RBR, AUC_prior, AUC_post, 
                                                 posterior_content){
  # Note: credible region is now based on the line plot.
  grid = open_bracket_grid(delta)
  AUC_RBR[is.na(AUC_RBR)] = 0
  # Computes the credible region. At first, there's no default input to avoid generating
  # a credible region automatically (it is not necessary.)
  if (check.numeric(gamma) == FALSE){
    err_msg = "Need to put in a valid input for gamma (see graph 1.)"
    return(list("credible_region" = err_msg, "rb_line" = err_msg))
  }
  else { # This condition runs if the user provides an actual numeric input.
    gamma = as.numeric(gamma)
    if(gamma >= posterior_content){
      err_msg = "Gamma must be less than the posterior content of the plausible region."
      return(list("credible_region" = err_msg, "rb_line" = err_msg))
    } 
    else {
      RBR_values = sort(AUC_RBR, decreasing = TRUE)
      RBR_values = RBR_values[RBR_values > 1] # sorting for values larger than 1
      for(i in 2:length(RBR_values)){ # doesn't start at the top as the AREA of a line is 0
        rb_line = RBR_values[i]
        credible_region = c()
        j_vals = c()
        # find the region associated with it
        # WARNING: BOLD ASSUMPTION NO BREAKPOINTS/PEAKS
        for(j in 1:length(AUC_RBR)){
          if(AUC_RBR[j] > rb_line){
            credible_region = c(credible_region, grid[j])
            j_vals = c(j_vals, j)
          }
        }
        credible_region = c(min(credible_region), max(credible_region))
        
        test_area = 0
        for (i in min(j_vals):max(j_vals)) {
          if (AUC_prior[i] > 0 & AUC_RBR[i] > 1) {test_area = test_area + AUC_post[i]}
        }
        if(test_area >= gamma){
          break # This means the credible region was actually found
        }
      }
      newlist = list("credible_region" = credible_region, "rb_line" = rb_line)

      return(newlist)
    }
  }
}

# The values inserted are for testing purposes only.
binormal_diag_AUC_prior_error_char_copt = function(w = FALSE, alpha1w = NA, alpha2w = NA,
                                                   coptest, nMonteprior, delta, 
                                                   lambda1, lambda2, mu0, tau0){
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  # samples from (muD,sigmaD,muND,sigmaND) and prevalence w
  sigmaD = sqrt(1/rgamma(nMonteprior, lambda1, lambda2))
  sigmaND = sigmaD
  muD = rnorm(nMonteprior, mu0, (tau0*sigmaD))
  priorimpwt = pnorm((muD - mu0)/(tau0*sigmaND))
  U = rbeta(nMonteprior,1,1)
  muND = mu0 + tau0*sigmaND*qnorm(priorimpwt*U)
  # prevalence
  pre_w = rep(0, nMonteprior)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, version = "prior")
  }
  
  FNR = pnorm((coptest-muD)/sigmaD)
  FPR = 1 - pnorm((coptest-muND)/sigmaND)
  Error = pre_w*FNR + (1 - pre_w)*FPR
  FDR = (1 - pre_w)*FPR/((1 - pre_w)*FPR + pre_w*(1 - FNR))
  FNDR = pre_w*FNR/(pre_w*FNR + (1 - pre_w)*(1 - FPR))
  
  priorFNR = rep(0,L)
  priorFPR = rep(0,L)
  priorError = rep(0,L)
  priorFDR = rep(0,L)
  priorFNDR = rep(0,L)
  
  for (iMonteprior in 1:nMonteprior) {
    for (igrid in 1:L){
      if ( (A[igrid] < FNR[iMonteprior]) & (FNR[iMonteprior] <= A[igrid+1]) ) {
        priorFNR[igrid] = priorFNR[igrid] + priorimpwt[iMonteprior]}
      if ( (A[igrid] < FPR[iMonteprior]) & (FPR[iMonteprior] <= A[igrid+1]) ) {
        priorFPR[igrid] = priorFPR[igrid] + priorimpwt[iMonteprior]}
      if ( (A[igrid] < Error[iMonteprior]) & (Error[iMonteprior] <= A[igrid+1]) ) {
        priorError[igrid] = priorError[igrid] + priorimpwt[iMonteprior]}
      if ( (A[igrid] < FDR[iMonteprior]) & (FDR[iMonteprior] <= A[igrid+1]) ) {
        priorFDR[igrid] = priorFDR[igrid] + priorimpwt[iMonteprior]}
      if ( (A[igrid] < FNDR[iMonteprior]) & (FNDR[iMonteprior] <= A[igrid+1]) ) {
        priorFNDR[igrid] = priorFNDR[igrid] + priorimpwt[iMonteprior]}
    }
  }
  priorFNR = average_vector_values(priorFNR) # applying the smoother
  priorFPR = average_vector_values(priorFPR) # applying the smoother
  priorError = average_vector_values(priorError) # applying the smoother
  priorFDR = average_vector_values(priorFDR) # applying the smoother
  priorFNDR = average_vector_values(priorFNDR) # applying the smoother
  
  priorFNR = priorFNR/sum(priorFNR)
  priorFNRdensity = L*priorFNR
  priorFPR = priorFPR/sum(priorFPR)
  priorFPRdensity = L*priorFPR
  priorError = priorError/sum(priorError)
  priorErrordensity = L*priorError
  priorFDR = priorFDR/sum(priorFDR)
  priorFDRdensity = L*priorFDR
  priorFNDR = priorFNDR/sum(priorFNDR)
  priorFNDRdensity = L*priorFNDR
  
  newlist = list("priorFNR" = priorFNR, "priorFNRdensity" = priorFNRdensity,
                 "priorFPR" = priorFPR, "priorFPRdensity" = priorFPRdensity,
                 "priorError" = priorError, "priorErrordensity" = priorErrordensity,
                 "priorFDR" = priorFDR, "priorFDRdensity" = priorFDRdensity,
                 "priorFNDR" = priorFNDR, "priorFNDRdensity" = priorFNDRdensity)
  return(newlist)
}

binormal_diag_AUC_post_error_char_copt = function(w = FALSE, alpha1w = NA, alpha2w = NA,
                                                  nND = NA, nD = NA, version,
                                                  coptest, nMontepost, delta, 
                                                  lambda1post, lambda2post, 
                                                  mu0Dpost, mu0NDpost, 
                                                  tau0D, tau0ND){
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  sigmaDpost = sqrt(1/rgamma(nMontepost, lambda1post, lambda2post))
  sigmaNDpost = sigmaDpost
  muDpost = rnorm(nMontepost, mu0Dpost, (tau0D*sigmaDpost))
  postimpwt = pnorm((muDpost - mu0NDpost)/(tau0ND*sigmaNDpost))
  U = rbeta(nMontepost, 1, 1)
  muNDpost = mu0NDpost + tau0ND*sigmaNDpost*qnorm(postimpwt*U)
  # prevalence
  wpost = rep(0, nMonteprior)
  for(i in 1:length(wpost)){
    wpost[i] = generate_w(w, alpha1w, alpha2w, nD, nND, version) 
  }
  
  FNRpost = pnorm((coptest - muDpost)/sigmaDpost)
  FPRpost = 1 - pnorm((coptest - muNDpost)/sigmaNDpost)
  Errorpost = wpost*FNRpost + (1 - wpost)*FPRpost
  FDRpost = (1 - wpost)*FPRpost/((1 - wpost)*FPRpost + wpost*(1 - FNRpost))
  FNDRpost = wpost*FNRpost/(wpost*FNRpost + (1 - wpost)*(1 - FPRpost))
  
  postFNR = rep(0, L)
  postFPR = rep(0, L)
  postError = rep(0, L)
  postFDR = rep(0, L)
  postFNDR = rep(0, L)
  
  for (iMontepost in 1:nMontepost) {
    for (igrid in 1:L){
      if ( (A[igrid] < FNRpost[iMontepost]) & (FNRpost[iMontepost] <= A[igrid+1]) ) {
        postFNR[igrid] = postFNR[igrid]+postimpwt[iMontepost]}
      if ( (A[igrid] < FPRpost[iMontepost]) & (FPRpost[iMontepost] <= A[igrid+1]) ) {
        postFPR[igrid] = postFPR[igrid]+postimpwt[iMontepost]}
      if ( (A[igrid] < Errorpost[iMontepost]) & (Errorpost[iMontepost] <= A[igrid+1]) ) {
        postError[igrid] = postError[igrid]+postimpwt[iMontepost]}
      if ( (A[igrid] < FDRpost[iMontepost]) & (FDRpost[iMontepost] <= A[igrid+1]) ) {
        postFDR[igrid] = postFDR[igrid]+postimpwt[iMontepost]}
      if ( (A[igrid] < FNDRpost[iMontepost]) & (FNDRpost[iMontepost] <= A[igrid+1]) ) {
        postFNDR[igrid] = postFNDR[igrid]+postimpwt[iMontepost]}
    }
  }
  postFNR = average_vector_values(postFNR) # applying the smoother
  postFPR = average_vector_values(postFPR) # applying the smoother
  postError = average_vector_values(postError) # applying the smoother
  postFDR = average_vector_values(postFDR) # applying the smoother
  postFNDR = average_vector_values(postFNDR) # applying the smoother
  
  postFNR = postFNR/sum(postFNR)
  postFNRdensity = L*postFNR
  postFPR = postFPR/sum(postFPR)
  postFPRdensity = L*postFPR
  postError = postError/sum(postError)
  postErrordensity = L*postError
  postFDR = postFDR/sum(postFDR)
  postFDRdensity = L*postFDR
  postFNDR = postFNDR/sum(postFNDR)
  postFNDRdensity = L*postFNDR
  
  newlist = list("postFNR" = postFNR, "postFNRdensity" = postFNRdensity,
                 "postFPR" = postFPR, "postFPRdensity" = postFPRdensity,
                 "postError" = postError, "postErrordensity" = postErrordensity,
                 "postFDR" = postFDR, "postFDRdensity" = postFDRdensity,
                 "postFNDR" = postFNDR, "postFNDRdensity" = postFNDRdensity)
  return(newlist)
}

binormal_diag_AUC_RBR_error_char_copt = function(delta, priorFNR, priorFPR, priorError,
                                                 priorFDR, priorFNDR, postFNR, postFPR,
                                                 postError, postFDR, postFNDR){
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  
  RBFNR = postFNR/priorFNR
  RBFPR = postFPR/priorFPR
  RBError = postError/priorError
  RBFDR = postFDR/priorFDR
  RBFNDR = postFNDR/priorFNDR
  
  # to get a starting value for imax
  for (i in 1:L) {
    if (priorFNR[i] > 0){imaxFNR = i}
    if (priorFPR[i] > 0){imaxFPR = i}
    if (priorError[i] > 0){imaxError = i}
    if (priorFDR[i] > 0){imaxFDR = i}
    if (priorFNDR[i] > 0){imaxFNDR = i}
  }

  for (i in 1:L) {
    if (priorFNR[i] > 0 & RBFNR[i] > RBFNR[imaxFNR] ){imaxFNR = i}
    if (priorFPR[i] > 0 & RBFPR[i] > RBFPR[imaxFPR] ){imaxFPR = i}
    if (priorError[i] > 0 & RBError[i] > RBError[imaxError] ){imaxError = i}
    if (priorFDR[i] > 0 & RBFDR[i] > RBFDR[imaxFDR] ){imaxFDR = i}
    if (priorFNDR[i] > 0 & RBFNDR[i] > RBFNDR[imaxFNDR] ){imaxFNDR = i}
  }
  
  FNRest = grid[imaxFNR]
  FPRest = grid[imaxFPR]
  Errorest = grid[imaxError]
  FDRest = grid[imaxFDR]
  FNDRest = grid[imaxFNDR]
  
  #RBFNR, RBFPR, RBError, RBFDR, RBFNDR
  newlist = list("RBFNR" = RBFNR, "RBFPR" = RBFPR, "RBError" = RBError,
                 "RBFDR" = RBFDR, "RBFNDR" = RBFNDR,
                 "FNRest" = FNRest, "FPRest" = FPRest, "Errorest" = Errorest,
                 "FDRest" = FDRest, "FNDRest" = FNDRest)
  return(newlist)
}







###############################
# TESTING VALUES
###############################

# the hyperparamters for the prior on the mu's and sigma's
#mu0=0
#tau0=0.5
#lambda1=1.787
#lambda2=1.056

# the data 
#nND=25
#meanND=-0.072
#sND_squared=19.638

#nD=20
#meanD=0.976
#sD_squared=16.778

#nMonteprior = 100000
#nMontepost = 100000
#delta = 0.005

#w = 0.40
#alpha1w = 15.3589 
#alpha2w = 22.53835
#gamma = 0.70

#post_hyperpara = binormal_compute_post_hyperpara(mu0, tau0, lambda1, lambda2, nND, meanND, 
#                                                 sND_squared, nD, meanD, sD_squared)

#prior_val = binormal_diag_prior(w, alpha1w, alpha2w, nMonteprior, delta, lambda1, lambda2, mu0, tau0)
#prior_val = binormal_diag_prior(w = FALSE, alpha1w, alpha2w, nMonteprior, delta, lambda1, lambda2, mu0, tau0)

#post_val = binormal_diag_post(w, alpha1w, alpha2w, nND, nD, version =  "post",
#                              nMontepost, delta, post_hyperpara$lambda1post, 
#                              post_hyperpara$lambda2post, post_hyperpara$mu0Dpost, 
#                              post_hyperpara$mu0NDpost, post_hyperpara$tau0D, 
#                              post_hyperpara$tau0ND)

#rbr_val = binormal_diag_RBR(delta, prior_val$probAUCprior, post_val$probAUCpost,
#                            prior_val$priorAUC, post_val$postAUC, prior_val$priorcmod,
#                            post_val$postcmod)
#par(mfrow=c(1,2))
#grid = open_bracket_grid(0.005)

#cr = binormal_diag_compute_credible_region(gamma = gamma, 
#                                           delta = delta, 
#                                           AUC_RBR = rbr_val$RB_AUC, 
#                                           AUC_prior = prior_val$priorAUC, 
#                                           AUC_post = post_val$postAUC, 
#                                           posterior_content = rbr_val$postPl_AUC)

#prior_err = binormal_diag_AUC_prior_error_char_copt(w, alpha1w, alpha2w, rbr_val$coptest, nMonteprior, delta, 
#                                                    lambda1, lambda2, mu0, tau0)

#prior_err = binormal_diag_AUC_prior_error_char_copt(w = FALSE, alpha1w, alpha2w, coptest = 0.715, nMonteprior, delta, 
#                                                    lambda1, lambda2, mu0, tau0)

#par(mfrow=c(1,1))
#plot(grid, prior_err$priorFNRdensity, xlab="FNR",ylab="prior",type="l",lty=1)

#plot(grid, prior_err$priorFPRdensity, xlab="FPR",ylab="prior",type="l",lty=1)

#plot(grid, prior_err$priorErrordensity, xlab="Error",ylab="prior",type="l",lty=1)

#plot(grid, prior_err$priorFDRdensity, xlab="FDR",ylab="prior",type="l",lty=1)

#plot(grid, prior_err$priorFNDRdensity, xlab="FNDR",ylab="prior",type="l",lty=1)



#binormal_diag_prior_post_graph(delta = delta, 
#                                 prior = prior_val$priorAUCdensity, 
#                                 post = post_val$postAUCdensity, 
#                                 plausible_region = rbr_val$plausible_region)

#binormal_diag_rbr_graph(delta = delta, 
#                          relative_belief_ratio = rbr_val$RB_AUC, 
#                          plausible_region = rbr_val$plausible_region)


#cat("P(AUC>1/2) = ", prior_val$probAUCprior, "\n")
#cat("P(AUC>1/2 | data) = ", post_val$probAUCpost, "\n")
#cat("rel. belief ratio of AUC>1/2 = ", rbr_val$RBprobAUC, 
#    "strength of the evidence = ", post_val$probAUCpost, "\n")
#cat("The posterior content of the plausible region = ", rbr_val$postPl_AUC,"\n")


