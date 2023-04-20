#source("C:\\Users\\AnnaH\\OneDrive\\Desktop\\Stats RA\\ShinyWebpage\\pages\\helper_functions.R")

#fcnAUC = function(z){return (dnorm(z)*pnorm((muDi-muNDi)/sigmaDi+sigmaNDi/sigmaDi*z))}

################################################################
# FUNCTIONS FOR COMPUTATIONS                                   #
################################################################

binormal_compute_post_hyperpara = function(mu0, tau0, lambda1, lambda2, nND, meanND, 
                                           sND_squared, nD, meanD, sD_squared){
  lambda1post = lambda1 + (nD+nND)/2
  tau0D = 1/sqrt(nD + 1/tau0^2)
  tau0ND = 1/sqrt(nND + 1/tau0^2)
  lambda2post = (lambda2 + (sD_squared+sND_squared)/2 + (tau0D**2)*(nD/tau0^2)*(meanD - mu0)^2/2 + 
                   (tau0ND**2)*(nND/tau0^2)*(meanND - mu0)^2/2)
  mu0Dpost = (tau0D**2)*(nD*meanD + mu0/tau0**2)
  mu0NDpost = (tau0ND**2)*(nND*meanND + mu0/tau0**2)
  
  newlist = list("lambda1post" = lambda1post, "tau0D" = tau0D, "tau0ND" = tau0ND,
                 "lambda2post" = lambda2post, "mu0Dpost" = mu0Dpost, "mu0NDpost" = mu0NDpost)
}

binormal_diag_prior = function(condition, nMonteprior, delta, lambda1, lambda2, mu0, tau0){
  if(condition != "conditional" && condition != "unconditional"){
    return("condition must either be 'conditional' or 'unconditional'.")
  } 
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  sigmaD = sqrt(1/rgamma(nMonteprior, lambda1, lambda2))
  sigmaND = sigmaD
  muD = rnorm(nMonteprior, mu0, (tau0*sigmaD))
  if(condition == "unconditional"){
    priorAUC = rep(0, L)
    probAUCprior = 0
    muND = rnorm(nMonteprior, mu0, (tau0*sigmaND))
  } else if (condition == "conditional"){
    # modify the A array so the intervals span [1/2,1]
    A = A[((L/2)+1):(L+1)]
    L = L/2
    priorAUC = rep(0, L)
    impwt = pnorm((muD - mu0)/(tau0*sigmaND))
    U = rbeta(nMonteprior, 1, 1)
    muND = mu0 + tau0*sigmaND*qnorm(impwt*U)
    n_accepted = 0 # keeping a counter for the accepted samples
  }
  
  for (iMonteprior in 1:nMonteprior){
    muDi = muD[iMonteprior]
    muNDi = muND[iMonteprior]
    if (condition == "unconditional"){
      if (muDi > muNDi){probAUCprior = probAUCprior + 1}
    } else if (condition == "conditional"){ # added - may cause bugs
      if(muDi < muNDi){next} else {n_accepted = n_accepted + 1} # added - may cause bugs
    } # added - may cause bugs
    sigmaDi = sigmaD[iMonteprior]
    sigmaNDi = sigmaDi
    # Need to create the function here to use for integration
    fcnAUC = function(z){return (dnorm(z)*pnorm((muDi - muNDi)/sigmaDi + sigmaNDi/sigmaDi*z))}
    AUC = integrate(fcnAUC, -Inf, Inf, abs.tol = 0.001) 
    
    for (igrid in 1:L){
      if ( (A[igrid] < as.numeric(AUC[1])) & (as.numeric(AUC[1]) <= A[igrid + 1]) ){
        if(condition == "unconditional"){
          priorAUC[igrid] = priorAUC[igrid] + 1 
        } else if (condition == "conditional"){
          priorAUC[igrid] = priorAUC[igrid] + impwt[iMonteprior] 
        }
      }
    }
  }

  priorAUC = average_vector_values(priorAUC) # APPLYING SMOOTHER
  
  if(condition == "unconditional"){
    priorAUC = priorAUC/nMonteprior
    priorAUCdensity = L*priorAUC
    probAUCprior = probAUCprior/nMonteprior
    newlist = list("priorAUC" = priorAUC, "probAUCprior" = probAUCprior,
                   "priorAUCdensity" = priorAUCdensity)
  } else if (condition == "conditional"){
    priorAUC = priorAUC/sum(priorAUC)
    priorAUCdensity = L*priorAUC
    newlist = list("priorAUC" = priorAUC, "priorAUCdensity" = priorAUCdensity,
                   "n_accepted" = n_accepted)
  }
  return(newlist)
}

binormal_diag_post = function(condition, nMontepost, delta, lambda1post, lambda2post, 
                              mu0Dpost, mu0NDpost, tau0D, tau0ND){
  if(condition != "conditional" && condition != "unconditional"){
    return("condition must either be 'conditional' or 'unconditional'.")
  }
  A = closed_bracket_grid(delta) # this is technically their grid
  L = (1/delta) # length
  sigmaDpost = sqrt(1/rgamma(nMontepost, lambda1post, lambda2post))
  sigmaNDpost = sigmaDpost
  muDpost = mu0Dpost + tau0D*sigmaDpost*rnorm(nMontepost, 0, 1)
  
  if(condition == "unconditional"){
    probAUCpost = 0 
    postAUC = rep(0, L)
    muNDpost = mu0NDpost + tau0ND*sigmaNDpost*rnorm(nMontepost, 0, 1)
  } else if (condition == "conditional"){
    # modify the A array so the intervals span [1/2,1]
    A = A[((L/2)+1):(L+1)]
    L = L/2
    postAUC = rep(0, L)
    impwt = pnorm((muDpost - mu0NDpost)/(tau0ND*sigmaNDpost))
    U = rbeta(nMontepost, 1, 1)
    muNDpost = mu0NDpost + tau0ND*sigmaNDpost*qnorm(impwt*U)
    n_accepted = 0 # keeping a counter for the accepted samples
  }
  
  # this is the loop for the Monte Carlo for the posterior
  for (iMontepost in 1:nMontepost) {
    muDi = muDpost[iMontepost]
    muNDi = muNDpost[iMontepost]
    if (condition == "unconditional"){
      if (muDi > muNDi){probAUCpost = probAUCpost + 1} 
    } else if (condition == "conditional"){ # added - may cause bugs
      if(muDi < muNDi){next} else {n_accepted = n_accepted + 1} # added - may cause bugs
    } # added - may cause bugs
    
    sigmaDi = sigmaDpost[iMontepost]
    sigmaNDi = sigmaDi
    # Need to create the function here to use for integration
    fcnAUC = function(z){return (dnorm(z)*pnorm((muDi-muNDi)/sigmaDi+sigmaNDi/sigmaDi*z))}
    AUC = integrate(fcnAUC, -Inf, Inf, abs.tol = 0.001) 
    for (igrid in 1:L){
      if ( (A[igrid] < as.numeric(AUC[1])) & (as.numeric(AUC[1]) <= A[igrid+1]) ) {
        if (condition == "unconditional"){
          postAUC[igrid] = postAUC[igrid] + 1 
        } else if (condition == "conditional"){
          postAUC[igrid] = postAUC[igrid] + impwt[iMontepost] 
        }
      }
    }
  }
  postAUC = average_vector_values(postAUC) # applying a smoother
  
  if (condition == "unconditional"){
    postAUC = postAUC/nMontepost
    postAUCdensity = L*postAUC
    probAUCpost = probAUCpost/nMontepost
    newlist = list("postAUC" = postAUC, "postAUCdensity" = postAUCdensity,
                   "probAUCpost" = probAUCpost)
  } else if (condition == "conditional"){
    postAUC = postAUC/sum(postAUC)
    postAUCdensity = L*postAUC
    newlist = list("postAUC" = postAUC, "postAUCdensity" = postAUCdensity,
                   "n_accepted" = n_accepted)
  }
  return(newlist)
}

binormal_diag_RBR = function(condition, delta, probAUCprior, probAUCpost, priorAUC, postAUC){
  if(condition != "conditional" && condition != "unconditional"){
    return("condition must either be 'conditional' or 'unconditional'.")
  }
  if(condition == "unconditional"){
    L = (1/delta) #- 1 # why the minus 1?
    grid = open_bracket_grid(delta)
    #L = ((1/delta) - 1)
  } else if (condition == "conditional"){
    L = (1/delta)/2
    grid = open_bracket_grid(delta)[(L+1):(2*L)]
  }
  # Note: rel. belief ratio of AUC > 1/2 = RBprobAUC
  # Strength of evidence = probAUCpost
  if(condition == "unconditional"){
    RBprobAUC = probAUCpost/probAUCprior 
  }
  
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
  plausible_region = c(plausible_region[1], plausible_region[length(plausible_region)])
  
  postPl_AUC=0 # posterior content
  temp_RB_AUC =  NA_to_0(RB_AUC)
  for (i in 1:L) {
    # reason: need prior to be non negative
    if (priorAUC[i] > 0 & temp_RB_AUC[i] > 1) {postPl_AUC = postPl_AUC + postAUC[i]}
  }
  
  if(condition == "unconditional"){
    newlist = list("RB_AUC" = RB_AUC, "RBprobAUC" = RBprobAUC,
                   "AUCest" = AUCest, "postPl_AUC" = postPl_AUC,
                   "plausible_region" = plausible_region)
  } else {
    newlist = list("RB_AUC" = RB_AUC, "AUCest" = AUCest, "postPl_AUC" = postPl_AUC,
                   "plausible_region" = plausible_region)
  }
  return(newlist)
}

binormal_diag_compute_credible_region = function(gamma, delta, AUC_RBR, AUC_prior, AUC_post, 
                                                 plausible_region, posterior_content){
  # Note: credible region is now based on the line plot.
  grid = open_bracket_grid(delta)
  AUC_RBR[is.na(AUC_RBR)] = 0
  # Computes the credible region. At first, there's no default input to avoid generating
  # a credible region automatically (it is not necessary.)
  if (check.numeric(gamma) == FALSE){
    err_msg = "Need to put in a valid input for gamma."
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
          if ((grid[j] >= plausible_region[1]) & (grid[j] <= plausible_region[2])) {
            if((AUC_RBR[j] > rb_line)){
              credible_region = c(credible_region, grid[j])
              j_vals = c(j_vals, j)
            }
          }
        }
        if (is.null(j_vals) == TRUE){ # when the credible region doesn't generate
          err_msg = "The credible region doesn't exist for the specified gamma."
          return(list("credible_region" = err_msg, "rb_line" = err_msg))
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

binormal_diag_prior_copt = function(w = FALSE, alpha1w = NA, alpha2w = NA, 
                                    nMonteprior, delta, lambda1, lambda2, mu0, tau0){
  A = closed_bracket_grid(delta)
  L = (1/delta) # length
  sigmaD = sqrt(1/rgamma(nMonteprior, lambda1, lambda2))
  sigmaND = sigmaD
  muD = rnorm(nMonteprior, mu0, (tau0*sigmaD))
  
  pre_w = rep(0, nMonteprior)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, version = "prior")
  }
  
  priorimpwt = pnorm((muD - mu0)/(tau0*sigmaND))
  U = rbeta(nMonteprior,1,1) 
  muND_copt = mu0 + tau0*sigmaND*qnorm(priorimpwt*U) 
  c = 0.5*(muD + muND_copt)+(sigmaD**2)*(log((1 - pre_w)/pre_w))/(muD - muND_copt) 
  cmod = (pi/2 + atan(c))/pi 
  cmodmax = max(cmod) 
  cmodmin = min(cmod) 
  priorcmod = rep(0,L) 
  
  for (iMonteprior in 1:nMonteprior) {
    for (igrid in 1:L){
      if ( (A[igrid] < cmod[iMonteprior]) & (cmod[iMonteprior] <= A[igrid+1]) ) {
        priorcmod[igrid] = priorcmod[igrid] + priorimpwt[iMonteprior]
      }
    }
  }
  priorcmod = average_vector_values(priorcmod) # APPLYING SMOOTHER
  
  priorcmod = priorcmod/sum(priorcmod) 
  priorcmoddensity = L*priorcmod 
  
  newlist = list("priorcmod" = priorcmod, "priorcmoddensity" = priorcmoddensity)
  
  return(newlist)
}

binormal_diag_post_copt = function(w = FALSE, alpha1w = NA, alpha2w = NA, nND = NA, nD = NA, 
                                   version, nMontepost, delta, lambda1post, lambda2post, 
                                   mu0Dpost, mu0NDpost, tau0D, tau0ND){
  A = closed_bracket_grid(delta) 
  L = (1/delta) # length
  sigmaDpost = sqrt(1/rgamma(nMontepost, lambda1post, lambda2post))
  sigmaNDpost = sigmaDpost
  muDpost = mu0Dpost + tau0D*sigmaDpost*rnorm(nMontepost,0,1)
  
  pre_w = rep(0, nMontepost)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, nD, nND, version) 
  }
  
  postimpwt = pnorm((muDpost - mu0NDpost)/(tau0ND*sigmaNDpost))
  U = rbeta(nMontepost,1,1) 
  muNDpost_copt = mu0NDpost + tau0ND*sigmaNDpost*qnorm(postimpwt*U) 
  
  c = 0.5*(muDpost + muNDpost_copt) + (sigmaDpost**2)*(log((1-pre_w)/pre_w))/(muDpost-muNDpost_copt) 
  cmod = (pi/2 + atan(c))/pi 
  cmodmax = max(cmod) 
  cmodmin = min(cmod) 
  
  postcmod = rep(0,L)
  
  for (iMontepost in 1:nMontepost) {
    for (igrid in 1:L){
      if ( (A[igrid] < cmod[iMontepost]) & (cmod[iMontepost] <= A[igrid+1]) ) {
        postcmod[igrid] = postcmod[igrid] + postimpwt[iMontepost] 
      }
    }
  }
  postcmod = average_vector_values(postcmod) # applying a smoother
  postcmod = postcmod/sum(postcmod)
  postcmoddensity = L*postcmod
  
  newlist = list("postcmod" = postcmod, "postcmoddensity" = postcmoddensity)
  return(newlist)
}

binormal_diag_RBR_copt = function(delta, priorcmod, postcmod){
  grid = open_bracket_grid(delta)
  L = ((1/delta) - 1)
  
  RBcmod = postcmod/priorcmod
  
  # Getting the plausible region for cmod
  postPlcmod = 0 # the posterior content of the plausible region
  plausible_region = c()
  for (i in 1:L) {
    if (priorcmod[i] > 0 & RBcmod[i] > 1) {
      postPlcmod = postPlcmod + postcmod[i]
      plausible_region = c(plausible_region, as.numeric(grid[i]))
    }
  }
  plausible_region = c(plausible_region[1], plausible_region[length(plausible_region)])
  
  imax = 1
  temp_RBcmod = NA_to_0(RBcmod)
  for (i in 1:L) {
    if (priorcmod[i] > 0 & temp_RBcmod[i] > temp_RBcmod[imax]){imax = i}
  }
  cmodest = grid[imax]
  coptest = tan(pi*cmodest - pi/2)
  
  newlist = list("RBcmod" = RBcmod, "postPlcmod" = postPlcmod,
                 "plausible_region" = plausible_region,
                 "cmodest" = cmodest, "coptest" = coptest)
}

binormal_diag_AUC_prior_error_char_copt = function(w = FALSE, alpha1w = NA, alpha2w = NA,
                                                   coptest, nMonteprior, delta, 
                                                   lambda1, lambda2, mu0, tau0){
  A = closed_bracket_grid(delta)
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
      if ((A[igrid] < FNR[iMonteprior]) & (FNR[iMonteprior] <= A[igrid + 1])) {
        priorFNR[igrid] = priorFNR[igrid] + priorimpwt[iMonteprior]}
      if ((A[igrid] < FPR[iMonteprior]) & (FPR[iMonteprior] <= A[igrid + 1])) {
        priorFPR[igrid] = priorFPR[igrid] + priorimpwt[iMonteprior]}
      if ((A[igrid] < Error[iMonteprior]) & (Error[iMonteprior] <= A[igrid + 1])) {
        priorError[igrid] = priorError[igrid] + priorimpwt[iMonteprior]}
      if ((A[igrid] < FDR[iMonteprior]) & (FDR[iMonteprior] <= A[igrid + 1])) {
        priorFDR[igrid] = priorFDR[igrid] + priorimpwt[iMonteprior]}
      if ((A[igrid] < FNDR[iMonteprior]) & (FNDR[iMonteprior] <= A[igrid + 1])) {
        priorFNDR[igrid] = priorFNDR[igrid] + priorimpwt[iMonteprior]}
    }
  }
  priorFNR = average_vector_values(priorFNR) # applying the smoother
  priorFPR = average_vector_values(priorFPR) 
  priorError = average_vector_values(priorError) 
  priorFDR = average_vector_values(priorFDR) 
  priorFNDR = average_vector_values(priorFNDR) 
  
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
  wpost = rep(0, nMontepost)
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
      if ((A[igrid] < FNRpost[iMontepost]) & (FNRpost[iMontepost] <= A[igrid + 1])) {
        postFNR[igrid] = postFNR[igrid] + postimpwt[iMontepost]}
      if ((A[igrid] < FPRpost[iMontepost]) & (FPRpost[iMontepost] <= A[igrid + 1])) {
        postFPR[igrid] = postFPR[igrid] + postimpwt[iMontepost]}
      if ((A[igrid] < Errorpost[iMontepost]) & (Errorpost[iMontepost] <= A[igrid + 1])) {
        postError[igrid] = postError[igrid] + postimpwt[iMontepost]}
      if ((A[igrid] < FDRpost[iMontepost]) & (FDRpost[iMontepost] <= A[igrid + 1])) {
        postFDR[igrid] = postFDR[igrid] + postimpwt[iMontepost]}
      if ((A[igrid] < FNDRpost[iMontepost]) & (FNDRpost[iMontepost] <= A[igrid + 1])) {
        postFNDR[igrid] = postFNDR[igrid] + postimpwt[iMontepost]}
    }
  }
  postFNR = average_vector_values(postFNR) # applying the smoother
  postFPR = average_vector_values(postFPR) 
  postError = average_vector_values(postError) 
  postFDR = average_vector_values(postFDR) 
  postFNDR = average_vector_values(postFNDR) 
  
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
  grid = open_bracket_grid(delta)
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

####################################################################### the unequal variance case


#post_hyperpara = binormal_compute_post_hyperpara(mu0, tau0, lambda1, lambda2, nND, meanND, 
#                                                 sND_squared, nD, meanD, sD_squared)

#post_val = binormal_diag_post("unconditional", nMontepost, delta, post_hyperpara$lambda1post, 
#                              post_hyperpara$lambda2post, post_hyperpara$mu0Dpost, 
#                              post_hyperpara$mu0NDpost, post_hyperpara$tau0D, 
#                              post_hyperpara$tau0ND)
#post_val = binormal_diag_post("conditional", nMontepost, delta, post_hyperpara$lambda1post, 
#                              post_hyperpara$lambda2post, post_hyperpara$mu0Dpost, 
#                              post_hyperpara$mu0NDpost, post_hyperpara$tau0D, 
#                              post_hyperpara$tau0ND)

#prior_val = binormal_diag_prior("unconditional", nMonteprior, delta, lambda1, lambda2, mu0, tau0)
#prior_val = binormal_diag_prior("conditional", nMonteprior, delta, lambda1, lambda2, mu0, tau0)

#rbr_val = binormal_diag_RBR("conditional", delta, prior_val$probAUCprior, post_val$probAUCpost,
#                            prior_val$priorAUC, post_val$postAUC)

#cr = binormal_diag_compute_credible_region(gamma = gamma, 
#                                           delta = delta, 
#                                           AUC_RBR = rbr_val$RB_AUC, 
#                                           AUC_prior = prior_val$priorAUC, 
#                                           AUC_post = post_val$postAUC, 
#                                           plausible_region = rbr_val$plausible_region,
#                                           posterior_content = rbr_val$postPl_AUC)


#grid=open_bracket_grid(delta)

#alternative
#L=100
#grid=rep(0,L)
#for (i in 1:L) {
#  grid[i]=(i-1/2)/L
#}
#grid=0.5*grid+0.5

#length(prior_val$priorAUCdensity)

#plot(grid, prior_val$priorAUCdensity, xlab="AUC",ylab="prior",type="l",lty=1)

#plot(grid, post_val$postAUCdensity, xlab="AUC",ylab="prior",type="l",lty=1)

#post_val = binormal_diag_post(w, alpha1w, alpha2w, nND, nD, version =  "post",
#                              nMontepost, delta, post_hyperpara$lambda1post, 
#                              post_hyperpara$lambda2post, post_hyperpara$mu0Dpost, 
#                              post_hyperpara$mu0NDpost, post_hyperpara$tau0D, 
#                              post_hyperpara$tau0ND)


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

#post_err = binormal_diag_AUC_post_error_char_copt(w, alpha1w, alpha2w,
#                                                  nND, nD, "post",
#                                                  rbr_val$coptest, 
#                                                  nMontepost, 
#                                                  delta, 
#                                                  post_hyperpara$lambda1post, 
#                                                  post_hyperpara$lambda2post, 
#                                                  post_hyperpara$mu0Dpost, 
#                                                  post_hyperpara$mu0NDpost, 
#                                                  post_hyperpara$tau0D, 
#                                                  post_hyperpara$tau0ND)

#binormal_diag_AUC_RBR_error_char_copt(delta = delta,
#                                      priorFNR =  prior_err$priorFNR, 
#                                      priorFPR = prior_err$priorFPR, 
#                                      priorError = prior_err$priorError,
#                                      priorFDR = prior_err$priorFDR, 
#                                      priorFNDR = prior_err$priorFNDR, 
#                                      postFNR = post_err$postFNR, 
#                                      postFPR = post_err$postFPR,
#                                      postError = post_err$postError, 
#                                      postFDR = post_err$postFDR, 
 #                                     postFNDR = post_err$postFNDR)

#par(mfrow=c(1,1))
#plot(grid, prior_err$priorFNRdensity, xlab="FNR",ylab="prior",type="l",lty=1)

#plot(grid, prior_err$priorFPRdensity, xlab="FPR",ylab="prior",type="l",lty=1)

#plot(grid, prior_err$priorErrordensity, xlab="Error",ylab="prior",type="l",lty=1)

#plot(grid, prior_err$priorFDRdensity, xlab="FDR",ylab="prior",type="l",lty=1)

#plot(grid, prior_err$priorFNDRdensity, xlab="FNDR",ylab="prior",type="l",lty=1)

