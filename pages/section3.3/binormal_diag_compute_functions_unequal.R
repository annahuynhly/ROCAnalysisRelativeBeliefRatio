################################################################
# FUNCTIONS FOR COMPUTATIONS                                   #
################################################################

binormal_compute_post_hyperpara_unequal = function(mu0, tau0, lambda1, lambda2, nND, meanND, 
                                                   sND_squared, nD, meanD, sD_squared){
  # The unequal variances case.
  lambda1Dpost = lambda1 + nD/2
  lambda1NDpost = lambda1 + nND/2
  tau0D = 1/sqrt(nD + 1/tau0^2)
  tau0ND = 1/sqrt(nND + 1/tau0^2)
  lambda2Dpost = lambda2 + sD_squared/2 + (tau0D**2)*(nD/tau0^2)*(meanD - mu0)^2/2
  lambda2NDpost = lambda2 + sND_squared/2 + (tau0ND**2)*(nND/tau0^2)*(meanND - mu0)^2/2
  mu0Dpost = (tau0D**2)*(nD*meanD + mu0/tau0**2)
  mu0NDpost = (tau0ND**2)*(nND*meanND + mu0/tau0**2)

  newlist = list("lambda1Dpost" = lambda1Dpost, "lambda1NDpost" = lambda1NDpost,
                 "tau0D" = tau0D, "tau0ND" = tau0ND, "lambda2Dpost" = lambda2Dpost,
                 "lambda2NDpost" = lambda2NDpost, "mu0Dpost" = mu0Dpost,
                 "mu0NDpost" = mu0NDpost)
}

binormal_diag_prior_unequal = function(condition, w = FALSE, alpha1w = NA, alpha2w = NA, 
                                       nMonteprior, delta, lambda1, lambda2, mu0, tau0){
  if(condition != "conditional" && condition != "unconditional"){
    return("condition must either be 'conditional' or 'unconditional'.")
  } 
  A = closed_bracket_grid(delta) # this is technically their grid
  L = (1/delta) # length
  sigmaD = sqrt(1/rgamma(nMonteprior, lambda1, lambda1))
  sigmaND = sqrt(1/rgamma(nMonteprior, lambda2, lambda2))
  muD = rnorm(nMonteprior, mu0, (tau0*sigmaD))
  
  pre_w = rep(0, nMonteprior)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, version = "prior")
  }
  
  priordeltaTmp = 2*(sigmaD**2 - sigmaND**2)*log(((1 - pre_w)/pre_w)*(sigmaD/sigmaND))
  priordeltaTmp[priordeltaTmp < 0] = 0
  priordeltaTmp = sqrt(priordeltaTmp)
  
  if(condition == "unconditional"){
    priorAUC = rep(0, L)
    probAUCprior = 0 
    muND = rnorm(nMonteprior, mu0, (tau0*sigmaND))
  } else if (condition == "conditional"){
    # modify the A array so the intervals span [1/2,1]
    A = A[((L/2)+1):(L+1)]
    L = L/2
    priorAUC = rep(0, L)
    impwt = pnorm((muD - priordeltaTmp - mu0)/(tau0*sigmaND))
    U = rbeta(nMonteprior, 1, 1)
    muND = mu0 + tau0*sigmaND*qnorm(impwt*U)
    n_accepted = 0 # keeping a counter for the accepted samples
  }
  
  for (iMonteprior in 1:nMonteprior) {
    muDi = muD[iMonteprior]
    muNDi = muND[iMonteprior]
    if (condition == "unconditional"){
      if (muDi - muNDi > priordeltaTmp[iMonteprior]){probAUCprior = probAUCprior + 1}
    } else if (condition == "conditional"){
      if( muDi - muNDi < priordeltaTmp[iMonteprior]){next} else {n_accepted = n_accepted + 1}
    }
    sigmaDi = sigmaD[iMonteprior]
    sigmaNDi = sigmaND[iMonteprior]
    fcnAUC = function(z){return (dnorm(z)*pnorm((muDi-muNDi)/sigmaDi + sigmaNDi/sigmaDi*z))}
    AUC = integrate(fcnAUC, -Inf, Inf, abs.tol = 0.001) 
    
    for (igrid in 1:L){
      if ((A[igrid] < as.numeric(AUC[1])) & (as.numeric(AUC[1]) <= A[igrid + 1])) {
        if (condition == "unconditional"){
          priorAUC[igrid] = priorAUC[igrid] + 1 
        } else if (condition == "conditional"){
          priorAUC[igrid] = priorAUC[igrid] + impwt[iMonteprior] 
        }
      }
    }
  }
  
  if (condition == "unconditional"){
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

binormal_diag_post_unequal = function(condition, w = FALSE, alpha1w = NA, alpha2w = NA, nND = NA, nD = NA, version,
                                      nMontepost, delta, lambda1Dpost, lambda1NDpost, lambda2Dpost, lambda2NDpost, 
                                      mu0Dpost, mu0NDpost, tau0D, tau0ND){
  if(condition != "conditional" && condition != "unconditional"){
    return("condition must either be 'conditional' or 'unconditional'.")
  }
  A = closed_bracket_grid(delta) # this is technically their grid
  L = (1/delta) # length
  
  sigmaDpost = sqrt(1/rgamma(nMontepost, lambda1Dpost, lambda2Dpost))
  sigmaNDpost = sqrt(1/rgamma(nMontepost, lambda1NDpost, lambda2NDpost))
  pre_w = rep(0, nMontepost)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, nD, nND, version) # ADDED FOR COPT
  }
  postdeltaTmp = 2*( sigmaDpost**2- sigmaNDpost**2)*log(((1-pre_w)/pre_w)*(sigmaDpost/sigmaNDpost))
  postdeltaTmp[postdeltaTmp<0] = 0
  postdeltaTmp = sqrt(postdeltaTmp)
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
    impwt = pnorm((muDpost - postdeltaTmp - mu0NDpost)/(tau0ND*sigmaNDpost))
    U = rbeta(nMontepost, 1, 1)
    muNDpost = mu0NDpost + tau0ND*sigmaNDpost*qnorm(impwt*U)
    n_accepted = 0 # keeping a counter for the accepted samples
  }
  
  # this is the loop for the Monte Carlo for the posterior
  for (iMontepost in 1:nMontepost) {
    muDi = muDpost[iMontepost]
    muNDi = muNDpost[iMontepost]
    if (condition == "unconditional"){
      if (muDi - muNDi > postdeltaTmp[iMontepost]){probAUCpost = probAUCpost + 1}
    } else if (condition == "conditional"){
      if(muDi - muNDi < postdeltaTmp[iMontepost]){next} else {n_accepted = n_accepted + 1}
    }
    sigmaDi = sigmaDpost[iMontepost]
    sigmaNDi = sigmaNDpost[iMontepost]
    fcnAUC = function(z){return (dnorm(z)*pnorm((muDi-muNDi)/sigmaDi+sigmaNDi/sigmaDi*z))}
    AUC = integrate(fcnAUC, -Inf, Inf, abs.tol = 0.001) 
    for (igrid in 1:L){
      if ( (A[igrid] < as.numeric(AUC[1])) & (as.numeric(AUC[1]) <= A[igrid + 1])) {
        if (condition == "unconditional"){
          postAUC[igrid] = postAUC[igrid] + 1 
        } else if (condition == "conditional"){
          postAUC[igrid] = postAUC[igrid]+impwt[iMontepost] 
        }
      }
    }
  }

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

binormal_diag_prior_copt_unequal = function(w = FALSE, alpha1w = NA, alpha2w = NA, 
                                            nMonteprior, delta, lambda1, lambda2, mu0, tau0,
                                            lambda){
  # need to add error characteristics to this!
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  
  sigmaD2 = 1/rgamma(nMonteprior, lambda1, lambda2)
  sigmaND2 = 1/rgamma(nMonteprior, lambda1, lambda2)
  sigmaD = sqrt(sigmaD2)
  sigmaND = sqrt(sigmaND2)
  # prevalence
  pre_w = rep(0, nMonteprior)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, version = "prior") # ADDED FOR COPT
  }
  muD = rnorm(nMonteprior, mu0, (tau0*sigmaD))
  priordeltaTmp = 2*(sigmaD2-sigmaND2)*log(((1-pre_w)/pre_w)*(sigmaD/sigmaND))
  priordeltaTmp[priordeltaTmp>0] = 0
  priordeltaTmp = sqrt(-priordeltaTmp)
  priorimpwt = pnorm((muD - priordeltaTmp-mu0)/(tau0*sigmaND))
  U = rbeta(nMonteprior, 1, 1)
  muND = mu0 + tau0*sigmaND*qnorm(priorimpwt*U)
  
  countinfsprior=0
  for (i in 1:nMonteprior){
    if (muND[i] == -Inf | muND[i] == Inf){countinfsprior = countinfsprior + 1}
  }
  
  cond2 = (muND - muD)**2 + 2*(sigmaD2 - sigmaND2)*log(((1 - pre_w)/pre_w)*(sigmaD/sigmaND))
  check1 = min(muD - priordeltaTmp - muND) 
  check2 = min(cond2)
  
  c=0*muND
  for (i in 1:nMonteprior){
    if (muND[i] == -Inf | muND[i] == Inf){c[i] = -Inf} 
    else {
      c[i] = (sigmaND2[i]*muD[i] - sigmaD2[i]*muND[i]) - (sigmaD[i]*sigmaND[i])*sqrt(cond2[i])}
  }
  c = c/(sigmaND2 - sigmaD2)
  
  cmod=pt(c,lambda)
  cmodmax=max(cmod)
  cmodmin=min(cmod)
  
  priorcmod = rep(0,L)
  for (iMonteprior in 1:nMonteprior){
    for (igrid in 1:L){
      if ( (A[igrid] < cmod[iMonteprior]) & (cmod[iMonteprior] <= A[igrid + 1])) {
        priorcmod[igrid] = priorcmod[igrid] + priorimpwt[iMonteprior]
      }
    }
  }
  
  
  priorcmod = priorcmod/sum(priorcmod)
  priorcopt = tan(pi*priorcmod-pi/2) # trying to get the cutoff
  priorcmoddensity = L*priorcmod
  
  newlist = list("priorcmod" = priorcmod, "priorcmoddensity" = priorcmoddensity,
                 "priorcopt" = priorcopt)
  return(newlist)
}

binormal_diag_post_copt_unequal = function(w = FALSE, alpha1w = NA, alpha2w = NA, nND = NA, nD = NA, 
                                   version, nMontepost, delta, lambda1Dpost, lambda1NDpost, 
                                   lambda2Dpost, lambda2NDpost, 
                                   mu0Dpost, mu0NDpost, tau0D, tau0ND, lambda){
  A = closed_bracket_grid(delta) # this is technically their grid
  L = (1/delta) # length
  
  sigmaDpost = sqrt(1/rgamma(nMontepost, lambda1Dpost, lambda2Dpost))
  sigmaNDpost = sqrt(1/rgamma(nMontepost, lambda1NDpost, lambda2NDpost))
  sigmaD2post = sigmaDpost^2
  sigmaND2post = sigmaNDpost^2
  
  pre_w = rep(0, nMontepost)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, nD, nND, version) # ADDED FOR COPT
  }
  
  muDpost = mu0Dpost + tau0D*sigmaDpost*rnorm(nMontepost, 0, 1)
  postdeltaTmp = 2*(sigmaD2post- sigmaND2post)*log(((1 - pre_w)/pre_w)*(sigmaDpost/sigmaNDpost))
  postdeltaTmp[postdeltaTmp > 0] = 0
  postdeltaTmp = sqrt(-postdeltaTmp)
  postimpwt = pnorm((muDpost - postdeltaTmp - mu0NDpost)/(tau0ND*sigmaNDpost))
  U = rbeta(nMontepost, 1, 1)
  muNDpost = mu0NDpost + tau0ND*sigmaNDpost*qnorm(postimpwt*U)
  
  countinfspost = 0
  for (i in 1:nMontepost){
    if (muNDpost[i]== -Inf | muNDpost[i] == Inf){countinfspost = countinfspost + 1}
  }
  
  cond2 = (muNDpost - muDpost)**2 + 2*(sigmaD2post-sigmaND2post)*log(((1-pre_w)/pre_w)*(sigmaDpost/sigmaNDpost))
  check1 = min(muDpost-postdeltaTmp-muNDpost) 
  check2 = min(cond2)
  
  c=0*muNDpost
  for (i in 1:nMontepost){
    if (muNDpost[i]==-Inf | muNDpost[i]==Inf){ c[i]=-Inf } 
    else {
      c[i] = (sigmaND2post[i]*muDpost[i]-sigmaD2post[i]*muNDpost[i])-(sigmaDpost[i]*sigmaNDpost[i])*sqrt(cond2[i])}
    }
  c = c/(sigmaND2post - sigmaD2post)
  
  cmod = pt(c,lambda)
  cmodmax = max(cmod)
  cmodmin = min(cmod)
  
  postcmod = rep(0,L)
  for (iMontepost in 1:nMontepost){
    for (igrid in 1:L){
      if ((A[igrid] < cmod[iMontepost]) & (cmod[iMontepost] <= A[igrid + 1])) {
        postcmod[igrid]=postcmod[igrid] + postimpwt[iMontepost]
      }
    }
  }
  
  postcmod = postcmod/sum(postcmod)
  postcopt = tan(pi*postcmod-pi/2)
  postcmoddensity = L*postcmod
  
  newlist = list("postcmod" = postcmod, "postcmoddensity" = postcmoddensity,
                 "postcopt" = postcopt)
  return(newlist)
}

# The relative belief ratio is the same for both cases.

binormal_diag_AUC_prior_error_char_copt_unequal = function(w = FALSE, alpha1w = NA, 
                                                           alpha2w = NA, coptest, 
                                                           nMonteprior, delta, lambda1, 
                                                           lambda2, mu0, tau0){
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  # samples from (muD,sigmaD,muND,sigmaND) and prevalence w
  sigmaD = sqrt(1/rgamma(nMonteprior, lambda1, lambda2))
  sigmaND = sqrt(1/rgamma(nMonteprior, lambda1, lambda2))
  sigmaD2 = sigmaD^2
  sigmaND2 = sigmaND^2
  muD = rnorm(nMonteprior, mu0, (tau0*sigmaD))
  # prevalence
  pre_w = rep(0, nMonteprior)
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, version = "prior")
  }
  priordeltaTmp = 2*(sigmaD2 - sigmaND2)*log(((1-pre_w)/pre_w)*(sigmaD/sigmaND))
  priordeltaTmp[priordeltaTmp>0] = 0
  priordeltaTmp = sqrt(-priordeltaTmp)
  priorimpwt = pnorm((muD - priordeltaTmp-mu0)/(tau0*sigmaND))
  U = rbeta(nMonteprior,1,1)
  muND = mu0 + tau0*sigmaND*qnorm(priorimpwt*U)
  
  FNR = pnorm((coptest - muD)/sigmaD)
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
    single_deltaTmp = priordeltaTmp[iMonteprior]
    if(muD[iMonteprior] - muND[iMonteprior] < single_deltaTmp){
      next
    }
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

binormal_diag_AUC_post_error_char_copt_unequal = function(
    w = FALSE, alpha1w = NA, alpha2w = NA, nND = NA, nD = NA, version,
    coptest, nMontepost, delta, lambda1Dpost, lambda1NDpost, 
    lambda2Dpost, lambda2NDpost,
    mu0Dpost, mu0NDpost, tau0D, tau0ND){
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  sigmaDpost = sqrt(1/rgamma(nMontepost, lambda1Dpost, lambda2Dpost))
  sigmaNDpost = sqrt(1/rgamma(nMontepost, lambda1NDpost, lambda2NDpost))
  sigmaD2post = sigmaDpost^2
  sigmaND2post = sigmaNDpost^2
  wpost = rep(0, nMontepost)
  for(i in 1:length(wpost)){
    wpost[i] = generate_w(w, alpha1w, alpha2w, nD, nND, version) 
  }
  muDpost = mu0Dpost+tau0D*sigmaDpost*rnorm(nMontepost,0,1)
  postdeltaTmp = 2*(sigmaD2post - sigmaND2post)*log(((1-wpost)/wpost)*(sigmaDpost/sigmaNDpost))
  postdeltaTmp[postdeltaTmp > 0] = 0
  postdeltaTmp = sqrt(-postdeltaTmp)
  postimpwt = pnorm((muDpost-postdeltaTmp-mu0NDpost)/(tau0ND*sigmaNDpost))
  U = rbeta(nMontepost, 1, 1)
  muNDpost = mu0NDpost+tau0ND*sigmaNDpost*qnorm(postimpwt*U)
  
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
    single_deltaTmp = postdeltaTmp[iMontepost]
    if(muDpost[iMontepost] - muNDpost[iMontepost] < single_deltaTmp){next}
    
    for (igrid in 1:L){
      if ((A[igrid] < FNRpost[iMontepost]) & (FNRpost[iMontepost] <= A[igrid+1])) {
        postFNR[igrid] = postFNR[igrid] + postimpwt[iMontepost]}
      if ((A[igrid] < FPRpost[iMontepost]) & (FPRpost[iMontepost] <= A[igrid+1])) {
        postFPR[igrid] = postFPR[igrid] + postimpwt[iMontepost]}
      if ((A[igrid] < Errorpost[iMontepost]) & (Errorpost[iMontepost] <= A[igrid+1])) {
        postError[igrid] = postError[igrid] + postimpwt[iMontepost]}
      if ((A[igrid] < FDRpost[iMontepost]) & (FDRpost[iMontepost] <= A[igrid+1])) {
        postFDR[igrid] = postFDR[igrid] + postimpwt[iMontepost]}
      if ((A[igrid] < FNDRpost[iMontepost]) & (FNDRpost[iMontepost] <= A[igrid+1])) {
        postFNDR[igrid] = postFNDR[igrid] + postimpwt[iMontepost]}
    }
  }
  
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

###############################
# TESTING VALUES
###############################

# lambda = df for transforming c values to {0,1] using Student(lambda) cdf
#lambda=1

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

#grid = open_bracket_grid(delta)

#post_hyperpara = binormal_compute_post_hyperpara_unequal(mu0, tau0, lambda1, lambda2, 
#                                                         nND, meanND, sND_squared, 
#                                                         nD, meanD, sD_squared)

#prior_vals = binormal_diag_prior_unequal("conditional", w, alpha1w, alpha2w, 
#                                         nMonteprior, delta, 
#                                         lambda1, lambda2, mu0, tau0)

#post_vals = binormal_diag_post_unequal("unconditional", w, alpha1w, alpha2w, 
#                          nND, nD, version, nMontepost, delta, 
#                          post_hyperpara$lambda1Dpost, post_hyperpara$lambda1NDpost, 
#                          post_hyperpara$lambda2Dpost, post_hyperpara$lambda2NDpost, 
#                          post_hyperpara$mu0Dpost, post_hyperpara$mu0NDpost, 
#                          post_hyperpara$tau0D, post_hyperpara$tau0ND)

#rbr_vals = binormal_diag_RBR("unconditional", delta, prior_vals$probAUCprior, 
#                             post_vals$probAUCpost, 
#                             prior_vals$priorAUC, 
#                             post_vals$postAUC)

#plot(grid,post_vals$postAUCdensity, xlab="AUC",ylab="prior and posterior", type = "l")
#lines(grid, prior_vals$priorAUCdensity, type="l")
#plot(grid,rbr_vals$RB_AUC,xlab="AUC",ylab=expression("RB"),type="l")

################# COPT

#w = FALSE
#prior_copt = binormal_diag_prior_copt_unequal(w, alpha1w, alpha2w, nMonteprior, 
#                                              delta, lambda1, lambda2, mu0, tau0, lambda)

#post_copt = binormal_diag_post_copt_unequal(w, alpha1w, alpha2w, nND, nD, 
#                                            "post", nMontepost, delta, 
#                                            post_hyperpara$lambda1Dpost, 
#                                            post_hyperpara$lambda1NDpost, 
#                                            post_hyperpara$lambda2Dpost, 
#                                            post_hyperpara$lambda2NDpost, 
#                                            post_hyperpara$mu0Dpost, 
#                                            post_hyperpara$mu0NDpost, 
#                                            post_hyperpara$tau0D, 
#                                            post_hyperpara$tau0ND, 
#                                            lambda)

#rbr_copt = binormal_diag_RBR_copt(delta, 
#                                  prior_copt$priorcmod, 
#                                  post_copt$postcmod)

#binormal_diag_plots_AUC_copt(delta = delta, 
#                             priorcmoddensity = prior_copt$priorcmoddensity,
#                             postcmoddensity = post_copt$postcmoddensity)

#binormal_diag_plots_AUC_copt(delta = delta,
#                             RBcmod = rbr_copt$RBcmod)
  

#plot(grid, priorcmoddensity, xlab="cmod",ylab="prior",type="l",lty=1)


#coptest = 0.7386115

#prior_vals_copt = binormal_diag_AUC_prior_error_char_copt_unequal(w, alpha1w, alpha2w, 
#                                                                  coptest, nMonteprior, 
#                                                                  delta, lambda1, 
#                                                                  lambda2, mu0, tau0)

#post_vals_copt = binormal_diag_AUC_post_error_char_copt_unequal(w, 
#    alpha1w, alpha2w, nND, nD, "post",
#    coptest, nMontepost, delta, 
#    post_hyperpara$lambda1Dpost, post_hyperpara$lambda1NDpost, 
#    post_hyperpara$lambda2Dpost, post_hyperpara$lambda2NDpost,
#    post_hyperpara$mu0Dpost, post_hyperpara$mu0NDpost, 
#    post_hyperpara$tau0D, post_hyperpara$tau0ND)


