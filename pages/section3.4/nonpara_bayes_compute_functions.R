###############################################
# Choosing a (if inputting epsilon)           #
###############################################

# Helper Functions ############################

elicit_beta = function(r, epsilon, s1, s2){
  # elicitation, for beta distribution probabilities
  v1 = min(r + epsilon, 1)
  v2 = max(0, r - epsilon)
  val = pbeta(v1, shape1 = s1, shape2 = s2) - pbeta(v2, shape1 = s1, shape2 = s2)
  return(val)
}

elicit_upper_bds = function(r, a, epsilon){
  # elicitation, upper values in bounds
  # search, parameter 'a'
  up_vals = c()
  for (i in 1:length(r)){
    up_val = elicit_beta(r[i], epsilon, a*r[i], a*(1-r[i])) 
    up_vals = c(up_vals, up_val)
  }
  return(up_vals)
}

# Computable Functions ########################

dirichlet_process_a = function(epsilon){
  # Computes the a for the dirichlet process
  it = 1 # iteration
  a = c(1) # initial
  up_bds = c()
  up_bd = c(0.11) #initialization
  
  # start
  while (it >= 1){
    
    final = list()
    r = seq(0.01, 0.99, by = 0.01)
    up_vals = elicit_upper_bds(r, a, epsilon)
    
    # next iteration
    a = a+0.05
    up_bd = max(1-up_vals)
    #print(up_bd)
    up_bds = c(up_bds, up_bd)
    
    # criteria
    if (up_bd < 0.1){
      final$a = a
      final$up_bd = up_bd
      final$up_bds = up_bds
      #return(final)
      break
    }
    it = it+1
  }
  
  return(list("a" = final$a, "upper_bound" = final$up_bd,
              "upper_bounds" = final$up_bds))
}

###############################################
# Calculations for inferences for the AUC     #
###############################################

# Helper Functions ############################

gen_emp = function(x, n_count, x_data){
  # this is a general function to save lines
  x_unique = sort(unique(x_data))
  arg = n_count*x
  ihold = n_count
  for (i in 1:n_count){
    if (((i-1) < arg ) & (arg <= i)){ihold = i} 
  }
  return(x_data[ihold])
}

nonpara_bayes_compute_post_hyperpara = function(mu0, tau0, lambda1, lambda2, 
                                                nD, nND, sD_squared, sND_squared, meanD, meanND){
  # the values of the hyperparameters for the posterior based on the prior and the data 
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

# Computable Functions ########################

nonpara_bayes_AUC_prior = function(condition, nMonteprior, nstar, a, delta,
                                   mu0, tau0, lambda1, lambda2){
  
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  if(condition == "conditional" || condition == "cond"){
    A = 1/2+A/2 # the new grid when conditioning H_0: AUC>=1/2 
  }
  
  AUC = rep(0, nMonteprior)
  priorAUC = rep(0, L)
  probAUCprior = 0 # prior probability AUC > 1/2
  
  sigmaD = sqrt(1/rgamma(nMonteprior, lambda1, lambda2))
  sigmaND = sqrt(1/rgamma(nMonteprior, lambda1, lambda2))
  muD = rnorm(nMonteprior, mu0,(tau0*sigmaD))
  muND = rnorm(nMonteprior, mu0,(tau0*sigmaND))
  
  # Set n* for approximating random measure
  alphaarray = (a + rep(0, nstar))/nstar
  pNDarray = rdirichlet(nMonteprior, alphaarray)
  pDarray = rdirichlet(nMonteprior, alphaarray)
  
  # Generate the c's 
  cND = 0*pNDarray
  cD = 0*pDarray
  for (i in 1:nMonteprior){
    cND[i,] = rnorm(nstar, muND[i], sigmaND[i])
    cD[i,] = rnorm(nstar, muD[i], sigmaD[i])
  }
  
  # here we are calculating F_D(c) at each c in the sample from F_ND (this is time consuming)
  FD = 0*pDarray
  for (i in 1:nMonteprior){
    for (j in 1:nstar){
      FD[i,j] = FD[i,j] + sum(pDarray[i, which(cD[i, 1:nstar] <= cND[i,j])])
    }
  }
  
  # here we compute the AUC for each Monte Carlo generted F_D, F_ND
  for(i in 1:nMonteprior){
    AUC[i]=(1 - FD[i,])%*%pNDarray[i,]
    if (AUC[i] > 0.5){probAUCprior = probAUCprior + 1}
  }
  
  AUCsum=0
  if(condition == "conditional" || condition == "cond"){
    for (i in 1:nMonteprior) {
      if (AUC[i] >= 1/2){ 
        AUCsum = AUCsum + 1 
        for (igrid in 1:L){
          if ((A[igrid] < as.numeric(AUC[i])) & (as.numeric(AUC[i]) <= A[igrid + 1])) {
            priorAUC[igrid] = priorAUC[igrid] + 1 
          } 
        }
      }
    }
  } else if (condition == "unconditional" || condition == "uncond"){
    for (i in 1:nMonteprior) {
      for (igrid in 1:L){
        if ((A[igrid] < as.numeric(AUC[i])) & (as.numeric(AUC[i]) <= A[igrid + 1])) {
          priorAUC[igrid] = priorAUC[igrid] + 1 
        }
      }
    }
  }
  
  if(condition == "conditional" || condition == "cond"){
    priorAUC = priorAUC/AUCsum # this is for conditional
  } else if (condition == "unconditional" || condition == "uncond"){
    priorAUC = priorAUC/nMonteprior
  }
  priorAUCdensity = L*priorAUC
  probAUCprior = probAUCprior/nMonteprior
  
  newlist = list("priorAUC" = priorAUC, "priorAUCdensity" = priorAUCdensity,
                 "probAUCprior" = probAUCprior, "pDarray" = pDarray,
                 "pNDarray" = pNDarray)
  return(newlist)
}

nonpara_bayes_AUC_post = function(condition, nMontepost, nstar, a, delta,
                                  mu0, tau0, lambda1, lambda2, xDdata = NA, xNDdata = NA,
                                  nD = NA, nND = NA, sD_squared = NA, sND_squared = NA, 
                                  meanD = NA, meanND = NA){
  L = 1/delta
  A = closed_bracket_grid(delta)# this is technically their grid
  
  # The user can either put in actual numbers or put in the actual data 
  # - need to check that the same is for the other function
  if(is.na(sD_squared) == TRUE || is.na(sND_squared) == TRUE || is.na(meanD) == TRUE || is.na(meanND) == TRUE){
    nD = length(xDdata)
    nND = length(xNDdata)
    sD_squared = (nD - 1)*var(xDdata)
    sND_squared = (nND - 1)*var(xNDdata)
    meanD = mean(xDdata)
    meanND = mean(xNDdata)
  } else if (is.na(sD_squared) == FALSE & is.na(sND_squared) == FALSE & is.na(meanD) == FALSE & is.na(meanND) == FALSE
             & is.na(nD) == FALSE & is.na(nND) == FALSE & is.na(xDdata) == TRUE & is.na(xNDdata) == TRUE){
    # this is the case where the data needs to be generated from a normal dist -- add to the other one as well
    z = rnorm(nND, 0, 1)
    xNDdata = meanND + sqrt(sND_squared/(nND - 1))*(z - mean(z))/sqrt(var(z))
    z = rnorm(nD, 0, 1)
    xDdata = meanD + sqrt(sD_squared/(nD - 1))*(z - mean(z))/sqrt(var(z))
  } else if (is.na(sD_squared) == FALSE & is.na(sND_squared) == FALSE & is.na(meanD) == FALSE & is.na(meanND) == FALSE
             & is.na(xDdata) == FALSE & is.na(xNDdata) == FALSE){
    return("There is no data. Either put in the descriptive statistics (nD, nND, sD_squared, sND_squared),
           or put in the data (xDdata, xNDdata.)")
  }
  
  if(condition == "conditional" || condition == "cond"){
    A = 1/2+A/2 # the new grid when conditioning H_0: AUC>=1/2 
  }
  
  AUC = rep(0,nMontepost)
  postAUC = rep(0, L)
  probAUCpost = 0
  hy = nonpara_bayes_compute_post_hyperpara(mu0, tau0, lambda1, lambda2, 
                                            nD, nND, sD_squared, sND_squared, meanD, meanND)
  # concentration parameters and mixture probabilities for the posterior Dirichlet processes
  aD = a + nD
  pD = a/aD
  aND = a + nND
  pND = a/aND
  
  # set nstar for approximating random measure and generate probabilities
  alphaDarraypost = (aD + rep(0, nstar))/nstar
  alphaNDarraypost = (aND + rep(0, nstar))/nstar
  pNDarraypost = rdirichlet(nMontepost, alphaNDarraypost)
  pDarraypost = rdirichlet(nMontepost, alphaDarraypost)
  
  #generate the mixture values 1 = a value from H and 0 from empirical dist
  indD = array(rbinom(nMontepost*nstar, 1, pD), c(nMontepost, nstar))
  indND = array(rbinom(nMontepost*nstar, 1, pND), c(nMontepost, nstar))
  uD = array(runif(nMontepost*nstar, 0, 1), c(nMontepost, nstar))
  uND = array(runif(nMontepost*nstar, 0, 1), c(nMontepost, nstar))
  
  #generate the mu's and sigma's
  sigmaDpost = sqrt(1/rgamma(nMontepost, hy$lambda1Dpost, hy$lambda2Dpost))
  sigmaNDpost = sqrt(1/rgamma(nMontepost, hy$lambda1NDpost, hy$lambda2NDpost))
  muDpost = hy$mu0Dpost + hy$tau0D*sigmaDpost*rnorm(nMontepost, 0, 1)
  muNDpost = hy$mu0NDpost + hy$tau0ND*sigmaNDpost*rnorm(nMontepost, 0, 1)
  
  # temp - for debugging
  #return(list("muDpost" = muDpost, "sigmaDpost" = sigmaDpost))
  
  # generate the c's 
  cND = 0*pNDarraypost # 0*pNDarray 
  cD = 0*pDarraypost  # 0*pDarray
  for(i in 1:nMontepost){
    for(j in 1:nstar){
      if(indD[i,j] == 1){
        cD[i,j] = rnorm(1, muDpost[i], sigmaDpost[i])}
      else {cD[i,j] = gen_emp(uD[i,j], nD, xDdata)}
      if(indND[i,j] == 1){
        cND[i,j] = rnorm(1, muNDpost[i], sigmaNDpost[i])}
      else {cND[i,j] = gen_emp(uND[i,j], nND, xNDdata)}}
  }
  
  # to calculate the generated AUC's
  FD = 0*pDarraypost #0*pDarray 
  for (i in 1:nMontepost){
    for (j in 1:nstar){
      #FD[i,j] = FD[i,j] + sum(pDarray[i, which(cD[i,1:nstar] <= cND[i,j])])
      FD[i,j] = FD[i,j] + sum(pDarraypost[i, which(cD[i,1:nstar] <= cND[i,j])])
    }
  }
  for (i in 1:nMontepost){
    AUC[i] = (1 - FD[i, ])%*%pNDarraypost[i,] #(1 - FD[i, ])%*%pNDarray[i,]
    if (AUC[i] > 0.5){probAUCpost = probAUCpost + 1}
  }
  AUCsum=0
  if(condition == "conditional" || condition == "cond"){
    for (i in 1:nMontepost) {
      if (AUC[i]>=1/2){
        AUCsum = AUCsum + 1
        for (igrid in 1:L){
          if ((A[igrid] < as.numeric(AUC[i])) & (as.numeric(AUC[i]) <= A[igrid+1])){
            postAUC[igrid] = postAUC[igrid] + 1 
          }
        }
      }
    }
  } else if (condition == "unconditional" || condition == "uncond"){
    for (i in 1:nMontepost) {
      for (igrid in 1:L){
        if ((A[igrid] < as.numeric(AUC[i])) & (as.numeric(AUC[i]) <= A[igrid+1])){
          postAUC[igrid] = postAUC[igrid] + 1 
        }
      }
    }
  }
  
  if(condition == "conditional" || condition == "cond"){
    postAUC = postAUC/AUCsum # this is for conditional
  } else if (condition == "unconditional" || condition == "uncond"){
    postAUC = postAUC/nMontepost
  }
  postAUCdensity = L*postAUC
  probAUCpost = probAUCpost/nMontepost
  
  newlist = list("postAUC" = postAUC, "postAUCdensity" = postAUCdensity,
                 "probAUCpost" = probAUCpost)
  return(newlist)
}

nonpara_bayes_AUC_rbr = function(delta, probAUCprior, probAUCpost,
                                 priorAUC, postAUC){
  grid = open_bracket_grid(delta)
  RBprobAUC = probAUCpost/probAUCprior
  L = 1/delta
  # RBprobAUC is rel. belief ratio of AUC>1/2
  # strength of evidence: probAUCpost
  
  RB_AUC = postAUC/priorAUC
  AUCest = grid[which.max(RB_AUC)]
  postPl_AUC = 0 # posterior content of the plausible region
  RB_AUC_naomit = RB_AUC
  RB_AUC_naomit[is.na(RB_AUC_naomit)] = 0
  
  for (i in 1:L){
    if(priorAUC[i] > 0 & RB_AUC_naomit[i] > 1 ){postPl_AUC = postPl_AUC + postAUC[i]}
  }
  
  plausible_region = c()
  for (i in 1:length(grid)){
    if (priorAUC[i] > 0 & RB_AUC[i] > 1){
      plausible_region = c(plausible_region, as.numeric(grid[i]))
    }
  }
  
  newlist = list("RBprobAUC" = RBprobAUC, "postPl_AUC" = postPl_AUC,
                 "RB_AUC" = RB_AUC, "AUCest" = AUCest, "plausible_region" = plausible_region)
  return(newlist)
}

# need to edit!!
nonpara_bayes_compute_credible_region = function(gamma, grid, AUC_RBR, AUC_prior, AUC_post, 
                                                 plausible_region, posterior_content){
  # Note: credible region is now based on the line plot.
  #grid = open_bracket_grid(delta) # NOTE: user needs to insert the grid as it changes.
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
          # debugging
          if ((grid[j] >= plausible_region[1]) & (grid[j] <= plausible_region[length(plausible_region)])) {
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
        
        #credible_region = c(min(credible_region), max(credible_region))
        
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

###############################################
# Calculations for inferences for copt        #
###############################################

# Helper Functions ############################

# might want to change function name later?
cdf = function(x,y,z){
  n = length(x)
  dum = 0*c(1:n)
  for(i in 1:n) {
    dum[i] = sum(z[which(y <= x[i])])
  }
  return(dum)
}

create_gridcopt = function(mincopt, maxcopt, A, grid){
  diffcopt = maxcopt - mincopt
  Acopt = mincopt + diffcopt * A
  gridcopt = mincopt + diffcopt*grid
  newlist = list("diffcopt" = diffcopt, "Acopt" = Acopt, "gridcopt" = gridcopt)
}

# Computable Functions ########################

nonpara_bayes_AUC_prior_copt = function(w = FALSE, alpha1w = NA, alpha2w = NA,
                                        nMonteprior, nstar, a, delta,
                                        mu0, tau0, lambda1, lambda2){
  
  # debugging
  #return(list(w, alpha1w, alpha2w, nMonteprior, nstar, a, delta, mu0, tau0, lambda1, lambda2))
  A = closed_bracket_grid(delta) # this is technically their grid
  grid = open_bracket_grid(delta)
  L = (1/delta) # length
  
  #generate mu's and sigma's
  sigmaD = sqrt(1/rgamma(nMonteprior, lambda1, lambda2))
  sigmaND = sqrt(1/rgamma(nMonteprior, lambda1, lambda2))
  muD = rnorm(nMonteprior, mu0, (tau0*sigmaD))
  muND = rnorm(nMonteprior, mu0, (tau0*sigmaND))
  
  pre_w = rep(0, nMonteprior) # generate value of w
  for(i in 1:length(pre_w)){
    pre_w[i] = generate_w(w, alpha1w, alpha2w, version = "prior") 
  }
  
  # get arrays for generating from distributions obtained from Dirichlet process
  alphaarray = (a + rep(0, nstar))/nstar
  pNDarray = rdirichlet(nMonteprior, alphaarray)
  pDarray = rdirichlet(nMonteprior, alphaarray)
  
  # generate the c's 
  cND = 0*pNDarray
  cD = 0*pDarray
  for (i in 1:nMonteprior){
    cND[i,] = rnorm(nstar, muND[i], sigmaND[i])
    cD[i,] = rnorm(nstar, muD[i], sigmaD[i])
  }
  
  # here we are calculating F_D(c) at each c in the sample from F_ND
  FD = 0*pDarray
  for(i in 1:nMonteprior){
    for(j in 1:nstar){
      FD[i,j] = FD[i,j] + sum(pDarray[i, which(cD[i, 1:nstar] <= cND[i,j])])
    }
  }
  
  # Here we compute the AUC for each Monte Carlo generated F_D, F_ND
  AUC = rep(0,nMonteprior)
  for(i in 1:nMonteprior){
    AUC[i] = (1 - FD[i,])%*%pNDarray[i,]
  }
  
  # Order the values
  for (i in 1:nMonteprior){
    pNDarray[i,] = pNDarray[i, order(cND[i,])]
    cND[i,] = sort(cND[i,])
    pDarray[i,] = pDarray[i, order(cD[i,])]
    cD[i,] = sort(cD[i,])
    pNDarray[i,] = pNDarray[i, order(cND[i,])]
    cND[i,] = sort(cND[i,])
  }
  
  # create relevant grid for c_opt
  grids_for_copt = create_gridcopt(mincopt = -10, maxcopt = 10, A, grid)
  Acopt = grids_for_copt$Acopt
  gridcopt = grids_for_copt$gridcopt
  diffcopt = grids_for_copt$diffcopt
  
  #computing FNR, FPR and Error at grid values
  priorFNR = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  priorFPR = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  priorError = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  priorFDR = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  priorFNDR = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  for(i in 1:nMonteprior){
    priorFNR[i,] = cdf(gridcopt, cD[i,], pDarray[i,])
    priorFPR[i,] = abs(1 - cdf(gridcopt, cND[i,], pNDarray[i,]))
    priorError[i,] = pre_w[i]*priorFNR[i,] + (1 - pre_w[i])*priorFPR[i,]
    for(j in 1:L){
      if((1 - pre_w[i])*priorFPR[i,j] + pre_w[i]*(1 - priorFNR[i,j]) > 10^(-15)){
        priorFDR[i,j] = (1 - pre_w[i])*priorFPR[i,j]/((1 - pre_w[i])*priorFPR[i,j] + pre_w[i]*(1 - priorFNR[i,j])) 
      }
      if((1 - pre_w[i])*priorFPR[i,j] + pre_w[i]*priorFNR[i,j] > 10^(-15)) {
        priorFNDR[i,j] = pre_w[i]*priorFNR[i,j]/((1 - pre_w[i])*priorFPR[i,j] + pre_w[i]*priorFNR[i,j])
      }
    }
  }
  
  # determine the minimizing c value of some criterion such as Error(c)
  # and when multiple c values minimize choose one most central
  copt = rep(0,nMonteprior)
  mid = L/2
  for(i in 1:nMonteprior) {
    errmin = priorError[i,which.min(priorError[i,])]
    hold = rep(L+1, L)
    j0 = 1
    for(j in 1:L){
      if(priorError[i,j] == errmin){
        hold[j0] = j
        j0 = j0 + 1}
    }
    copt[i] = gridcopt[hold[which.min(abs(hold - mid))]]
  }
  
  coptmod = .5+atan(copt)/pi # grid for coptmod
  Acoptmod = .5+atan(Acopt)/pi
  gridmod = .5+atan(gridcopt)/pi
  
  #estimate the prior density of copt
  AUCchecksumprior=0
  priorcopt=rep(0,L)
  priorcoptmod=rep(0,L)
  for (i in 1:nMonteprior) {
    if( AUC[i] >= .5){
      AUCchecksumprior=AUCchecksumprior+1
      for (igrid in 1:L){
        if ( (Acopt[igrid] < copt[i]) & (copt[i] <= Acopt[igrid+1]) ) {
          priorcopt[igrid]=priorcopt[igrid]+1 }
        if ( (Acoptmod[igrid] < coptmod[i]) & (coptmod[i] <= Acoptmod[igrid+1]) ) {
          priorcoptmod[igrid]=priorcoptmod[igrid]+1 }
      }
    }
  }
  
  priorcopt = priorcopt/AUCchecksumprior
  priorcoptdensity = L*priorcopt/diffcopt
  priorcoptmod = priorcoptmod/AUCchecksumprior
  priorcoptmoddensity = rep(0,L)
  
  for(i in 1:L){
    priorcoptmoddensity[i] = priorcoptmod[i]/(Acoptmod[i+1] - Acoptmod[i])
  }
  
  int=0
  for(i in 1:L){
    int = int + (priorcoptmoddensity[i])*(Acoptmod[i+1] - Acoptmod[i])
  }
  
  newlist = list("priorcopt" = priorcopt, "priorcoptmod" = priorcoptmod,
                 "gridmod" = gridmod, "priorcoptmoddensity" = priorcoptmoddensity,
                 "gridcopt" = gridcopt, "priorcoptdensity" = priorcoptdensity,
                 "priorFNR" = priorFNR, "priorFPR" = priorFPR, 
                 "priorError" = priorError, "priorFDR" = priorFDR, 
                 "priorFNDR" = priorFNDR)
  return(newlist)
}

nonpara_bayes_AUC_post_copt = function(w = FALSE, alpha1w = NA, alpha2w = NA,
                                       nD = NA, nND = NA, version,
                                       nMontepost, nstar, a, delta,
                                       mu0, tau0, lambda1, lambda2,
                                       xDdata = NA, xNDdata = NA,
                                       sD_squared = NA, sND_squared = NA, 
                                       meanD = NA, meanND = NA){
  
  L = 1/delta
  A = closed_bracket_grid(delta) # this is technically their grid
  grid = open_bracket_grid(delta)
  grids_for_copt = create_gridcopt(mincopt = -10, maxcopt = 10, A, grid)
  Acopt = grids_for_copt$Acopt
  gridcopt = grids_for_copt$gridcopt
  diffcopt = grids_for_copt$diffcopt
  
  # The user can either put in actual numbers or put in the actual data 
  # - need to check that the same is for the other function
  if(is.na(sD_squared) == TRUE || is.na(sND_squared) == TRUE || is.na(meanD) == TRUE || is.na(meanND) == TRUE){
    nD = length(xDdata)
    nND = length(xNDdata)
    sD_squared = (nD - 1)*var(xDdata)
    sND_squared = (nND - 1)*var(xNDdata)
    meanD = mean(xDdata)
    meanND = mean(xNDdata)
  } else if (is.na(sD_squared) == FALSE & is.na(sND_squared) == FALSE & is.na(meanD) == FALSE & is.na(meanND) == FALSE
             & is.na(nD) == FALSE & is.na(nND) == FALSE & is.na(xDdata) == TRUE & is.na(xNDdata) == TRUE){
    # this is the case where the data needs to be generated from a normal dist -- add to the other one as well
    z = rnorm(nND, 0, 1)
    xNDdata = meanND + sqrt(sND_squared/(nND - 1))*(z - mean(z))/sqrt(var(z))
    z = rnorm(nD, 0, 1)
    xDdata = meanD + sqrt(sD_squared/(nD - 1))*(z - mean(z))/sqrt(var(z))
  } else if (is.na(sD_squared) == FALSE & is.na(sND_squared) == FALSE & is.na(meanD) == FALSE & is.na(meanND) == FALSE
             & is.na(xDdata) == FALSE & is.na(xNDdata) == FALSE){
    return("There is no data. Either put in the descriptive statistics (nD, nND, sD_squared, sND_squared),
           or put in the data (xDdata, xNDdata.)")
  }
  
  hy = nonpara_bayes_compute_post_hyperpara(mu0, tau0, lambda1, lambda2, 
                                            nD, nND, sD_squared, sND_squared, meanD, meanND)
  # concentration parameters and mixture probabilities for the posterior Dirichlet processes
  aD = a + nD
  pD = a/aD
  aND = a + nND
  pND = a/aND
  #generate the mu's and sigma's
  
  sigmaDpost = sqrt(1/rgamma(nMontepost, hy$lambda1Dpost, hy$lambda2Dpost))
  sigmaNDpost = sqrt(1/rgamma(nMontepost, hy$lambda1NDpost, hy$lambda2NDpost))
  muDpost = hy$mu0Dpost + hy$tau0D*sigmaDpost*rnorm(nMontepost, 0, 1)
  muNDpost = hy$mu0NDpost + hy$tau0ND*sigmaNDpost*rnorm(nMontepost, 0, 1)
  wpost = rep(0, nMontepost) # generate value of w
  for(i in 1:length(wpost)){
    wpost[i] = generate_w(w, alpha1w, alpha2w, nD, nND, version = version) 
  }
  
  # is condition satisfied for optimal c to exist? The program actually only conditions on AUC >= 1/2.
  #checkpost=rep(0,nMontepost)
  #criterionpost=rep(0,nMontepost)
  #for (i in 1: nMontepost){
  #criterionpost[i]=muNDpost[i] - muDpost[i] +sqrt(max(0,-2*(sigmaDpost[i]**2-sigmaNDpost[i]**2)*log(((1-wpost[i])/wpost[i])*sigmaDpost[i]/sigmaNDpost[i]) ) )
  #if (criterionpost[i] <= 0  ){
  #checkpost[i]=1}
  #}
  #sum(checkpost)
  
  # arrays for approximating random measure
  alphaDarraypost = (aD + rep(0,nstar))/nstar
  alphaNDarraypost = (aND + rep(0,nstar))/nstar
  pNDarraypost = rdirichlet(nMontepost, alphaDarraypost)
  pDarraypost = rdirichlet(nMontepost, alphaNDarraypost)
  
  #generate the mixture values 1 = a value from H and 0 from empirical dist
  indD = array(rbinom(nMontepost*nstar, 1, pD), c(nMontepost, nstar))
  indND = array(rbinom(nMontepost*nstar, 1, pND), c(nMontepost, nstar))
  uD = array(runif(nMontepost*nstar, 0, 1), c(nMontepost, nstar))
  uND = array(runif(nMontepost*nstar, 0, 1), c(nMontepost, nstar))
  
  # generate the c's 
  cNDpost = 0*pNDarraypost
  cDpost = 0*pDarraypost
  for (i in 1:nMontepost){
    for (j in 1:nstar){
      if(indD[i,j] == 1){
        cDpost[i,j] = rnorm(1, muDpost[i], sigmaDpost[i])}
      else{cDpost[i,j] = gen_emp(uD[i,j], nD, xDdata)}
      if(indND[i,j] == 1){
        cNDpost[i,j] = rnorm(1, muNDpost[i], sigmaNDpost[i])}
      else {cNDpost[i,j] = gen_emp(uND[i,j], nND, xNDdata)}
    }
  }
  
  # to calculate the generated AUC's
  FDpost = 0*pDarraypost
  AUCpost = rep(0,nMontepost)
  for(i in 1:nMontepost){
    for(j in 1:nstar){
      FDpost[i,j] = FDpost[i,j] + sum(pDarraypost[i,which(cDpost[i,1:nstar] <= cNDpost[i,j])])}}
  for(i in 1:nMontepost){
    AUCpost[i] = (1 - FDpost[i,])%*%pNDarraypost[i,]
  }
  
  #computing FNR, FPR and Error at grid values
  postFNR = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  postFPR = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  postError = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  postFDR = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  postFNDR = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  for (i in 1:nMontepost){
    postFNR[i,] = abs(cdf(gridcopt, cDpost[i,], pDarraypost[i,]))
    postFPR[i,] = abs(1 - cdf(gridcopt, cNDpost[i,], pNDarraypost[i,]))
    postError[i,] = wpost[i]*postFNR[i,] + (1 - wpost[i])*postFPR[i,]
  }
  for (j in 1:L){
    if ((1 - wpost[i])*postFPR[i,j] + abs(wpost[i]*(1 - postFNR[i,j])) > 0) {
      postFDR[i,j] = (1 - wpost[i])*postFPR[i,j]/((1 - wpost[i])*postFPR[i,j] + wpost[i]*(1 - postFNR[i,j])) }
    if (abs((1 - wpost[i])*(1 - postFPR[i,j])) + wpost[i]*postFNR[i,j] > 0) {
      postFNDR[i,j] = wpost[i]*postFNR[i,j]/(abs((1 - wpost[i])*(1-postFPR[i,j])) + wpost[i]*postFNR[i,j])}
  }
  
  # determine the minimizing c value of some criterion such as Error(c)
  # and when multiple c values minimize choose one most central
  copt = rep(0,nMontepost)
  mid = L/2
  for(i in 1:nMontepost){
    errmin = postError[i,which.min(postError[i,])]
    hold = rep(L+1, L)
    j0 = 1
    for(j in 1:L){
      if(postError[i,j] == errmin){
        hold[j0] = j
        j0 = j0 + 1
      }
    }
    copt[i] = gridcopt[hold[which.min(abs(hold - mid))]]
  }
  
  coptmod = .5 + atan(copt)/pi # grid for coptmod
  Acoptmod = .5+atan(Acopt)/pi
  gridmod = .5+atan(gridcopt)/pi
  
  #estimate the posterior density of c_opt and coptmod
  AUCchecksumpost = 0
  postcopt = rep(0, L)
  postcoptmod = rep(0, L)
  for(i in 1:nMontepost) {
    if(AUCpost[i] >= 0.5){
      AUCchecksumpost = AUCchecksumpost + 1
      for (igrid in 1:L){
        if ((Acopt[igrid] < copt[i]) & (copt[i] <= Acopt[igrid + 1])) {
          postcopt[igrid] = postcopt[igrid] + 1 }
        if ((Acoptmod[igrid] < coptmod[i]) & (coptmod[i] <= Acoptmod[igrid + 1])) {
          postcoptmod[igrid] = postcoptmod[igrid]+1 }
      }
    }
  }
  
  postcopt = postcopt/AUCchecksumpost
  postcoptdensity = L*postcopt/diffcopt
  postcoptmod = postcoptmod/AUCchecksumpost
  postcoptmoddensity = rep(0,L)
  for (i in 1:L){
    postcoptmoddensity[i] = postcoptmod[i]/(Acoptmod[i + 1] - Acoptmod[i])
  }
  
  newlist = list("gridcopt" = gridcopt, "gridmod" = gridmod,
                 "postcopt" = postcopt, "postcoptdensity" = postcoptdensity, 
                 "postcoptmod" = postcoptmod, "postcoptmoddensity" = postcoptmoddensity,
                 "postFNR" = postFNR, "postFPR" = postFPR, 
                 "postError" = postError, "postFDR" = postFDR, 
                 "postFNDR" = postFNDR)
  return(newlist)
}

nonpara_bayes_AUC_rbr_copt = function(delta, gridcopt, gridmod, 
                                      priorcoptdensity, postcoptdensity,
                                      priorcopt, postcopt, 
                                      priorcoptmod, postcoptmod){
  L = 1/delta
  # note: check to see if this is repeated so we can just re-use it instead.
  RBcopt = postcoptdensity/priorcoptdensity
  RBcoptmod = postcoptmod/priorcoptmod
  
  imax = NA
  for (i in 1:L){
    if (priorcoptdensity[i] > 0){imax = i}
  }
  
  if(is.na(imax) == TRUE){
    err_msg = "ERROR: Could not find values when priorcoptdensity is greater than 0."
    newlist = list("RBcopt" = err_msg, "coptest" = err_msg, 
                   "postPlcopt" = err_msg, "copt_plausible_region" = err_msg,
                   "RBcoptmod" = err_msg, "cmod_plausible_region" = err_msg)
    return(newlist)
  }
  
  for (i in 1:L){
    if((priorcopt[i] > 0) & (postcopt[i] > 0)){
      if(RBcopt[i] > RBcopt[imax]){imax=i}
    }
  }
  
  coptest = gridcopt[imax]
  postPlcopt = 0 # Posterior content of the plausible region for copt
  postPlcmod = 0 # Posterior content of the plausible region for cmod
  
  #for (i in 1:L) { # NOTE: this section may be redundant
  #  if (priorcopt[i] > 0 & (postcopt[i] > 0)){
  #    if( RBcopt[i] > 1 ){postPlcopt = postPlcopt + postcopt[i]}
  #  }
  #}
  
  # to remove the NAs to reduce errors
  RBcopt_alt = RBcopt
  RBcopt_alt[is.na(RBcopt_alt)] = 0
  RBcoptmod_alt = RBcoptmod
  RBcoptmod_alt[is.na(RBcoptmod_alt)] = 0
  
  copt_plausible_region = c()
  for (i in 1:length(gridcopt)){
    if(priorcopt[i] > 0 & postcopt[i] > 0){ # note: this may be too much.
      if (RBcopt_alt[i] > 1){ 
        postPlcopt = postPlcopt + postcopt[i]
        copt_plausible_region = c(copt_plausible_region, as.numeric(gridcopt[i]))
      }
    }
  }
  cmod_plausible_region = c()
  for (i in 1:length(gridmod)){
    if (RBcoptmod_alt[i] > 1){ # may not need the extra measure for this one?
      postPlcmod = postPlcmod + postcoptmod[i]
      cmod_plausible_region = c(cmod_plausible_region, as.numeric(gridmod[i]))
    }
  }
  
  newlist = list("RBcopt" = RBcopt, "coptest" = coptest, 
                 "postPlcopt" = postPlcopt, "copt_plausible_region" = copt_plausible_region,
                 "RBcoptmod" = RBcoptmod, "postPlcmod" = postPlcmod,
                 "cmod_plausible_region" = cmod_plausible_region)
  return(newlist)
}

nonpara_bayes_AUC_rbr_error_char_copt = function(grid, # usually use gridcopt
                                                 priorFNR, priorFPR, 
                                                 priorError, priorFDR, 
                                                 priorFNDR, postFNR, 
                                                 postFPR, postError, 
                                                 postFDR, postFNDR){
  L = length(grid)
  RBFNR = postFNR/priorFNR
  RBFPR = postFPR/priorFPR
  RBError = postError/priorError
  RBFDR = postFDR/priorFDR
  RBFNDR = postFNDR/priorFNDR
  
  # mutating the values to deal with the errors
  priorFNR[is.na(priorFNR)] = 0
  priorFPR[is.na(priorFPR)] = 0
  priorError[is.na(priorError)] = 0
  priorFDR[is.na(priorFDR)] = 0
  priorFNDR[is.na(priorFNDR)] = 0
  RBFNR_alt = RBFNR
  RBFPR_alt = RBFPR
  RBError_alt = RBError
  RBFDR_alt = RBFDR
  RBFNDR_alt = RBFNDR
  RBFNR_alt[is.na(RBFNR_alt)] = 0
  RBFPR_alt[is.na(RBFPR_alt)] = 0
  RBError_alt[is.na(RBError_alt)] = 0
  RBFDR_alt[is.na(RBFDR_alt)] = 0
  RBFNDR_alt[is.na(RBFNDR_alt)] = 0
  
  imaxFNR = NA
  imaxFPR = NA 
  imaxError = NA
  imaxFDR = NA
  imaxFNDR = NA
  
  # to get a starting value for imax
  for (i in 1:L) {
    if (priorFNR[i] > 0){imaxFNR = i}
    if (priorFPR[i] > 0){imaxFPR = i}
    if (priorError[i] > 0){imaxError = i}
    if (priorFDR[i] > 0){imaxFDR = i}
    if (priorFNDR[i] > 0){imaxFNDR = i}
  }
  
  for (i in 1:L) {
    if(is.na(imaxFNR) == TRUE){
      FNRest = NA
    } else {
      if (priorFNR[i] > 0 & RBFNR_alt[i] > RBFNR_alt[imaxFNR]){imaxFNR = i}
      FNRest = grid[imaxFNR]
    }
    if(is.na(imaxFPR) == TRUE){
      FPRest = NA
    } else {
      if (priorFPR[i] > 0 & RBFPR_alt[i] > RBFPR_alt[imaxFPR]){imaxFPR = i}
      FPRest = grid[imaxFPR]
    }
    if(is.na(imaxError) == TRUE){
      Errorest = NA
    } else {
      if (priorError[i] > 0 & RBError_alt[i] > RBError_alt[imaxError] ){imaxError = i}
      Errorest = grid[imaxError]
    }
    if(is.na(imaxFDR) == TRUE){
      FDRest = NA
    } else {
      if (priorFDR[i] > 0 & RBFDR_alt[i] > RBFDR_alt[imaxFDR]){imaxFDR = i}
      FDRest = grid[imaxFDR]
    }
    
    if(is.na(imaxFNDR) == TRUE){
      FNDRest = NA
    } else {
      if (priorFNDR[i] > 0 & RBFNDR_alt[i] > RBFNDR_alt[imaxFNDR]){imaxFNDR = i}
      FNDRest = grid[imaxFNDR]
    }

  }
  #RBFNR, RBFPR, RBError, RBFDR, RBFNDR
  newlist = list("RBFNR" = RBFNR, "RBFPR" = RBFPR, "RBError" = RBError,
                 "RBFDR" = RBFDR, "RBFNDR" = RBFNDR,
                 "FNRest" = FNRest, "FPRest" = FPRest, "Errorest" = Errorest,
                 "FDRest" = FDRest, "FNDRest" = FNDRest)
  return(newlist)
}




