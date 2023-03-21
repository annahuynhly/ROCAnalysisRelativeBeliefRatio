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
  
  pre_w = generate_w(w, alpha1w, alpha2w, version = "prior")
    
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
  
  pre_w = generate_w(w, alpha1w, alpha2w, nD, nND, version) # ADDED FOR COPT
  
  postimpwt = pnorm((muDpost - mu0NDpost)/(tau0ND*sigmaNDpost)) # ADDED FOR COPT
  U = rbeta(nMontepost,1,1) # ADDED FOR COPT
  muNDpost_copt = mu0NDpost + tau0ND*sigmaNDpost*qnorm(postimpwt*U) # ADDED FOR COPT
  
  c = 0.5*(muDpost+muNDpost_copt)+(sigmaDpost**2)*(log((1-pre_w)/pre_w))/(muDpost-muNDpost_copt) # ADDED FOR COPT
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
  # Note: rel. belief ratio of AUC>1/2 = RBprobAUC
  # Strength of evidence = probAUCpost
  RBprobAUC=probAUCpost/probAUCprior
  
  RBcmod = postcmod/priorcmod # ADDED FOR COPT
  
  RB_AUC = rep(0, length(priorAUC)) # Assuming priorAUC and postAUC are of same length
  for (i in 1:length(RB_AUC)){
    if (priorAUC[i] != 0){
      RB_AUC[i] = postAUC[i]/priorAUC[i]
    } else {
      RB_AUC[i] = NA
    }
  }

  AUCest=grid[which.max(RB_AUC)]
  
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
  
  postPl_AUC=0
  for (i in 1:L) {
    if (priorAUC[i] > 0 & RB_AUC[i] > 1 ) { postPl_AUC = postPl_AUC+postAUC[i]}
  }
  
  newlist = list("RB_AUC" = RB_AUC, "RBprobAUC" = RBprobAUC,
                 "AUCest" = AUCest, "postPl_AUC" = postPl_AUC,
                 "plausible_region" = plausible_region, "RBcmod" = RBcmod)
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
#delta = 0.01

#post_hyperpara = binormal_compute_post_hyperpara(mu0, tau0, lambda1, lambda2, nND, meanND, 
#                                                 sND_squared, nD, meanD, sD_squared)

#post_hyperpara

#prior_val = binormal_diag_prior(nMonteprior, delta, lambda1, lambda2, mu0, tau0)

#post_val = binormal_diag_post(nMontepost, delta, post_hyperpara$lambda1post, 
#                                  post_hyperpara$lambda2post, post_hyperpara$mu0Dpost, 
#                                  post_hyperpara$mu0NDpost, post_hyperpara$tau0D, 
#                                  post_hyperpara$tau0ND)

#rbr_val = binormal_diag_RBR(delta, prior_val$probAUCprior, post_val$probAUCpost,
#                                prior_val$priorAUC, post_val$postAUC)
#par(mfrow=c(1,2))

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


