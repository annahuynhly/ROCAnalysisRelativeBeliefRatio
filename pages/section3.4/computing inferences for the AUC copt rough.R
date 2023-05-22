# 1. The setup

library(rBeta2009)

source("C:\\Users\\AnnaH\\OneDrive\\Desktop\\Stats RA\\ShinyWebpage\\pages\\helper_functions.R")

# the hyperparameters for the prior on the mu's and sigma's
mu0=0
tau0=0.5
lambda1=1.787
lambda2=1.056
alpha1w=15.3589
alpha2w=22.53835
#concentration parameter for the Dirichlet process
a=20

nMonteprior = 10000 # MAY NEED TO CHANGE THIS LATER
nstar = 100 # set nstar for approximating random measure
w = 0.4
delta = 0.005
nMontepost =  10000
nstar = 100


test1 = nonpara_bayes_AUC_prior_copt(w = FALSE, 
                                   alpha1w = 15.3589, 
                                   alpha2w = 22.53835,
                                   nMonteprior = 10000, #sect3.4_copt_nMonteCarlo(), 
                                   nstar = 100, #sect3.4_copt_nstar(), 
                                   a = 20, #sect3.4_a_copt(), 
                                   delta = 0.005, #sect3.4_copt_delta(),
                                   mu0 = 0, #sect3.4_copt_mu0(), 
                                   tau0 = 0.5, # sect3.4_copt_tau0(), 
                                   lambda1 = 1.787, #sect3.4_copt_lambda1(), 
                                   lambda2 = 1.056 #sect3.4_copt_lambda2(),
)


plot(test$gridmod, test$priorcoptmoddensity,xlab="coptmod",ylab="prior density",type="l",lty=1)
plot(test$gridcopt, test$priorcoptdensity,xlab="copt",ylab="prior density",type="l",lty=1)


# Running the functions
#test = nonpara_bayes_AUC_prior_copt(w, alpha1w = NA, alpha2w = NA,
#                                        nMonteprior, nstar, a, delta,
#                                        mu0, tau0, lambda1, lambda2)



xNDdata=c(-0.11315894,  0.03273954, -0.69180664, -0.05459313, -1.22760962, -0.25705819,
          -1.55799712, -0.34339482, -1.11229004,  0.11031882,  0.37785845,  0.76029521,
          -0.34052122,  1.03882232, -0.26665494,  0.48965747,  0.80441378, -1.31205550,
          -1.09934759,  1.55522803, -0.19981736,  0.51936199,  0.95234605,  1.56027376,
          -1.42501031)
xDdata=c(0.89345810, -0.09544302,  1.52694609,  2.30531596,  0.45009081,  0.97189716,
         0.85430995,  2.40987144,  1.44936186, -0.31305846,  0.19524931,  0.75202021,
         1.63136183,  1.31617751, -0.26481975,  1.69469220,  1.67520405,  1.50587628,
         -1.18927465,  1.75076313)
nD = length(xDdata)
nND = length(xNDdata)
sD_squared = (nD - 1)*var(xDdata)
sND_squared = (nND - 1)*var(xNDdata)
meanD = mean(xDdata)
meanND = mean(xNDdata)

# this one is with raw data
test2 = nonpara_bayes_AUC_post_copt(w = 0.4, 
                                    alpha1w = NA, 
                                    alpha2w = NA,
                                    nND = NA, 
                                    nD = NA, 
                                    version = "prior",
                                    nMontepost, 
                                    nstar, 
                                    a, 
                                    delta,
                                    mu0, 
                                    tau0, 
                                    lambda1, 
                                    lambda2,
                                    sD_squared = NA, 
                                    sND_squared = NA, 
                                    meanD = NA, 
                                    meanND = NA,
                                    xDdata = xDdata, 
                                    xNDdata = xNDdata)


nonpara_bayes_AUC_rbr_error_char_copt(grid = test1$gridcopt, # usually use gridcopt
                                      priorFNR = test1$priorFNR, 
                                      priorFPR = test1$priorFPR, 
                                      priorError = test1$priorError, 
                                      priorFDR = test1$priorFDR, 
                                      priorFNDR = test1$priorFNDR, 
                                      postFNR = test2$postFNR, 
                                      postFPR = test2$postFPR, 
                                      postError = test2$postError, 
                                      postFDR = test2$postFDR, 
                                      postFNDR = test2$postFNDR)


# this one is without raw data
test2 = nonpara_bayes_AUC_post_copt(w = 0.4, 
                                    alpha1w = NA, 
                                    alpha2w = NA,
                                    nND = nND, 
                                    nD = nD, 
                                    version = "prior",
                                    nMontepost = nMontepost, 
                                    nstar = nstar, 
                                    a = a, 
                                    delta = delta,
                                    mu0 = mu0, 
                                    tau0 = tau0, 
                                    lambda1 = lambda1, 
                                    lambda2 = lambda2,
                                    sD_squared = sD_squared, 
                                    sND_squared = sND_squared, 
                                    meanD = meanD, 
                                    meanND = meanND,
                                    xDdata = NA, 
                                    xNDdata = NA)

test3 = nonpara_bayes_AUC_rbr_copt(delta = delta, 
                                   gridcopt = test1$gridcopt, 
                                   gridmod = test1$gridmod, 
                                   priorcoptdensity = test1$priorcoptdensity, 
                                   postcoptdensity = test2$postcoptdensity,
                                   priorcopt = test1$priorcopt, 
                                   postcopt = test2$postcopt,
                                   priorcoptmod = test1$priorcoptmod,
                                   postcoptmod = test2$postcoptmod)

pr_modified = test3$copt_plausible_region
pr_modified = c(pr_modified[1], pr_modified[length(pr_modified)])

nonpara_bayes_plots_AUC_copt(grid = test1$gridcopt, # used gridcopt
                             prior = test1$priorcoptdensity, 
                             post = test2$postcoptdensity,
                             rbr = FALSE,
                             plausible_region = pr_modified,
                             credible_region = FALSE,
                             rb_line = FALSE,
                             lty_type = c(2, 1, 6, 3, 2, 3),
                             colour_choice = c("blue", "red", "green",
                                               "#b3bfff", "royalblue1", "#81ddff"),
                             transparency = 0.1)

nonpara_bayes_plots_AUC_copt(grid = test1$gridcopt, # used gridcopt
                             prior = FALSE,
                             post = FALSE,
                             rbr = test3$RBcopt,
                             plausible_region = pr_modified,
                             credible_region = FALSE,
                             rb_line = FALSE,
                             lty_type = c(2, 1, 6, 3, 2, 3),
                             colour_choice = c("blue", "red", "green",
                                               "#b3bfff", "royalblue1", "#81ddff"),
                             transparency = 0.1)

pr_modified = test3$cmod_plausible_region
pr_modified = c(pr_modified[1], pr_modified[length(pr_modified)])

nonpara_bayes_plots_AUC_copt(grid = test1$gridmod, # used gridcopt
                             prior = test1$priorcoptmoddensity, 
                             post = test2$postcoptmoddensity,
                             rbr = FALSE,
                             plausible_region = pr_modified,
                             credible_region = FALSE,
                             rb_line = FALSE,
                             lty_type = c(2, 1, 6, 3, 2, 3),
                             colour_choice = c("blue", "red", "green",
                                               "#b3bfff", "royalblue1", "#81ddff"),
                             transparency = 0.1)

nonpara_bayes_plots_AUC_copt(grid = test1$gridmod, # used gridcopt
                             prior = FALSE,
                             post = FALSE,
                             rbr = test3$RBcoptmod,
                             plausible_region = pr_modified,
                             credible_region = FALSE,
                             rb_line = FALSE,
                             lty_type = c(2, 1, 6, 3, 2, 3),
                             colour_choice = c("blue", "red", "green",
                                               "#b3bfff", "royalblue1", "#81ddff"),
                             transparency = 0.1)













plot(test3$gridcopt,
     test3$postcoptdensity,xlab="copt",ylab="posterior density",type="l",lty=1)

plot(test3$gridmod,
     test3$postcoptmoddensity,xlab="coptmod",ylab="posterior density",type="l",lty=1)



test5 = nonpara_bayes_AUC_rbr_copt(test$gridcopt, test$priorcoptdensity, test3$postcoptdensity, 
                                   test$priorcopt, test3$postcopt)

# HELPER FUNCTIONS #############################################

cdf = function(x,y,z) {
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

# algorithm for generating from the xDdata discrete empirical distibution

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

# note: this function exists in the other file.
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


nonpara_bayes_AUC_prior_copt = function(w = FALSE, alpha1w = NA, alpha2w = NA,
                                        nMonteprior, nstar, a, delta,
                                        mu0, tau0, lambda1, lambda2){
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
  FNR = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  FPR = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  Error = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  FDR = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  FNDR = array(0*c(1:nMonteprior*L), dim = c(nMonteprior, L))
  for(i in 1:nMonteprior){
    FNR[i,] = cdf(gridcopt, cD[i,], pDarray[i,])
    FPR[i,] = abs(1 - cdf(gridcopt, cND[i,], pNDarray[i,]))
    #print(FNR[i,])
    #print(pre_w[i])
    Error[i,] = pre_w[i]*FNR[i,] + (1 - pre_w[i])*FPR[i,]
    for(j in 1:L){
      if((1 - pre_w[i])*FPR[i,j] + pre_w[i]*(1 - FNR[i,j]) > 10^(-15)){
        FDR[i,j] = (1 - pre_w[i])*FPR[i,j]/((1 - pre_w[i])*FPR[i,j] + pre_w[i]*(1 - FNR[i,j])) 
      }
      if((1 - pre_w[i])*FPR[i,j] + pre_w[i]*FNR[i,j] > 10^(-15)) {
        FNDR[i,j] = pre_w[i]*FNR[i,j]/((1 - pre_w[i])*FPR[i,j] + pre_w[i]*FNR[i,j])
      }
    }
  }
  
  # determine the minimizing c value of some criterion such as Error(c)
  # and when multiple c values minimize choose one most central
  copt = rep(0,nMonteprior)
  mid = L/2
  for(i in 1:nMonteprior) {
    errmin = Error[i,which.min(Error[i,])]
    hold = rep(L+1, L)
    j0 = 1
    for(j in 1:L){
      if(Error[i,j] == errmin){
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
                 "gridcopt" = gridcopt, "priorcoptdensity" = priorcoptdensity)
  return(newlist)
}

nonpara_bayes_AUC_post_copt = function(w = FALSE, alpha1w = NA, alpha2w = NA,
                                       nND = NA, nD = NA, version,
                                       nMontepost, nstar, a, delta,
                                       mu0, tau0, lambda1, lambda2,
                                       xDdata = NA, xNDdata = NA,
                                       sD_squared = NA, sND_squared = NA, meanD = NA, meanND = NA){
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
    sD_squared = (nD - 1)*var(xDdata)
    sND_squared = (nND - 1)*var(xNDdata)
    meanD = mean(xDdata)
    meanND = mean(xNDdata)
  } else if (is.na(sD_squared) == FALSE & is.na(sND_squared) == FALSE & is.na(meanD) == FALSE & is.na(meanND) == FALSE
             & is.na(xDdata) == TRUE & is.na(xNDdata) == TRUE){
    # this is the case where the data needs to be generated -- add to the other one as well
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
  FNRpost = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  FPRpost = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  Errorpost = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  FDRpost = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  FNDRpost = array(0*c(1:nMontepost*L), dim = c(nMontepost, L))
  for (i in 1:nMontepost){
    FNRpost[i,] = abs(cdf(gridcopt, cDpost[i,], pDarraypost[i,]))
    FPRpost[i,] = abs(1 - cdf(gridcopt, cNDpost[i,], pNDarraypost[i,]))
    Errorpost[i,] = wpost[i]*FNRpost[i,] + (1 - wpost[i])*FPRpost[i,]
  }
  for (j in 1:L){
    if ((1 - wpost[i])*FPRpost[i,j] + abs(wpost[i]*(1 - FNRpost[i,j])) > 0) {
      FDRpost[i,j] = (1 - wpost[i])*FPRpost[i,j]/((1 - wpost[i])*FPRpost[i,j] + wpost[i]*(1 - FNRpost[i,j])) }
    if (abs((1 - wpost[i])*(1 - FPRpost[i,j])) + wpost[i]*FNRpost[i,j] > 0) {
      FNDRpost[i,j] = wpost[i]*FNRpost[i,j]/(abs((1 - wpost[i])*(1-FPRpost[i,j])) + wpost[i]*FNRpost[i,j])}
  }
  
  # determine the minimizing c value of some criterion such as Error(c)
  # and when multiple c values minimize choose one most central
  copt = rep(0,nMontepost)
  mid = L/2
  for(i in 1:nMontepost){
    errmin = Errorpost[i,which.min(Errorpost[i,])]
    hold = rep(L+1, L)
    j0 = 1
    for(j in 1:L){
      if(Errorpost[i,j] == errmin){
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
                 "FNRpost" = FNRpost, "FPRpost" = FPRpost, 
                 "Errorpost" = Errorpost, "FDRpost" = FDRpost, 
                 "FNDRpost" = FNDRpost)
  return(newlist)
}



#-------------------------------------------------------------------------------------
#5. obtain relative belief ratio and inferences

nonpara_bayes_AUC_rbr_copt = function(gridcopt, priorcoptdensity, postcoptdensity,
                                      priorcopt, postcopt){
  # note: check to see if this is repeated so we can just re-use it instead.
  RBcopt = postcoptdensity/priorcoptdensity
  for (i in 1:L){
    if (priorcoptdensity[i] > 0){imax = i}
  }
  for (i in 1:L){
    if((priorcopt[i] > 0) & (postcopt[i] > 0)){
      if(RBcopt[i] > RBcopt[imax]){max=i}
    }
  }
  coptest = gridcopt[imax]
  postPlcopt = 0 # Posterior content of the plausible region
  for (i in 1:L) {
    if (priorcopt[i] > 0 & (postcopt[i] > 0)){
      if( RBcopt[i] > 1 ){postPlcopt = postPlcopt + postcopt[i]}
    }
  }
  
  newlist = list("RBcopt" = RBcopt, "coptest" = coptest, 
                 "postPlcopt" = postPlcopt)
  return(newlist)
}


par(mfrow=c(1,2))
plot(gridcopt,postcoptdensity,xlab="copt",ylab="prior and posterior",type="l",lty=1)
lines(gridcopt, priorcoptdensity, type="l",lty=2)


plot(test1$gridcopt,test3$RBcopt,xlab="copt",ylab=expression("RB"),type="l",lty=1)
