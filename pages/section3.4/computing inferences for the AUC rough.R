library(rBeta2009)
# 1. The setup

source("C:\\Users\\AnnaH\\OneDrive\\Desktop\\Stats RA\\ShinyWebpage\\pages\\helper_functions.R")

nonpara_bayes_AUC_prior = function(condition, nMonteprior, nstar, a, delta,
                                   mu0, tau0, lambda1, lambda2){
  
  A = closed_bracket_grid(delta)# this is technically their grid
  L = (1/delta) # length
  if(condition == "conditional"){
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
  if(condition == "conditional"){
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
  } else if (condition == "unconditional"){
    for (i in 1:nMonteprior) {
      for (igrid in 1:L){
        if ((A[igrid] < as.numeric(AUC[i])) & (as.numeric(AUC[i]) <= A[igrid + 1])) {
          priorAUC[igrid] = priorAUC[igrid] + 1 
        }
      }
    }
  }
  
  if(condition == "conditional"){
    priorAUC = priorAUC/AUCsum # this is for conditional
  } else if (condition == "unconditional"){
    priorAUC = priorAUC/nMonteprior
  }
  priorAUCdensity = L*priorAUC
  probAUCprior = probAUCprior/nMonteprior
  
  newlist = list("priorAUC" = priorAUC, "priorAUCdensity" = priorAUCdensity,
                 "probAUCprior" = probAUCprior, "pDarray" = pDarray,
                 "pNDarray" = pNDarray)
  return(newlist)
}

# algorithms for generating from the two empirical distibutions
genFDemp = function(x, nD, xDdata){
  xDunique = sort(unique(xDdata))
  arg = nD*x
  ihold = nD
  for (i in 1:nD){
    if (((i-1) < arg ) & (arg <= i)){
      ihold=i} 
  }
  return(xDunique[ihold])
}

genFNDemp = function(x, nND, xNDdata){
  xNDunique = sort(unique(xNDdata))
  arg = nND*x
  ihold = nND
  for (i in 1:nND){
    if (((i-1) < arg ) & (arg <= i)){ihold = i} 
  }
  return(xNDunique[ihold])
}

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
                                                nD, nND, sD2, sND2, xD, xND){
  # the values of the hyperparameters for the posterior based on the prior and the data 
  lambda1Dpost = lambda1 + nD/2
  lambda1NDpost = lambda1 + nND/2
  tau0D = 1/sqrt(nD + 1/tau0^2)
  tau0ND = 1/sqrt(nND + 1/tau0^2)
  lambda2Dpost = lambda2 + sD2/2 + (tau0D**2)*(nD/tau0^2)*(xD - mu0)^2/2
  lambda2NDpost = lambda2 + sND2/2 + (tau0ND**2)*(nND/tau0^2)*(xND - mu0)^2/2
  mu0Dpost = (tau0D**2)*(nD*xD + mu0/tau0**2)
  mu0NDpost = (tau0ND**2)*(nND*xND + mu0/tau0**2)
  
  newlist = list("lambda1Dpost" = lambda1Dpost, "lambda1NDpost" = lambda1NDpost,
                 "tau0D" = tau0D, "tau0ND" = tau0ND, "lambda2Dpost" = lambda2Dpost,
                 "lambda2NDpost" = lambda2NDpost, "mu0Dpost" = mu0Dpost,
                 "mu0NDpost" = mu0NDpost)
}

# note: may want to add support such as...
# if nD, nND, sD2, sND2 are unknown -> compute from distribution?
# also don't forget to add support for whether xDdata is known or needs to be
# generated.

nonpara_bayes_AUC_post = function(condition, nMontepost, nstar, a, delta,
                                  mu0, tau0, lambda1, lambda2, 
                                  nD, nND, sD2, sND2, xD, xND,
                                  xDdata, xNDdata){
  L = 1/delta
  A = closed_bracket_grid(delta)# this is technically their grid
  
  if(condition == "conditional"){
    A = 1/2+A/2 # the new grid when conditioning H_0: AUC>=1/2 
  }
  
  AUC = rep(0,nMontepost)
  postAUC = rep(0, L)
  probAUCpost = 0
  hy = nonpara_bayes_compute_post_hyperpara(mu0, tau0, lambda1, lambda2, 
                                            nD, nND, sD2, sND2, xD, xND)
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
  #wpost=rbeta(nMontepost,a1post,a2post)
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
  if(condition == "conditional"){
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
  } else if (condition == "unconditional"){
    for (i in 1:nMontepost) {
      for (igrid in 1:L){
        if ((A[igrid] < as.numeric(AUC[i])) & (as.numeric(AUC[i]) <= A[igrid+1])){
          postAUC[igrid] = postAUC[igrid] + 1 
        }
      }
    }
  }
  
  if(condition == "conditional"){
    postAUC = postAUC/AUCsum # this is for conditional
  } else if (condition == "unconditional"){
    postAUC = postAUC/nMontepost
  }
  postAUCdensity = L*postAUC
  probAUCpost = probAUCpost/nMontepost
  
  newlist = list("postAUC" = postAUC, "postAUCdensity" = postAUCdensity,
                 "probAUCpost" = probAUCpost)
  return(newlist)
}



#-------------------------------------------------------------------------------------
#5. obtain relative belief ratio and inferences

# the hyperparameters for the prior on the mu's and sigma's
mu0=0
tau0=0.5
lambda1=1.787
lambda2=1.056
a1=15.3589
a2=22.53835
#concentration parameter for the Dirichlet process
a=20

# the data 
nND = 25
xND = -0.072
sND2 = 19.638
z = rnorm(nND,0,1)
xNDdata = xND+sqrt(sND2/(nND-1))*(z-mean(z))/sqrt(var(z))

nD=20
xD=0.976
sD2=16.778
z=rnorm(nD,0,1)
xDdata=xD+sqrt(sD2/(nD-1))*(z-mean(z))/sqrt(var(z)) # either let people enter their own

nMontepost= 50000
delta = 0.005
nMonteprior = 50000
nstar=200

test1 = nonpara_bayes_AUC_prior("unconditional", 
                                nMonteprior, nstar, a, delta, mu0, tau0, lambda1, lambda2)


grid = open_bracket_grid(delta)

plot(grid, test1$priorAUCdensity, xlab="AUC",ylab="prior",type="l",lty=1)


test2 = nonpara_bayes_AUC_post("unconditional", nMontepost, nstar, a, delta,
                                    mu0, tau0, lambda1, lambda2, 
                                    nD, nND, sD2, sND2, xD, xND,
                                    xDdata)

#plot(grid, test4$postAUCdensity,xlab="AUC",ylab="prior and posterior",type="l",lty=1)


nonpara_bayes_AUC_rbr = function(delta, probAUCprior, probAUCpost,
                                 priorAUC, postAUC){
  grid = open_bracket_grid(delta)
  RBprobAUC=probAUCpost/probAUCprior
  L = 1/delta
  # RBprobAUC is rel. belief ratio of AUC>1/2
  # strength of evidence: probAUCpost

  RB_AUC = postAUC/priorAUC
  AUCest = grid[which.max(RB_AUC)]
  postPl_AUC = 0 # posterior content of the plausible region
  for (i in 1:L){
    if(priorAUC[i] > 0 & RB_AUC[i] > 1 ){postPl_AUC = postPl_AUC + postAUC[i]}
  }
  
  newlist = list("RBprobAUC" = RBprobAUC, "postPl_AUC" = postPl_AUC,
                 "RB_AUC" = RB_AUC, "AUCest" = AUCest)
  return(newlist)
}


test3 = nonpara_bayes_AUC_rbr(delta, test1$probAUCprior, test2$probAUCpost,
                      test1$priorAUC, test2$postAUC)


par(mfrow=c(1,2))
plot(grid,test2$postAUCdensity,xlab="AUC",ylab="prior and posterior",type="l",lty=1)
lines(grid, test1$priorAUCdensity, type="l",lty=2)

# remove NA
test3$RB_AUC[is.na(test3$RB_AUC)] = 0
grid = open_bracket_grid(delta)
plot(grid, test3$RB_AUC,xlab="AUC",ylab=expression("RB"),type="l",lty=1)


