library(rBeta2009)
source("C:\\Users\\AnnaH\\OneDrive\\Desktop\\Stats RA\\ShinyWebpage\\pages\\helper_functions.R")

# HELPER FUNCTIONS

# algorithms for generating from the two empirical distibutions
genFDemp = function(x, nD, xDdata){
  # note: SHOULD NOT NEED ANYMORE.
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
  # note: SHOULD NOT NEED ANYMORE.
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


nonpara_bayes_AUC_post = function(condition, nMontepost, nstar, a, delta,
                                  mu0, tau0, lambda1, lambda2, xDdata = NA, xNDdata = NA,
                                  nD, nND, sD_squared = NA, sND_squared = NA, 
                                  meanD = NA, meanND = NA){
  L = 1/delta
  A = closed_bracket_grid(delta)# this is technically their grid
  
  # The user can either put in actual numbers or put in the actual data 
  # - need to check that the same is for the other function
  if(is.na(sD_squared) == TRUE || is.na(sND_squared) == TRUE || is.na(meanD) == TRUE || is.na(meanND) == TRUE){
    sD_squared = (nD - 1)*var(xDdata)
    sND_squared = (nND - 1)*var(xNDdata)
    meanD = mean(xDdata)
    meanND = mean(xNDdata)
  } else if (is.na(sD_squared) == FALSE & is.na(sND_squared) == FALSE & 
             is.na(sND_squared) == FALSE & is.na(meanD) == FALSE){
    if (length(xDdata) == 1 & length(xNDdata) == 1){
      # this is the case where the data needs to be generated -- add to the other one as well
      z = rnorm(nND, 0, 1)
      xNDdata = meanND + sqrt(sND_squared/(nND - 1))*(z - mean(z))/sqrt(var(z))
      z = rnorm(nD, 0, 1)
      xDdata = meanD + sqrt(sD_squared/(nD - 1))*(z - mean(z))/sqrt(var(z)) 
    }
  } else if (is.na(sD_squared) == FALSE & is.na(sND_squared) == FALSE & is.na(meanD) == FALSE & is.na(meanND) == FALSE
             & is.na(xDdata) == FALSE & is.na(xNDdata) == FALSE){
    return("There is no data. Either put in the descriptive statistics (nD, nND, sD_squared, sND_squared),
           or put in the data (xDdata, xNDdata.)")
  }
  
  if(condition == "conditional"){
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


# the hyperparameters for the prior on the mu's and sigma's
mu0=0
tau0=0.5
lambda1=1.787
lambda2=1.056
alpha1w=15.3589
alpha2w=22.53835
#concentration parameter for the Dirichlet process
a=20
nMontepost = 10000 # MAY NEED TO CHANGE THIS LATER
nMonteprior = 10000
nstar = 100 # set nstar for approximating random measure

nND=25
xND=-0.072
sND2=19.638
z=rnorm(nND,0,1)
xNDdata=xND+sqrt(sND2/(nND-1))*(z-mean(z))/sqrt(var(z))
nD=20
xD=0.976
sD2=16.778
z=rnorm(nD,0,1)
xDdata=xD+sqrt(sD2/(nD-1))*(z-mean(z))/sqrt(var(z))

delta = 0.005

test1 = nonpara_bayes_AUC_prior(condition = "conditional", 
                                nMonteprior, 
                                nstar, 
                                a, 
                                delta,
                                mu0, 
                                tau0, 
                                lambda1, 
                                lambda2)

test2 = nonpara_bayes_AUC_post("conditional", 
                               nMontepost = 10000,
                               nstar, #100
                               a, #20
                               delta, #0.005
                               mu0, #0
                               tau0, #0.5 
                               lambda1, #1.787
                               lambda2, #1.056
                               xDdata = NA, 
                               xNDdata = NA,
                               nD, #20
                               nND, #25
                               sD_squared = sD2, #16.778
                               sND_squared = sND2, #19.638
                               meanD = xD, #xD=0.976
                               meanND = xND) # -0.072

test3 = nonpara_bayes_AUC_rbr(delta, test1$probAUCprior, test2$probAUCpost,
                              test1$priorAUC, test2$postAUC)


nonpara_bayes_rbr_graph(delta,
                        test3$RB_AUC, 
                        c(test3$plausible_region[1], test3$plausible_region[length(test3$plausible_region)]),
                        transparency = 0.3)

nonpara_bayes_prior_post_graph(condition = "unconditional",
                               delta = 0.005, 
                               prior = test1$priorAUCdensity, 
                               post = test2$postAUCdensity, 
                               plausible_region = c(test3$plausible_region[1], test3$plausible_region[length(test3$plausible_region)]),
                               credible_region = NA, #nonpara_bayes_cr_AUC(),
                               transparency = 0.3)








set.seed(1)
test1 = nonpara_bayes_AUC_prior(condition = "conditional",   #input$binormal_diag_condition, 
                        nMonteprior = 1000, #input$nonpara_bayes_nMonteCarlo, 
                        nstar = 100, #input$nonpara_bayes_nstar, 
                        a = 20, #sect3.4_a(), 
                        delta = 0.005, #input$nonpara_bayes_delta,
                        mu0 = 0, #input$nonpara_bayes_mu0, 
                        tau0 = 0.5, #input$nonpara_bayes_tau0, 
                        lambda1 = 1.787, #input$nonpara_bayes_lambda1, 
                        lambda2 = 1.056 #input$nonpara_bayes_lambda2)
                        )



xNDdata=c(-0.11315894,  0.03273954, -0.69180664, -0.05459313, -1.22760962, -0.25705819,
          -1.55799712, -0.34339482, -1.11229004,  0.11031882,  0.37785845,  0.76029521,
          -0.34052122,  1.03882232, -0.26665494,  0.48965747,  0.80441378, -1.31205550,
          -1.09934759,  1.55522803, -0.19981736,  0.51936199,  0.95234605,  1.56027376,
          -1.42501031)

xDdata=c(0.89345810, -0.09544302,  1.52694609,  2.30531596,  0.45009081,  0.97189716,
         0.85430995,  2.40987144,  1.44936186, -0.31305846,  0.19524931,  0.75202021,
         1.63136183,  1.31617751, -0.26481975,  1.69469220,  1.67520405,  1.50587628,
         -1.18927465,  1.75076313)

test2 = nonpara_bayes_AUC_post(condition = "conditional", #input$binormal_diag_condition, 
                       nMontepost = 10000, #input$nonpara_bayes_nMonteCarlo, 
                       nstar = 1000, #input$nonpara_bayes_nstar, 
                       a = 20, #sect3.4_a(), 
                       delta = 0.005, #input$nonpara_bayes_delta,
                       mu0 = 0, #input$nonpara_bayes_mu0, 
                       tau0 = 0.5, #input$nonpara_bayes_tau0, 
                       lambda1 = 1.787, #input$nonpara_bayes_lambda1, 
                       lambda2 = 1.056, #input$nonpara_bayes_lambda2, 
                       xDdata = xDdata, 
                       xNDdata = xNDdata,
                       nD = NA, #input$nonpara_bayes_nD, 
                       nND = NA, #input$nonpara_bayes_nND, 
                       sD_squared = NA, #input$nonpara_bayes_sD_squared, 
                       sND_squared = NA, #input$nonpara_bayes_sND_squared, 
                       meanD = NA, #input$nonpara_bayes_meanD, 
                       meanND = NA #input$nonpara_bayes_meanND)
)

delta = 0.005
grid = open_bracket_grid(delta)
plot(grid, test2$postAUCdensity, xlab="AUC",ylab="prior and posterior",type="l",lty=1)


test3 = nonpara_bayes_AUC_rbr(delta, test1$probAUCprior, test2$probAUCpost,
                              test1$priorAUC, test2$postAUC)











#plot(grid, test1$priorAUCdensity, xlab="AUC",ylab="prior",type="l",lty=1)

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



#plot(grid,test2$postAUCdensity,xlab="AUC",ylab="prior and posterior",type="l",lty=1)

# remove NA
#test3$RB_AUC[is.na(test3$RB_AUC)] = 0
#grid = open_bracket_grid(delta)
#plot(grid, test3$RB_AUC,xlab="AUC",ylab=expression("RB"),type="l",lty=1)


