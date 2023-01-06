
#NOTE: this is binormalcoptequalvariance.txt
compute_hyperparameters = function(mu0, tau0, alpha0, beta0, a1, a2,
                                   nND, xND, sND2,
                                   nD, xD, sD2){
  # the values of the hyperparameters for the posterior based on the prior and the data 
  alpha0post = alpha0+(nD+nND)/2
  alpha0Dpost = alpha0+nD/2
  alpha0NDpost = alpha0+nND/2
  tau0D = 1/sqrt(nD+1/tau0^2)
  tau0ND = 1/sqrt(nND+1/tau0^2)
  beta0post = beta0+(sD2+sND2)/2+(tau0D**2)*(nD/tau0^2)*(xD-mu0)^2/2+(tau0ND**2)*(nND/tau0^2)*(xND-mu0)^2/2
  beta0Dpost = beta0+sD2/2+(tau0D**2)*(nD/tau0^2)*(xD-mu0)^2/2
  beta0NDpost = beta0+sND2/2+(tau0ND**2)*(nND/tau0^2)*(xND-mu0)^2/2
  mu0Dpost = (tau0D**2)*(nD*xD+mu0/tau0**2)
  mu0NDpost = (tau0ND**2)*(nND*xND+mu0/tau0**2)
  a1post = a1+nD
  a2post = a2+nND
  newlist = list("alpha0post" = alpha0post, "beta0post" = beta0post,"alpha0Dpost" = alpha0Dpost, "alpha0NDpost" = alpha0NDpost,
                 "tau0D" = tau0D, "tau0ND" = tau0ND, "beta0Dpost" = beta0Dpost,
                 "beta0NDpost" = beta0NDpost, "mu0Dpost" = mu0Dpost,
                 "mu0NDpost" = mu0NDpost, "a1post" = a1post, "a2post" = a2post)
  return(newlist)
}

fcnAUC <- function(z){
  return (dnorm(z)*pnorm((muDi-muNDi)/sigmaDi+sigmaNDi/sigmaDi*z))
}