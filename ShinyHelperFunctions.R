# This represents all helper functions!

library(stringr)

# Helper Functions for: realdataROC_1_page.R

# NEED TO IMPROVE THE FOLLOWING FUNCTION
convert_char_to_vector = function(x){
  # This function turns characters, such as "1, 2, 3", 
  # into a vector: c(1, 2, 3)
  x = str_replace_all(x, fixed(" "), "") # removes all spaces
  x = as.numeric(strsplit(x, ",")[[1]]) # converts to vector
  #x = as.integer(strsplit(x, ",")[[1]]) # converts to vector
  x = x[!is.na(x)]
  return(as.double(x))
}

valid_vector = function(x){
  # This function double checks to insure that the vector
  # is valid to use for any weird edge-cases that the player
  # might try to initiate.
  for (i in 1:length(x)) {
    if (is.numeric(x[[i]]) == FALSE){
      # This input contains invalid characters
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

# Helper functions for realdataROC_placeholder_2

# Below is supposed to be a helper function to make the code look neater long-term
prior_monte_carlo = function(m, ngrid, results, nMonteprior, p){
  grid = (c(1:(ngrid+1))-1)/ngrid
  FNc_optprior = rep(0,ngrid)
  FPc_optprior = rep(0,ngrid)
  ERRORc_optprior = rep(0,ngrid)
  for (iMonteprior in 1:nMonteprior) {
    #generate dirichlet values for pND and pD
    pND_prior = rdirichlet(1,rep(1,m))
    pD_prior = rdirichlet(1,rep(1,m))
    # compute FNc_optprior, FPc_optprior and ERRORc_optprior  
    FNc_opt = sum(pD_prior[1:results$c_optfDfND])
    FPc_opt = 1-sum(pND_prior[1:results$c_optfDfND])
    ERRORc_opt = p*FNc_opt +  (1-p)*FPc_opt
    for (i in 1:(ngrid)) {
      if ((FNc_opt > grid[i]) & (FNc_opt <= grid[i+1])) {FNc_optprior[i]=FNc_optprior[i]+1}
      if ((FPc_opt > grid[i]) & (FPc_opt <= grid[i+1])) {FPc_optprior[i]=FPc_optprior[i]+1}
      if ((ERRORc_opt > grid[i]) & (ERRORc_opt <= grid[i+1])) {ERRORc_optprior[i]=ERRORc_optprior[i]+1}
    }
  }
  FNc_optprior = FNc_optprior/nMonteprior
  FPc_optprior = FPc_optprior/nMonteprior
  ERRORc_optprior = ERRORc_optprior/nMonteprior
  newlist = list("FNc_optprior" = FNc_optprior, "FPc_optprior" = FPc_optprior,
                 "ERRORc_optprior" = ERRORc_optprior)
  return(newlist)
}

# for the posterior
post_monte_carlo = function(m, ngrid, results, nMontepost, fND, fD, p){
  grid = (c(1:(ngrid+1))-1)/ngrid
  FNc_optpost <- rep(0,ngrid)
  FPc_optpost <- rep(0,ngrid)
  ERRORc_optpost <- rep(0,ngrid)
  for (iMontepost in 1:nMontepost) {
    #generate dirichlet values for pND and pD
    pND_post <- rdirichlet(1,rep(1,m)+fND)
    pD_post <- rdirichlet(1,rep(1,m)+fD)
    # compute FN_post, FP_post and ERROR_post
    FNc_opt <- sum(pD_post[1:results$c_optfDfND])
    FPc_opt <- 1-sum(pND_post[1:results$c_optfDfND])
    ERRORc_opt <- p*FNc_opt +  (1-p)*FPc_opt
    for (i in 1:(ngrid)) {
      if ((FNc_opt > grid[i]) & (FNc_opt <= grid[i+1])) {FNc_optpost[i]  <-  FNc_optpost[i]+1}
      if ((FPc_opt > grid[i]) & (FPc_opt <= grid[i+1])) {FPc_optpost[i]  <-  FPc_optpost[i]+1}
      if ((ERRORc_opt > grid[i]) & (ERRORc_opt <= grid[i+1])) {ERRORc_optpost[i]  <-  ERRORc_optpost[i]+1}
    }
  }
  FNc_optpost = FNc_optpost/nMontepost
  FPc_optpost = FPc_optpost/nMontepost
  ERRORc_optpost = ERRORc_optpost/nMontepost
  newlist = list("FNc_optpost" = FNc_optpost, "FPc_optpost" = FPc_optpost,
                 "ERRORc_optpost" = ERRORc_optpost)
  return(newlist)
}





