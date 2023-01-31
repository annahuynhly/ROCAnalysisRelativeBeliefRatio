# This represents all helper functions!

library(stringr)
library(varhandle)

# Helper Functions for: realdataROC_1_page.R

# NEED TO IMPROVE THE FOLLOWING FUNCTION
convert_char_to_vector = function(x){
  if (is.character(x) == FALSE){
    return("Invalid input: the input is not a character.")
  }
  # This function turns characters, such as "1, 2, 3", 
  # into a vector: c(1, 2, 3)
  x = str_replace_all(x, fixed(" "), "") # removes all spaces
  x = (strsplit(x, ",")[[1]])
  
  check_numeric_count = 0
  for(i in 1:length(x)){
    if(check.numeric(x[i])){
      check_numeric_count = check_numeric_count + 1
    }
  }
  if (check_numeric_count == length(x)){
    x = as.numeric(x) # converts to vector
    #x = as.integer(strsplit(x, ",")[[1]]) # converts to vector
    x = x[!is.na(x)]
    return(as.double(x))
  } else {
    return("Invalid vector. Not all numbers are numeric.")
  }
}

# NOTE: CHANGED FUNCTION NAME
valid_numeric_vector = function(x){
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

create_necessary_vector = function(x){
  # Given a string of values, such as "1, 1, 1, 1, 1", converts it to a vector if
  # it isn't already numeric.
  if(is.character(x) == TRUE){
    return(convert_char_to_vector(x))
  } else if (typeof(x) == "double" | valid_numeric_vector(x) == TRUE){
    return(x)
  } else {
    return("Invalid vector.")
  }
}


# Helper functions for realdataROC_placeholder_2 -> found in realdataROC

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

##############################################
# Helper functions for ROC_page_R

generate_A_and_grid = function(L){
  # L= number of subintervals of [0,1] for estimating densities of continuous quantities 
  A = rep(0,L+1)
  for (i in 1:(L+1)) {
    A[i] = (i-1)/L
  }
  grid = rep(0,L)
  for (i in 1:L) {
    grid[i] = (i-1/2)/L
  }
  newlist = list("A" = A, "grid" = grid)
  return(newlist)
}

##############################################
# The following are specifically for ROC_compute_some_outputs_2







