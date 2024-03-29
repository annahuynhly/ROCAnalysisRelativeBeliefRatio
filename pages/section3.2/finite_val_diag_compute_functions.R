################################################################
# HELPER FUNCTIONS                                             #
################################################################

finite_diag_check_condition = function(pD_array, pND_array){
  # example input (based on format of data): pD_array[i, ], pND_array[i, ]
  if (length(pD_array) != length(pND_array)){
    return("Error: pD_array is not the same length as pND_array.")
  }
  condition_sum = 0
  for(i in 1:length(pD_array)){
    condition_sum = condition_sum + sum(pD_array[1:i])*pND_array[i]
  }
  if (condition_sum <= 0.5){return(TRUE)} else {return(FALSE)}
}

AUC_prior_error_char_copt = function(c_optfDfND, nMonteCarlo, w = FALSE, 
                                     alpha1w = NA, alpha2w = NA,
                                     delta, pND_array, pD_array, 
                                     FNR, FPR, ERROR_w, PPV, priorc_opt){
  
  if(length(pND_array) != length(pD_array)){
    return("Error: the length of pND_array and pD_array are different.")
  }
  A = closed_bracket_grid(delta)
  priorFPRc_opt = rep(0, (1/delta))
  priorFNRc_opt = rep(0, (1/delta))
  priorERROR_wc_opt = rep(0, (1/delta))
  priorFDRc_opt = rep(0, 1/delta)
  priorFNDRc_opt = rep(0, 1/delta)
  priorPPVc_opt = rep(0, 1/delta)
  for(i in 1:nMonteCarlo){
    # This is for the prevalence w.
    pre_w = generate_w(w, alpha1w, alpha2w, version = "prior")
    FPRc_opt = FPR[i, ][c_optfDfND]
    FNRc_opt = FNR[i, ][c_optfDfND]
    ERROR_wc_opt = ERROR_w[i, ][c_optfDfND]
    PPVc_opt = PPV[i, ][c_optfDfND]
    
    if ((pre_w*(1 - FNRc_opt) + (1 - pre_w)*FPRc_opt) !=  0){
      FDRc_opt = (1 - pre_w)*FPRc_opt/(pre_w*(1 - FNRc_opt) + (1 - pre_w)*FPRc_opt)}
    if ((pre_w*FNRc_opt + (1 - pre_w)*(1 - FPRc_opt)) != 0){
      FNDRc_opt = pre_w*FNRc_opt/(pre_w*FNRc_opt + (1 - pre_w)*(1 - FPRc_opt))}
    
    for (i in 1:length(A)) {
      if ((FPRc_opt > A[i]) & (FPRc_opt <= A[i + 1])) {priorFPRc_opt[i] = priorFPRc_opt[i] + 1}
      if ((FNRc_opt > A[i]) & (FNRc_opt <= A[i + 1])) {priorFNRc_opt[i] = priorFNRc_opt[i] + 1}
      if ((ERROR_wc_opt > A[i]) & (ERROR_wc_opt <= A[i + 1])) {priorERROR_wc_opt[i] = priorERROR_wc_opt[i] + 1}
      if ((FDRc_opt > A[i]) & (FDRc_opt <= A[i + 1])) {priorFDRc_opt[i] = priorFDRc_opt[i] + 1}
      if ((FNDRc_opt > A[i]) & (FNDRc_opt <= A[i + 1])) {priorFNDRc_opt[i] = priorFNDRc_opt[i] + 1}
      if ((PPVc_opt > A[i]) & (PPVc_opt <= A[i + 1])) {priorPPVc_opt[i] = priorPPVc_opt[i] + 1}
    }
  }
  priorFPRc_opt = priorFPRc_opt/nMonteCarlo
  priorFNRc_opt = priorFNRc_opt/nMonteCarlo
  priorERROR_wc_opt = priorERROR_wc_opt/nMonteCarlo
  priorFDRc_opt = priorFDRc_opt/nMonteCarlo
  priorFNDRc_opt = priorFNDRc_opt/nMonteCarlo
  priorPPVc_opt = priorPPVc_opt/nMonteCarlo
  newlist = list("priorFPRc_opt" = priorFPRc_opt, "priorFNRc_opt" = priorFNRc_opt,
                 "priorERROR_wc_opt" = priorERROR_wc_opt, "priorFDRc_opt" = priorFDRc_opt,
                 "priorFNDRc_opt" = priorFNDRc_opt, "priorPPVc_opt" = priorPPVc_opt)
  return(newlist)
}

AUC_post_error_char_copt = function(c_optfDfND, nMonteCarlo, w = FALSE, 
                                    alpha1w = NA, alpha2w = NA, nD = NA, nND = NA, version = NA,
                                    delta, pND_array, pD_array, 
                                    FNR, FPR, ERROR_w, PPV, postc_opt){

  if(length(pND_array) != length(pD_array)){
    return("Error: the length of pND_array and pD_array are different.")
  }
  A = closed_bracket_grid(delta)
  postFPRc_opt = rep(0, (1/delta))
  postFNRc_opt = rep(0, (1/delta))
  postERROR_wc_opt = rep(0, (1/delta))
  postFDRc_opt = rep(0, 1/delta)
  postFNDRc_opt = rep(0, 1/delta)
  postPPVc_opt = rep(0, 1/delta)
  for(i in 1:nMonteCarlo){
    # This is for the prevalence w.
    pre_w = generate_w(w, alpha1w, alpha2w, nD, nND, version)
    
    FPRc_opt = FPR[i, ][c_optfDfND]
    FNRc_opt = FNR[i, ][c_optfDfND]
    ERROR_wc_opt = ERROR_w[i, ][c_optfDfND]
    PPVc_opt = PPV[i, ][c_optfDfND]
    
    if ((pre_w*(1 - FNRc_opt) + (1 - pre_w)*FPRc_opt) !=  0){
      FDRc_opt = (1 - pre_w)*FPRc_opt/(pre_w*(1 - FNRc_opt) + (1 - pre_w)*FPRc_opt)}
    if ((pre_w*FNRc_opt + (1 - pre_w)*(1 - FPRc_opt)) != 0){
      FNDRc_opt = pre_w*FNRc_opt/(pre_w*FNRc_opt + (1 - pre_w)*(1-FPRc_opt))}
    
    for (i in 1:length(A)) {
      if ((FPRc_opt > A[i]) & (FPRc_opt <= A[i + 1])) {postFPRc_opt[i] = postFPRc_opt[i] + 1}
      if ((FNRc_opt > A[i]) & (FNRc_opt <= A[i + 1])) {postFNRc_opt[i] = postFNRc_opt[i] + 1}
      if ((ERROR_wc_opt > A[i]) & (ERROR_wc_opt <= A[i + 1])) {postERROR_wc_opt[i] = postERROR_wc_opt[i] + 1}
      if ((FDRc_opt > A[i]) & (FDRc_opt <= A[i + 1])) {postFDRc_opt[i] = postFDRc_opt[i] + 1}
      if ((FNDRc_opt > A[i]) & (FNDRc_opt <= A[i + 1])) {postFNDRc_opt[i] = postFNDRc_opt[i] + 1}
      if ((PPVc_opt > A[i]) & (PPVc_opt <= A[i + 1])) {postPPVc_opt[i] = postPPVc_opt[i] + 1}
    }
  }
  postFPRc_opt = postFPRc_opt/nMonteCarlo
  postFNRc_opt = postFNRc_opt/nMonteCarlo
  postERROR_wc_opt = postERROR_wc_opt/nMonteCarlo
  postFDRc_opt = postFDRc_opt/nMonteCarlo
  postFNDRc_opt = postFNDRc_opt/nMonteCarlo
  postPPVc_opt = postPPVc_opt/nMonteCarlo
  newlist = list("postFPRc_opt" = postFPRc_opt, "postFNRc_opt" = postFNRc_opt,
                 "postERROR_wc_opt" = postERROR_wc_opt, "postFDRc_opt" = postFDRc_opt,
                 "postFNDRc_opt" = postFNDRc_opt, "postPPVc_opt" = postPPVc_opt)
  return(newlist)
}

################################################################
# FUNCTIONS FOR COMPUTATIONS                                   #
################################################################

finite_val_prior = function(condition = "unconditional", resample = FALSE,
                            nMonteCarlo, alpha_ND, alpha_D){ 
  # This is meant to simulate the prior of the AUC.
  # Remark: this is because the input can be a string due to R shiny's inputs
  alpha_priorND = create_necessary_vector(alpha_ND)
  alpha_priorD = create_necessary_vector(alpha_D)
  
  if (length(alpha_priorND) != length(alpha_priorD)){
    return("Lengths of alpha prior ND and alpha prior D are not the same.")
  }
  m = length(alpha_priorND)
  
  pND_array = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  pD_array = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  FNR = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  
  AUC = rep(0, nMonteCarlo)
  i = 1 # changed to while loop
  num_reject = 0
  while (i < nMonteCarlo){ # changed to while loop
    pND_array[i, ] = rdirichlet(1, alpha_priorND)
    pD_array[i, ] = rdirichlet(1, alpha_priorD)
    
    if(condition == "conditional"){
      check = finite_diag_check_condition(pD_array[i, ], pND_array[i, ])
      if((check == FALSE) & (resample == TRUE)){
        next # This forces the function to re-sample instead.
      } else if ((check == FALSE) & (resample == FALSE)){
        num_reject = num_reject + 1 
      }
    }
    
    FNR[i, ] = cumsum(pD_array[i, ]) #sum(pD_prior[1:i])
    AUC[i] = sum((1 - FNR[i, ])*pND_array[i,])
    
    i = i + 1 # changed to while loop
  }
  
  newlist = list("pND_array" = pND_array, "pD_array" = pD_array,
                 "FNR" = FNR, "AUC" = AUC, "n_rejected" = num_reject)
  return(newlist)
}

finite_val_post = function(condition = "unconditional", resample = FALSE,
                           nMonteCarlo, alpha_ND, alpha_D, fND, fD){
  alpha_priorND = create_necessary_vector(alpha_ND)
  alpha_priorD = create_necessary_vector(alpha_D)
  fND = create_necessary_vector(fND)
  fD = create_necessary_vector(fD)
  
  test_valid_list = c(length(alpha_priorD), length(fND), length(fD))
  for(i in test_valid_list){
    if(i != length(alpha_priorND)){
      return("At least one of the vectors (alpha ND, alpha D, fND, or fD) 
             are not the same length.")
    }
  }
  m = length(alpha_priorND)
  
  pND_array = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  pD_array = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  FNR = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  
  AUC = rep(0, nMonteCarlo)
  i = 1 # changed to while loop 
  num_reject = 0
  while (i < nMonteCarlo){ # changed to while loop
    pND_array[i, ] = rdirichlet(1, alpha_priorND + fND)
    pD_array[i, ] = rdirichlet(1, alpha_priorD + fD)
    
    if(condition == "conditional"){
      check = finite_diag_check_condition(pD_array[i, ], pND_array[i, ])
      if(check == FALSE){
        next # This forces the function to re-sample instead.
      } else if ((check == FALSE) & (resample == FALSE)){
        num_reject = num_reject + 1 
      }
    }
    
    FNR[i, ] = cumsum(pD_array[i, ])
    AUC[i] = sum((1-FNR[i, ])*pND_array[i,])
    i = i + 1 # changed to while loop
  }
  
  newlist = list("pND_array" = pND_array, "pD_array" = pD_array,
                 "FNR" = FNR, "AUC" = AUC, "n_rejected" = num_reject)
  return(newlist)
}

simulate_AUC_mc_prior = function(condition = "unconditional", resample = FALSE,
                                 nND, nD, nMonteCarlo, w = FALSE, 
                                 alpha1w = NA, alpha2w = NA,
                                 alpha_ND, alpha_D, copt_method = "error"){ 
  if (copt_method != "error" & copt_method != "closest"){
    return("copt_method must either be 'error' (minimizing Error(c)) or 'closest' 
          (closest to (0, 1) index).")
  }
  # This is meant to simulate the prior of the AUC.
  # Remark: this is because the input can be a string due to R shiny's inputs
  alpha_priorND = create_necessary_vector(alpha_ND)
  alpha_priorD = create_necessary_vector(alpha_D)
  
  if (length(alpha_priorND) != length(alpha_priorD)){
    return("Lengths of alpha prior ND and alpha prior D are not the same.")
  }
  m = length(alpha_priorND)
  
  priorc_opt = rep(0, m) # NEW
  
  pND_array = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  pD_array = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  FNR = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  FPR = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  ERROR_w = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  PPV = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  
  AUC = rep(0, nMonteCarlo)
  i = 1 # changed to while loop
  num_reject = 0
  while (i < nMonteCarlo){ # changed to while loop
    # This is for the prevalence w.
    pre_w = generate_w(w, alpha1w, alpha2w, version = "prior")
    
    pND_array[i, ] = rdirichlet(1, alpha_priorND)
    pD_array[i, ] = rdirichlet(1, alpha_priorD)
    
    if(condition == "conditional"){
      check = finite_diag_check_condition(pD_array[i, ], pND_array[i, ])
      if((check == FALSE) & (resample == TRUE)){
        next # This forces the function to re-sample instead.
      } else if ((check == FALSE) & (resample == FALSE)){
        num_reject = num_reject + 1 
      }
    }
    
    FNR[i, ] = cumsum(pD_array[i, ]) #sum(pD_prior[1:i])
    FPR[i, ] = 1 - cumsum(pND_array[i, ])
    ERROR_w[i, ] = pre_w*FNR[i, ] + (1-pre_w)*FPR[i, ]
    PPV[i, ] = (pre_w*(1 - FNR[i, ]))/(pre_w*(1 - FNR[i, ]) + (1-pre_w)*FPR[i, ]) # TPR[i, ] = 1 - FNR[i, ]
    
    AUC[i] = sum((1 - FNR[i, ])*pND_array[i,])
    
    # update the prior distribution of c_opt
    
    if(copt_method == "error"){
      c_opt = which.min(ERROR_w[i, ])
    } else if (copt_method == "closest"){
      c_opt = which.min((FPR[i, ])^{2} + (FNR[i, ])^{2})
    }
    
    priorc_opt[c_opt] = priorc_opt[c_opt] + 1
    
    i = i + 1 # changed to while loop
  }
  priorc_opt = priorc_opt/nMonteCarlo
  
  newlist = list("pND_array" = pND_array, "pD_array" = pD_array,
                 "FNR" = FNR, "FPR" = FPR, "ERROR_w" = ERROR_w, 
                 "PPV" = PPV, "priorc_opt" = priorc_opt,
                 "AUC" = AUC, "n_rejected" = num_reject)
  return(newlist)
}

simulate_AUC_mc_post = function(condition = "unconditional", resample = FALSE,
                                nND, nD, nMonteCarlo, w = FALSE, 
                                alpha1w = NA, alpha2w = NA, version = NA,
                                alpha_ND, alpha_D, fND, fD, copt_method = "error"){
  if (copt_method != "error" & copt_method != "closest"){
    return("copt_method must either be 'error' (minimizing Error(c)) or 'closest' 
          (closest to (0, 1) index).")
  }
  alpha_priorND = create_necessary_vector(alpha_ND)
  alpha_priorD = create_necessary_vector(alpha_D)
  fND = create_necessary_vector(fND)
  fD = create_necessary_vector(fD)
  
  test_valid_list = c(length(alpha_priorD), length(fND), length(fD))
  for(i in test_valid_list){
    if(i != length(alpha_priorND)){
      return("At least one of the vectors (alpha ND, alpha D, fND, or fD) 
             are not the same length.")
    }
  }
  m = length(alpha_priorND)
  
  postc_opt = rep(0, m) 
  
  pND_array = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  pD_array = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  FNR = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  FPR = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  ERROR_w = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  PPV = array(0*c(1:nMonteCarlo*m), dim = c(nMonteCarlo, m))
  
  AUC = rep(0, nMonteCarlo)
  i = 1 # changed to while loop 
  num_reject = 0
  while (i < nMonteCarlo){ # changed to while loop
    pre_w = generate_w(w, alpha1w, alpha2w, nD, nND, version)
    
    pND_array[i, ] = rdirichlet(1, alpha_priorND + fND)
    pD_array[i, ] = rdirichlet(1, alpha_priorD + fD)
    
    if(condition == "conditional"){
      check = finite_diag_check_condition(pD_array[i, ], pND_array[i, ])
      if(check == FALSE){
        next # This forces the function to re-sample instead.
      } else if ((check == FALSE) & (resample == FALSE)){
        num_reject = num_reject + 1 
      }
    }
    
    FNR[i, ] = cumsum(pD_array[i, ])
    FPR[i, ] = 1 - cumsum(pND_array[i, ])
    ERROR_w[i, ] = pre_w*FNR[i, ] + (1-pre_w)*FPR[i, ]
    PPV[i, ] = (pre_w*(1 - FNR[i, ]))/(pre_w*(1 - FNR[i, ]) + (1 - pre_w)*FPR[i, ]) # TPR[i, ] = 1 - FNR[i, ]
    
    AUC[i] = sum((1-FNR[i, ])*pND_array[i,])
    # update the posterior distribution of c_opt
    if(copt_method == "error"){
      c_opt = which.min(ERROR_w[i, ])
    } else if (copt_method == "closest"){
      c_opt = which.min((FPR[i, ])^{2} + (FNR[i, ])^{2})
    }
    postc_opt[c_opt] = postc_opt[c_opt] + 1
    
    i = i + 1 # changed to while loop
  }
  

  postc_opt = postc_opt/nMonteCarlo
  
  newlist = list("pND_array" = pND_array, "pD_array" = pD_array,
                 "FNR" = FNR, "FPR" = FPR, "ERROR_w" = ERROR_w, 
                 "PPV" = PPV, "postc_opt" = postc_opt,
                 "AUC" = AUC, "n_rejected" = num_reject)
  return(newlist)
}

grab_AUC_densities_breaks = function(delta, AUC){
  # Essentially grabs the information of the histogram
  bins = closed_bracket_grid(delta)
  x = hist(AUC, breaks = bins, plot = FALSE)
  return(list("density" = x$density, "breaks" = x$breaks))
}

grab_AUC_RBR_densities_breaks = function(delta, AUC){
  # similar as grab_AUC_densities_breaks BUT for RBR data only.
  bins = closed_bracket_grid(delta)
  AUC[is.na(AUC)] = 0
  
  myhist = list(breaks = bins, counts = AUC, density = AUC/delta)
  class(myhist) = "histogram"
  
  return(list("density" = myhist$density, "breaks" = myhist$breaks, "counts" = myhist$counts))
}

finite_val_RBR = function(delta, AUC_prior, AUC_post){
  
  bins = closed_bracket_grid(delta)
  AUC_RBR = rep(0, length(bins))
  AUC_prior_pts = grab_AUC_densities_breaks(delta, AUC_prior)$density
  AUC_post_pts = grab_AUC_densities_breaks(delta, AUC_post)$density
  
  for(i in 1:(length(bins) - 1)){
    # if statement is to prevent division by 0
    if((AUC_prior_pts[i] > 0) == TRUE){
      AUC_RBR[i] = AUC_post_pts[i] / AUC_prior_pts[i]
    } else {
      AUC_RBR[i] = NA
    }
  }
  
  # REMARK: AUC_RBR goes from (0 to bin[1]), ..., to (bin[1/delta], 1)
  newlist = list("grid" = bins, "AUC_RBR" = AUC_RBR)
  return(newlist)
}

compute_AUC_RBR = function(delta, AUC_prior, AUC_post, priorc_opt, postc_opt){
  
  RBc_opt = postc_opt/priorc_opt
  
  bins = closed_bracket_grid(delta)
  AUC_RBR = rep(0, length(bins))
  AUC_prior_pts = grab_AUC_densities_breaks(delta, AUC_prior)$density
  AUC_post_pts = grab_AUC_densities_breaks(delta, AUC_post)$density
  
  for(i in 1:(length(bins) - 1)){
    # if statement is to prevent division by 0
    if((AUC_prior_pts[i] > 0) == TRUE){
      AUC_RBR[i] = AUC_post_pts[i] / AUC_prior_pts[i]
    } else {
      AUC_RBR[i] = NA
    }
  }
  
  pr = c() # plausible region, & obtaining the posterior content
  #post_content = 0
  for(i in 1:length(RBc_opt)){
    if(RBc_opt[i] > 1){
      pr = c(pr, i)
  #    post_content = post_content + postc_opt[i]
    }
  }
  
  c_optfDfND = which.max(RBc_opt)
  post_content = postc_opt[c_optfDfND]
  
  # REMARK: AUC_RBR goes from (0 to bin[1]), ..., to (bin[1/delta], 1)
  newlist = list("grid" = bins, "AUC_RBR" = AUC_RBR, "RBc_opt" = RBc_opt, 
                 "c_optfDfND" = c_optfDfND, "plausible_region" = pr,
                 "posterior_content" = post_content)
  return(newlist)
}

compute_AUC_plausible_region = function(delta, AUC_RBR, num_average_pts = 3){
  # Grab the initial densities and breaks
  initial_vals = grab_AUC_RBR_densities_breaks(delta, AUC_RBR)
  hist_breaks = initial_vals$breaks
  hist_counts = initial_vals$counts # NOTE: COUNTS, not DENSITY
  # Converts it to a line plot
  line_plot = convert_hist_to_density_plot(hist_counts, hist_breaks, num_average_pts)
  density = line_plot$density # note: not actually density in this case.
  grid = line_plot$grid
  
  density[is.na(density)] = 0 # change values to 0
  
  # Outputs a plausible region 
  plausible_region = c()
  for (i in 1:(length(density)-1)){
    if (density[i] > 1){
      #print(i)
      plausible_region = c(plausible_region, grid[i])
    }
  }
  plausible_region = c(plausible_region[1], plausible_region[length(plausible_region)])
  
  newlist = list("plausible_region" = plausible_region, "density" = density, "grid" = grid)
  return(newlist)
}

compute_AUC_credible_region = function(gamma, grid, density, AUC_post,
                                       posterior_content, plausible_region){
  # Note: credible region is now based on the line plot.
  
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
    } else {
      RBR_values = sort(density, decreasing = TRUE)
      RBR_values = RBR_values[RBR_values > 1] # sorting for values larger than 1
      for(i in 2:length(RBR_values)){ # doesnt start at the top as the AREA of a line is 0
        rb_line = RBR_values[i]
        credible_region = c()
        # find the region associated with it
        # WARNING: BOLD ASSUMPTION NO BREAKPOINTS/PEAKS
        for(j in 1:length(density)){
          if(density[j] > rb_line){
            credible_region = c(credible_region, grid[j])
          }
        }
        credible_region = c(min(credible_region), max(credible_region))
        
        delta = grid[3] - grid[2] # hardcoded, but very unlikely that grid values will be less than 3.
        test_area = compute_AUC_post_content(delta, AUC_post, credible_region)
        if(test_area >= gamma){
          break # This means the credible region was actually found
        }
      }
      newlist = list("credible_region" = credible_region, "rb_line" = rb_line)
      #Note: rb_line should be the upper dotted line - helps define a valid credible region
      return(newlist)
    }
  }
}

compute_AUC_error_char_copt = function(delta, c_optfDfND, priorFPRc_opt, priorFNRc_opt, 
                                       priorERROR_wc_opt, priorFDRc_opt, priorFNDRc_opt,
                                       priorPPVc_opt, postFPRc_opt, postFNRc_opt, 
                                       postERROR_wc_opt, postFDRc_opt, postFNDRc_opt,
                                       postPPVc_opt){
  grid = closed_bracket_grid(delta)
  
  RBFPRc_opt = postFPRc_opt/priorFPRc_opt
  FPRc_optfDfND = grid[which.max(RBFPRc_opt)]
  
  RBFNRc_opt = postFNRc_opt/priorFNRc_opt
  FNRc_optfDfND = grid[which.max(RBFNRc_opt)]
  
  RBERROR_wc_opt = postERROR_wc_opt/priorERROR_wc_opt
  ERROR_wc_optfDfND = grid[which.max(RBERROR_wc_opt)]
  
  RBFDRc_opt = postFDRc_opt/priorFDRc_opt
  FDRc_optfDfND = grid[which.max(RBFDRc_opt)]
  
  RBFNDRc_opt=postFNDRc_opt/priorFNDRc_opt
  FNDRc_optfDfND=grid[which.max(RBFNDRc_opt)]
  
  RBPPVc_opt = postPPVc_opt/priorPPVc_opt
  PPVc_optfDfND=grid[which.max(RBPPVc_opt)]
  
  newlist = list("FPRest" = FPRc_optfDfND, "FNRest" = FNRc_optfDfND,
                 "ERRORest" = ERROR_wc_optfDfND, "FDRest" = FDRc_optfDfND,
                 "FNDRest" = FNDRc_optfDfND, "PPVest" = PPVc_optfDfND)
  return(newlist)
}

compute_AUC_post_content = function(delta, AUC_post, plausible_region){
  # Delta doesn't exactly start at 0,
  # so the area computed here is different than what's on the paper!!!!!
  bins = closed_bracket_grid(delta)
  #bins = c(bins, 1)
  AUC_post_content = 0
  AUC_post_pts = grab_AUC_densities_breaks(delta, AUC_post)$density
  
  # Check to see if a match exists:
  if(plausible_region[1] %in% bins){
    start = match(plausible_region[1], bins)
  } else {
    for(i in 1:length(bins)){
      if(bins[i] > plausible_region[1]){
        start = i
        break
      }
    }
  }
  if(plausible_region[2] %in% bins){
    end = match(plausible_region[2], bins)
  } else {
    for(i in length(bins):1){
      if(bins[i] < plausible_region[2]){
        end = i
        break
      }
    }
  }
  for(i in start:end){
    AUC_post_content = AUC_post_content + AUC_post_pts[i] * delta
  }
  
  return(AUC_post_content)
}

grab_density_plot_area = function(grid, density){
  area = sum(diff(grid) * (head(density, -1) + tail(density, -1)))/2
  return(area)
}

hypothesized_AUC_compute_values = function(hypo_AUC, delta, AUC_prior, AUC_post){
  priors = c(0, grab_AUC_densities_breaks(delta, AUC_prior)$density*delta)
  posts = c(0, grab_AUC_densities_breaks(delta, AUC_post)$density*delta)
  
  grid = closed_bracket_grid(delta)
  for(i in 1:length(grid)){
    if(grid[i] >= hypo_AUC){
      start_loop = i
      break
    }
  }
  
  priorprob = 0
  postprob = 0
  for(i in start_loop:length(grid)){
    priorprob=priorprob + priors[i]
    postprob=postprob + posts[i]
  }
  RB=postprob/priorprob
  
  return(RB)
}


