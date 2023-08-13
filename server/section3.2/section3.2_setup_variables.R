################################################################
# SETUP VARIABLES                                              #
################################################################

sect3.2_resample = reactive({
  if(input$finite_val_condition_resampling == 'no'){
    FALSE
  } else if (input$finite_val_condition_resampling == 'yes'){
    TRUE
  }
})

sect3.2_resample_copt = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    if(input$finite_val_condition_resampling_alt == 'no'){
      FALSE
    } else if (input$finite_val_condition_resampling_alt == 'yes'){
      TRUE
    }
  } else {
    sect3.2_resample()
  }
})

sect3.2_copt_condition = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_condition_resampling_alt
  } else {
    input$finite_val_condition_resampling
  }
})

sect3.2_copt_nMonteCarlo = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_nMonteCarlo_alt
  } else {
    input$finite_val_nMonteCarlo
  }
})

sect3.2_copt_delta = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_delta_alt
  } else {
    input$finite_val_delta
  }
})

sect3.2_copt_alpha_ND = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_alpha_ND_alt
  } else {
    input$finite_val_alpha_ND
  }
})

sect3.2_copt_alpha_D = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_alpha_D_alt
  } else {
    input$finite_val_alpha_D
  }
})

sect3.2_copt_nND = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_nND_alt
  } else {
    input$finite_val_nND
  }
})

sect3.2_copt_nD = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_nD_alt
  } else {
    input$finite_val_nD
  }
})

sect3.2_copt_fND = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_fND_alt
  } else {
    input$finite_val_fND
  }
})

sect3.2_copt_fD = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_fD_alt
  } else {
    input$finite_val_fD
  }
})

################################################################
# COMPUTATION FOR YOUDEN'S                                     #
################################################################

sect3.2_AUC_prior_copt_youden = reactive({
  set.seed(SECT3.2_SEED_COPT()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$finite_val_diag_case1 == 1){
    simulate_AUC_mc_prior(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                          resample = sect3.2_resample_copt(), #sect3.2_resample(),
                          nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                          nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                          nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo,
                          w = 0.5, 
                          alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                          alpha_D = sect3.2_copt_alpha_D()) #input$finite_val_alpha_D)
  } else if (input$finite_val_diag_case2 == "A" | input$finite_val_diag_case2 == "B"){ 
    simulate_AUC_mc_prior(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                          resample = sect3.2_resample_copt(), #sect3.2_resample(),
                          nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                          nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                          nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                          w = FALSE, 
                          alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                          alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                          alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                          alpha_D = sect3.2_copt_alpha_D())#input$finite_val_alpha_D)
  }
})

sect3.2_AUC_post_copt_youden = reactive({
  set.seed(SECT3.2_SEED_COPT()) # seeing what happens when we always set the seed...
  if(input$finite_val_diag_case1 == 1){
    simulate_AUC_mc_post(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                         resample = sect3.2_resample_copt(), #sect3.2_resample(),
                         nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                         nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                         nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                         w = 0.5, 
                         alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                         alpha_D = sect3.2_copt_alpha_D(), #input$finite_val_alpha_D, 
                         fND = sect3.2_copt_fND(), #input$finite_val_fND, 
                         fD = sect3.2_copt_fD()) #input$finite_val_fD)
  } else if (input$finite_val_diag_case2 == "A"){ # only know the prior
    simulate_AUC_mc_post(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                         resample = sect3.2_resample_copt(), #sect3.2_resample(),
                         nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                         nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                         nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                         w = FALSE, 
                         alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                         alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                         version = "prior", 
                         alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                         alpha_D = sect3.2_copt_alpha_D(), #input$finite_val_alpha_D, 
                         fND = sect3.2_copt_fND(), #input$finite_val_fND, 
                         fD = sect3.2_copt_fD()) #input$finite_val_fD)
  } else if (input$finite_val_diag_case2 == "B"){ # know both prior and posterior
    simulate_AUC_mc_post(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                         resample = sect3.2_resample_copt(), #sect3.2_resample(),
                         nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                         nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                         nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                         w = FALSE, 
                         alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                         alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                         version = "post",
                         alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                         alpha_D = sect3.2_copt_alpha_D(), #input$finite_val_alpha_D, 
                         fND = sect3.2_copt_fND(), #input$finite_val_fND, 
                         fD = sect3.2_copt_fD())#input$finite_val_fD)
  }
})

sect3.2_AUC_RBR_copt_youden = reactive({
  compute_AUC_RBR(delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                  AUC_prior = sect3.2_AUC_prior_copt_youden()$AUC, 
                  AUC_post = sect3.2_AUC_post_copt_youden()$AUC, 
                  priorc_opt = sect3.2_AUC_prior_copt_youden()$priorc_opt, 
                  postc_opt = sect3.2_AUC_post_copt_youden()$postc_opt)
})


sect3.2_copt_prior_youden = reactive({
  set.seed(SECT3.2_SEED_COPT()) # seeing what happens when we always set the seed...
  if(input$finite_val_diag_case1 == 1){
    AUC_prior_error_char_copt(c_optfDfND = sect3.2_AUC_RBR_copt_youden()$c_optfDfND, 
                              nMonteCarlo = sect3.2_copt_nMonteCarlo(),  
                              w = input$finite_val_diag_prevalence_w, 
                              delta = sect3.2_copt_delta(), 
                              pND_array = sect3.2_AUC_prior_copt_youden()$pND_array, 
                              pD_array = sect3.2_AUC_prior_copt_youden()$pD_array, 
                              FNR = sect3.2_AUC_prior_copt_youden()$FNR, 
                              FPR = sect3.2_AUC_prior_copt_youden()$FPR, 
                              ERROR_w = sect3.2_AUC_prior_copt_youden()$ERROR_w, 
                              PPV = sect3.2_AUC_prior_copt_youden()$PPV, 
                              priorc_opt = sect3.2_AUC_prior_copt_youden()$priorc_opt)
  } else if (input$finite_val_diag_case2 == "A" | input$finite_val_diag_case2 == "B"){
    AUC_prior_error_char_copt(c_optfDfND = sect3.2_AUC_RBR_copt_youden()$c_optfDfND, 
                              nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                              w = FALSE, 
                              alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                              alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence 
                              delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                              pND_array = sect3.2_AUC_prior_copt()$pND_array, 
                              pD_array = sect3.2_AUC_prior_copt()$pD_array, 
                              FNR = sect3.2_AUC_prior_copt()$FNR, 
                              FPR = sect3.2_AUC_prior_copt()$FPR, 
                              ERROR_w = sect3.2_AUC_prior_copt()$ERROR_w, 
                              PPV = sect3.2_AUC_prior_copt()$PPV, 
                              priorc_opt = sect3.2_AUC_prior_copt()$priorc_opt)
  }
})

sect3.2_copt_post_youden = reactive({
  set.seed(SECT3.2_SEED_COPT()) # seeing what happens when we always set the seed...
  if(input$finite_val_diag_case1 == 1){
    AUC_post_error_char_copt(c_optfDfND = sect3.2_AUC_RBR_copt_youden()$c_optfDfND, 
                             nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                             w = input$finite_val_diag_prevalence_w, 
                             delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                             pND_array = sect3.2_AUC_post_copt()$pND_array, 
                             pD_array = sect3.2_AUC_post_copt()$pD_array, 
                             FNR = sect3.2_AUC_post_copt()$FNR, 
                             FPR = sect3.2_AUC_post_copt()$FPR, 
                             ERROR_w = sect3.2_AUC_post_copt()$ERROR_w, 
                             PPV = sect3.2_AUC_post_copt()$PPV, 
                             postc_opt = sect3.2_AUC_post_copt()$postc_opt)
  } else if (input$finite_val_diag_case2 == "A"){ # know the prior only
    AUC_post_error_char_copt(c_optfDfND = sect3.2_AUC_RBR_copt_youden()$c_optfDfND,  
                             nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                             w = FALSE, 
                             alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                             alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                             version = "prior", 
                             delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                             pND_array = sect3.2_AUC_post_copt_youden()$pND_array, 
                             pD_array = sect3.2_AUC_post_copt_youden()$pD_array, 
                             FNR = sect3.2_AUC_post_copt_youden()$FNR,
                             FPR = sect3.2_AUC_post_copt_youden()$FPR,
                             ERROR_w = sect3.2_AUC_post_copt_youden()$ERROR_w, 
                             PPV = sect3.2_AUC_post_copt_youden()$PPV, 
                             postc_opt = sect3.2_AUC_post_copt_youden()$postc_opt)
  } else if (input$finite_val_diag_case2 == "B"){ # know both prior and posterior
    AUC_post_error_char_copt(c_optfDfND = sect3.2_AUC_RBR_copt_youden()$c_optfDfND,  
                             nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                             w = FALSE, 
                             alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                             alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                             nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                             nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                             version = "post", 
                             delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                             pND_array = sect3.2_AUC_post_copt_youden()$pND_array, 
                             pD_array = sect3.2_AUC_post_copt_youden()$pD_array, 
                             FNR = sect3.2_AUC_post_copt_youden()$FNR, 
                             FPR = sect3.2_AUC_post_copt_youden()$FPR, 
                             ERROR_w = sect3.2_AUC_post_copt_youden()$ERROR_w, 
                             PPV = sect3.2_AUC_post_copt_youden()$PPV, 
                             postc_opt = sect3.2_AUC_post_copt_youden()$postc_opt)
  }
})

sect3.2_copt_est_youden = reactive({
  compute_AUC_error_char_copt(delta = sect3.2_copt_delta(), 
                              c_optfDfND = sect3.2_AUC_RBR_copt_youden()$c_optfDfND,
                              priorFPRc_opt = sect3.2_copt_prior_youden()$priorFPRc_opt, 
                              priorFNRc_opt = sect3.2_copt_prior_youden()$priorFNRc_opt, 
                              priorERROR_wc_opt = sect3.2_copt_prior_youden()$priorERROR_wc_opt, 
                              priorFDRc_opt = sect3.2_copt_prior_youden()$priorFDRc_opt, 
                              priorFNDRc_opt = sect3.2_copt_prior_youden()$priorFNDRc_opt,
                              priorPPVc_opt = sect3.2_copt_prior_youden()$priorPPVc_opt,
                              postFPRc_opt = sect3.2_copt_post_youden()$postFPRc_opt, 
                              postFNRc_opt = sect3.2_copt_post_youden()$postFNRc_opt, 
                              postERROR_wc_opt = sect3.2_copt_post_youden()$postERROR_wc_opt, 
                              postFDRc_opt = sect3.2_copt_post_youden()$postFDRc_opt, 
                              postFNDRc_opt = sect3.2_copt_post_youden()$postFNDRc_opt,
                              postPPVc_opt = sect3.2_copt_post_youden()$postPPVc_opt)
})


################################################################
# COMPUTATION FOR CLOSEST TO (0, 1) INDEX                      #
################################################################

sect3.2_AUC_prior_copt_closest = reactive({
  set.seed(SECT3.2_SEED_COPT()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$finite_val_diag_case1 == 1){
    simulate_AUC_mc_prior(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                          resample = sect3.2_resample_copt(), #sect3.2_resample(),
                          nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                          nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                          nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo,
                          w = input$finite_val_diag_prevalence_w, 
                          alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                          alpha_D = sect3.2_copt_alpha_D(), #input$finite_val_alpha_D)
                          copt_method = "closest")
  } else if (input$finite_val_diag_case2 == "A" | input$finite_val_diag_case2 == "B"){ 
    simulate_AUC_mc_prior(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                          resample = sect3.2_resample_copt(), #sect3.2_resample(),
                          nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                          nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                          nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                          w = FALSE, 
                          alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                          alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                          alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                          alpha_D = sect3.2_copt_alpha_D(), #input$finite_val_alpha_D)
                          copt_method = "closest")
  }
})

sect3.2_AUC_post_copt_closest = reactive({
  set.seed(SECT3.2_SEED_COPT()) # seeing what happens when we always set the seed...
  if(input$finite_val_diag_case1 == 1){
    simulate_AUC_mc_post(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                         resample = sect3.2_resample_copt(), #sect3.2_resample(),
                         nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                         nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                         nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                         w = input$finite_val_diag_prevalence_w, 
                         alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                         alpha_D = sect3.2_copt_alpha_D(), #input$finite_val_alpha_D, 
                         fND = sect3.2_copt_fND(), #input$finite_val_fND, 
                         fD = sect3.2_copt_fD(), #input$finite_val_fD)
                         copt_method = "closest")
  } else if (input$finite_val_diag_case2 == "A"){ # only know the prior
    simulate_AUC_mc_post(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                         resample = sect3.2_resample_copt(), #sect3.2_resample(),
                         nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                         nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                         nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                         w = FALSE, 
                         alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                         alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                         version = "prior", 
                         alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                         alpha_D = sect3.2_copt_alpha_D(), #input$finite_val_alpha_D, 
                         fND = sect3.2_copt_fND(), #input$finite_val_fND, 
                         fD = sect3.2_copt_fD(), #input$finite_val_fD)
                         copt_method = "closest")
  } else if (input$finite_val_diag_case2 == "B"){ # know both prior and posterior
    simulate_AUC_mc_post(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                         resample = sect3.2_resample_copt(), #sect3.2_resample(),
                         nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                         nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                         nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                         w = FALSE, 
                         alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                         alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                         version = "post",
                         alpha_ND = sect3.2_copt_alpha_ND(), #input$finite_val_alpha_ND, 
                         alpha_D = sect3.2_copt_alpha_D(), #input$finite_val_alpha_D, 
                         fND = sect3.2_copt_fND(), #input$finite_val_fND, 
                         fD = sect3.2_copt_fD(), #input$finite_val_fD)
                         copt_method = "closest")
  }
})

sect3.2_AUC_RBR_copt_closest = reactive({
  compute_AUC_RBR(delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                  AUC_prior = sect3.2_AUC_prior_copt_closest()$AUC, 
                  AUC_post = sect3.2_AUC_post_copt_closest()$AUC, 
                  priorc_opt = sect3.2_AUC_prior_copt_closest()$priorc_opt, 
                  postc_opt = sect3.2_AUC_post_copt_closest()$postc_opt)
})