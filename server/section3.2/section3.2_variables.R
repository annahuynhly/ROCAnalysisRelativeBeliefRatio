################################################################
# VARIABLES                                                    #
################################################################

# Setting the seed
SECT3.2_SEED = reactive(input$finite_val_diag_seed)
SECT3.2_SEED_COPT = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == "no"){
    input$finite_val_diag_seed_copt
  } else {
    input$finite_val_diag_seed
  }
})

# Inferences for the AUC #######################################

sect3.2_AUC_prior = reactive({
  set.seed(SECT3.2_SEED()) 
  finite_val_prior(condition = input$finite_val_condition,
                   resample = sect3.2_resample(),
                   nMonteCarlo = input$finite_val_nMonteCarlo,
                   alpha_ND = input$finite_val_alpha_ND, 
                   alpha_D = input$finite_val_alpha_D)
})

sect3.2_AUC_post = reactive({
  set.seed(SECT3.2_SEED()) 
  finite_val_post(condition = input$finite_val_condition,
                  resample = sect3.2_resample(),
                  nMonteCarlo = input$finite_val_nMonteCarlo, 
                  alpha_ND = input$finite_val_alpha_ND, 
                  alpha_D = input$finite_val_alpha_D, 
                  fND = input$finite_val_fND, 
                  fD = input$finite_val_fD)
})

sect3.2_AUC_RBR = reactive({
  finite_val_RBR(delta = input$finite_val_delta, 
                 AUC_prior = sect3.2_AUC_prior()$AUC, 
                 AUC_post = sect3.2_AUC_post()$AUC)
})

sect3.2_pr = reactive({
  compute_AUC_plausible_region(delta = input$finite_val_delta, 
                               AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR)
})

sect3.2_AUC_post_content = reactive({
  compute_AUC_post_content(delta = input$finite_val_delta, 
                           AUC_post = sect3.2_AUC_post()$AUC, 
                           plausible_region = sect3.2_pr()$plausible_region)
})

sect3.2_cr = reactive({
  compute_AUC_credible_region(gamma = input$finite_val_gamma, 
                              grid = sect3.2_pr()$grid, 
                              density = sect3.2_pr()$density, 
                              AUC_post = sect3.2_AUC_post()$AUC, 
                              posterior_content = sect3.2_AUC_post_content(), 
                              plausible_region = sect3.2_pr()$plausible_region)
}) 

################################################################
# Inferences for the Cutoff ####################################

sect3.2_AUC_prior_copt = reactive({
  set.seed(SECT3.2_SEED_COPT()) 
  if(input$finite_val_diag_case1 == 1){
    simulate_AUC_mc_prior(condition = sect3.2_copt_condition(), #input$finite_val_condition,
                          resample = sect3.2_resample_copt(), #sect3.2_resample(),
                          nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                          nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                          nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo,
                          w = input$finite_val_diag_prevalence_w, 
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

sect3.2_AUC_post_copt = reactive({
  set.seed(SECT3.2_SEED_COPT()) 
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

sect3.2_AUC_RBR_copt = reactive({
  compute_AUC_RBR(delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                  AUC_prior = sect3.2_AUC_prior_copt()$AUC, 
                  AUC_post = sect3.2_AUC_post_copt()$AUC, 
                  priorc_opt = sect3.2_AUC_prior_copt()$priorc_opt, 
                  postc_opt = sect3.2_AUC_post_copt()$postc_opt)
})

sect3.2_copt_est_hardcode = reactive({
  if(input$finite_val_cutoff_denote_copt == 'yes'){
    input$finite_val_optimal_cutoff_copt
  } else if (input$finite_val_cutoff_denote_copt == 'no') {
    sect3.2_AUC_RBR_copt()$c_optfDfND
  } else if (input$finite_val_cutoff_denote_copt == 'youden'){
    sect3.2_AUC_RBR_copt_youden()$c_optfDfND
  } else if (input$finite_val_cutoff_denote_copt == 'distance'){
    sect3.2_AUC_RBR_copt_closest()$c_optfDfND
  }
})

sect3.2_copt_prior = reactive({
  set.seed(SECT3.2_SEED_COPT()) 
  if(input$finite_val_diag_case1 == 1){
    AUC_prior_error_char_copt(c_optfDfND = sect3.2_copt_est_hardcode(), #sect3.2_AUC_RBR()$c_optfDfND, 
                              nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                              w = input$finite_val_diag_prevalence_w, 
                              delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                              pND_array = sect3.2_AUC_prior_copt()$pND_array, 
                              pD_array = sect3.2_AUC_prior_copt()$pD_array, 
                              FNR = sect3.2_AUC_prior_copt()$FNR, 
                              FPR = sect3.2_AUC_prior_copt()$FPR, 
                              ERROR_w = sect3.2_AUC_prior_copt()$ERROR_w, 
                              PPV = sect3.2_AUC_prior_copt()$PPV, 
                              priorc_opt = sect3.2_AUC_prior_copt()$priorc_opt)
  } else if (input$finite_val_diag_case2 == "A" | input$finite_val_diag_case2 == "B"){
    AUC_prior_error_char_copt(c_optfDfND = sect3.2_copt_est_hardcode(), #sect3.2_AUC_RBR()$c_optfDfND, 
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

sect3.2_copt_post = reactive({
  set.seed(SECT3.2_SEED_COPT()) 
  if(input$finite_val_diag_case1 == 1){
    AUC_post_error_char_copt(c_optfDfND = sect3.2_copt_est_hardcode(), #sect3.2_AUC_RBR()$c_optfDfND, 
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
    AUC_post_error_char_copt(c_optfDfND = sect3.2_copt_est_hardcode(), #sect3.2_AUC_RBR()$c_optfDfND, 
                             nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                             w = FALSE, 
                             alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                             alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                             version = "prior", 
                             delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                             pND_array = sect3.2_AUC_post_copt()$pND_array, 
                             pD_array = sect3.2_AUC_post_copt()$pD_array, 
                             FNR = sect3.2_AUC_post_copt()$FNR,
                             FPR = sect3.2_AUC_post_copt()$FPR,
                             ERROR_w = sect3.2_AUC_post_copt()$ERROR_w, 
                             PPV = sect3.2_AUC_post_copt()$PPV, 
                             postc_opt = sect3.2_AUC_post_copt()$postc_opt)
  } else if (input$finite_val_diag_case2 == "B"){ # know both prior and posterior
    AUC_post_error_char_copt(c_optfDfND = sect3.2_copt_est_hardcode(), #sect3.2_AUC_RBR()$c_optfDfND, 
                             nMonteCarlo = sect3.2_copt_nMonteCarlo(), #input$finite_val_nMonteCarlo, 
                             w = FALSE, 
                             alpha1w = input$finite_val_diag_prevalence_alpha1w, # from the prevalence
                             alpha2w = input$finite_val_diag_prevalence_alpha2w, # from the prevalence
                             nD = sect3.2_copt_nD(), #input$finite_val_nD, 
                             nND = sect3.2_copt_nND(), #input$finite_val_nND, 
                             version = "post", 
                             delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                             pND_array = sect3.2_AUC_post_copt()$pND_array, 
                             pD_array = sect3.2_AUC_post_copt()$pD_array, 
                             FNR = sect3.2_AUC_post_copt()$FNR, 
                             FPR = sect3.2_AUC_post_copt()$FPR, 
                             ERROR_w = sect3.2_AUC_post_copt()$ERROR_w, 
                             PPV = sect3.2_AUC_post_copt()$PPV, 
                             postc_opt = sect3.2_AUC_post_copt()$postc_opt)
  }
})

sect3.2_copt_est = reactive({
  compute_AUC_error_char_copt(delta = sect3.2_copt_delta(), #input$finite_val_delta, 
                              c_optfDfND = sect3.2_copt_est_hardcode(), #sect3.2_AUC_RBR()$c_optfDfND, 
                              priorFPRc_opt = sect3.2_copt_prior()$priorFPRc_opt, 
                              priorFNRc_opt = sect3.2_copt_prior()$priorFNRc_opt, 
                              priorERROR_wc_opt = sect3.2_copt_prior()$priorERROR_wc_opt, 
                              priorFDRc_opt = sect3.2_copt_prior()$priorFDRc_opt, 
                              priorFNDRc_opt = sect3.2_copt_prior()$priorFNDRc_opt,
                              priorPPVc_opt = sect3.2_copt_prior()$priorPPVc_opt,
                              postFPRc_opt = sect3.2_copt_post()$postFPRc_opt, 
                              postFNRc_opt = sect3.2_copt_post()$postFNRc_opt, 
                              postERROR_wc_opt = sect3.2_copt_post()$postERROR_wc_opt, 
                              postFDRc_opt = sect3.2_copt_post()$postFDRc_opt, 
                              postFNDRc_opt = sect3.2_copt_post()$postFNDRc_opt,
                              postPPVc_opt = sect3.2_copt_post()$postPPVc_opt)
})

showbarplots = reactive({
  if(input$finite_val_hist_visual == "finite_val_withbars"){
    TRUE
  }
  else if (input$finite_val_hist_visual == "finite_val_withoutbars"){
    FALSE 
  }
})

sect3.2_lineplot_area = reactive({
  priorpost = density_hist_AUC_prior_post(delta = input$finite_val_delta, 
                                          AUC_prior = sect3.2_AUC_prior()$AUC, 
                                          AUC_post = sect3.2_AUC_post()$AUC)
  as.data.frame(priorpost)
})


sect3.2_hypo_test = reactive({
  RBR_hypo = hypothesized_AUC_compute_values(hypo_AUC = 0.5, #input$finite_val_hypoAUC, 
                                             delta = input$finite_val_delta,
                                             AUC_prior = sect3.2_AUC_prior()$AUC, 
                                             AUC_post = sect3.2_AUC_post()$AUC)
  strength = compute_AUC_post_content(delta = input$finite_val_delta, 
                                      AUC_post = sect3.2_AUC_post()$AUC, 
                                      #plausible_region = c(as.numeric(input$finite_val_hypoAUC), 0.99999999))
                                      plausible_region = c(0.5, 0.99999999))
  # header names change depending on context
  
  newlist = list(RBR_header = RBR_hypo, strength_header = strength)
  names(newlist) <- c(paste("Relative Belief Ratio of Event AUC > ", 
                            #as.character(input$finite_val_hypoAUC), sep = ""), 
                            as.character(0.5), sep = ""), 
                      paste("Posterior Probability of Event AUC > ", 
                            #as.character(input$finite_val_hypoAUC), sep = ""))
                            as.character(0.5), sep = ""))
  
  return(newlist)
})


