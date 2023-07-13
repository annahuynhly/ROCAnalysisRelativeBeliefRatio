################################################################
# VARIABLES                                                    #
################################################################

# Setting the seeds
SECT3.3_SEED = reactive(input$binormal_diag_seed)
SECT3.3_SEED_COPT = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == "yes"){
    input$binormal_diag_seed
  } else if(input$binormal_optimal_cutoff_denote_variables == "no"){
    input$binormal_diag_seed_copt
  }
})

# This is for the equal variances case
sect3.3_hyperpara = reactive({
  binormal_compute_post_hyperpara(mu0 = input$binormal_diag_mu0, 
                                  tau0 = input$binormal_diag_tau0, 
                                  lambda1 = input$binormal_diag_lambda1, 
                                  lambda2 = input$binormal_diag_lambda2, 
                                  nND = sect3.3_AUC_nND(), #input$binormal_diag_nND, 
                                  meanND = sect3.3_AUC_meanND(), #input$binormal_diag_meanND, 
                                  sND_squared = sect3.3_AUC_sND_squared(), #input$binormal_diag_sND_squared, 
                                  nD = sect3.3_AUC_nD(), #input$binormal_diag_nD, 
                                  meanD = sect3.3_AUC_meanD(), #input$binormal_diag_meanD, 
                                  sD_squared = sect3.3_AUC_sD_squared() #input$binormal_diag_sD_squared)
  )
})

sect3.3_hyperpara_copt = reactive({
  binormal_compute_post_hyperpara(mu0 = sect3.3_copt_mu0(), 
                                  tau0 = sect3.3_copt_tau0(), 
                                  lambda1 = sect3.3_copt_lambda1(), 
                                  lambda2 = sect3.3_copt_lambda2(), 
                                  nND = sect3.3_copt_nND_use(), 
                                  meanND = sect3.3_copt_meanND_use(), 
                                  sND_squared = sect3.3_copt_sND_squared_use(), 
                                  nD = sect3.3_copt_nD_use(), 
                                  meanD = sect3.3_copt_meanD_use(), 
                                  sD_squared = sect3.3_copt_sD_squared_use())
})

sect3.3_AUC_prior = reactive({
  set.seed(SECT3.3_SEED()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  binormal_diag_prior(condition = sect3.3_condition(),
                      nMonteprior = input$binormal_diag_nMonteCarlo, 
                      delta = input$binormal_diag_delta, 
                      lambda1 = input$binormal_diag_lambda1, 
                      lambda2 = input$binormal_diag_lambda2, 
                      mu0 = input$binormal_diag_mu0, 
                      tau0 = input$binormal_diag_tau0)
})

sect3.3_AUC_post = reactive({
  binormal_diag_post(condition = sect3.3_condition(),
                     nMontepost = input$binormal_diag_nMonteCarlo, 
                     delta = input$binormal_diag_delta, 
                     lambda1post = sect3.3_hyperpara()$lambda1post, 
                     lambda2post = sect3.3_hyperpara()$lambda2post, 
                     mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                     mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                     tau0D = sect3.3_hyperpara()$tau0D, 
                     tau0ND = sect3.3_hyperpara()$tau0ND)
})

sect3.3_AUC_RBR = reactive({
  priorAUC_smo = average_vector_values(sect3.3_AUC_prior()$priorAUC, 
                                       input$binormal_diag_smoother)
  postAUC_smo = average_vector_values(sect3.3_AUC_post()$postAUC, 
                                      input$binormal_diag_smoother)
  if (sect3.3_condition() == "conditional"){
    binormal_diag_RBR(condition = sect3.3_condition(),
                      delta = input$binormal_diag_delta,
                      priorAUC = priorAUC_smo, #sect3.3_AUC_prior()$priorAUC,
                      postAUC = postAUC_smo) #sect3.3_AUC_post()$postAUC)
  } else if (sect3.3_condition() == "unconditional"){
    binormal_diag_RBR(condition = sect3.3_condition(),
                      delta = input$binormal_diag_delta, 
                      probAUCprior = sect3.3_AUC_prior()$probAUCprior, 
                      probAUCpost = sect3.3_AUC_post()$probAUCpost,
                      priorAUC = priorAUC_smo, #sect3.3_AUC_prior()$priorAUC, 
                      postAUC = postAUC_smo) #sect3.3_AUC_post()$postAUC)
  }
})

sect3.3_cr = reactive({
  priorAUC_smo = average_vector_values(sect3.3_AUC_prior()$priorAUC, 
                                       input$binormal_diag_smoother)
  postAUC_smo = average_vector_values(sect3.3_AUC_post()$postAUC, 
                                      input$binormal_diag_smoother)
  binormal_diag_compute_credible_region(gamma = input$binormal_diag_gamma, 
                                        delta = input$binormal_diag_delta, 
                                        AUC_RBR = sect3.3_AUC_RBR()$RB_AUC, 
                                        AUC_prior = priorAUC_smo, #sect3.3_AUC_prior()$priorAUC, 
                                        AUC_post = postAUC_smo, #sect3.3_AUC_post()$postAUC, 
                                        plausible_region = sect3.3_AUC_RBR()$plausible_region,
                                        posterior_content = sect3.3_AUC_RBR()$postPl_AUC)
})

sect3.3_AUC_prior_copt = reactive({
  set.seed(SECT3.3_SEED_COPT()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$binormal_diag_case1 == 1){
    binormal_diag_prior_copt(w = input$binormal_diag_prevalence_w,
                             alpha1w = NA, 
                             alpha2w = NA, 
                             nMonteprior = sect3.3_copt_nMonteCarlo(), 
                             delta = sect3.3_copt_delta(), 
                             lambda1 = sect3.3_copt_lambda1(), 
                             lambda2 = sect3.3_copt_lambda2(), 
                             mu0 = sect3.3_copt_mu0(), 
                             tau0 = sect3.3_copt_tau0())
  } else if (input$binormal_diag_case2 == "A" | input$binormal_diag_case2 == "B"){ 
    binormal_diag_prior_copt(w = FALSE,
                             alpha1w = input$binormal_diag_prevalence_alpha1w, 
                             alpha2w = input$binormal_diag_prevalence_alpha2w, 
                             nMonteprior = sect3.3_copt_nMonteCarlo(), 
                             delta = sect3.3_copt_delta(), 
                             lambda1 = sect3.3_copt_lambda1(), 
                             lambda2 = sect3.3_copt_lambda2(), 
                             mu0 = sect3.3_copt_mu0(), 
                             tau0 = sect3.3_copt_tau0())
  }
})

sect3.3_AUC_post_copt = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_post_copt(w = input$binormal_diag_prevalence_w,
                            alpha1w = NA, 
                            alpha2w = NA, 
                            nND = NA, 
                            nD = NA,
                            version = NA,
                            nMontepost = sect3.3_copt_nMonteCarlo(), 
                            delta = sect3.3_copt_delta(), 
                            lambda1post = sect3.3_hyperpara_copt()$lambda1post, 
                            lambda2post = sect3.3_hyperpara_copt()$lambda2post, 
                            mu0Dpost = sect3.3_hyperpara_copt()$mu0Dpost, 
                            mu0NDpost = sect3.3_hyperpara_copt()$mu0NDpost, 
                            tau0D = sect3.3_hyperpara_copt()$tau0D, 
                            tau0ND = sect3.3_hyperpara_copt()$tau0ND)
  } else if (input$binormal_diag_case2 == "A"){ 
    binormal_diag_post_copt(w = FALSE,
                            alpha1w = input$binormal_diag_prevalence_alpha1w, 
                            alpha2w = input$binormal_diag_prevalence_alpha2w,
                            nND = NA, 
                            nD = NA,
                            version = "prior",
                            nMontepost = sect3.3_copt_nMonteCarlo(), 
                            delta = sect3.3_copt_delta(), 
                            lambda1post = sect3.3_hyperpara_copt()$lambda1post, 
                            lambda2post = sect3.3_hyperpara_copt()$lambda2post, 
                            mu0Dpost = sect3.3_hyperpara_copt()$mu0Dpost, 
                            mu0NDpost = sect3.3_hyperpara_copt()$mu0NDpost, 
                            tau0D = sect3.3_hyperpara_copt()$tau0D, 
                            tau0ND = sect3.3_hyperpara_copt()$tau0ND)
  } else if (input$binormal_diag_case2 == "B"){
    binormal_diag_post_copt(w = FALSE,
                            alpha1w = input$binormal_diag_prevalence_alpha1w, 
                            alpha2w = input$binormal_diag_prevalence_alpha2w, 
                            nND = sect3.3_copt_nND_use(), 
                            nD = sect3.3_copt_nD_use(), 
                            version = "post",
                            nMontepost = sect3.3_copt_nMonteCarlo(), 
                            delta = sect3.3_copt_delta(), 
                            lambda1post = sect3.3_hyperpara_copt()$lambda1post, 
                            lambda2post = sect3.3_hyperpara_copt()$lambda2post, 
                            mu0Dpost = sect3.3_hyperpara_copt()$mu0Dpost, 
                            mu0NDpost = sect3.3_hyperpara_copt()$mu0NDpost, 
                            tau0D = sect3.3_hyperpara_copt()$tau0D, 
                            tau0ND = sect3.3_hyperpara_copt()$tau0ND)
  }
})

sect3.3_AUC_RBR_copt = reactive({
  priorcmod_smo = average_vector_values(sect3.3_AUC_prior_copt()$priorcmod, 
                                        input$binormal_diag_smoother_copt)
  postcmod_smo = average_vector_values(sect3.3_AUC_post_copt()$postcmod, 
                                       input$binormal_diag_smoother_copt)
  binormal_diag_RBR_copt(delta = sect3.3_copt_delta(), 
                         priorcmod = priorcmod_smo, #sect3.3_AUC_prior_copt()$priorcmod, 
                         postcmod = postcmod_smo) #sect3.3_AUC_post_copt()$postcmod)
})

sect3.3_cr_copt = reactive({
  priorcmod_smo = average_vector_values(sect3.3_AUC_prior_copt()$priorcmod, 
                                        input$binormal_diag_smoother_copt)
  postcmod_smo = average_vector_values(sect3.3_AUC_post_copt()$postcmod, 
                                       input$binormal_diag_smoother_copt)
  binormal_diag_compute_credible_region(gamma = input$binormal_diag_gamma_copt, 
                                        delta = input$binormal_diag_delta, 
                                        AUC_RBR = sect3.3_AUC_RBR_copt()$RBcmod, 
                                        AUC_prior = priorcmod_smo, #sect3.3_AUC_prior_copt()$priorcmod, 
                                        AUC_post = postcmod_smo, #sect3.3_AUC_post_copt()$postcmod, 
                                        plausible_region = sect3.3_AUC_RBR_copt()$plausible_region,
                                        posterior_content = sect3.3_AUC_RBR_copt()$postPlcmod)
})

sect3.3_copt_est_hardcode = reactive({
  # previously named sect3.3_copt
  if(input$binormal_optimal_cutoff_denote_copt == 'yes'){
    input$binormal_diag_optimal_cutoff_copt
  } else if (input$binormal_optimal_cutoff_denote_copt == 'no') {
    sect3.3_AUC_RBR_copt()$coptest
  } else if (input$binormal_optimal_cutoff_denote_copt == 'youden'){
    sect3.3_AUC_RBR_copt_youden()$coptest
  }
})

sect3.3_AUC_prior_error_char_copt = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_AUC_prior_error_char_copt(w = input$binormal_diag_prevalence_w,
                                            coptest = sect3.3_copt_est_hardcode(), 
                                            nMonteprior = sect3.3_copt_nMonteCarlo(), 
                                            delta = sect3.3_copt_delta(), 
                                            lambda1 = sect3.3_copt_lambda1(),
                                            lambda2 = sect3.3_copt_lambda2(), 
                                            mu0 = sect3.3_copt_mu0(),
                                            tau0 = sect3.3_copt_tau0())
  } else if (input$binormal_diag_case2 == "A" | input$binormal_diag_case2 == "B"){ 
    binormal_diag_AUC_prior_error_char_copt(w = FALSE,
                                            alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                            alpha2w = input$binormal_diag_prevalence_alpha2w,
                                            coptest = sect3.3_copt_est_hardcode(), 
                                            nMonteprior = sect3.3_copt_nMonteCarlo(), 
                                            delta = sect3.3_copt_delta(), 
                                            lambda1 = sect3.3_copt_lambda1(),
                                            lambda2 = sect3.3_copt_lambda2(), 
                                            mu0 = sect3.3_copt_mu0(),
                                            tau0 = sect3.3_copt_tau0())
  }
})

sect3.3_AUC_post_error_char_copt = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_AUC_post_error_char_copt(w = input$binormal_diag_prevalence_w,
                                           coptest = sect3.3_copt_est_hardcode(),  
                                           nMontepost = sect3.3_copt_nMonteCarlo(),
                                           delta = sect3.3_copt_delta(),
                                           lambda1post = sect3.3_hyperpara_copt()$lambda1post, 
                                           lambda2post = sect3.3_hyperpara_copt()$lambda2post, 
                                           mu0Dpost = sect3.3_hyperpara_copt()$mu0Dpost, 
                                           mu0NDpost = sect3.3_hyperpara_copt()$mu0NDpost, 
                                           tau0D = sect3.3_hyperpara_copt()$tau0D, 
                                           tau0ND = sect3.3_hyperpara_copt()$tau0ND)
  } else if (input$binormal_diag_case2 == "A"){ 
    binormal_diag_AUC_post_error_char_copt(w = FALSE,
                                           alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                           alpha2w = input$binormal_diag_prevalence_alpha2w,
                                           version = "prior",
                                           coptest = sect3.3_copt_est_hardcode(), 
                                           nMontepost = sect3.3_copt_nMonteCarlo(),
                                           delta = sect3.3_copt_delta(),
                                           lambda1post = sect3.3_hyperpara_copt()$lambda1post, 
                                           lambda2post = sect3.3_hyperpara_copt()$lambda2post, 
                                           mu0Dpost = sect3.3_hyperpara_copt()$mu0Dpost, 
                                           mu0NDpost = sect3.3_hyperpara_copt()$mu0NDpost, 
                                           tau0D = sect3.3_hyperpara_copt()$tau0D, 
                                           tau0ND = sect3.3_hyperpara_copt()$tau0ND)
  } else if (input$binormal_diag_case2 == "B"){
    binormal_diag_AUC_post_error_char_copt(w = FALSE,
                                           alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                           alpha2w = input$binormal_diag_prevalence_alpha2w,
                                           nND = sect3.3_copt_nND_use(), 
                                           nD = sect3.3_copt_nD_use(),
                                           version = "post",
                                           coptest = sect3.3_copt_est_hardcode(),
                                           nMontepost = sect3.3_copt_nMonteCarlo(),
                                           delta = sect3.3_copt_delta(),
                                           lambda1post = sect3.3_hyperpara_copt()$lambda1post, 
                                           lambda2post = sect3.3_hyperpara_copt()$lambda2post, 
                                           mu0Dpost = sect3.3_hyperpara_copt()$mu0Dpost, 
                                           mu0NDpost = sect3.3_hyperpara_copt()$mu0NDpost, 
                                           tau0D = sect3.3_hyperpara_copt()$tau0D, 
                                           tau0ND = sect3.3_hyperpara_copt()$tau0ND)
  }
})

sect3.3_AUC_RBR_error_char_copt = reactive({
  priorFNR_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt()$priorFNR, 
                                       input$binormal_diag_smoother_inferences)
  priorFPR_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt()$priorFPR, 
                                       input$binormal_diag_smoother_inferences)
  priorError_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt()$priorError, 
                                         input$binormal_diag_smoother_inferences)
  priorFDR_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt()$priorFDR, 
                                       input$binormal_diag_smoother_inferences)
  priorFNDR_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt()$priorFNDR, 
                                        input$binormal_diag_smoother_inferences)
  postFNR_smo = average_vector_values(sect3.3_AUC_post_error_char_copt()$postFNR, 
                                      input$binormal_diag_smoother_inferences)
  postFPR_smo = average_vector_values(sect3.3_AUC_post_error_char_copt()$postFPR, 
                                      input$binormal_diag_smoother_inferences)
  postError_smo = average_vector_values(sect3.3_AUC_post_error_char_copt()$postError, 
                                        input$binormal_diag_smoother_inferences)
  postFDR_smo = average_vector_values(sect3.3_AUC_post_error_char_copt()$postFDR, 
                                      input$binormal_diag_smoother_inferences)
  postFNDR_smo = average_vector_values(sect3.3_AUC_post_error_char_copt()$postFNDR, 
                                       input$binormal_diag_smoother_inferences)
  binormal_diag_AUC_RBR_error_char_copt(delta = sect3.3_copt_delta(),
                                        priorFNR =  priorFNR_smo, priorFPR = priorFPR_smo, 
                                        priorError = priorError_smo, priorFDR = priorFDR_smo, 
                                        priorFNDR = priorFNDR_smo, 
                                        postFNR = postFNR_smo, postFPR = postFPR_smo,
                                        postError = postError_smo, postFDR = postFDR_smo, 
                                        postFNDR = postFNDR_smo)
})

binormal_diag_err_char_plot_type = reactive({
  if (input$binormal_diag_inferences_plot_type == "FNR"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt()$priorFNRdensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt()$postFNRdensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt()$RBFNR))
  } else if (input$binormal_diag_inferences_plot_type == "FPR"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt()$priorFPRdensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt()$postFPRdensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt()$RBFPR))
  } else if (input$binormal_diag_inferences_plot_type == "Error"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt()$priorErrordensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt()$postErrordensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt()$RBError))
  } else if (input$binormal_diag_inferences_plot_type == "FDR"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt()$priorFDRdensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt()$postFDRdensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt()$RBFDR))
  } else if (input$binormal_diag_inferences_plot_type == "FNDR"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt()$priorFNDRdensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt()$postFNDRdensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt()$RBFNR))
  }
})
