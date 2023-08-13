################################################################
# UNEQUAL VARIANCES CASE                                       #
################################################################

sect3.3_hyperpara_unequal = reactive({
  binormal_compute_post_hyperpara_unequal(mu0 = input$binormal_diag_mu0, 
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

sect3.3_hyperpara_copt_unequal = reactive({
  binormal_compute_post_hyperpara_unequal(mu0 = sect3.3_copt_mu0(), 
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

sect3.3_AUC_prior_unequal = reactive({
  set.seed(SECT3.3_SEED()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$binormal_diag_case1 == 1){
    binormal_diag_prior_unequal(condition = sect3.3_condition(), 
                                w = input$binormal_diag_prevalence_w,
                                alpha1w = NA, 
                                alpha2w = NA, 
                                nMonteprior = input$binormal_diag_nMonteCarlo, 
                                delta = input$binormal_diag_delta, 
                                lambda1 = input$binormal_diag_lambda1, 
                                lambda2 = input$binormal_diag_lambda2, 
                                mu0 = input$binormal_diag_mu0, 
                                tau0 = input$binormal_diag_tau0)
  } else if (input$binormal_diag_case2 == "A" | input$binormal_diag_case2 == "B"){ 
    binormal_diag_prior_unequal(condition = sect3.3_condition(), 
                                w = FALSE,
                                alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                alpha2w = input$binormal_diag_prevalence_alpha2w,
                                nMonteprior = input$binormal_diag_nMonteCarlo, 
                                delta = input$binormal_diag_delta, 
                                lambda1 = input$binormal_diag_lambda1, 
                                lambda2 = input$binormal_diag_lambda2, 
                                mu0 = input$binormal_diag_mu0, 
                                tau0 = input$binormal_diag_tau0)
  }
})

sect3.3_AUC_post_unequal = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_post_unequal(condition = sect3.3_condition(),
                               w = input$binormal_diag_prevalence_w,
                               alpha1w = NA, 
                               alpha2w = NA, 
                               nND = NA, 
                               nD = NA,
                               version = NA,
                               nMontepost = input$binormal_diag_nMonteCarlo, 
                               delta = input$binormal_diag_delta, 
                               lambda1Dpost = sect3.3_hyperpara_unequal()$lambda1Dpost, 
                               lambda1NDpost = sect3.3_hyperpara_unequal()$lambda1NDpost, 
                               lambda2Dpost = sect3.3_hyperpara_unequal()$lambda2Dpost, 
                               lambda2NDpost = sect3.3_hyperpara_unequal()$lambda2NDpost, 
                               mu0Dpost = sect3.3_hyperpara_unequal()$mu0Dpost, 
                               mu0NDpost = sect3.3_hyperpara_unequal()$mu0NDpost, 
                               tau0D = sect3.3_hyperpara_unequal()$tau0D, 
                               tau0ND = sect3.3_hyperpara_unequal()$tau0ND)
  } else if (input$binormal_diag_case2 == "A"){ 
    binormal_diag_post_unequal(condition = sect3.3_condition(),
                               w = FALSE,
                               alpha1w = input$binormal_diag_prevalence_alpha1w, 
                               alpha2w = input$binormal_diag_prevalence_alpha2w,
                               nND = NA, 
                               nD = NA,
                               version = "prior",
                               nMontepost = input$binormal_diag_nMonteCarlo, 
                               delta = input$binormal_diag_delta, 
                               lambda1Dpost = sect3.3_hyperpara_unequal()$lambda1Dpost, 
                               lambda1NDpost = sect3.3_hyperpara_unequal()$lambda1NDpost, 
                               lambda2Dpost = sect3.3_hyperpara_unequal()$lambda2Dpost, 
                               lambda2NDpost = sect3.3_hyperpara_unequal()$lambda2NDpost, 
                               mu0Dpost = sect3.3_hyperpara_unequal()$mu0Dpost, 
                               mu0NDpost = sect3.3_hyperpara_unequal()$mu0NDpost, 
                               tau0D = sect3.3_hyperpara_unequal()$tau0D, 
                               tau0ND = sect3.3_hyperpara_unequal()$tau0ND)
  } else if (input$binormal_diag_case2 == "B"){
    binormal_diag_post_unequal(condition = sect3.3_condition(),
                               w = FALSE,
                               alpha1w = input$binormal_diag_prevalence_alpha1w, 
                               alpha2w = input$binormal_diag_prevalence_alpha2w, 
                               nND = sect3.3_copt_nND(), 
                               nD = sect3.3_copt_nD(), 
                               version = "post",
                               nMontepost = input$binormal_diag_nMonteCarlo, 
                               delta = input$binormal_diag_delta, 
                               lambda1Dpost = sect3.3_hyperpara_unequal()$lambda1Dpost, 
                               lambda1NDpost = sect3.3_hyperpara_unequal()$lambda1NDpost, 
                               lambda2Dpost = sect3.3_hyperpara_unequal()$lambda2Dpost, 
                               lambda2NDpost = sect3.3_hyperpara_unequal()$lambda2NDpost, 
                               mu0Dpost = sect3.3_hyperpara_unequal()$mu0Dpost, 
                               mu0NDpost = sect3.3_hyperpara_unequal()$mu0NDpost, 
                               tau0D = sect3.3_hyperpara_unequal()$tau0D, 
                               tau0ND = sect3.3_hyperpara_unequal()$tau0ND)
  }
})

sect3.3_AUC_RBR_unequal = reactive({
  priorAUC_smo = average_vector_values(sect3.3_AUC_prior_unequal()$priorAUC, 
                                       input$binormal_diag_smoother)
  postAUC_smo = average_vector_values(sect3.3_AUC_post_unequal()$postAUC, 
                                      input$binormal_diag_smoother)
  if (sect3.3_condition() == "conditional"){
    binormal_diag_RBR(condition = sect3.3_condition(),
                      delta = input$binormal_diag_delta,
                      priorAUC = priorAUC_smo, #sect3.3_AUC_prior_unequal()$priorAUC,
                      postAUC = postAUC_smo) #sect3.3_AUC_post_unequal()$postAUC)
  } else if (sect3.3_condition() == "unconditional"){
    binormal_diag_RBR(condition = sect3.3_condition(),
                      delta = input$binormal_diag_delta, 
                      probAUCprior = sect3.3_AUC_prior_unequal()$probAUCprior, 
                      probAUCpost = sect3.3_AUC_post_unequal()$probAUCpost,
                      priorAUC = priorAUC_smo, #sect3.3_AUC_prior_unequal()$priorAUC, 
                      postAUC = postAUC_smo) #sect3.3_AUC_post_unequal()$postAUC)
  }
})

sect3.3_cr_unequal = reactive({
  priorAUC_smo = average_vector_values(sect3.3_AUC_prior_unequal()$priorAUC, 
                                       input$binormal_diag_smoother)
  postAUC_smo = average_vector_values(sect3.3_AUC_post_unequal()$postAUC, 
                                      input$binormal_diag_smoother)
  binormal_diag_compute_credible_region(gamma = input$binormal_diag_gamma, 
                                        delta = input$binormal_diag_delta, 
                                        AUC_RBR = sect3.3_AUC_RBR_unequal()$RB_AUC, 
                                        AUC_prior = priorAUC_smo, #sect3.3_AUC_prior_unequal()$priorAUC, 
                                        AUC_post = postAUC_smo, #sect3.3_AUC_post_unequal()$postAUC, 
                                        plausible_region = sect3.3_AUC_RBR_unequal()$plausible_region,
                                        posterior_content = sect3.3_AUC_RBR_unequal()$postPl_AUC)
})

sect3.3_AUC_prior_copt_unequal = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_prior_copt_unequal(w = input$binormal_diag_prevalence_w,
                                     alpha1w = NA, 
                                     alpha2w = NA, 
                                     nMonteprior = sect3.3_copt_nMonteCarlo(), 
                                     delta = sect3.3_copt_delta(), 
                                     lambda1 = sect3.3_copt_lambda1(), 
                                     lambda2 = sect3.3_copt_lambda2(), 
                                     mu0 = sect3.3_copt_mu0(), 
                                     tau0 = sect3.3_copt_tau0(),
                                     lambda = sect3.3_copt_lambda())
  } else if (input$binormal_diag_case2 == "A" | input$binormal_diag_case2 == "B"){
    binormal_diag_prior_copt_unequal(w = FALSE, 
                                     alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                     alpha2w = input$binormal_diag_prevalence_alpha2w, 
                                     nMonteprior = sect3.3_copt_nMonteCarlo(), 
                                     delta = sect3.3_copt_delta(), 
                                     lambda1 = sect3.3_copt_lambda1(), 
                                     lambda2 = sect3.3_copt_lambda2(), 
                                     mu0 = sect3.3_copt_mu0(), 
                                     tau0 = sect3.3_copt_tau0(),
                                     lambda = sect3.3_copt_lambda())
  }
})

sect3.3_AUC_post_copt_unequal = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_post_copt_unequal(w = input$binormal_diag_prevalence_w,
                                    alpha1w = NA, 
                                    alpha2w = NA, 
                                    nND = NA, 
                                    nD = NA,
                                    version = NA, 
                                    nMontepost = sect3.3_copt_nMonteCarlo(), 
                                    delta = sect3.3_copt_delta(), 
                                    lambda1Dpost = sect3.3_hyperpara_copt_unequal()$lambda1Dpost, 
                                    lambda1NDpost = sect3.3_hyperpara_copt_unequal()$lambda1NDpost, 
                                    lambda2Dpost = sect3.3_hyperpara_copt_unequal()$lambda2Dpost, 
                                    lambda2NDpost = sect3.3_hyperpara_copt_unequal()$lambda2NDpost, 
                                    mu0Dpost = sect3.3_hyperpara_copt_unequal()$mu0Dpost, 
                                    mu0NDpost = sect3.3_hyperpara_copt_unequal()$mu0NDpost, 
                                    tau0D = sect3.3_hyperpara_copt_unequal()$tau0D, 
                                    tau0ND = sect3.3_hyperpara_copt_unequal()$tau0ND,
                                    lambda = sect3.3_copt_lambda())
  } else if (input$binormal_diag_case2 == "A"){ 
    binormal_diag_post_copt_unequal(w = FALSE,
                                    alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                    alpha2w = input$binormal_diag_prevalence_alpha2w,
                                    nND = NA, 
                                    nD = NA,
                                    version = "prior",
                                    nMontepost = sect3.3_copt_nMonteCarlo(), 
                                    delta = sect3.3_copt_delta(), 
                                    lambda1Dpost = sect3.3_hyperpara_copt_unequal()$lambda1Dpost, 
                                    lambda1NDpost = sect3.3_hyperpara_copt_unequal()$lambda1NDpost, 
                                    lambda2Dpost = sect3.3_hyperpara_copt_unequal()$lambda2Dpost, 
                                    lambda2NDpost = sect3.3_hyperpara_copt_unequal()$lambda2NDpost, 
                                    mu0Dpost = sect3.3_hyperpara_copt_unequal()$mu0Dpost, 
                                    mu0NDpost = sect3.3_hyperpara_copt_unequal()$mu0NDpost, 
                                    tau0D = sect3.3_hyperpara_copt_unequal()$tau0D, 
                                    tau0ND = sect3.3_hyperpara_copt_unequal()$tau0ND,
                                    lambda = sect3.3_copt_lambda())
  } else if (input$binormal_diag_case2 == "B"){
    binormal_diag_post_copt_unequal(w = FALSE,
                                    alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                    alpha2w = input$binormal_diag_prevalence_alpha2w, 
                                    nND = sect3.3_copt_nND(), 
                                    nD = sect3.3_copt_nD(), 
                                    version = "post",
                                    nMontepost = sect3.3_copt_nMonteCarlo(), 
                                    delta = sect3.3_copt_delta(), 
                                    lambda1Dpost = sect3.3_hyperpara_copt_unequal()$lambda1Dpost, 
                                    lambda1NDpost = sect3.3_hyperpara_copt_unequal()$lambda1NDpost, 
                                    lambda2Dpost = sect3.3_hyperpara_copt_unequal()$lambda2Dpost, 
                                    lambda2NDpost = sect3.3_hyperpara_copt_unequal()$lambda2NDpost, 
                                    mu0Dpost = sect3.3_hyperpara_copt_unequal()$mu0Dpost, 
                                    mu0NDpost = sect3.3_hyperpara_copt_unequal()$mu0NDpost, 
                                    tau0D = sect3.3_hyperpara_copt_unequal()$tau0D, 
                                    tau0ND = sect3.3_hyperpara_copt_unequal()$tau0ND,
                                    lambda = sect3.3_copt_lambda())
  }
})

sect3.3_AUC_RBR_copt_unequal = reactive({
  priorcmod_smo = average_vector_values(sect3.3_AUC_prior_copt_unequal()$priorcmod, 
                                        input$binormal_diag_smoother_copt)
  priorcopt_smo = average_vector_values(sect3.3_AUC_prior_copt_unequal()$priorcopt, 
                                        input$binormal_diag_smoother_cutoff)
  postcmod_smo = average_vector_values(sect3.3_AUC_post_copt_unequal()$postcmod, 
                                       input$binormal_diag_smoother_copt)
  postcopt_smo = average_vector_values(sect3.3_AUC_post_copt_unequal()$postcopt, 
                                       input$binormal_diag_smoother_cutoff)
  
  binormal_diag_RBR_copt(delta = sect3.3_copt_delta(), 
                         priorcmod = priorcmod_smo, #sect3.3_AUC_prior_copt_unequal()$priorcmod, 
                         postcmod = postcmod_smo, #sect3.3_AUC_post_copt_unequal()$postcmod)
                         priorcopt = priorcopt_smo,
                         postcopt = postcopt_smo)
})

sect3.3_cr_copt_unequal = reactive({
  priorcmod_smo = average_vector_values(sect3.3_AUC_prior_copt_unequal()$priorcmod, 
                                        input$binormal_diag_smoother_copt)
  postcmod_smo = average_vector_values(sect3.3_AUC_post_copt_unequal()$postcmod, 
                                       input$binormal_diag_smoother_copt)
  binormal_diag_compute_credible_region(gamma = input$binormal_diag_gamma_copt, 
                                        delta = sect3.3_copt_delta(), 
                                        AUC_RBR = sect3.3_AUC_RBR_copt_unequal()$RBcmod, 
                                        AUC_prior = priorcmod_smo, #sect3.3_AUC_prior_copt_unequal()$priorcmod, 
                                        AUC_post = postcmod_smo, #sect3.3_AUC_post_copt_unequal()$postcmod, 
                                        plausible_region = sect3.3_AUC_RBR_copt_unequal()$plausible_region,
                                        posterior_content = sect3.3_AUC_RBR_copt_unequal()$postPlcmod)
})

sect3.3_copt_est_hardcode_unequal = reactive({
  # previously named sect3.3_copt_unequal
  if(input$binormal_optimal_cutoff_denote_copt == 'yes'){
    input$binormal_diag_optimal_cutoff_copt
  } else if (input$binormal_optimal_cutoff_denote_copt == 'no') {
    sect3.3_AUC_RBR_copt_unequal()$coptest
  } else if (input$binormal_optimal_cutoff_denote_copt == 'youden'){
    sect3.3_AUC_RBR_copt_youden_unequal()$coptest
  }
})

sect3.3_AUC_prior_error_char_copt_unequal = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_AUC_prior_error_char_copt_unequal(w = input$binormal_diag_prevalence_w,
                                                    alpha1w = NA, 
                                                    alpha2w = NA, 
                                                    coptest = sect3.3_copt_est_hardcode_unequal(), 
                                                    nMonteprior = sect3.3_copt_nMonteCarlo(), 
                                                    delta = sect3.3_copt_delta(), 
                                                    lambda1 = sect3.3_copt_lambda1(),
                                                    lambda2 = sect3.3_copt_lambda2(), 
                                                    mu0 = sect3.3_copt_mu0(),
                                                    tau0 = sect3.3_copt_tau0())
  } else if (input$binormal_diag_case2 == "A" | input$binormal_diag_case2 == "B"){ 
    binormal_diag_AUC_prior_error_char_copt_unequal(w = FALSE,
                                                    alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                                    alpha2w = input$binormal_diag_prevalence_alpha2w,
                                                    coptest = sect3.3_copt_est_hardcode_unequal(), 
                                                    nMonteprior = sect3.3_copt_nMonteCarlo(), 
                                                    delta = sect3.3_copt_delta(), 
                                                    lambda1 = sect3.3_copt_lambda1(),
                                                    lambda2 = sect3.3_copt_lambda2(), 
                                                    mu0 = sect3.3_copt_mu0(),
                                                    tau0 = sect3.3_copt_tau0())
  }
})

sect3.3_AUC_post_error_char_copt_unequal = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_AUC_post_error_char_copt_unequal(
      w = input$binormal_diag_prevalence_w,
      alpha1w = NA, 
      alpha2w = NA, 
      nND = NA, 
      nD = NA, 
      version = NA,
      coptest = sect3.3_copt_est_hardcode_unequal(),  
      nMontepost = sect3.3_copt_nMonteCarlo(),
      delta = sect3.3_copt_delta(),
      lambda1Dpost = sect3.3_hyperpara_copt_unequal()$lambda1Dpost, 
      lambda1NDpost = sect3.3_hyperpara_copt_unequal()$lambda1NDpost, 
      lambda2Dpost = sect3.3_hyperpara_copt_unequal()$lambda2Dpost, 
      lambda2NDpost = sect3.3_hyperpara_copt_unequal()$lambda2NDpost,
      mu0Dpost = sect3.3_hyperpara_copt_unequal()$mu0Dpost, 
      mu0NDpost = sect3.3_hyperpara_copt_unequal()$mu0NDpost, 
      tau0D = sect3.3_hyperpara_copt_unequal()$tau0D, 
      tau0ND = sect3.3_hyperpara_copt_unequal()$tau0ND)
  } else if (input$binormal_diag_case2 == "A"){ 
    binormal_diag_AUC_post_error_char_copt_unequal(
      w = FALSE,
      alpha1w = input$binormal_diag_prevalence_alpha1w, 
      alpha2w = input$binormal_diag_prevalence_alpha2w,
      version = "prior",
      coptest = sect3.3_copt_est_hardcode_unequal(),  
      nMontepost = sect3.3_copt_nMonteCarlo(),
      delta = sect3.3_copt_delta(),
      lambda1Dpost = sect3.3_hyperpara_copt_unequal()$lambda1Dpost, 
      lambda1NDpost = sect3.3_hyperpara_copt_unequal()$lambda1NDpost, 
      lambda2Dpost = sect3.3_hyperpara_copt_unequal()$lambda2Dpost, 
      lambda2NDpost = sect3.3_hyperpara_copt_unequal()$lambda2NDpost,
      mu0Dpost = sect3.3_hyperpara_copt_unequal()$mu0Dpost, 
      mu0NDpost = sect3.3_hyperpara_copt_unequal()$mu0NDpost, 
      tau0D = sect3.3_hyperpara_copt_unequal()$tau0D, 
      tau0ND = sect3.3_hyperpara_copt_unequal()$tau0ND)
  } else if (input$binormal_diag_case2 == "B"){
    binormal_diag_AUC_post_error_char_copt_unequal(
      w = FALSE,
      alpha1w = input$binormal_diag_prevalence_alpha1w, 
      alpha2w = input$binormal_diag_prevalence_alpha2w,
      nND = sect3.3_copt_nND(), 
      nD = sect3.3_copt_nD(),
      version = "post",
      coptest = sect3.3_copt_est_hardcode_unequal(),  
      nMontepost = sect3.3_copt_nMonteCarlo(),
      delta = sect3.3_copt_delta(),
      lambda1Dpost = sect3.3_hyperpara_copt_unequal()$lambda1Dpost, 
      lambda1NDpost = sect3.3_hyperpara_copt_unequal()$lambda1NDpost, 
      lambda2Dpost = sect3.3_hyperpara_copt_unequal()$lambda2Dpost, 
      lambda2NDpost = sect3.3_hyperpara_copt_unequal()$lambda2NDpost,
      mu0Dpost = sect3.3_hyperpara_copt_unequal()$mu0Dpost, 
      mu0NDpost = sect3.3_hyperpara_copt_unequal()$mu0NDpost, 
      tau0D = sect3.3_hyperpara_copt_unequal()$tau0D, 
      tau0ND = sect3.3_hyperpara_copt_unequal()$tau0ND)
  }
})

sect3.3_AUC_RBR_error_char_copt_unequal = reactive({
  priorFNR_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt_unequal()$priorFNR, 
                                       input$binormal_diag_smoother_inferences)
  priorFPR_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt_unequal()$priorFPR, 
                                       input$binormal_diag_smoother_inferences)
  priorError_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt_unequal()$priorError, 
                                         input$binormal_diag_smoother_inferences)
  priorFDR_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt_unequal()$priorFDR, 
                                       input$binormal_diag_smoother_inferences)
  priorFNDR_smo = average_vector_values(sect3.3_AUC_prior_error_char_copt_unequal()$priorFNDR, 
                                        input$binormal_diag_smoother_inferences)
  postFNR_smo = average_vector_values(sect3.3_AUC_post_error_char_copt_unequal()$postFNR, 
                                      input$binormal_diag_smoother_inferences)
  postFPR_smo = average_vector_values(sect3.3_AUC_post_error_char_copt_unequal()$postFPR, 
                                      input$binormal_diag_smoother_inferences)
  postError_smo = average_vector_values(sect3.3_AUC_post_error_char_copt_unequal()$postError, 
                                        input$binormal_diag_smoother_inferences)
  postFDR_smo = average_vector_values(sect3.3_AUC_post_error_char_copt_unequal()$postFDR, 
                                      input$binormal_diag_smoother_inferences)
  postFNDR_smo = average_vector_values(sect3.3_AUC_post_error_char_copt_unequal()$postFNDR, 
                                       input$binormal_diag_smoother_inferences)
  binormal_diag_AUC_RBR_error_char_copt(delta = sect3.3_copt_delta(),
                                        priorFNR =  priorFNR_smo, priorFPR = priorFPR_smo, 
                                        priorError = priorError_smo, priorFDR = priorFDR_smo, 
                                        priorFNDR = priorFNDR_smo, 
                                        postFNR = postFNR_smo, postFPR = postFPR_smo,
                                        postError = postError_smo, postFDR = postFDR_smo, 
                                        postFNDR = postFNDR_smo)
  #binormal_diag_AUC_RBR_error_char_copt(
  #  delta = sect3.3_copt_delta(),
  #  priorFNR =  sect3.3_AUC_prior_error_char_copt_unequal()$priorFNR, 
  #  priorFPR = sect3.3_AUC_prior_error_char_copt_unequal()$priorFPR, 
  #  priorError = sect3.3_AUC_prior_error_char_copt_unequal()$priorError,
  #  priorFDR = sect3.3_AUC_prior_error_char_copt_unequal()$priorFDR, 
  #  priorFNDR = sect3.3_AUC_prior_error_char_copt_unequal()$priorFNDR, 
  #  postFNR = sect3.3_AUC_post_error_char_copt_unequal()$postFNR, 
  #  postFPR = sect3.3_AUC_post_error_char_copt_unequal()$postFPR,
  #  postError = sect3.3_AUC_post_error_char_copt_unequal()$postError, 
  #  postFDR = sect3.3_AUC_post_error_char_copt_unequal()$postFDR, 
  #  postFNDR = sect3.3_AUC_post_error_char_copt_unequal()$postFNDR)
})

binormal_diag_err_char_plot_type_unequal = reactive({
  if (input$binormal_diag_inferences_plot_type == "FNR"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt_unequal()$priorFNRdensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt_unequal()$postFNRdensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt_unequal()$RBFNR))
  } else if (input$binormal_diag_inferences_plot_type == "FPR"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt_unequal()$priorFPRdensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt_unequal()$postFPRdensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt_unequal()$RBFPR))
  } else if (input$binormal_diag_inferences_plot_type == "Error"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt_unequal()$priorErrordensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt_unequal()$postErrordensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt_unequal()$RBError))
  } else if (input$binormal_diag_inferences_plot_type == "FDR"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt_unequal()$priorFDRdensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt_unequal()$postFDRdensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt_unequal()$RBFDR))
  } else if (input$binormal_diag_inferences_plot_type == "FNDR"){
    list("prior" = NA_to_0(sect3.3_AUC_prior_error_char_copt_unequal()$priorFNDRdensity),
         "post" = NA_to_0(sect3.3_AUC_post_error_char_copt_unequal()$postFNDRdensity),
         "RBR" = NA_to_0(sect3.3_AUC_RBR_error_char_copt_unequal()$RBFNR))
  }
})
