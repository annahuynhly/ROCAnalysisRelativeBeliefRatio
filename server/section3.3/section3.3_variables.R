################################################################
# VARIABLES                                                    #
################################################################

# Setting the seed
SECT3.3_SEED = reactive(input$binormal_diag_seed)

# This is for the equal variances case
sect3.3_hyperpara = reactive({
  binormal_compute_post_hyperpara(mu0 = input$binormal_diag_mu0, 
                                  tau0 = input$binormal_diag_tau0, 
                                  lambda1 = input$binormal_diag_lambda1, 
                                  lambda2 = input$binormal_diag_lambda2, 
                                  nND = input$binormal_diag_nND, 
                                  meanND = input$binormal_diag_meanND, 
                                  sND_squared = input$binormal_diag_sND_squared, 
                                  nD = input$binormal_diag_nD, 
                                  meanD = input$binormal_diag_meanD, 
                                  sD_squared = input$binormal_diag_sD_squared)
})

sect3.3_hyperpara_copt = reactive({
  binormal_compute_post_hyperpara(mu0 = sect3.3_copt_mu0(), 
                                  tau0 = sect3.3_copt_tau0(), 
                                  lambda1 = sect3.3_copt_lambda1(), 
                                  lambda2 = sect3.3_copt_lambda2(), 
                                  nND = sect3.3_copt_nND(), 
                                  meanND = sect3.3_copt_meanND(), 
                                  sND_squared = sect3.3_copt_sND_squared(), 
                                  nD = sect3.3_copt_nD(), 
                                  meanD = sect3.3_copt_meanD(), 
                                  sD_squared = sect3.3_copt_sD_squared())
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
  if (sect3.3_condition() == "conditional"){
    binormal_diag_RBR(condition = sect3.3_condition(),
                      delta = input$binormal_diag_delta,
                      priorAUC = sect3.3_AUC_prior()$priorAUC,
                      postAUC = sect3.3_AUC_post()$postAUC)
  } else if (sect3.3_condition() == "unconditional"){
    binormal_diag_RBR(condition = sect3.3_condition(),
                      delta = input$binormal_diag_delta, 
                      probAUCprior = sect3.3_AUC_prior()$probAUCprior, 
                      probAUCpost = sect3.3_AUC_post()$probAUCpost,
                      priorAUC = sect3.3_AUC_prior()$priorAUC, 
                      postAUC = sect3.3_AUC_post()$postAUC)
  }
})

sect3.3_cr = reactive({
  binormal_diag_compute_credible_region(gamma = input$binormal_diag_gamma, 
                                        delta = input$binormal_diag_delta, 
                                        AUC_RBR = sect3.3_AUC_RBR()$RB_AUC, 
                                        AUC_prior = sect3.3_AUC_prior()$priorAUC, 
                                        AUC_post = sect3.3_AUC_post()$postAUC, 
                                        posterior_content = sect3.3_AUC_RBR()$postPl_AUC)
})

sect3.3_AUC_prior_copt = reactive({
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
                            nND = sect3.3_copt_nND(), 
                            nD = sect3.3_copt_nD(), 
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
  binormal_diag_RBR_copt(sect3.3_copt_delta(), 
                         sect3.3_AUC_prior_copt()$priorcmod, 
                         sect3.3_AUC_post_copt()$postcmod)
})

sect3.3_copt = reactive({
  if (input$binormal_optimal_cutoff_denote_copt == 'no'){
    sect3.3_AUC_RBR_copt()$coptest
  } else {
    input$binormal_diag_optimal_cutoff_copt
  }
})

sect3.3_AUC_prior_error_char_copt = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_AUC_prior_error_char_copt(w = input$binormal_diag_prevalence_w,
                                            coptest = sect3.3_copt(), 
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
                                            coptest = sect3.3_copt(), 
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
                                           coptest = sect3.3_copt(),  
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
                                           coptest = sect3.3_copt(), 
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
                                           nND = sect3.3_copt_nND(), 
                                           nD = sect3.3_copt_nD(),
                                           version = "post",
                                           coptest = sect3.3_copt(),
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
  binormal_diag_AUC_RBR_error_char_copt(delta = sect3.3_copt_delta(),
                                        priorFNR =  sect3.3_AUC_prior_error_char_copt()$priorFNR, 
                                        priorFPR = sect3.3_AUC_prior_error_char_copt()$priorFPR, 
                                        priorError = sect3.3_AUC_prior_error_char_copt()$priorError,
                                        priorFDR = sect3.3_AUC_prior_error_char_copt()$priorFDR, 
                                        priorFNDR = sect3.3_AUC_prior_error_char_copt()$priorFNDR, 
                                        postFNR = sect3.3_AUC_post_error_char_copt()$postFNR, 
                                        postFPR = sect3.3_AUC_post_error_char_copt()$postFPR,
                                        postError = sect3.3_AUC_post_error_char_copt()$postError, 
                                        postFDR = sect3.3_AUC_post_error_char_copt()$postFDR, 
                                        postFNDR = sect3.3_AUC_post_error_char_copt()$postFNDR)
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

# This is for the unequal variances case

sect3.3_hyperpara_unequal = reactive({
  binormal_compute_post_hyperpara_unequal(mu0 = input$binormal_diag_mu0, 
                                  tau0 = input$binormal_diag_tau0, 
                                  lambda1 = input$binormal_diag_lambda1, 
                                  lambda2 = input$binormal_diag_lambda2, 
                                  nND = input$binormal_diag_nND, 
                                  meanND = input$binormal_diag_meanND, 
                                  sND_squared = input$binormal_diag_sND_squared, 
                                  nD = input$binormal_diag_nD, 
                                  meanD = input$binormal_diag_meanD, 
                                  sD_squared = input$binormal_diag_sD_squared)
})

sect3.3_hyperpara_copt_unequal = reactive({
  binormal_compute_post_hyperpara_unequal(mu0 = sect3.3_copt_mu0(), 
                                  tau0 = sect3.3_copt_tau0(), 
                                  lambda1 = sect3.3_copt_lambda1(), 
                                  lambda2 = sect3.3_copt_lambda2(), 
                                  nND = sect3.3_copt_nND(), 
                                  meanND = sect3.3_copt_meanND(), 
                                  sND_squared = sect3.3_copt_sND_squared(), 
                                  nD = sect3.3_copt_nD(), 
                                  meanD = sect3.3_copt_meanD(), 
                                  sD_squared = sect3.3_copt_sD_squared())
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
  if (sect3.3_condition() == "conditional"){
    binormal_diag_RBR(condition = sect3.3_condition(),
                      delta = input$binormal_diag_delta,
                      priorAUC = sect3.3_AUC_prior_unequal()$priorAUC,
                      postAUC = sect3.3_AUC_post_unequal()$postAUC)
  } else if (sect3.3_condition() == "unconditional"){
    binormal_diag_RBR(condition = sect3.3_condition(),
                      delta = input$binormal_diag_delta, 
                      probAUCprior = sect3.3_AUC_prior_unequal()$probAUCprior, 
                      probAUCpost = sect3.3_AUC_post_unequal()$probAUCpost,
                      priorAUC = sect3.3_AUC_prior_unequal()$priorAUC, 
                      postAUC = sect3.3_AUC_post_unequal()$postAUC)
  }
})

sect3.3_cr_unequal = reactive({
  binormal_diag_compute_credible_region(gamma = input$binormal_diag_gamma, 
                                        delta = input$binormal_diag_delta, 
                                        AUC_RBR = sect3.3_AUC_RBR_unequal()$RB_AUC, 
                                        AUC_prior = sect3.3_AUC_prior_unequal()$priorAUC, 
                                        AUC_post = sect3.3_AUC_post_unequal()$postAUC, 
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
  binormal_diag_RBR_copt(sect3.3_copt_delta(), 
                         sect3.3_AUC_prior_copt_unequal()$priorcmod, 
                         sect3.3_AUC_post_copt_unequal()$postcmod)
})

sect3.3_copt_unequal = reactive({
  if (input$binormal_optimal_cutoff_denote_copt == 'no'){
    sect3.3_AUC_RBR_copt_unequal()$coptest
  } else {
    input$binormal_diag_optimal_cutoff_copt
  }
})

sect3.3_AUC_prior_error_char_copt_unequal = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_AUC_prior_error_char_copt_unequal(w = input$binormal_diag_prevalence_w,
                                                    alpha1w = NA, 
                                                    alpha2w = NA, 
                                                    coptest = sect3.3_copt(), 
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
                                                    coptest = sect3.3_copt(), 
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
      coptest = sect3.3_copt(),  
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
      coptest = sect3.3_copt(),  
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
      coptest = sect3.3_copt(),  
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
  binormal_diag_AUC_RBR_error_char_copt(
    delta = sect3.3_copt_delta(),
    priorFNR =  sect3.3_AUC_prior_error_char_copt_unequal()$priorFNR, 
    priorFPR = sect3.3_AUC_prior_error_char_copt_unequal()$priorFPR, 
    priorError = sect3.3_AUC_prior_error_char_copt_unequal()$priorError,
    priorFDR = sect3.3_AUC_prior_error_char_copt_unequal()$priorFDR, 
    priorFNDR = sect3.3_AUC_prior_error_char_copt_unequal()$priorFNDR, 
    postFNR = sect3.3_AUC_post_error_char_copt_unequal()$postFNR, 
    postFPR = sect3.3_AUC_post_error_char_copt_unequal()$postFPR,
    postError = sect3.3_AUC_post_error_char_copt_unequal()$postError, 
    postFDR = sect3.3_AUC_post_error_char_copt_unequal()$postFDR, 
    postFNDR = sect3.3_AUC_post_error_char_copt_unequal()$postFNDR)
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
