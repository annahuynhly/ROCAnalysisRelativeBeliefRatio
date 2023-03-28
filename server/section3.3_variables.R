################################################################
# VARIABLES                                                    #
################################################################

# Setting the seed
SECT3.3_SEED = reactive(input$binormal_diag_seed)

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

sect3.3_AUC_prior = reactive({
  set.seed(SECT3.3_SEED()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$binormal_diag_case1 == 1){
    binormal_diag_prior(w = input$binormal_diag_prevalence_w,
                        alpha1w = NA, 
                        alpha2w = NA, 
                        nMonteprior = 10000, #input$binormal_diag_nMonteCarlo, 
                        delta = input$binormal_diag_delta, 
                        lambda1 = input$binormal_diag_lambda1, 
                        lambda2 = input$binormal_diag_lambda2, 
                        mu0 = input$binormal_diag_mu0, 
                        tau0 = input$binormal_diag_tau0)
  } else if (input$binormal_diag_case2 == "A" | input$binormal_diag_case2 == "B"){ 
    binormal_diag_prior(w = FALSE,
                        alpha1w = input$binormal_diag_prevalence_alpha1w, 
                        alpha2w = input$binormal_diag_prevalence_alpha2w, 
                        nMonteprior = 10000, #input$binormal_diag_nMonteCarlo, 
                        delta = input$binormal_diag_delta, 
                        lambda1 = input$binormal_diag_lambda1, 
                        lambda2 = input$binormal_diag_lambda2, 
                        mu0 = input$binormal_diag_mu0, 
                        tau0 = input$binormal_diag_tau0)
  }
})

sect3.3_AUC_post = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_post(w = input$binormal_diag_prevalence_w,
                       alpha1w = NA, 
                       alpha2w = NA, 
                       nND = NA, 
                       nD = NA,
                       version = NA,
                       nMontepost = 10000, #input$binormal_diag_nMonteCarlo, 
                       delta = input$binormal_diag_delta, 
                       lambda1post = sect3.3_hyperpara()$lambda1post, 
                       lambda2post = sect3.3_hyperpara()$lambda2post, 
                       mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                       mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                       tau0D = sect3.3_hyperpara()$tau0D, 
                       tau0ND = sect3.3_hyperpara()$tau0ND)
  } else if (input$binormal_diag_case2 == "A"){ 
    binormal_diag_post(w = FALSE,
                       alpha1w = input$binormal_diag_prevalence_alpha1w, 
                       alpha2w = input$binormal_diag_prevalence_alpha2w,
                       nND = NA, 
                       nD = NA,
                       version = "prior",
                       nMontepost = 10000, #input$binormal_diag_nMonteCarlo, 
                       delta = input$binormal_diag_delta, 
                       lambda1post = sect3.3_hyperpara()$lambda1post, 
                       lambda2post = sect3.3_hyperpara()$lambda2post, 
                       mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                       mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                       tau0D = sect3.3_hyperpara()$tau0D, 
                       tau0ND = sect3.3_hyperpara()$tau0ND)
  } else if (input$binormal_diag_case2 == "B"){
    binormal_diag_post(w = FALSE,
                       alpha1w = input$binormal_diag_prevalence_alpha1w, 
                       alpha2w = input$binormal_diag_prevalence_alpha2w, 
                       nND = input$binormal_diag_nND, 
                       nD = input$binormal_diag_nD, 
                       version = "post",
                       nMontepost = 10000, #input$binormal_diag_nMonteCarlo, 
                       delta = input$binormal_diag_delta, 
                       lambda1post = sect3.3_hyperpara()$lambda1post, 
                       lambda2post = sect3.3_hyperpara()$lambda2post, 
                       mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                       mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                       tau0D = sect3.3_hyperpara()$tau0D, 
                       tau0ND = sect3.3_hyperpara()$tau0ND)
  }
})

sect3.3_AUC_RBR = reactive({
  binormal_diag_RBR(delta = input$binormal_diag_delta, 
                    probAUCprior = sect3.3_AUC_prior()$probAUCprior, 
                    probAUCpost = sect3.3_AUC_post()$probAUCpost,
                    priorAUC = sect3.3_AUC_prior()$priorAUC, 
                    postAUC = sect3.3_AUC_post()$postAUC,
                    priorcmod = sect3.3_AUC_prior()$priorcmod,
                    postcmod = sect3.3_AUC_post()$postcmod)
})

sect3.3_cr = reactive({
  binormal_diag_compute_credible_region(gamma = input$binormal_diag_gamma, 
                                        delta = input$binormal_diag_delta, 
                                        AUC_RBR = sect3.3_AUC_RBR()$RB_AUC, 
                                        AUC_prior = sect3.3_AUC_prior()$priorAUC, 
                                        AUC_post = sect3.3_AUC_post()$postAUC, 
                                        posterior_content = sect3.3_AUC_RBR()$postPl_AUC)
})

sect3.3_AUC_prior_error_char_copt = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_AUC_prior_error_char_copt(w = input$binormal_diag_prevalence_w,
                                            coptest = 0.715, #sect3.3_AUC_RBR()$coptest, 
                                            nMonteprior = input$binormal_diag_nMonteCarlo, 
                                            delta = input$binormal_diag_delta, 
                                            lambda1 = input$binormal_diag_lambda1,
                                            lambda2 = input$binormal_diag_lambda2, 
                                            mu0 = input$binormal_diag_mu0,
                                            tau0 = input$binormal_diag_tau0)
  } else if (input$binormal_diag_case2 == "A" | input$binormal_diag_case2 == "B"){ 
    binormal_diag_AUC_prior_error_char_copt(w = FALSE,
                                            alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                            alpha2w = input$binormal_diag_prevalence_alpha2w,
                                            coptest = 0.715, #sect3.3_AUC_RBR()$coptest, 
                                            nMonteprior = input$binormal_diag_nMonteCarlo, 
                                            delta = input$binormal_diag_delta, 
                                            lambda1 = input$binormal_diag_lambda1,
                                            lambda2 = input$binormal_diag_lambda2, 
                                            mu0 = input$binormal_diag_mu0,
                                            tau0 = input$binormal_diag_tau0)
  }
})

sect3.3_AUC_post_error_char_copt = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_AUC_post_error_char_copt(w = input$binormal_diag_prevalence_w,
                                           coptest = 0.715, #sect3.3_AUC_RBR()$coptest, 
                                           nMontepost = input$binormal_diag_nMonteCarlo,
                                           delta = input$binormal_diag_delta,
                                           lambda1post = sect3.3_hyperpara()$lambda1post, 
                                           lambda2post = sect3.3_hyperpara()$lambda2post, 
                                           mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                                           mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                                           tau0D = sect3.3_hyperpara()$tau0D, 
                                           tau0ND = sect3.3_hyperpara()$tau0ND)
  } else if (input$binormal_diag_case2 == "A"){ 
    binormal_diag_AUC_post_error_char_copt(w = FALSE,
                                           alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                           alpha2w = input$binormal_diag_prevalence_alpha2w,
                                           version = "prior",
                                           coptest = 0.715, #sect3.3_AUC_RBR()$coptest, 
                                           nMontepost = input$binormal_diag_nMonteCarlo,
                                           delta = input$binormal_diag_delta,
                                           lambda1post = sect3.3_hyperpara()$lambda1post, 
                                           lambda2post = sect3.3_hyperpara()$lambda2post, 
                                           mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                                           mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                                           tau0D = sect3.3_hyperpara()$tau0D, 
                                           tau0ND = sect3.3_hyperpara()$tau0ND)
  } else if (input$binormal_diag_case2 == "B"){
    binormal_diag_AUC_post_error_char_copt(w = FALSE,
                                           alpha1w = input$binormal_diag_prevalence_alpha1w, 
                                           alpha2w = input$binormal_diag_prevalence_alpha2w,
                                           nND = input$binormal_diag_nND, 
                                           nD = input$binormal_diag_nD,
                                           version = "post",
                                           coptest = 0.715, #sect3.3_AUC_RBR()$coptest, 
                                           nMontepost = input$binormal_diag_nMonteCarlo,
                                           delta = input$binormal_diag_delta,
                                           lambda1post = sect3.3_hyperpara()$lambda1post, 
                                           lambda2post = sect3.3_hyperpara()$lambda2post, 
                                           mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                                           mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                                           tau0D = sect3.3_hyperpara()$tau0D, 
                                           tau0ND = sect3.3_hyperpara()$tau0ND)
  }
})

sect3.3_AUC_RBR_error_char_copt = reactive({
  binormal_diag_AUC_RBR_error_char_copt(delta = input$binormal_diag_delta,
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

