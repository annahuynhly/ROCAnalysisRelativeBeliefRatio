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
  if(input$finite_val_diag_case1 == 1){
    binormal_diag_prior(w = input$binormal_diag_prevalence_w,
                        nMonteprior = input$binormal_diag_nMonteCarlo, 
                        delta = input$binormal_diag_delta, 
                        lambda1 = input$binormal_diag_lambda1, 
                        lambda2 = input$binormal_diag_lambda2, 
                        mu0 = input$binormal_diag_mu0, 
                        tau0 = input$binormal_diag_tau0)
  } else if (input$finite_val_diag_case2 == "A" | input$finite_val_diag_case2 == "B"){ 
    binormal_diag_prior(alpha1w = input$binormal_diag_prevalence_alpha1w, 
                        alpha2w = input$binormal_diag_prevalence_alpha2w, 
                        nMonteprior = input$binormal_diag_nMonteCarlo, 
                        delta = input$binormal_diag_delta, 
                        lambda1 = input$binormal_diag_lambda1, 
                        lambda2 = input$binormal_diag_lambda2, 
                        mu0 = input$binormal_diag_mu0, 
                        tau0 = input$binormal_diag_tau0)
  }
})

sect3.3_AUC_post = reactive({
  if(input$finite_val_diag_case1 == 1){
    binormal_diag_post(w = input$binormal_diag_prevalence_w,
                       nMontepost = input$binormal_diag_nMonteCarlo, 
                       delta = input$binormal_diag_delta, 
                       lambda1post = sect3.3_hyperpara()$lambda1post, 
                       lambda2post = sect3.3_hyperpara()$lambda2post, 
                       mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                       mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                       tau0D = sect3.3_hyperpara()$tau0D, 
                       tau0ND = sect3.3_hyperpara()$tau0ND)
  } else if (input$finite_val_diag_case2 == "A"){ 
    binormal_diag_post(alpha1w = input$binormal_diag_prevalence_alpha1w, 
                       alpha2w = input$binormal_diag_prevalence_alpha2w, 
                       version = "prior",
                       nMontepost = input$binormal_diag_nMonteCarlo, 
                       delta = input$binormal_diag_delta, 
                       lambda1post = sect3.3_hyperpara()$lambda1post, 
                       lambda2post = sect3.3_hyperpara()$lambda2post, 
                       mu0Dpost = sect3.3_hyperpara()$mu0Dpost, 
                       mu0NDpost = sect3.3_hyperpara()$mu0NDpost, 
                       tau0D = sect3.3_hyperpara()$tau0D, 
                       tau0ND = sect3.3_hyperpara()$tau0ND)
  } else if (input$finite_val_diag_case2 == "B"){
    binormal_diag_post(alpha1w = input$binormal_diag_prevalence_alpha1w, 
                       alpha2w = input$binormal_diag_prevalence_alpha2w, 
                       nND = input$binormal_diag_nND, 
                       nD = input$binormal_diag_nD, 
                       version = "post",
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

sect3.3_AUC_RBR = reactive({
  binormal_diag_RBR(delta = input$binormal_diag_delta, 
                    probAUCprior = sect3.3_AUC_prior()$probAUCprior, 
                    probAUCpost = sect3.3_AUC_post()$probAUCpost,
                    priorAUC = sect3.3_AUC_prior()$priorAUC, 
                    postAUC = sect3.3_AUC_post()$postAUC,
                    priorcmod = sect3.3_AUC_prior()$priorcmod,
                    postcmod = sect3.3_AUC_post()$postcmod)
})
