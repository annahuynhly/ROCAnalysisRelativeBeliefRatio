################################################################
# VARIABLES                                                    #
################################################################

# Setting the seed
SECT3.4_SEED = reactive(input$nonpara_bayes_seed)

# Getting the value for a... note: need to change this for the copt case
sect3.4_a = reactive({
  if(input$nonpara_bayes_DP_method == "aD"){
    input$nonpara_bayes_aD
  } else if (input$nonpara_bayes_DP_method == "epsilon"){
    dirichlet_process_a(input$nonpara_bayes_epsilon)$a
  }
})

sect3.4_a_copt = reactive({
  if(input$nonpara_bayes_DP_method_alt == "aD"){
    input$nonpara_bayes_aD_alt
  } else if (input$nonpara_bayes_DP_method_alt == "epsilon"){
    dirichlet_process_a(input$nonpara_bayes_epsilon_alt)$a
  }
})

sect3.4_AUC_prior = reactive({
  set.seed(SECT3.4_SEED()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  nonpara_bayes_AUC_prior(condition = input$nonpara_bayes_condition, 
                          nMonteprior = input$nonpara_bayes_nMonteCarlo, 
                          nstar = input$nonpara_bayes_nstar, 
                          a = sect3.4_a(), 
                          delta = input$nonpara_bayes_delta,
                          mu0 = input$nonpara_bayes_mu0, 
                          tau0 = input$nonpara_bayes_tau0, 
                          lambda1 = input$nonpara_bayes_lambda1, 
                          lambda2 = input$nonpara_bayes_lambda2)
}) 

sect3.4_AUC_post = reactive({
  set.seed(SECT3.4_SEED()) # SETTING THE SEED 
  # check whether or not setting the seed is necessary
  nonpara_bayes_AUC_post(condition = input$nonpara_bayes_condition, 
                         nMontepost = input$nonpara_bayes_nMonteCarlo, 
                         nstar = input$nonpara_bayes_nstar, 
                         a = sect3.4_a(), 
                         delta = input$nonpara_bayes_delta,
                         mu0 = input$nonpara_bayes_mu0, 
                         tau0 = input$nonpara_bayes_tau0, 
                         lambda1 = input$nonpara_bayes_lambda1, 
                         lambda2 = input$nonpara_bayes_lambda2, 
                         xDdata = nonpara_bayes_df()$diseased, #nonpara_bayes_diseased_vector$x, 
                         xNDdata = nonpara_bayes_df()$nondiseased, #nonpara_bayes_nondiseased_vector$x,
                         nD = NA, nND = NA, sD_squared = NA, sND_squared = NA, meanD = NA, meanND = NA)
})

sect3.4_AUC_RBR = reactive({
  # smoothing the results
  priorAUC_smo = average_vector_values(sect3.4_AUC_prior()$priorAUC, input$nonpara_bayes_smoother)
  postAUC_smo = average_vector_values(sect3.4_AUC_post()$postAUC, input$nonpara_bayes_smoother)
  nonpara_bayes_AUC_rbr(delta = input$nonpara_bayes_delta, 
                        probAUCprior = sect3.4_AUC_prior()$probAUCprior, 
                        probAUCpost = sect3.4_AUC_post()$probAUCpost,
                        priorAUC = priorAUC_smo, #sect3.4_AUC_prior()$priorAUC, 
                        postAUC = postAUC_smo) #sect3.4_AUC_post()$postAUC)
})

sect3.4_AUC_prior_copt = reactive({
  set.seed(SECT3.4_SEED()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$nonpara_bayes_case1 == 1){
    nonpara_bayes_AUC_prior_copt(w = input$nonpara_bayes_prevalence_w,
                                 alpha1w = NA, 
                                 alpha2w = NA,
                                 nMonteprior = sect3.4_copt_nMonteCarlo(), 
                                 nstar = sect3.4_copt_nstar(), 
                                 a = sect3.4_a_copt(), 
                                 delta = sect3.4_copt_delta(),
                                 mu0 = sect3.4_copt_mu0(), 
                                 tau0 = sect3.4_copt_tau0(), 
                                 lambda1 = sect3.4_copt_lambda1(), 
                                 lambda2 = sect3.4_copt_lambda2())
  } else if (input$nonpara_bayes_case1 == 2){
    nonpara_bayes_AUC_prior_copt(w = FALSE,
                                 alpha1w = input$nonpara_bayes_prevalence_alpha1w, 
                                 alpha2w = input$nonpara_bayes_prevalence_alpha2w,
                                 nMonteprior = sect3.4_copt_nMonteCarlo(), 
                                 nstar = sect3.4_copt_nstar(), 
                                 a = sect3.4_a_copt(), 
                                 delta = sect3.4_copt_delta(),
                                 mu0 = sect3.4_copt_mu0(), 
                                 tau0 = sect3.4_copt_tau0(), 
                                 lambda1 = sect3.4_copt_lambda1(), 
                                 lambda2 = sect3.4_copt_lambda2())
  }
})

sect3.4_AUC_post_copt = reactive({
  set.seed(SECT3.4_SEED()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$nonpara_bayes_case1 == 1){
    nonpara_bayes_AUC_post_copt(w = input$nonpara_bayes_prevalence_w, 
                                alpha1w = NA, 
                                alpha2w = NA,
                                nD = sect3.4_copt_nD(),
                                nND = sect3.4_copt_nND(),
                                version = NA,
                                nMontepost = sect3.4_copt_nMonteCarlo(), 
                                nstar = sect3.4_copt_nstar(), 
                                a = sect3.4_a_copt(), 
                                delta = sect3.4_copt_delta(),
                                mu0 = sect3.4_copt_mu0(), 
                                tau0 = sect3.4_copt_tau0(), 
                                lambda1 = sect3.4_copt_lambda1(), 
                                lambda2 = sect3.4_copt_lambda2(),
                                xDdata = sect3.4_copt_nonpara_bayes_diseased_vector(), 
                                xNDdata = sect3.4_copt_nonpara_bayes_nondiseased_vector(),
                                sD_squared = NA, sND_squared = NA, meanD = NA, meanND = NA)
  } else if (input$nonpara_bayes_case2 == "A"){
    nonpara_bayes_AUC_post_copt(w = FALSE, 
                                alpha1w = input$nonpara_bayes_prevalence_alpha1w, 
                                alpha2w = input$nonpara_bayes_prevalence_alpha2w,
                                nD = sect3.4_copt_nD(),
                                nND = sect3.4_copt_nND(),
                                version = "prior",
                                nMontepost = sect3.4_copt_nMonteCarlo(), 
                                nstar = sect3.4_copt_nstar(), 
                                a = sect3.4_a_copt(), 
                                delta = sect3.4_copt_delta(),
                                mu0 = sect3.4_copt_mu0(), 
                                tau0 = sect3.4_copt_tau0(), 
                                lambda1 = sect3.4_copt_lambda1(), 
                                lambda2 = sect3.4_copt_lambda2(),
                                xDdata = sect3.4_copt_nonpara_bayes_diseased_vector(), 
                                xNDdata = sect3.4_copt_nonpara_bayes_nondiseased_vector(),
                                sD_squared = NA, sND_squared = NA, meanD = NA, meanND = NA)
  } else if (input$nonpara_bayes_case2 == "B"){
    nonpara_bayes_AUC_post_copt(w = FALSE, 
                                alpha1w = input$nonpara_bayes_prevalence_alpha1w, 
                                alpha2w = input$nonpara_bayes_prevalence_alpha2w,
                                nD = sect3.4_copt_nD(),
                                nND = sect3.4_copt_nND(),
                                version = "post",
                                nMontepost = sect3.4_copt_nMonteCarlo(), 
                                nstar = sect3.4_copt_nstar(), 
                                a = sect3.4_a_copt(), 
                                delta = sect3.4_copt_delta(),
                                mu0 = sect3.4_copt_mu0(), 
                                tau0 = sect3.4_copt_tau0(), 
                                lambda1 = sect3.4_copt_lambda1(), 
                                lambda2 = sect3.4_copt_lambda2(),
                                xDdata = sect3.4_copt_nonpara_bayes_diseased_vector(), 
                                xNDdata = sect3.4_copt_nonpara_bayes_nondiseased_vector(),
                                sD_squared = NA, sND_squared = NA, meanD = NA, meanND = NA)
  }
})

sect3.4_AUC_RBR_copt = reactive({
  # smoothing the results
  priorcoptdensity_smo = average_vector_values(sect3.4_AUC_prior_copt()$priorcoptdensity, 
                                               input$nonpara_bayes_smoother_copt)
  postcoptdensity_smo = average_vector_values(sect3.4_AUC_post_copt()$postcoptdensity, 
                                               input$nonpara_bayes_smoother_copt)
  priorcoptmod_smo = average_vector_values(sect3.4_AUC_prior_copt()$priorcoptmod, 
                                           input$nonpara_bayes_smoother_copt)
  postcoptmod_smo = average_vector_values(sect3.4_AUC_post_copt()$postcoptmod, 
                                          input$nonpara_bayes_smoother_copt)
  nonpara_bayes_AUC_rbr_copt(delta = sect3.4_copt_delta(),
                             gridcopt = sect3.4_AUC_prior_copt()$gridcopt, 
                             gridmod = sect3.4_AUC_prior_copt()$gridmod,
                             priorcoptdensity = priorcoptdensity_smo, #sect3.4_AUC_prior_copt()$priorcoptdensity, 
                             postcoptdensity = postcoptdensity_smo, #sect3.4_AUC_post_copt()$postcoptdensity,
                             priorcopt = sect3.4_AUC_prior_copt()$priorcopt, 
                             postcopt = sect3.4_AUC_post_copt()$postcopt,
                             priorcoptmod = priorcoptmod_smo, #sect3.4_AUC_prior_copt()$priorcoptmod,
                             postcoptmod = postcoptmod_smo) #sect3.4_AUC_post_copt()$postcoptmod)
})

sect3.4_AUC_RBR_error_char_copt = reactive({
  nonpara_bayes_AUC_rbr_error_char_copt(grid = sect3.4_AUC_prior_copt()$gridcopt,
                                        priorFNR = sect3.4_AUC_prior_copt()$priorFNR, 
                                        priorFPR = sect3.4_AUC_prior_copt()$priorFPR, 
                                        priorError = sect3.4_AUC_prior_copt()$priorError, 
                                        priorFDR = sect3.4_AUC_prior_copt()$priorFDR, 
                                        priorFNDR = sect3.4_AUC_prior_copt()$priorFNDR, 
                                        postFNR = sect3.4_AUC_post_copt()$postFNR, 
                                        postFPR = sect3.4_AUC_post_copt()$postFPR, 
                                        postError = sect3.4_AUC_post_copt()$postError, 
                                        postFDR = sect3.4_AUC_post_copt()$postFDR, 
                                        postFNDR = sect3.4_AUC_post_copt()$postFNDR)
})

sect3.4_cr = reactive({
  # smoothing the results
  priorAUC_smo = average_vector_values(sect3.4_AUC_prior()$priorAUC, input$nonpara_bayes_smoother)
  postAUC_smo = average_vector_values(sect3.4_AUC_post()$postAUC, input$nonpara_bayes_smoother)
  nonpara_bayes_compute_credible_region(gamma = input$nonpara_bayes_gamma, 
                                        grid = open_bracket_grid(input$nonpara_bayes_delta), 
                                        AUC_RBR = sect3.4_AUC_RBR()$RB_AUC, 
                                        AUC_prior = priorAUC_smo, #sect3.4_AUC_prior()$priorAUC, 
                                        AUC_post = postAUC_smo, #sect3.4_AUC_post()$postAUC, 
                                        plausible_region = sect3.4_AUC_RBR()$plausible_region, 
                                        posterior_content = sect3.4_AUC_RBR()$postPl_AUC)
})

# Note: these credible regions are NOT within the code yet.

sect3.4_cr_copt = reactive({
  priorcopt_smo = average_vector_values(sect3.4_AUC_prior_copt()$priorcopt, 
                                               input$nonpara_bayes_smoother_copt)
  postcopt_smo = average_vector_values(sect3.4_AUC_post_copt()$postcopt, 
                                              input$nonpara_bayes_smoother_copt)
  nonpara_bayes_compute_credible_region(gamma = input$nonpara_bayes_gamma_alt, 
                                        grid = sect3.4_AUC_prior_copt()$gridcopt, 
                                        AUC_RBR = sect3.4_AUC_RBR_copt()$RBcopt, 
                                        AUC_prior = priorcopt_smo, #sect3.4_AUC_prior_copt()$priorcopt, 
                                        AUC_post =  postcopt_smo, #sect3.4_AUC_post_copt()$postcopt, 
                                        plausible_region = sect3.4_AUC_RBR_copt()$copt_plausible_region, 
                                        posterior_content = sect3.4_AUC_RBR_copt()$postPlcopt)
})

sect3.4_cr_cmod = reactive({
  priorcoptmod_smo = average_vector_values(sect3.4_AUC_prior_copt()$priorcoptmod, 
                                           input$nonpara_bayes_smoother_copt)
  postcoptmod_smo = average_vector_values(sect3.4_AUC_post_copt()$postcoptmod, 
                                          input$nonpara_bayes_smoother_copt)
  nonpara_bayes_compute_credible_region(gamma = input$nonpara_bayes_gamma_alt, 
                                        grid = sect3.4_AUC_prior_copt()$gridmod, 
                                        AUC_RBR = NA_to_0(sect3.4_AUC_RBR_copt()$RBcoptmod), 
                                        AUC_prior = sect3.4_AUC_prior_copt()$priorcoptmod, #priorcoptmod_smo, #sect3.4_AUC_prior_copt()$priorcoptmod, 
                                        AUC_post = sect3.4_AUC_post_copt()$postcoptmod, #postcoptmod_smo, #sect3.4_AUC_post_copt()$postcoptmod, 
                                        plausible_region = sect3.4_AUC_RBR_copt()$cmod_plausible_region, 
                                        posterior_content = sect3.4_AUC_RBR_copt()$postPlcmod)
})





