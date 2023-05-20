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
  # check whether or not setting the seed is necessary
  if(input$nonpara_bayes_data_method == 1){ # inserting descriptive statistics
    nonpara_bayes_AUC_post(condition = input$nonpara_bayes_condition, 
                           nMontepost = input$nonpara_bayes_nMonteCarlo, 
                           nstar = input$nonpara_bayes_nstar, 
                           a = sect3.4_a(), 
                           delta = input$nonpara_bayes_delta,
                           mu0 = input$nonpara_bayes_mu0, 
                           tau0 = input$nonpara_bayes_tau0, 
                           lambda1 = input$nonpara_bayes_lambda1, 
                           lambda2 = input$nonpara_bayes_lambda2, 
                           xDdata = NA, 
                           xNDdata = NA,
                           nD = input$nonpara_bayes_nD, 
                           nND = input$nonpara_bayes_nND, 
                           sD_squared = input$nonpara_bayes_sD_squared, 
                           sND_squared = input$nonpara_bayes_sND_squared, 
                           meanD = input$nonpara_bayes_meanD, 
                           meanND = input$nonpara_bayes_meanND)
  } else if (input$nonpara_bayes_data_method == 2){
    nonpara_bayes_AUC_post(condition = input$nonpara_bayes_condition, 
                           nMontepost = input$nonpara_bayes_nMonteCarlo, 
                           nstar = input$nonpara_bayes_nstar, 
                           a = sect3.4_a(), 
                           delta = input$nonpara_bayes_delta,
                           mu0 = input$nonpara_bayes_mu0, 
                           tau0 = input$nonpara_bayes_tau0, 
                           lambda1 = input$nonpara_bayes_lambda1, 
                           lambda2 = input$nonpara_bayes_lambda2, 
                           xDdata = nonpara_bayes_diseased_vector$x, 
                           xNDdata = nonpara_bayes_nondiseased_vector$x,
                           nD = NA, 
                           nND = NA, 
                           sD_squared = NA, 
                           sND_squared = NA, 
                           meanD = NA, 
                           meanND = NA)
  }
})

sect3.4_AUC_RBR = reactive({
  nonpara_bayes_AUC_rbr(delta = input$nonpara_bayes_delta, 
                        probAUCprior = sect3.4_AUC_prior()$probAUCprior, 
                        probAUCpost = sect3.4_AUC_post()$probAUCpost,
                        priorAUC = sect3.4_AUC_prior()$priorAUC, 
                        postAUC = sect3.4_AUC_post()$postAUC)
})

sect3.4_AUC_prior_copt = reactive({
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
    nonpara_bayes_AUC_prior_copt(w = NA,
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
                                xDdata = sect3.4_copt_xDdata_post(), 
                                xNDdata = sect3.4_copt_xNDdata_post(),
                                sD_squared = sect3.4_copt_sD_squared_post(), 
                                sND_squared = sect3.4_copt_sND_squared_post(), 
                                meanD = sect3.4_copt_meanD_post(), 
                                meanND = sect3.4_copt_meanND_post())
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
                                xDdata = sect3.4_copt_xDdata_post(), 
                                xNDdata = sect3.4_copt_xNDdata_post(),
                                sD_squared = sect3.4_copt_sD_squared_post(), 
                                sND_squared = sect3.4_copt_sND_squared_post(), 
                                meanD = sect3.4_copt_meanD_post(), 
                                meanND = sect3.4_copt_meanND_post())
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
                                xDdata = sect3.4_copt_xDdata_post(), 
                                xNDdata = sect3.4_copt_xNDdata_post(),
                                sD_squared = sect3.4_copt_sD_squared_post(), 
                                sND_squared = sect3.4_copt_sND_squared_post(), 
                                meanD = sect3.4_copt_meanD_post(), 
                                meanND = sect3.4_copt_meanND_post())
  }
})

sect3.4_AUC_RBR_copt = reactive({
  nonpara_bayes_AUC_rbr_copt(gridcopt = sect3.4_AUC_prior_copt()$gridcopt, 
                             priorcoptdensity = sect3.4_AUC_prior_copt()$priorcoptdensity, 
                             postcoptdensity = sect3.4_AUC_post_copt()$postcoptdensity,
                             priorcopt = sect3.4_AUC_prior_copt()$priorcopt, 
                             postcopt = sect3.4_AUC_post_copt()$postcopt)
})

sect3.4_cr = reactive({
  nonpara_bayes_compute_credible_region(gamma = input$nonpara_bayes_gamma, 
                                        delta = sect3.4_copt_delta(), 
                                        AUC_RBR = sect3.4_AUC_RBR()$RB_AUC, 
                                        AUC_prior = sect3.4_AUC_prior()$priorAUC, 
                                        AUC_post = sect3.4_AUC_post()$postAUC, 
                                        plausible_region = sect3.4_AUC_RBR()$plausible_region, 
                                        posterior_content = sect3.4_AUC_RBR()$postPl_AUC)
})


