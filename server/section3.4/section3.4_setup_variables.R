################################################################
# SETUP VARIABLES                                              #
################################################################

# minor variables (this is based on whether they want to carry on or use a previous selection)

# NOTE: the selection for which xData or xND data is within the other section.
sect3.4_condition = reactive({
  if(input$nonpara_bayes_condition == "uncond"){
    "unconditional"
  } else if (input$nonpara_bayes_condition == "cond"){
    "conditional"
  }
})

sect3.4_copt_epsilon = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_epsilon_alt
  } else {
    input$nonpara_bayes_epsilon
  }
})

sect3.4_copt_nstar = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_nstar_alt
  } else {
    input$nonpara_bayes_nstar
  }
})

sect3.4_copt_nMonteCarlo = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_nMonteCarlo_alt
  } else {
    input$nonpara_bayes_nMonteCarlo
  }
})

sect3.4_copt_delta = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_delta_alt
  } else {
    input$nonpara_bayes_delta
  }
})

sect3.4_copt_mu0 = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_mu0_alt
  } else {
    input$nonpara_bayes_mu0
  }
})

sect3.4_copt_tau0 = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_tau0_alt
  } else {
    input$nonpara_bayes_tau0
  }
})

sect3.4_copt_lambda1 = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_lambda1_alt
  } else {
    input$nonpara_bayes_lambda1
  }
})

sect3.4_copt_lambda2 = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_lambda2_alt
  } else {
    input$nonpara_bayes_lambda2
  }
})

sect3.4_copt_nND = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_nND_alt
  } else {
    input$nonpara_bayes_nND
  }
})

sect3.4_copt_nD = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_nD_alt
  } else {
    input$nonpara_bayes_nD
  }
})

sect3.4_copt_meanND = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_meanND_alt
  } else {
    input$nonpara_bayes_meanND
  }
})

sect3.4_copt_meanD = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_meanD_alt
  } else {
    input$nonpara_bayes_meanD
  }
})

sect3.4_copt_sND_squared = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_sND_squared_alt
  } else {
    input$nonpara_bayes_sND_squared
  }
})

sect3.4_copt_sD_squared = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    input$nonpara_bayes_sD_squared_alt
  } else {
    input$nonpara_bayes_sD_squared
  }
})

################################################################
# Setting up the data frame                                    #
################################################################

nonpara_bayes_df = reactive({
  # LOADING THE FILE
  #req(input$nonpara_bayes_csv)
  tryCatch(
    {
      df <- read.csv(input$nonpara_bayes_csv$datapath)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  nondiseased = df[, 1]
  nondiseased = nondiseased[!is.na(nondiseased)]
  diseased = df[, 2]
  diseased = diseased[!is.na(diseased)]
  
  list("nondiseased" = nondiseased, # switch this back, seeing if it is a bug...
       "diseased" = diseased)
})

nonpara_bayes_df_alt = reactive({
  # LOADING THE FILE
  #req(input$nonpara_bayes_csv)
  tryCatch(
    {
      df <- read.csv(input$nonpara_bayes_csv_alt$datapath)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  nondiseased = df[, 1]
  nondiseased = nondiseased[!is.na(nondiseased)]
  diseased = df[, 2]
  diseased = diseased[!is.na(diseased)]
  
  list("nondiseased" = nondiseased, # switch this back, seeing if it is a bug...
       "diseased" = diseased)
})

# Setting up which vector to use for the copt case
sect3.4_copt_nonpara_bayes_diseased_vector = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    nonpara_bayes_df_alt()$diseased
  } else {
    nonpara_bayes_df()$diseased
  }
})

sect3.4_copt_nonpara_bayes_nondiseased_vector = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    #nonpara_bayes_nondiseased_vector_alt
    nonpara_bayes_df_alt()$nondiseased
  } else {
    #nonpara_bayes_nondiseased_vector
    nonpara_bayes_df()$nondiseased
  }
})

################################################################
# COMPUTATION FOR YOUDEN'S                                     #
################################################################

sect3.4_AUC_prior_copt_youden = reactive({
  set.seed(SECT3.4_SEED_COPT()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$nonpara_bayes_case1 == 1){
    nonpara_bayes_AUC_prior_copt(w = 1/2,
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

sect3.4_AUC_post_copt_youden = reactive({
  set.seed(SECT3.4_SEED_COPT()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$nonpara_bayes_case1 == 1){
    nonpara_bayes_AUC_post_copt(w = 1/2, 
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

sect3.4_AUC_RBR_copt_youden = reactive({
  # smoothing the results
  priorcoptdensity_smo = average_vector_values(sect3.4_AUC_prior_copt_youden()$priorcoptdensity, 
                                               input$nonpara_bayes_smoother_copt)
  postcoptdensity_smo = average_vector_values(sect3.4_AUC_post_copt_youden()$postcoptdensity, 
                                              input$nonpara_bayes_smoother_copt)
  priorcoptmod_smo = average_vector_values(sect3.4_AUC_prior_copt_youden()$priorcoptmod, 
                                           input$nonpara_bayes_smoother_copt)
  postcoptmod_smo = average_vector_values(sect3.4_AUC_post_copt_youden()$postcoptmod, 
                                          input$nonpara_bayes_smoother_copt)
  nonpara_bayes_AUC_rbr_copt(delta = sect3.4_copt_delta(),
                             gridcopt = sect3.4_AUC_prior_copt_youden()$gridcopt, 
                             gridmod = sect3.4_AUC_prior_copt_youden()$gridmod,
                             priorcoptdensity = priorcoptdensity_smo, #sect3.4_AUC_prior_copt()$priorcoptdensity, 
                             postcoptdensity = postcoptdensity_smo, #sect3.4_AUC_post_copt()$postcoptdensity,
                             priorcopt = sect3.4_AUC_prior_copt_youden()$priorcopt, 
                             postcopt = sect3.4_AUC_post_copt_youden()$postcopt,
                             priorcoptmod = priorcoptmod_smo, #sect3.4_AUC_prior_copt()$priorcoptmod,
                             postcoptmod = postcoptmod_smo) #sect3.4_AUC_post_copt()$postcoptmod)
})

