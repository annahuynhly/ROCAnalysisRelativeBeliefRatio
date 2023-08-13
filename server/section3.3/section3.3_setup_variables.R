################################################################
# Setting up the data frame                                    #
################################################################

binormal_diag_df = reactive({
  # LOADING THE FILE
  #req(input$binormal_diag_csv)
  tryCatch(
    {
      df <- read.csv(input$binormal_diag_csv$datapath)
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
  
  # actually obtaining the data
  nD = length(diseased)
  nND = length(nondiseased)
  sD_squared = (nD - 1)*var(diseased)
  sND_squared = (nND - 1)*var(nondiseased)
  meanD = mean(diseased)
  meanND = mean(nondiseased)
  
  list("nD" = nD, "nND" = nND, "sD_squared" = sD_squared,
       "sND_squared" = sND_squared, "meanD" = meanD, "meanND" = meanND)
})

binormal_diag_df_alt = reactive({
  # LOADING THE FILE
  #req(input$binormal_diag_csv)
  tryCatch(
    {
      df <- read.csv(input$binormal_diag_csv_alt$datapath)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  
  # actually obtaining the data
  nD = length(diseased)
  nND = length(nondiseased)
  sD_squared = (nD - 1)*var(diseased)
  sND_squared = (nND - 1)*var(nondiseased)
  meanD = mean(diseased)
  meanND = mean(nondiseased)
  
  list("nD" = nD, "nND" = nND, "sD_squared" = sD_squared,
       "sND_squared" = sND_squared, "meanD" = meanD, "meanND" = meanND)
})

# Setting up which descriptive statistics to use

# For inferences for the AUC

sect3.3_AUC_nND = reactive({
  if(input$binormal_diag_data_method == 1){
    input$binormal_diag_nND
  } else if (input$binormal_diag_data_method == 2){
    binormal_diag_df()$nND
  }
})

sect3.3_AUC_nD = reactive({
  if(input$binormal_diag_data_method == 1){
    input$binormal_diag_nD
  } else if (input$binormal_diag_data_method == 2){
    binormal_diag_df()$nD
  }
})

sect3.3_AUC_meanND = reactive({
  if(input$binormal_diag_data_method == 1){
    input$binormal_diag_meanND
  } else if (input$binormal_diag_data_method == 2){
    binormal_diag_df()$meanND
  }
})

sect3.3_AUC_meanD = reactive({
  if(input$binormal_diag_data_method == 1){
    input$binormal_diag_meanD
  } else if (input$binormal_diag_data_method == 2){
    binormal_diag_df()$meanD
  }
})

sect3.3_AUC_sND_squared = reactive({
  if(input$binormal_diag_data_method == 1){
    input$binormal_diag_sND_squared
  } else if (input$binormal_diag_data_method == 2){
    binormal_diag_df()$sND_squared
  }
})

sect3.3_AUC_sD_squared = reactive({
  if(input$binormal_diag_data_method == 1){
    input$binormal_diag_sD_squared
  } else if (input$binormal_diag_data_method == 2){
    binormal_diag_df()$sD_squared
  }
})

# for Copt

sect3.3_copt_nND = reactive({
  if(input$binormal_diag_data_method_alt == 1){
    input$binormal_diag_nND_alt
  } else if (input$binormal_diag_data_method_alt == 2){
    binormal_diag_df_alt()$nND
  }
})

sect3.3_copt_nD = reactive({
  if(input$binormal_diag_data_method_alt == 1){
    input$binormal_diag_nD_alt
  } else if (input$binormal_diag_data_method_alt == 2){
    binormal_diag_df_alt()$nD
  }
})

sect3.3_copt_meanND = reactive({
  if(input$binormal_diag_data_method_alt == 1){
    input$binormal_diag_meanND_alt
  } else if (input$binormal_diag_data_method_alt == 2){
    binormal_diag_df_alt()$meanND
  }
})

sect3.3_copt_meanD = reactive({
  if(input$binormal_diag_data_method_alt == 1){
    input$binormal_diag_meanD_alt
  } else if (input$binormal_diag_data_method_alt == 2){
    binormal_diag_df_alt()$meanD
  }
})

sect3.3_copt_sND_squared = reactive({
  if(input$binormal_diag_data_method_alt == 1){
    input$binormal_diag_sND_squared_alt
  } else if (input$binormal_diag_data_method_alt == 2){
    binormal_diag_df_alt()$sND_squared
  }
})

sect3.3_copt_sD_squared = reactive({
  if(input$binormal_diag_data_method_alt == 1){
    input$binormal_diag_sD_squared_alt
  } else if (input$binormal_diag_data_method_alt == 2){
    binormal_diag_df_alt()$sD_squared
  }
})

################################################################
# SETUP VARIABLES                                              #
################################################################

# minor variables (this is based on whether they want to carry on or use a previous selection)

sect3.3_condition = reactive({
  if(input$binormal_diag_condition == "uncond"){
    "unconditional"
  } else if (input$binormal_diag_condition == "cond"){
    "conditional"
  }
})

sect3.3_copt_case = reactive({
  # this is for variances equal or unequal 
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_case_alt
  } else {
    input$binormal_case
  }
})

sect3.3_copt_lambda = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    # note: this case likely shouldn't work since lambda seems to be for copt only.
    input$binormal_diag_lambda
  } else {
    input$binormal_diag_lambda
  }
})

sect3.3_copt_nMonteCarlo = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_nMonteCarlo_alt
  } else {
    input$binormal_diag_nMonteCarlo
  }
})

sect3.3_copt_delta = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_delta_alt
  } else {
    input$binormal_diag_delta
  }
})

sect3.3_copt_mu0 = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_mu0_alt
  } else {
    input$binormal_diag_mu0
  }
})

sect3.3_copt_tau0 = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_tau0_alt
  } else {
    input$binormal_diag_tau0
  }
})

sect3.3_copt_lambda1 = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_lambda1_alt
  } else {
    input$binormal_diag_lambda1
  }
})

sect3.3_copt_lambda2 = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_lambda2_alt
  } else {
    input$binormal_diag_lambda2
  }
})

sect3.3_copt_nND_use = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    #input$binormal_diag_nND_alt
    sect3.3_copt_nND()
  } else {
    #input$binormal_diag_nND
    sect3.3_AUC_nND()
  }
})

sect3.3_copt_nD_use = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    #input$binormal_diag_nD_alt
    sect3.3_copt_nD()
  } else {
    #input$binormal_diag_nD
    sect3.3_AUC_nD()
  }
})

sect3.3_copt_meanND_use = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    #input$binormal_diag_meanND_alt
    sect3.3_copt_meanND()
  } else {
    #input$binormal_diag_meanND
    sect3.3_AUC_meanND()
  }
})

sect3.3_copt_meanD_use = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    #input$binormal_diag_meanD_alt
    sect3.3_copt_meanD()
  } else {
    #input$binormal_diag_meanD
    sect3.3_AUC_meanD()
  }
})

sect3.3_copt_sND_squared_use = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    #input$binormal_diag_sND_squared_alt
    sect3.3_copt_sND_squared()
  } else {
    #input$binormal_diag_sND_squared
    sect3.3_AUC_sND_squared()
  }
})

sect3.3_copt_sD_squared_use = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    #input$binormal_diag_sD_squared_alt
    sect3.3_copt_sD_squared()
  } else {
    #input$binormal_diag_sD_squared
    sect3.3_AUC_sD_squared()
  }
})

################################################################
# COMPUTATION FOR YOUDEN'S                                     #
################################################################

sect3.3_AUC_prior_copt_youden = reactive({
  set.seed(SECT3.3_SEED_COPT()) # SETTING THE SEED -> STARTING AT THE PRIOR CASE
  if(input$binormal_diag_case1 == 1){
    binormal_diag_prior_copt(w = 1/2,
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

sect3.3_AUC_post_copt_youden = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_post_copt(w = 1/2,
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

sect3.3_AUC_RBR_copt_youden = reactive({
  priorcmod_smo = average_vector_values(sect3.3_AUC_prior_copt_youden()$priorcmod, 
                                        input$binormal_diag_smoother_copt)
  priorcopt_smo = average_vector_values(sect3.3_AUC_prior_copt_youden()$priorcopt, 
                                        input$binormal_diag_smoother_cutoff)
  postcmod_smo = average_vector_values(sect3.3_AUC_post_copt_youden()$postcmod, 
                                       input$binormal_diag_smoother_copt)
  postcopt_smo = average_vector_values(sect3.3_AUC_post_copt_youden()$postcopt, 
                                       input$binormal_diag_smoother_cutoff)
  binormal_diag_RBR_copt(delta = sect3.3_copt_delta(), 
                         priorcmod = priorcmod_smo, #sect3.3_AUC_prior_copt()$priorcmod, 
                         postcmod = postcmod_smo, #sect3.3_AUC_post_copt()$postcmod)
                         priorcopt = priorcopt_smo,
                         postcopt = postcopt_smo)
})

# unequal case

sect3.3_AUC_prior_copt_youden_unequal = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_prior_copt_unequal(w = 1/2,
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

sect3.3_AUC_post_copt_youden_unequal = reactive({
  if(input$binormal_diag_case1 == 1){
    binormal_diag_post_copt_unequal(w = 1/2,
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

sect3.3_AUC_RBR_copt_youden_unequal = reactive({
  priorcmod_smo = average_vector_values(sect3.3_AUC_prior_copt_youden_unequal()$priorcmod, 
                                        input$binormal_diag_smoother_copt)
  priorcopt_smo = average_vector_values(sect3.3_AUC_prior_copt_youden_unequal()$priorcopt, 
                                        input$binormal_diag_smoother_cutoff)
  postcmod_smo = average_vector_values(sect3.3_AUC_post_copt_youden_unequal()$postcmod, 
                                       input$binormal_diag_smoother_copt)
  postcopt_smo = average_vector_values(sect3.3_AUC_post_copt_youden_unequal()$postcopt, 
                                       input$binormal_diag_smoother_cutoff)
  binormal_diag_RBR_copt(delta = sect3.3_copt_delta(), 
                         priorcmod = priorcmod_smo, #sect3.3_AUC_prior_copt_unequal()$priorcmod, 
                         postcmod = postcmod_smo, #sect3.3_AUC_post_copt_unequal()$postcmod)
                         priorcopt = priorcopt_smo,
                         postcopt = postcopt_smo)
})
