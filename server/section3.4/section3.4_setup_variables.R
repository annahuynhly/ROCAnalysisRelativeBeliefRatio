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

#sect3.4_copt_case = reactive({
#  # this is for variances equal or unequal 
#  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
#    input$binormal_case_alt
#  } else {
#    input$binormal_case
#  }
#})

#sect3.4_copt_lambda = reactive({
#  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
#    # note: this case likely shouldn't work since lambda seems to be for copt only.
#    input$nonpara_bayes_lambda
#  } else {
#    input$nonpara_bayes_lambda
#  }
#})


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


