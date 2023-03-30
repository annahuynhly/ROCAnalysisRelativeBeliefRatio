################################################################
# SETUP VARIABLES                                              #
################################################################

# minor variables (this is based on whether they want to carry on or use a previous selection)

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

sect3.3_copt_nND = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_nND_alt
  } else {
    input$binormal_diag_nND
  }
})

sect3.3_copt_nD = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_nD_alt
  } else {
    input$binormal_diag_nD
  }
})

sect3.3_copt_meanND = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_meanND_alt
  } else {
    input$binormal_diag_meanND
  }
})

sect3.3_copt_meanD = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_meanD_alt
  } else {
    input$binormal_diag_meanD
  }
})

sect3.3_copt_sND_squared = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_sND_squared_alt
  } else {
    input$binormal_diag_sND_squared
  }
})

sect3.3_copt_sD_squared = reactive({
  if(input$binormal_optimal_cutoff_denote_variables == 'no'){
    input$binormal_diag_sD_squared_alt
  } else {
    input$binormal_diag_sD_squared
  }
})





