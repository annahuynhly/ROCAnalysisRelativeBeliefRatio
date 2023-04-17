################################################################
# SETUP VARIABLES                                              #
################################################################

sect3.2_copt_nMonteCarlo = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_nMonteCarlo_alt
  } else {
    input$finite_val_nMonteCarlo
  }
})

sect3.2_copt_delta = reactive({
  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
    input$finite_val_delta_alt
  } else {
    input$finite_val_delta
  }
})

#sect3.2_copt_alpha_ND = reactive({
#  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
#    input$finite_val_alpha_ND_alt
#  } else {
#    input$finite_val_alpha_ND
#  }
#})

#sect3.2_copt_alpha_D = reactive({
#  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
#    input$finite_val_alpha_D_alt
#  } else {
#    input$finite_val_alpha_D
#  }
#})

#sect3.2_copt_nND = reactive({
#  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
#    input$finite_val_nND_alt
#  } else {
#    input$finite_val_nND
#  }
#})

#sect3.2_copt_nD = reactive({
#  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
#    input$finite_val_nD_alt
#  } else {
#    input$finite_val_nD
#  }
#})

#sect3.2_copt_fND = reactive({
#  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
#    input$finite_val_fND_alt
#  } else {
#    input$finite_val_fND
#  }
#})

#sect3.2_copt_fD = reactive({
#  if(input$finite_val_optimal_cutoff_denote_variables == 'no'){
#    input$finite_val_fD_alt
#  } else {
#    input$finite_val_fD
#  }
#})