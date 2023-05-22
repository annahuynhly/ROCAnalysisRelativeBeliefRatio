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

# Initializing the new entries
nonpara_bayes_new_vals_vector = reactive({
  convert_char_to_vector(input$nonpara_bayes_data_values)
})

# Initializing the table
nonpara_bayes_diseased_vector = reactiveValues(
  x = c(0.89345810, -0.09544302,  1.52694609,  2.30531596,  0.45009081,  0.97189716,
        0.85430995,  2.40987144,  1.44936186, -0.31305846,  0.19524931,  0.75202021,
        1.63136183,  1.31617751, -0.26481975,  1.69469220,  1.67520405,  1.50587628,
        -1.18927465,  1.75076313)
)
nonpara_bayes_nondiseased_vector = reactiveValues(
  x = c(-0.11315894,  0.03273954, -0.69180664, -0.05459313, -1.22760962, -0.25705819,
        -1.55799712, -0.34339482, -1.11229004,  0.11031882,  0.37785845,  0.76029521,
        -0.34052122,  1.03882232, -0.26665494,  0.48965747,  0.80441378, -1.31205550,
        -1.09934759,  1.55522803, -0.19981736,  0.51936199,  0.95234605,  1.56027376,
        -1.42501031)
)

# This is for adding the rows
observeEvent(input$nonpara_bayes_add_values_diseased, {
  nonpara_bayes_diseased_vector$x = c(nonpara_bayes_diseased_vector$x, 
                                      nonpara_bayes_new_vals_vector())
})

observeEvent(input$nonpara_bayes_add_values_nondiseased, {
  nonpara_bayes_nondiseased_vector$x = c(nonpara_bayes_nondiseased_vector$x, 
                                         nonpara_bayes_new_vals_vector())
})

# This is for removing the last entry
observeEvent(input$nonpara_bayes_reset_last_diseased, {
  nonpara_bayes_diseased_vector$x = nonpara_bayes_diseased_vector$x[-length(nonpara_bayes_diseased_vector$x)]
})

observeEvent(input$nonpara_bayes_reset_last_nondiseased, {
  nonpara_bayes_nondiseased_vector$x = nonpara_bayes_nondiseased_vector$x[-length(nonpara_bayes_nondiseased_vector$x)]
})

# This is for resetting the tables
observeEvent(input$nonpara_bayes_reset_diseased, {
  nonpara_bayes_diseased_vector$x = c()
})
observeEvent(input$nonpara_bayes_reset_nondiseased, {
  nonpara_bayes_nondiseased_vector$x = c()
})

output$nonpara_bayes_show_diseased_vec = renderPrint({
  nonpara_bayes_diseased_vector$x
})

output$nonpara_bayes_show_nondiseased_vec = renderPrint({
  nonpara_bayes_nondiseased_vector$x
})

################################################################
# Setting up the data frame - alternative case                 #
################################################################

nonpara_bayes_new_vals_vector_alt = reactive({
  convert_char_to_vector(input$nonpara_bayes_data_values_alt)
})

nonpara_bayes_diseased_vector_alt = reactiveValues(
  x = c(0.89345810, -0.09544302,  1.52694609,  2.30531596,  0.45009081,  0.97189716,
        0.85430995,  2.40987144,  1.44936186, -0.31305846,  0.19524931,  0.75202021,
        1.63136183,  1.31617751, -0.26481975,  1.69469220,  1.67520405,  1.50587628,
        -1.18927465,  1.75076313)
)
nonpara_bayes_nondiseased_vector_alt = reactiveValues(
  x = c(-0.11315894,  0.03273954, -0.69180664, -0.05459313, -1.22760962, -0.25705819,
        -1.55799712, -0.34339482, -1.11229004,  0.11031882,  0.37785845,  0.76029521,
        -0.34052122,  1.03882232, -0.26665494,  0.48965747,  0.80441378, -1.31205550,
        -1.09934759,  1.55522803, -0.19981736,  0.51936199,  0.95234605,  1.56027376,
        -1.42501031)
)

# This is for adding the rows
observeEvent(input$nonpara_bayes_add_values_diseased_alt, {
  nonpara_bayes_diseased_vector_alt$x = c(nonpara_bayes_diseased_vector_alt$x, 
                                          nonpara_bayes_new_vals_vector_alt())
})

observeEvent(input$nonpara_bayes_add_values_nondiseased_alt, {
  nonpara_bayes_nondiseased_vector_alt$x = c(nonpara_bayes_nondiseased_vector_alt$x, 
                                             nonpara_bayes_new_vals_vector_alt())
})

# This is for removing the last entry
observeEvent(input$nonpara_bayes_reset_last_diseased_alt, {
  nonpara_bayes_diseased_vector_alt$x = nonpara_bayes_diseased_vector_alt$x[-length(nonpara_bayes_diseased_vector_alt$x)]
})

observeEvent(input$nonpara_bayes_reset_last_nondiseased_alt, {
  nonpara_bayes_nondiseased_vector_alt$x = nonpara_bayes_nondiseased_vector_alt$x[-length(nonpara_bayes_nondiseased_vector_alt$x)]
})

# This is for resetting the tables
observeEvent(input$nonpara_bayes_reset_diseased_alt, {
  nonpara_bayes_diseased_vector_alt$x = c()
})
observeEvent(input$nonpara_bayes_reset_nondiseased_alt, {
  nonpara_bayes_nondiseased_vector_alt$x = c()
})

output$nonpara_bayes_show_diseased_vec_alt = renderPrint({
  nonpara_bayes_diseased_vector_alt$x
})

output$nonpara_bayes_show_nondiseased_vec_alt = renderPrint({
  nonpara_bayes_nondiseased_vector_alt$x
})

# Setting up which vector to use for the copt case
sect3.4_copt_nonpara_bayes_diseased_vector = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    nonpara_bayes_diseased_vector_alt
  } else {
    nonpara_bayes_diseased_vector
  }
})

sect3.4_copt_nonpara_bayes_nondiseased_vector = reactive({
  if(input$nonpara_bayes_optimal_cutoff_denote_variables == 'no'){
    nonpara_bayes_nondiseased_vector_alt
  } else {
    nonpara_bayes_nondiseased_vector
  }
})

################################################################
# copt - choosing between descriptive statistics vs raw data   #
################################################################

# I think it's supposed to be the opposite?
sect3.4_copt_xDdata_post = reactive({
  if(input$nonpara_bayes_data_method_alt == 1){
    NA
  } else if (input$nonpara_bayes_data_method_alt == 2){
    nonpara_bayes_diseased_vector_alt$x
  }
})

sect3.4_copt_xNDdata_post = reactive({
  if(input$nonpara_bayes_data_method_alt == 1){
    NA
  } else if (input$nonpara_bayes_data_method_alt == 2){
    nonpara_bayes_nondiseased_vector_alt$x
  }
})

sect3.4_copt_meanD_post = reactive({
  if(input$nonpara_bayes_data_method_alt == 1){
    sect3.4_copt_meanD()
  } else if (input$nonpara_bayes_data_method_alt == 2){
    NA
  }
})

sect3.4_copt_meanND_post = reactive({
  if(input$nonpara_bayes_data_method_alt == 1){
    sect3.4_copt_meanND()
  } else if (input$nonpara_bayes_data_method_alt == 2){
    NA
  }
})

sect3.4_copt_sD_squared_post = reactive({
  if(input$nonpara_bayes_data_method_alt == 1){
    sect3.4_copt_sD_squared()
  } else if (input$nonpara_bayes_data_method_alt == 2){
    NA
  }
})

sect3.4_copt_sND_squared_post = reactive({
  if(input$nonpara_bayes_data_method_alt == 1){
    sect3.4_copt_sND_squared()
  } else if (input$nonpara_bayes_data_method_alt == 2){
    NA
  }
})

