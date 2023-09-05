################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

binormal_diag_accepted_samples = reactive({
  if (sect3.3_condition() == "conditional" & input$binormal_case == "equal_var"){
    temp_df = data.frame(sect3.3_AUC_prior()$n_accepted,
                         sect3.3_AUC_post()$n_accepted)
  } else if (sect3.3_condition() == "conditional" & input$binormal_case == "unequal_var"){
    temp_df = data.frame(sect3.3_AUC_prior_unequal()$n_accepted,
                         sect3.3_AUC_post_unequal()$n_accepted)
  }
  colnames(temp_df) = c("Prior", " | Posterior")
  temp_df
})

output$binormal_diag_hypoAUC_value = renderPrint({
  if (sect3.3_condition() == "unconditional" & input$binormal_case == "equal_var"){
    list("Prior Probability: P(AUC > 1/2)" = sect3.3_AUC_prior()$probAUCprior,
         "Posterior Probability: P(AUC > 1/2 | data) / Strength of the evidence" = sect3.3_AUC_post()$probAUCpost,
         "Relative Belief Ratio of AUC > 1/2" = sect3.3_AUC_RBR()$RBprobAUC,
         "Relative Belief Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(open_bracket_grid(input$binormal_diag_delta), sect3.3_AUC_RBR()$RB_AUC),
         "Plausible Region for the AUC" = sect3.3_AUC_RBR()$plausible_region,
         "Posterior Content of the Plausible Region for the AUC" = sect3.3_AUC_RBR()$postPl_AUC,
         "Credible region for the AUC" = sect3.3_cr()$credible_region)
  } else if (sect3.3_condition() == "conditional" & input$binormal_case == "equal_var"){
    list("Relative Belief Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(open_bracket_grid(input$binormal_diag_delta), sect3.3_AUC_RBR()$RB_AUC, "conditional"),
         "Plausible Region for the AUC" = sect3.3_AUC_RBR()$plausible_region,
         "Posterior Content of the Plausible Region for the AUC" = sect3.3_AUC_RBR()$postPl_AUC,
         "Credible region for the AUC" = sect3.3_cr()$credible_region,
         "Accepted Monte Carlo Samples" = binormal_diag_accepted_samples())
  } else if (sect3.3_condition() == "unconditional" & input$binormal_case == "unequal_var"){
    list("Prior Probability: P(AUC > 1/2)" = sect3.3_AUC_prior_unequal()$probAUCprior,
         "Posterior Probability: P(AUC > 1/2 | data) / Strength of the evidence" = sect3.3_AUC_post_unequal()$probAUCpost,
         "Relative Belief Ratio of AUC > 1/2" = sect3.3_AUC_RBR_unequal()$RBprobAUC,
         "Relative Belief Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(open_bracket_grid(input$binormal_diag_delta), sect3.3_AUC_RBR_unequal()$RB_AUC),
         "Plausible Region for the AUC" = sect3.3_AUC_RBR_unequal()$plausible_region,
         "Posterior Content of the Plausible Region for the AUC" = sect3.3_AUC_RBR_unequal()$postPl_AUC,
         "Credible region for the AUC" = sect3.3_cr_unequal()$credible_region)
  } else if (sect3.3_condition() == "conditional" & input$binormal_case == "unequal_var"){
    list("Relative Belief Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(open_bracket_grid(input$binormal_diag_delta), sect3.3_AUC_RBR_unequal()$RB_AUC, "conditional"),
         "Plausible Region for the AUC" = sect3.3_AUC_RBR_unequal()$plausible_region,
         "Posterior Content of the Plausible Region for the AUC" = sect3.3_AUC_RBR_unequal()$postPl_AUC,
         "Credible region for the AUC" = sect3.3_cr_unequal()$credible_region,
         "Accepted Monte Carlo Samples" = binormal_diag_accepted_samples())
  }
})

output$binormal_diag_inf_opt_cutoff = renderPrint({
  # denoting the type of cutoff estimate
  if(input$binormal_optimal_cutoff_denote_copt == 'yes'){
    list1 = list("Specified Cutoff Estimate" = sect3.3_copt_est_hardcode())
  } else if (input$binormal_optimal_cutoff_denote_copt == 'no') {
    list1 = list("Cutoff Minimizing Error(c)" = sect3.3_copt_est_hardcode())
  } else if (input$binormal_optimal_cutoff_denote_copt == 'youden'){
    list1 = list("Cutoff Maximizing Youden's Index" = sect3.3_copt_est_hardcode())
  }
  if (sect3.3_copt_case() == "equal_var"){
    temp_df = data.frame(sect3.3_AUC_RBR_error_char_copt()$FNRest,
                         sect3.3_AUC_RBR_error_char_copt()$FPRest,
                         sect3.3_AUC_RBR_error_char_copt()$Errorest,
                         sect3.3_AUC_RBR_error_char_copt()$FDRest,
                         sect3.3_AUC_RBR_error_char_copt()$FNDRest)
    colnames(temp_df) = c("FNRest", "FPRest", "Errorest", "FDRest", "FNDRest")
    list2 = list(
         "Plausible Region for the Cutoff" = copt_transform(sect3.3_AUC_RBR_copt()$plausible_region),
         "Credible Region for the Cutoff" = copt_transform(sect3.3_cr_copt()$credible_region),
         "Error Characteristics" = temp_df
    )
  } else if (sect3.3_copt_case() == "unequal_var"){
    # BELOW REPRESENTS ACTUAL CODE
    temp_df = data.frame(sect3.3_AUC_RBR_error_char_copt_unequal()$FNRest,
                         sect3.3_AUC_RBR_error_char_copt_unequal()$FPRest,
                         sect3.3_AUC_RBR_error_char_copt_unequal()$Errorest,
                         sect3.3_AUC_RBR_error_char_copt_unequal()$FDRest,
                         sect3.3_AUC_RBR_error_char_copt_unequal()$FNDRest)
    colnames(temp_df) = c("FNRest", "FPRest", "Errorest", "FDRest", "FNDRest")
    list2 = list(
         "Plausible Region for the Cutoff" = copt_transform(sect3.3_AUC_RBR_copt_unequal()$plausible_region),
         "Credible Region for the Cutoff" = copt_transform(sect3.3_cr_copt_unequal()$credible_region),
         "Error Characteristics" = temp_df)
  }
  c(list1, list2)
})

################################################################
# HISTOGRAMS                                                   #
################################################################

# Denoting line type ###########################################

sect3.3_prior_post_lty = reactive({
  c(as.numeric(input$binormal_diag_lty_prior),
    as.numeric(input$binormal_diag_lty_post))
})

sect3.3_rbr_lty = reactive({
  c(as.numeric(input$binormal_diag_lty_rbr),
    as.numeric(input$binormal_diag_lty_line_1),
    as.numeric(input$binormal_diag_lty_cr))
})

binormal_diag_lty_types_copt = reactive({
  c(as.numeric(input$binormal_diag_priorc_opt_label),
    as.numeric(input$binormal_diag_postc_opt_label),
    as.numeric(input$binormal_diag_rbrc_opt_label),
    as.numeric(input$binormal_diag_line_1c_opt_label),
    as.numeric(input$binormal_diag_crc_opt_label))
})

binormal_diag_lty_types_cutoff = reactive({
  c(as.numeric(input$binormal_diag_prior_cutoff_label),
    as.numeric(input$binormal_diag_post_cutoff_label),
    as.numeric(input$binormal_diag_rbr_cutoff_label),
    as.numeric(input$binormal_diag_line_1_cutoff_label),
    as.numeric(input$binormal_diag_cr_cutoff_label))
})

# Denoting colours #############################################

binormal_diag_colours = reactive({
  # Total order of ALL colours: prior, posterior, relative belief ratio, 
  # plausible region, y = 1 line, credible region, 
  if(input$binormal_diag_colour == 'default1'){
    default1
  } else if(input$binormal_diag_colour == 'default2'){
    default2
  } else if (input$binormal_diag_colour == 'dull'){
    dull
  } else if (input$binormal_diag_colour == 'lovelymei'){
    lovelymei
  } else if (input$binormal_diag_colour == 'jackin'){
    jackin_execute
  } else if (input$binormal_diag_colour == 'manual'){
    c(convert_to_hex(input$binormal_diag_colour_prior),
      convert_to_hex(input$binormal_diag_colour_post),
      convert_to_hex(input$binormal_diag_colour_rbr),
      convert_to_hex(input$binormal_diag_colour_line_1),
      convert_to_hex(input$binormal_diag_colour_cr)
    )
  }
})

binormal_diag_copt_colours = reactive({
  if(input$binormal_diag_c_opt_carry_colour == 'default1'){
    default1
  } else if (input$binormal_diag_c_opt_carry_colour == 'default2'){
    default2
  } else if (input$binormal_diag_c_opt_carry_colour == 'dull'){
    dull
  } else if (input$binormal_diag_c_opt_carry_colour == 'lovelymei'){
    lovelymei
  } else if (input$binormal_diag_c_opt_carry_colour == 'jackin'){
    jackin_execute
  } else if (input$binormal_diag_c_opt_carry_colour == 'custom'){
    binormal_diag_colours()
  } else if (input$binormal_diag_c_opt_carry_colour == 'manual'){
    c(convert_to_hex(input$binormal_diag_priorc_opt_colour),
      convert_to_hex(input$binormal_diag_postc_opt_colour),
      convert_to_hex(input$binormal_diag_rbrc_opt_colour),
      convert_to_hex(input$binormal_diag_line_1c_opt_colour),
      convert_to_hex(input$binormal_diag_crc_opt_colour))
  }
})

binormal_diag_cutoff_colours = reactive({
  if(input$binormal_diag_cutoff_carry_colour == 'default1'){
    default1
  } else if (input$binormal_diag_cutoff_carry_colour == 'default2'){
    default2
  } else if (input$binormal_diag_cutoff_carry_colour == 'dull'){
    dull
  } else if (input$binormal_diag_cutoff_carry_colour == 'lovelymei'){
    lovelymei
  } else if (input$binormal_diag_cutoff_carry_colour == 'jackin'){
    jackin_execute
  } else if (input$binormal_diag_cutoff_carry_colour == 'custom'){
    binormal_diag_colours()
  } else if (input$binormal_diag_cutoff_carry_colour == 'manual'){
    c(convert_to_hex(input$binormal_diag_prior_cutoff_colour),
      convert_to_hex(input$binormal_diag_post_cutoff_colour),
      convert_to_hex(input$binormal_diag_rbr_cutoff_colour),
      convert_to_hex(input$binormal_diag_line_1_cutoff_colour),
      convert_to_hex(input$binormal_diag_cr_cutoff_colour))
  }
})

binormal_diag_inferences_colours = reactive({
  if(input$binormal_diag_inferences_colour == 'default1'){
    default1[c(1, 2, 3)]
  } else if (input$binormal_diag_inferences_colour == 'default2'){
    default2[c(1, 2, 3)]
  } else if (input$binormal_diag_inferences_colour == 'dull'){
    dull[c(1, 2, 3)]
  } else if (input$binormal_diag_inferences_colour == 'lovelymei'){
    lovelymei[c(1, 2, 3)]
  } else if (input$binormal_diag_inferences_colour == 'jackin'){
    jackin_execute[c(1, 2, 3)]
  } else if (input$binormal_diag_inferences_colour == 'custom'){
    binormal_diag_colours()[c(1, 2, 3)]
  } else if (input$binormal_diag_inferences_colour == 'manual'){
    c(convert_to_hex(input$binormal_diag_colour_inferences_prior),
      convert_to_hex(input$binormal_diag_colour_inferences_post),
      convert_to_hex(input$binormal_diag_colour_inferences_rbr))
  }
})

################################################################
# PLOTS FOR THE AUC                                            #
################################################################

# Determining the existence of a credible region ###############

binormal_diag_cr_AUC = reactive({
  # For the plots, determines if there will be a credible region.
  if (check.numeric(input$binormal_diag_gamma) == TRUE){
    sect3.3_cr()$credible_region
  } else {
    FALSE
  }
})

binormal_diag_rb_line_AUC = reactive({
  # Assuming there will be a credible region, there will be a line associated
  # to make the graph.
  if (check.numeric(input$binormal_diag_gamma) == TRUE){
    sect3.3_cr()$rb_line
  } else {
    FALSE
  }
})

binormal_diag_cr_AUC_unequal = reactive({
  # For the plots, determines if there will be a credible region.
  if (check.numeric(input$binormal_diag_gamma) == TRUE){
    sect3.3_cr_unequal()$credible_region
  } else {
    FALSE
  }
})

binormal_diag_rb_line_AUC_unequal = reactive({
  # Assuming there will be a credible region, there will be a line associated
  # to make the graph.
  if (check.numeric(input$binormal_diag_gamma) == TRUE){
    sect3.3_cr_unequal()$rb_line
  } else {
    FALSE
  }
})

# Plotting the AUC #############################################

output$binormal_diag_postprior_graph = renderPlot({
  if (input$binormal_case == "equal_var"){
    priorAUCdensity_smo = average_vector_values(sect3.3_AUC_prior()$priorAUCdensity, 
                                         input$binormal_diag_smoother)
    postAUCdensity_smo = average_vector_values(sect3.3_AUC_post()$postAUCdensity, 
                                        input$binormal_diag_smoother)
    binormal_diag_prior_post_graph(condition = sect3.3_condition(),
                                   delta = input$binormal_diag_delta, 
                                   prior = priorAUCdensity_smo, #sect3.3_AUC_prior()$priorAUCdensity, 
                                   post = postAUCdensity_smo, #sect3.3_AUC_post()$postAUCdensity, 
                                   colour_choice = binormal_diag_colours()[c(1, 2)],
                                   lty_type = sect3.3_prior_post_lty(),
                                   transparency = input$binormal_diag_col_transparency,
                                   legend_position = input$binormal_diag_AUC_legend_position)
  } else if (input$binormal_case == "unequal_var"){
    priorAUCdensity_smo = average_vector_values(sect3.3_AUC_prior_unequal()$priorAUCdensity, 
                                                input$binormal_diag_smoother)
    postAUCdensity_smo = average_vector_values(sect3.3_AUC_post_unequal()$postAUCdensity, 
                                               input$binormal_diag_smoother)
    binormal_diag_prior_post_graph(condition = sect3.3_condition(),
                                   delta = input$binormal_diag_delta, 
                                   prior = priorAUCdensity_smo, #sect3.3_AUC_prior_unequal()$priorAUCdensity, 
                                   post = postAUCdensity_smo, #sect3.3_AUC_post_unequal()$postAUCdensity, 
                                   colour_choice = binormal_diag_colours()[c(1, 2)],
                                   lty_type = sect3.3_prior_post_lty(),
                                   transparency = input$binormal_diag_col_transparency,
                                   legend_position = input$binormal_diag_AUC_legend_position)
  }
})

output$binormal_diag_RB_graph = renderPlot({
  if (input$binormal_case == "equal_var"){
    binormal_diag_rbr_graph(condition = sect3.3_condition(),
                            delta = input$binormal_diag_delta,
                            relative_belief_ratio = sect3.3_AUC_RBR()$RB_AUC, 
                            rb_line = binormal_diag_rb_line_AUC(),
                            colour_choice = binormal_diag_colours()[c(3:5)],
                            lty_type = sect3.3_rbr_lty(),
                            transparency = input$binormal_diag_col_transparency,
                            legend_position = input$binormal_diag_AUC_legend_position)
  } else if (input$binormal_case == "unequal_var"){
    binormal_diag_rbr_graph(condition = sect3.3_condition(),
                            delta = input$binormal_diag_delta,
                            relative_belief_ratio = sect3.3_AUC_RBR_unequal()$RB_AUC, 
                            rb_line = binormal_diag_rb_line_AUC_unequal(),
                            colour_choice = binormal_diag_colours()[c(3:5)],
                            lty_type = sect3.3_rbr_lty(),
                            transparency = input$binormal_diag_col_transparency,
                            legend_position = input$binormal_diag_AUC_legend_position)
  }
})

################################################################
# PLOTS FOR CMOD                                               #
################################################################

# Determining the existence of a credible region (cmod) ########

binormal_diag_cr_AUC_cmod = reactive({
  if (check.numeric(input$binormal_diag_gamma_copt) == TRUE){
    sect3.3_cr_copt()$credible_region
  } else {
    FALSE
  }
})

binormal_diag_rb_line_AUC_cmod = reactive({
  if (check.numeric(input$binormal_diag_gamma_copt) == TRUE){
    sect3.3_cr_copt()$rb_line
  } else {
    FALSE
  }
})

binormal_diag_cr_AUC_cmod_unequal = reactive({
  if (check.numeric(input$binormal_diag_gamma_copt) == TRUE){
    sect3.3_cr_copt_unequal()$credible_region
  } else {
    FALSE
  }
})

binormal_diag_rb_line_AUC_cmod_unequal = reactive({
  if (check.numeric(input$binormal_diag_gamma_copt) == TRUE){
    sect3.3_cr_copt_unequal()$rb_line
  } else {
    FALSE
  }
})

# Plots for cmod ###############################################

output$binormal_diag_postprior_cmod_graph = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    priorcmoddensity_smo = average_vector_values(sect3.3_AUC_prior_copt()$priorcmoddensity, 
                                                input$binormal_diag_smoother_copt)
    postcmoddensity_smo = average_vector_values(sect3.3_AUC_post_copt()$postcmoddensity, 
                                               input$binormal_diag_smoother_copt)
  } else if (sect3.3_copt_case() == "unequal_var"){
    priorcmoddensity_smo = average_vector_values(sect3.3_AUC_prior_copt_unequal()$priorcmoddensity, 
                                                 input$binormal_diag_smoother_copt)
    postcmoddensity_smo = average_vector_values(sect3.3_AUC_post_copt_unequal()$postcmoddensity, 
                                                input$binormal_diag_smoother_copt)
  }
  binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                               prior = priorcmoddensity_smo, #sect3.3_AUC_prior_copt_unequal()$priorcmoddensity, 
                               post = postcmoddensity_smo, #sect3.3_AUC_post_copt_unequal()$postcmoddensity,
                               lty_type = binormal_diag_lty_types_copt(),
                               colour_choice = binormal_diag_copt_colours(),
                               transparency = input$binormal_diag_c_opt_col_transparency,
                               main_title = "Plot of the Prior and the Posterior of Cmod",
                               legend_position = input$binormal_diag_c_opt_legend_position)
})

output$binormal_diag_RB_cmod_graph = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                                 RBR = sect3.3_AUC_RBR_copt()$RBcmod, 
                                 rb_line = binormal_diag_rb_line_AUC_cmod(),
                                 lty_type = binormal_diag_lty_types_copt(),
                                 colour_choice = binormal_diag_copt_colours(),
                                 transparency = input$binormal_diag_c_opt_col_transparency,
                                 main_title = "Plot of the Relative Belief Ratio of Cmod",
                                 legend_position = input$binormal_diag_c_opt_legend_position)
  } else if (sect3.3_copt_case() == "unequal_var"){
    binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                                 RBR = sect3.3_AUC_RBR_copt_unequal()$RBcmod, 
                                 rb_line = binormal_diag_rb_line_AUC_cmod_unequal(),
                                 lty_type = binormal_diag_lty_types_copt(),
                                 colour_choice = binormal_diag_copt_colours(),
                                 transparency = input$binormal_diag_c_opt_col_transparency,
                                 main_title = "Plot of the Relative Belief Ratio of Cmod",
                                 legend_position = input$binormal_diag_c_opt_legend_position)
  }
})

################################################################
# PLOTS FOR THE CUTOFF                                         #
################################################################

output$binormal_diag_postprior_cutoff_graph = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    priorcopt_smo = average_vector_values(sect3.3_AUC_prior_copt()$priorcopt, 
                                          input$binormal_diag_smoother_cutoff)
    postcopt_smo = average_vector_values(sect3.3_AUC_post_copt()$postcopt, 
                                         input$binormal_diag_smoother_cutoff)
  } else if (sect3.3_copt_case() == "unequal_var"){
    priorcopt_smo = average_vector_values(sect3.3_AUC_prior_copt_unequal()$priorcopt, 
                                          input$binormal_diag_smoother_cutoff)
    postcopt_smo = average_vector_values(sect3.3_AUC_post_copt_unequal()$postcopt, 
                                         input$binormal_diag_smoother_cutoff)
  }
  binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                               cutoff_plot = TRUE,
                               prior = priorcopt_smo,
                               post = postcopt_smo,
                               lty_type = binormal_diag_lty_types_cutoff(),
                               colour_choice = binormal_diag_cutoff_colours(),
                               transparency = input$binormal_diag_cutoff_col_transparency,
                               main_title = "Plot of the Prior and the Posterior of the Cutoff",
                               legend_position = input$binormal_diag_cutoff_legend_position)
})

output$binormal_diag_RB_cutoff_graph = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                                 cutoff_plot = TRUE,
                                 RBR = sect3.3_AUC_RBR_copt()$RBcutoff, 
                                 rb_line = binormal_diag_rb_line_AUC_cmod(),
                                 lty_type = binormal_diag_lty_types_cutoff(),
                                 colour_choice = binormal_diag_cutoff_colours(),
                                 transparency = input$binormal_diag_cutoff_col_transparency,
                                 main_title = "Plot of the Relative Belief Ratio of the Cutoff",
                                 legend_position = input$binormal_diag_cutoff_legend_position)
  } else if (sect3.3_copt_case() == "unequal_var"){
    binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                                 cutoff_plot = TRUE,
                                 RBR = sect3.3_AUC_RBR_copt_unequal()$RBcutoff, 
                                 rb_line = binormal_diag_rb_line_AUC_cmod_unequal(),
                                 lty_type = binormal_diag_lty_types_cutoff(),
                                 colour_choice = binormal_diag_cutoff_colours(),
                                 transparency = input$binormal_diag_cutoff_col_transparency,
                                 main_title = "Plot of the Relative Belief Ratio of the Cutoff",
                                 legend_position = input$binormal_diag_cutoff_legend_position)
  }
})


################################################################
# PLOTS FOR ERROR CHARACTERISTICS                              #
################################################################

output$binormal_diag_inf_opt_cutoff_plot1 = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    prior_vals_smo = average_vector_values(binormal_diag_err_char_plot_type()$prior, 
                                           input$binormal_diag_smoother_inferences)
    post_vals_smo = average_vector_values(binormal_diag_err_char_plot_type()$post, 
                                          input$binormal_diag_smoother_inferences)
    
    binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                                 prior_vals = prior_vals_smo, #binormal_diag_err_char_plot_type()$prior, 
                                 post_vals = post_vals_smo, #binormal_diag_err_char_plot_type()$post, 
                                 err_type = input$binormal_diag_inferences_plot_type, 
                                 prior_lty = as.numeric(input$binormal_diag_lty_inferences_prior), 
                                 post_lty = as.numeric(input$binormal_diag_lty_inferences_post),
                                 colour_choice = binormal_diag_inferences_colours(), 
                                 transparency = input$binormal_diag_inferences_col_transparency,
                                 legend_position = input$binormal_diag_inferences_legend_position)
  } else if (sect3.3_copt_case() == "unequal_var"){
    prior_vals_smo = average_vector_values(binormal_diag_err_char_plot_type_unequal()$prior, 
                                           input$binormal_diag_smoother_inferences)
    post_vals_smo = average_vector_values(binormal_diag_err_char_plot_type_unequal()$post, 
                                          input$binormal_diag_smoother_inferences)
    
    binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                                 prior_vals = prior_vals_smo, #binormal_diag_err_char_plot_type_unequal()$prior, 
                                 post_vals = post_vals_smo, #binormal_diag_err_char_plot_type_unequal()$post, 
                                 err_type = input$binormal_diag_inferences_plot_type, 
                                 prior_lty = as.numeric(input$binormal_diag_lty_inferences_prior), 
                                 post_lty = as.numeric(input$binormal_diag_lty_inferences_post),
                                 colour_choice = binormal_diag_inferences_colours(), 
                                 transparency = input$binormal_diag_inferences_col_transparency,
                                 legend_position = input$binormal_diag_inferences_legend_position)
  }
})

output$binormal_diag_inf_opt_cutoff_plot2 = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                                 rbr_vals = binormal_diag_err_char_plot_type()$RBR, 
                                 err_type = input$binormal_diag_inferences_plot_type, 
                                 rbr_lty = as.numeric(input$binormal_diag_lty_inferences_rbr),
                                 colour_choice = binormal_diag_inferences_colours(), 
                                 transparency = input$binormal_diag_inferences_col_transparency,
                                 legend_position = input$binormal_diag_inferences_legend_position)
  } else if (sect3.3_copt_case() == "unequal_var"){
    binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                                 rbr_vals = binormal_diag_err_char_plot_type_unequal()$RBR, 
                                 err_type = input$binormal_diag_inferences_plot_type, 
                                 rbr_lty = as.numeric(input$binormal_diag_lty_inferences_rbr),
                                 colour_choice = binormal_diag_inferences_colours(), 
                                 transparency = input$binormal_diag_inferences_col_transparency,
                                 legend_position = input$binormal_diag_inferences_legend_position)
  }
})

################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

binormal_diag_generate_dataframe = reactive({
  grid = open_bracket_grid(input$binormal_diag_delta)
  if (sect3.3_condition() == "conditional"){
    grid = grid[grid >= 0.5]
  }
  if (input$binormal_case == "equal_var"){
    df = data.frame(grid,
                    sect3.3_AUC_prior()$priorAUCdensity, 
                    sect3.3_AUC_post()$postAUCdensity, 
                    sect3.3_AUC_RBR()$RB_AUC)
  } else if (input$binormal_case == "unequal_var"){
    df = data.frame(grid,
                    sect3.3_AUC_prior_unequal()$priorAUCdensity, 
                    sect3.3_AUC_post_unequal()$postAUCdensity, 
                    sect3.3_AUC_RBR_unequal()$RB_AUC)
  }
  colnames(df) = c("Grid point", "Prior of the AUC", "Posterior of the AUC", 
                   "Relative Belief Ratio of the AUC")
  df
})

binormal_diag_download = reactive({
  binormal_diag_generate_dataframe()
})

output$binormal_diag_dataframe = renderDataTable({
  binormal_diag_download()
})

output$binormal_diag_downloadData = downloadHandler(
  filename = function() {
    paste(input$binormal_diag_filename, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(binormal_diag_download(), file, row.names = FALSE)
  }
)

################################################################
# DOWNLOADING SAMPLE DATA (EXAMPLE)                            #
################################################################

sample_data = reactive({ 
  nondiseased = c(-0.11315894, 0.03273954, -0.69180664, -0.05459313, -1.22760962)
  diseased = c(0.8934581, -0.09544302, 1.52694609, 2.30531596, 0.45009081)
  df = data.frame(nondiseased, diseased)
  df
})

output$binormal_diag_sample = downloadHandler(
  filename = function() {
    # Use the selected dataset as the suggested file name
    "sample data for upload.csv"
  },
  content = function(file) {
    # Write the dataset to the `file` that will be downloaded
    write.csv(sample_data(), file, row.names = FALSE)
  }
)

output$binormal_diag_sample_2 = downloadHandler(
  filename = function() {
    # Use the selected dataset as the suggested file name
    "sample data for upload.csv"
  },
  content = function(file) {
    # Write the dataset to the `file` that will be downloaded
    write.csv(sample_data(), file, row.names = FALSE)
  }
)

