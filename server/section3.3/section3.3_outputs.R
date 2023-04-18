################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

# numbers of samples that were accepted - seems like all are accepted now...?
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
    list("Relative Belief Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(open_bracket_grid(input$binormal_diag_delta), sect3.3_AUC_RBR()$RB_AUC),
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
    list("Relative Belief Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(open_bracket_grid(input$binormal_diag_delta), sect3.3_AUC_RBR_unequal()$RB_AUC),
         "Plausible Region for the AUC" = sect3.3_AUC_RBR_unequal()$plausible_region,
         "Posterior Content of the Plausible Region for the AUC" = sect3.3_AUC_RBR_unequal()$postPl_AUC,
         "Credible region for the AUC" = sect3.3_cr_unequal()$credible_region,
         "Accepted Monte Carlo Samples" = binormal_diag_accepted_samples())
  }
})

output$binormal_diag_inf_opt_cutoff = renderPrint({
  if (sect3.3_copt_case() == "equal_var"){
    temp_df = data.frame(sect3.3_AUC_RBR_error_char_copt()$FNRest,
                         sect3.3_AUC_RBR_error_char_copt()$FPRest,
                         sect3.3_AUC_RBR_error_char_copt()$Errorest,
                         sect3.3_AUC_RBR_error_char_copt()$FDRest,
                         sect3.3_AUC_RBR_error_char_copt()$FNDRest)
    colnames(temp_df) = c("FNRest", "FPRest", "Errorest", "FDRest", "FNDRest")
    list("Copt Estimate" = sect3.3_AUC_RBR_copt()$coptest,
         "Cmod Estimate" = sect3.3_AUC_RBR_copt()$cmodest,
         "Plausible Region for Cmod" = sect3.3_AUC_RBR_copt()$plausible_region,
         "Posterior Content of the Plausible Region for Cmod" = sect3.3_AUC_RBR_copt()$postPlcmod,
         "Credible region for Cmod" = sect3.3_cr_copt()$credible_region,
         "Error Characteristics" = temp_df)
  } else if (sect3.3_copt_case() == "unequal_var"){
    # BELOW REPRESENTS ACTUAL CODE
    temp_df = data.frame(sect3.3_AUC_RBR_error_char_copt_unequal()$FNRest,
                         sect3.3_AUC_RBR_error_char_copt_unequal()$FPRest,
                         sect3.3_AUC_RBR_error_char_copt_unequal()$Errorest,
                         sect3.3_AUC_RBR_error_char_copt_unequal()$FDRest,
                         sect3.3_AUC_RBR_error_char_copt_unequal()$FNDRest)
    colnames(temp_df) = c("FNRest", "FPRest", "Errorest", "FDRest", "FNDRest")
    list("Copt Estimate" = sect3.3_AUC_RBR_copt_unequal()$coptest,
         "Cmod Estimate" = sect3.3_AUC_RBR_copt_unequal()$cmodest,
         "Plausible Region for Cmod" = sect3.3_AUC_RBR_copt_unequal()$plausible_region,
         "Posterior Content of the Plausible Region for Cmod" = sect3.3_AUC_RBR_copt_unequal()$postPlcmod,
         "Credible region for Cmod" = sect3.3_cr_copt_unequal()$credible_region,
         "Error Characteristics" = temp_df)
  }
})

################################################################
# HISTOGRAMS                                                   #
################################################################

# Denoting line type (note: support for 1 function, for now)

sect3.3_prior_post_lty = reactive({
  c(as.numeric(input$binormal_diag_lty_prior),
    as.numeric(input$binormal_diag_lty_post),
    as.numeric(input$binormal_diag_lty_pr),
    as.numeric(input$binormal_diag_lty_cr))
})

sect3.3_rbr_lty = reactive({
  c(as.numeric(input$binormal_diag_lty_rbr),
    as.numeric(input$binormal_diag_lty_pr),
    as.numeric(input$binormal_diag_lty_line_1),
    as.numeric(input$binormal_diag_lty_cr))
})

binormal_diag_lty_types_copt = reactive({
  c(as.numeric(input$binormal_diag_priorc_opt_label),
    as.numeric(input$binormal_diag_postc_opt_label),
    as.numeric(input$binormal_diag_rbrc_opt_label),
    as.numeric(input$binormal_diag_prc_opt_label),
    as.numeric(input$binormal_diag_line_1c_opt_label),
    as.numeric(input$binormal_diag_crc_opt_label))
})

# Denoting colours

binormal_diag_colours = reactive({
  # Total order of ALL colours: prior, posterior, relative belief ratio, 
  # plausible region, y = 1 line, credible region, 
  if(input$binormal_diag_colour == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2", "#947aff", "#3333FF", "#5b10a7")
  } else if(input$binormal_diag_colour == 'default2'){
    c("blue", "green", "red", "#b3bfff", "royalblue1", "#81ddff")
  } else if (input$binormal_diag_colour == 'dull'){
    c("#EE4266", "#3cbbb1", "#b33c86", "#403f4c", "#0a0f0d", "#3185fc")
  } else if (input$binormal_diag_colour == 'lovelymei'){
    c("#3800c2", "#676bf8", "#58887a", "#e69eb7", "#372f66", "#a2cda3")
  } else if (input$binormal_diag_colour == 'manual'){
    c(convert_to_hex(input$binormal_diag_colour_prior),
      convert_to_hex(input$binormal_diag_colour_post),
      convert_to_hex(input$binormal_diag_colour_rbr),
      convert_to_hex(input$binormal_diag_colour_pr),
      convert_to_hex(input$binormal_diag_colour_line_1),
      convert_to_hex(input$binormal_diag_colour_cr)
    )
  }
})

binormal_diag_copt_colours = reactive({
  if(input$binormal_diag_c_opt_carry_colour == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2", "#947aff", "#3333FF", "#5b10a7")
  } else if (input$binormal_diag_c_opt_carry_colour == 'default2'){
    c("blue", "green", "red", "#b3bfff", "royalblue1", "#81ddff")
  } else if (input$binormal_diag_c_opt_carry_colour == 'dull'){
    c("#EE4266", "#3cbbb1", "#b33c86", "#403f4c", "#0a0f0d", "#3185fc")
  } else if (input$binormal_diag_c_opt_carry_colour == 'lovelymei'){
    c("#3800c2", "#676bf8", "#58887a", "#e69eb7", "#372f66", "#a2cda3")
  } else if (input$binormal_diag_c_opt_carry_colour == 'custom'){
    binormal_diag_colours()
  } else if (input$binormal_diag_c_opt_carry_colour == 'manual'){
    c(convert_to_hex(input$binormal_diag_priorc_opt_colour),
      convert_to_hex(input$binormal_diag_postc_opt_colour),
      convert_to_hex(input$binormal_diag_rbrc_opt_colour),
      convert_to_hex(input$binormal_diag_prc_opt_colour),
      convert_to_hex(input$binormal_diag_line_1c_opt_colour),
      convert_to_hex(input$binormal_diag_crc_opt_colour))
  }
})

binormal_diag_inferences_colours = reactive({
  if(input$binormal_diag_inferences_colour == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2")
  } else if (input$binormal_diag_inferences_colour == 'default2'){
    c("blue", "green", "red", "#b3bfff")
  } else if (input$binormal_diag_inferences_colour == 'dull'){
    c("#EE4266", "#3cbbb1", "#b33c86")
  } else if (input$binormal_diag_inferences_colour == 'lovelymei'){
    c("#3800c2", "#676bf8", "#58887a")
  } else if (input$binormal_diag_inferences_colour == 'custom'){
    binormal_diag_colours()[c(1, 2, 3)]
  } else if (input$binormal_diag_inferences_colour == 'manual'){
    c(convert_to_hex(input$binormal_diag_colour_inferences_prior),
      convert_to_hex(input$binormal_diag_colour_inferences_post),
      convert_to_hex(input$binormal_diag_colour_inferences_rbr))
  }
})

# Determining the existence of a credible region

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

# Outputting the graphs of the inferences for the AUC

output$binormal_diag_postprior_graph = renderPlot({
  if (input$binormal_case == "equal_var"){
    binormal_diag_prior_post_graph(condition = sect3.3_condition(),
                                   delta = input$binormal_diag_delta, 
                                   prior = sect3.3_AUC_prior()$priorAUCdensity, 
                                   post = sect3.3_AUC_post()$postAUCdensity, 
                                   plausible_region = sect3.3_AUC_RBR()$plausible_region,
                                   credible_region = binormal_diag_cr_AUC(),
                                   colour_choice = binormal_diag_colours()[c(1, 2, 4, 6)],
                                   lty_type = sect3.3_prior_post_lty(),
                                   transparency = input$binormal_diag_col_transparency)
  } else if (input$binormal_case == "unequal_var"){
    binormal_diag_prior_post_graph(condition = sect3.3_condition(),
                                   delta = input$binormal_diag_delta, 
                                   prior = sect3.3_AUC_prior_unequal()$priorAUCdensity, 
                                   post = sect3.3_AUC_post_unequal()$postAUCdensity, 
                                   plausible_region = sect3.3_AUC_RBR_unequal()$plausible_region,
                                   credible_region = binormal_diag_cr_AUC(),
                                   colour_choice = binormal_diag_colours()[c(1, 2, 4, 6)],
                                   lty_type = sect3.3_prior_post_lty(),
                                   transparency = input$binormal_diag_col_transparency) 
  }
})

output$binormal_diag_RB_graph = renderPlot({
  if (input$binormal_case == "equal_var"){
    binormal_diag_rbr_graph(condition = sect3.3_condition(),
                            delta = input$binormal_diag_delta,
                            relative_belief_ratio = sect3.3_AUC_RBR()$RB_AUC, 
                            plausible_region = sect3.3_AUC_RBR()$plausible_region,
                            credible_region = binormal_diag_cr_AUC(),
                            rb_line = binormal_diag_rb_line_AUC(),
                            colour_choice = binormal_diag_colours()[c(3:6)],
                            lty_type = sect3.3_rbr_lty(),
                            transparency = input$binormal_diag_col_transparency)
  } else if (input$binormal_case == "unequal_var"){
    binormal_diag_rbr_graph(condition = sect3.3_condition(),
                            delta = input$binormal_diag_delta,
                            relative_belief_ratio = sect3.3_AUC_RBR_unequal()$RB_AUC, 
                            plausible_region = sect3.3_AUC_RBR_unequal()$plausible_region,
                            credible_region = binormal_diag_cr_AUC(),
                            rb_line = binormal_diag_rb_line_AUC(),
                            colour_choice = binormal_diag_colours()[c(3:6)],
                            lty_type = sect3.3_rbr_lty(),
                            transparency = input$binormal_diag_col_transparency)
  }
})

# Determining the existence of a credible region (for the cmod plots)

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

# Plots for cmod
output$binormal_diag_postprior_cmod_graph = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                                 priorcmoddensity = sect3.3_AUC_prior_copt()$priorcmoddensity, 
                                 postcmoddensity = sect3.3_AUC_post_copt()$postcmoddensity,
                                 plausible_region = sect3.3_AUC_RBR_copt()$plausible_region,
                                 credible_region = binormal_diag_cr_AUC_cmod(),
                                 lty_type = binormal_diag_lty_types_copt(),
                                 colour_choice = binormal_diag_copt_colours(),
                                 transparency = input$binormal_diag_c_opt_col_transparency)
  } else if (sect3.3_copt_case() == "unequal_var"){
    binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                                 priorcmoddensity = sect3.3_AUC_prior_copt_unequal()$priorcmoddensity, 
                                 postcmoddensity = sect3.3_AUC_post_copt_unequal()$postcmoddensity,
                                 plausible_region = sect3.3_AUC_RBR_copt()$plausible_region,
                                 credible_region = binormal_diag_cr_AUC_cmod(),
                                 lty_type = binormal_diag_lty_types_copt(),
                                 colour_choice = binormal_diag_copt_colours(),
                                 transparency = input$binormal_diag_c_opt_col_transparency)
  }
  
})

output$binormal_diag_RB_cmod_graph = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                                 RBcmod = sect3.3_AUC_RBR_copt()$RBcmod, 
                                 plausible_region = sect3.3_AUC_RBR_copt()$plausible_region,
                                 credible_region = binormal_diag_cr_AUC_cmod(),
                                 rb_line = binormal_diag_rb_line_AUC_cmod(),
                                 lty_type = binormal_diag_lty_types_copt(),
                                 colour_choice = binormal_diag_copt_colours(),
                                 transparency = input$binormal_diag_c_opt_col_transparency)
  } else if (sect3.3_copt_case() == "unequal_var"){
    binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                                 RBcmod = sect3.3_AUC_RBR_copt_unequal()$RBcmod, 
                                 plausible_region = sect3.3_AUC_RBR_copt()$plausible_region,
                                 credible_region = binormal_diag_cr_AUC_cmod(),
                                 rb_line = binormal_diag_rb_line_AUC_cmod(),
                                 lty_type = binormal_diag_lty_types_copt(),
                                 colour_choice = binormal_diag_copt_colours(),
                                 transparency = input$binormal_diag_c_opt_col_transparency)
  }
})

# Plots for error characteristics
# Note: colour situation here is temporary -- needs to be modified!!
output$binormal_diag_inf_opt_cutoff_plot1 = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                                 prior_vals = binormal_diag_err_char_plot_type()$prior, 
                                 post_vals = binormal_diag_err_char_plot_type()$post, 
                                 err_type = input$binormal_diag_inferences_plot_type, 
                                 prior_lty = as.numeric(input$binormal_diag_lty_inferences_prior), 
                                 post_lty = as.numeric(input$binormal_diag_lty_inferences_post),
                                 colour_choice = binormal_diag_inferences_colours(), 
                                 transparency = input$binormal_diag_inferences_col_transparency)
  } else if (sect3.3_copt_case() == "unequal_var"){
    binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                                 prior_vals = binormal_diag_err_char_plot_type_unequal()$prior, 
                                 post_vals = binormal_diag_err_char_plot_type_unequal()$post, 
                                 err_type = input$binormal_diag_inferences_plot_type, 
                                 prior_lty = as.numeric(input$binormal_diag_lty_inferences_prior), 
                                 post_lty = as.numeric(input$binormal_diag_lty_inferences_post),
                                 colour_choice = binormal_diag_inferences_colours(), 
                                 transparency = input$binormal_diag_inferences_col_transparency)
  }
})

output$binormal_diag_inf_opt_cutoff_plot2 = renderPlot({
  if (sect3.3_copt_case() == "equal_var"){
    binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                                 rbr_vals = binormal_diag_err_char_plot_type()$RBR, 
                                 err_type = input$binormal_diag_inferences_plot_type, 
                                 rbr_lty = as.numeric(input$binormal_diag_lty_inferences_rbr),
                                 colour_choice = binormal_diag_inferences_colours(), 
                                 transparency = input$binormal_diag_inferences_col_transparency)
  } else if (sect3.3_copt_case() == "unequal_var"){
    binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                                 rbr_vals = binormal_diag_err_char_plot_type_unequal()$RBR, 
                                 err_type = input$binormal_diag_inferences_plot_type, 
                                 rbr_lty = as.numeric(input$binormal_diag_lty_inferences_rbr),
                                 colour_choice = binormal_diag_inferences_colours(), 
                                 transparency = input$binormal_diag_inferences_col_transparency)
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
