################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

output$nonpara_bayes_hypoAUC_value = renderPrint({
  #nonpara_bayes_df()
  pr = sect3.4_AUC_RBR()$plausible_region
  pr = c(pr[1], pr[length(pr)])
  cr = sect3.4_cr()$credible_region
  cr = c(cr[1], cr[length(cr)])
  newlst = list("Prior Probability: P(AUC > 1/2)" = sect3.4_AUC_prior()$probAUCprior,
           "Posterior Probability: P(AUC > 1/2 | data) / Strength of the evidence" = sect3.4_AUC_post()$probAUCpost,
           "Relative Belief Ratio of AUC > 1/2" = sect3.4_AUC_RBR()$RBprobAUC,
           "Relative Belief Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(open_bracket_grid(input$nonpara_bayes_delta), sect3.4_AUC_RBR()$RB_AUC),
           "Plausible Region for the AUC" = pr,
           "Posterior Content of the Plausible Region for the AUC" = sect3.4_AUC_RBR()$postPl_AUC,
           "Credible region for the AUC" = sect3.4_cr()$credible_region
  )
  if(input$nonpara_bayes_DP_method == "epsilon"){
    a_df = list("Computed a" = sect3.4_a())
    c(a_df, newlst)
  } else {
    newlst
  }
})

output$nonpara_bayes_inf_opt_cutoff = renderPrint({
  temp_df = data.frame(sect3.4_AUC_RBR_error_char_copt()$FNRest,
                       sect3.4_AUC_RBR_error_char_copt()$FPRest,
                       sect3.4_AUC_RBR_error_char_copt()$Errorest,
                       sect3.4_AUC_RBR_error_char_copt()$FDRest,
                       sect3.4_AUC_RBR_error_char_copt()$FNDRest)
  colnames(temp_df) = c("FNRest", "FPRest", "Errorest", "FDRest", "FNDRest")
  newlst = list("Copt Estimate" = sect3.4_copt_est_hardcode(), #sect3.4_AUC_RBR_copt()$coptest,
       "Plausible Region for Copt" = sect3.4_AUC_RBR_copt()$copt_plausible_region,
       "Plausible Region for Cmod" = sect3.4_AUC_RBR_copt()$cmod_plausible_region,
       "Posterior Content of the Plausible Region for Copt" = sect3.4_AUC_RBR_copt()$postPlcopt,
       "Posterior Content of the Plausible Region for Cmod" = sect3.4_AUC_RBR_copt()$postPlcmod,
       "Error Characteristics" = temp_df
  #     "Plausible Region for Copt" = copt_transform(sect3.4_AUC_RBR_copt()$copt_plausible_region),
  #     "Credible Region for Copt" = copt_transform(sect3.4_cr_copt()$credible_region),
  #     "Cmod Estimate" = sect3.4_AUC_RBR_copt()$cmodest,
  #     "Credible Region for Cmod" = sect3.4_cr_copt()$credible_region,
  )
  if(input$nonpara_bayes_DP_method_alt == "epsilon" & input$nonpara_bayes_optimal_cutoff_denote_variables == "no"){
    a_df = list("Computed a" = sect3.4_a_copt())
    c(a_df, newlst)
  } else {
    newlst
  }
})


################################################################
# HISTOGRAMS                                                   #
################################################################

sect3.4_prior_post_lty = reactive({
  c(as.numeric(input$nonpara_bayes_lty_prior),
    as.numeric(input$nonpara_bayes_lty_post),
    as.numeric(input$nonpara_bayes_lty_pr),
    as.numeric(input$nonpara_bayes_lty_cr))
})

sect3.4_rbr_lty = reactive({
  c(as.numeric(input$nonpara_bayes_lty_rbr),
    as.numeric(input$nonpara_bayes_lty_pr),
    as.numeric(input$nonpara_bayes_lty_line_1),
    as.numeric(input$nonpara_bayes_lty_cr))
})

nonpara_bayes_lty_types_copt = reactive({
  c(as.numeric(input$nonpara_bayes_priorc_opt_label),
    as.numeric(input$nonpara_bayes_postc_opt_label),
    as.numeric(input$nonpara_bayes_rbrc_opt_label),
    as.numeric(input$nonpara_bayes_prc_opt_label),
    as.numeric(input$nonpara_bayes_line_1c_opt_label),
    as.numeric(input$nonpara_bayes_crc_opt_label))
})


# Denoting colours

nonpara_bayes_colours = reactive({
  # Total order of ALL colours: prior, posterior, relative belief ratio, 
  # plausible region, y = 1 line, credible region, 
  if(input$nonpara_bayes_colour == 'default1'){
    default1
  } else if(input$nonpara_bayes_colour == 'default2'){
    default2
  } else if (input$nonpara_bayes_colour == 'dull'){
    dull
  } else if (input$nonpara_bayes_colour == 'lovelymei'){
    lovelymei
  } else if (input$nonpara_bayes_colour == 'jackin'){
    jackin_execute
  } else if (input$nonpara_bayes_colour == 'manual'){
    c(convert_to_hex(input$nonpara_bayes_colour_prior),
      convert_to_hex(input$nonpara_bayes_colour_post),
      convert_to_hex(input$nonpara_bayes_colour_rbr),
      convert_to_hex(input$nonpara_bayes_colour_line_1),
      convert_to_hex(input$nonpara_bayes_colour_cr)
    )
  }
})

nonpara_bayes_copt_colours = reactive({
  if(input$nonpara_bayes_c_opt_carry_colour == 'default1'){
    default1
  } else if (input$nonpara_bayes_c_opt_carry_colour == 'default2'){
    default2
  } else if (input$nonpara_bayes_c_opt_carry_colour == 'dull'){
    dull
  } else if (input$nonpara_bayes_c_opt_carry_colour == 'lovelymei'){
    lovelymei
  } else if (input$nonpara_bayes_c_opt_carry_colour == 'jackin'){
    jackin_execute
  } else if (input$nonpara_bayes_c_opt_carry_colour == 'custom'){
    nonpara_bayes_colours()
  } else if (input$nonpara_bayes_c_opt_carry_colour == 'manual'){
    c(convert_to_hex(input$nonpara_bayes_priorc_opt_colour),
      convert_to_hex(input$nonpara_bayes_postc_opt_colour),
      convert_to_hex(input$nonpara_bayes_rbrc_opt_colour),
      convert_to_hex(input$nonpara_bayes_line_1c_opt_colour),
      convert_to_hex(input$nonpara_bayes_crc_opt_colour))
  }
})

nonpara_bayes_inferences_colours = reactive({
  if(input$nonpara_bayes_inferences_colour == 'default1'){
    default1[c(1, 2, 3)]
  } else if (input$nonpara_bayes_inferences_colour == 'default2'){
    default2[c(1, 2, 3)]
  } else if (input$nonpara_bayes_inferences_colour == 'dull'){
    dull[c(1, 2, 3)]
  } else if (input$nonpara_bayes_inferences_colour == 'lovelymei'){
    lovelymei[c(1, 2, 3)]
  } else if (input$nonpara_bayes_inferences_colour == 'jackin'){
    jackin_execute[c(1, 2, 3)]
  } else if (input$nonpara_bayes_inferences_colour == 'custom'){
    nonpara_bayes_colours()[c(1, 2, 3)]
  } else if (input$nonpara_bayes_inferences_colour == 'manual'){
    c(convert_to_hex(input$nonpara_bayes_colour_inferences_prior),
      convert_to_hex(input$nonpara_bayes_colour_inferences_post),
      convert_to_hex(input$nonpara_bayes_colour_inferences_rbr))
  }
})

# Determining the existence of a credible region

nonpara_bayes_cr_AUC = reactive({
  # For the plots, determines if there will be a credible region.
  if (check.numeric(input$nonpara_bayes_gamma) == TRUE){
    sect3.4_cr()$credible_region
  } else {
    FALSE
  }
})

nonpara_bayes_rb_line_AUC = reactive({
  # Assuming there will be a credible region, there will be a line associated
  # to make the graph.
  if (check.numeric(input$nonpara_bayes_gamma) == TRUE){
    sect3.4_cr()$rb_line
  } else {
    FALSE
  }
})

nonpara_bayes_pr_modified = reactive({
  pr = sect3.4_AUC_RBR()$plausible_region
  c(pr[1], pr[length(pr)])
})

nonpara_bayes_AUC_smoothing = reactive({
  list("priorAUCdensity" = average_vector_values(sect3.4_AUC_prior()$priorAUCdensity,
                                                 input$nonpara_bayes_smoother),
       "postAUCdensity" = average_vector_values(sect3.4_AUC_post()$postAUCdensity,
                                                input$nonpara_bayes_smoother),
       # not using this since it has already been smoothed
       "RB_AUC" = average_vector_values(sect3.4_AUC_RBR()$RB_AUC,
                                        input$nonpara_bayes_smoother))
})

output$nonpara_bayes_postprior_graph = renderPlot({
  nonpara_bayes_prior_post_graph(delta = input$nonpara_bayes_delta, 
                                 prior = nonpara_bayes_AUC_smoothing()$priorAUCdensity, #sect3.4_AUC_prior()$priorAUCdensity, 
                                 post = nonpara_bayes_AUC_smoothing()$postAUCdensity, #sect3.4_AUC_post()$postAUCdensity, 
                                 colour_choice = nonpara_bayes_colours()[c(1, 2)],
                                 lty_type = sect3.4_prior_post_lty(),
                                 transparency = input$nonpara_bayes_col_transparency)
})

output$nonpara_bayes_RB_graph = renderPlot({
  nonpara_bayes_rbr_graph(delta = input$nonpara_bayes_delta,
                          relative_belief_ratio = sect3.4_AUC_RBR()$RB_AUC, #nonpara_bayes_AUC_smoothing()$RB_AUC,
                          rb_line = nonpara_bayes_rb_line_AUC(),
                          colour_choice = nonpara_bayes_colours()[c(3:5)],
                          lty_type = sect3.4_rbr_lty(),
                          transparency = input$nonpara_bayes_col_transparency)
})

# FOR COPT ###################################

nonpara_bayes_pr_modified_cmod = reactive({
  pr = sect3.4_AUC_RBR_copt()$cmod_plausible_region
  c(pr[1], pr[length(pr)])
})

nonpara_bayes_pr_modified_copt = reactive({
  pr = sect3.4_AUC_RBR_copt()$copt_plausible_region
  c(pr[1], pr[length(pr)])
})

# Determining the existence of a credible region

nonpara_bayes_cr_AUC_copt = reactive({
  # For the plots, determines if there will be a credible region.
  if (check.numeric(input$nonpara_bayes_gamma_alt) == TRUE){
    sect3.4_cr_copt()$credible_region
  } else {
    FALSE
  }
})

nonpara_bayes_rb_line_AUC_copt = reactive({
  # Assuming there will be a credible region, there will be a line associated
  # to make the graph.
  if (check.numeric(input$nonpara_bayes_gamma_alt) == TRUE){
    sect3.4_cr_copt()$rb_line
  } else {
    FALSE
  }
})

nonpara_bayes_cr_AUC_cmod = reactive({
  # For the plots, determines if there will be a credible region.
  if (check.numeric(input$nonpara_bayes_gamma_alt) == TRUE){
    sect3.4_cr_cmod()$credible_region
  } else {
    FALSE
  }
})

nonpara_bayes_rb_line_AUC_cmod = reactive({
  # Assuming there will be a credible region, there will be a line associated
  # to make the graph.
  if (check.numeric(input$nonpara_bayes_gamma_alt) == TRUE){
    sect3.4_cr_cmod()$rb_line
  } else {
    FALSE
  }
})


nonpara_bayes_AUC_smoothing_copt = reactive({
  list("priorcoptmoddensity" = average_vector_values(sect3.4_AUC_prior_copt()$priorcoptmoddensity,
                                                     input$nonpara_bayes_smoother_copt),
       "postcoptmoddensity" = average_vector_values(sect3.4_AUC_post_copt()$postcoptmoddensity,
                                                    input$nonpara_bayes_smoother_copt),
       "priorcoptdensity" = average_vector_values(sect3.4_AUC_prior_copt()$priorcoptdensity,
                                                  input$nonpara_bayes_smoother_copt),
       "postcoptdensity" = average_vector_values(sect3.4_AUC_post_copt()$postcoptdensity,
                                                input$nonpara_bayes_smoother_copt))
  # Removed the following for now since the smoother is already applied:
      # "RBcoptmod" = average_vector_values(sect3.4_AUC_RBR_copt()$RBcoptmod,
      #                                     input$nonpara_bayes_smoother_copt),
      # "RBcopt" = average_vector_values(sect3.4_AUC_RBR_copt()$RBcopt,
      #                                  input$nonpara_bayes_smoother_copt))
})

output$nonpara_bayes_postprior_cmod_graph = renderPlot({
  if(input$nonpara_bayes_plot_type == "cmod"){
    nonpara_bayes_plots_AUC_copt(grid = sect3.4_AUC_prior_copt()$gridmod,
                                 prior = nonpara_bayes_AUC_smoothing_copt()$priorcoptmoddensity, 
                                 post = nonpara_bayes_AUC_smoothing_copt()$postcoptmoddensity,
                                 lty_type = nonpara_bayes_lty_types_copt(),
                                 colour_choice = nonpara_bayes_copt_colours(),
                                 transparency = input$nonpara_bayes_c_opt_col_transparency,
                                 x_title = "cmod")
  } else if (input$nonpara_bayes_plot_type == "copt"){
    nonpara_bayes_plots_AUC_copt(grid = sect3.4_AUC_prior_copt()$gridcopt,
                                 prior = nonpara_bayes_AUC_smoothing_copt()$priorcoptdensity,
                                 post = nonpara_bayes_AUC_smoothing_copt()$postcoptdensity,
                                 lty_type = nonpara_bayes_lty_types_copt(),
                                 colour_choice = nonpara_bayes_copt_colours(),
                                 transparency = input$nonpara_bayes_c_opt_col_transparency,
                                 x_title = "copt")
  }
})

output$nonpara_bayes_RB_cmod_graph = renderPlot({
  if(input$nonpara_bayes_plot_type == "cmod"){
    nonpara_bayes_plots_AUC_copt(grid = sect3.4_AUC_prior_copt()$gridmod,
                                 rbr = sect3.4_AUC_RBR_copt()$RBcoptmod, #nonpara_bayes_AUC_smoothing_copt()$RBcoptmod, 
                                 rb_line = nonpara_bayes_rb_line_AUC_cmod(),
                                 lty_type = nonpara_bayes_lty_types_copt(),
                                 colour_choice = nonpara_bayes_copt_colours(),
                                 transparency = input$nonpara_bayes_c_opt_col_transparency,
                                 x_title = "cmod")
  } else if (input$nonpara_bayes_plot_type == "copt"){
    nonpara_bayes_plots_AUC_copt(grid = sect3.4_AUC_prior_copt()$gridcopt,
                                 rbr = sect3.4_AUC_RBR_copt()$RBcopt, #nonpara_bayes_AUC_smoothing_copt()$RBcopt, 
                                 rb_line = nonpara_bayes_rb_line_AUC_copt(),
                                 lty_type = nonpara_bayes_lty_types_copt(),
                                 colour_choice = nonpara_bayes_copt_colours(),
                                 transparency = input$nonpara_bayes_c_opt_col_transparency,
                                 x_title = "copt")
  }
})


################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

nonpara_bayes_generate_dataframe = reactive({
  grid = open_bracket_grid(input$nonpara_bayes_delta)
  if (sect3.4_condition() == "conditional"){
    grid = grid[grid >= 0.5]
  }
  if (input$binormal_case == "equal_var"){
    df = data.frame(grid,
                    sect3.4_AUC_prior()$priorAUCdensity, 
                    sect3.4_AUC_post()$postAUCdensity, 
                    sect3.4_AUC_RBR()$RB_AUC)
  }
  colnames(df) = c("Grid point", "Prior of the AUC", "Posterior of the AUC", 
                   "Relative Belief Ratio of the AUC")
  df
})

output$nonpara_bayes_dataframe = renderDataTable({
  nonpara_bayes_generate_dataframe()
})

output$nonpara_bayes_downloadData = downloadHandler(
  filename = function() {
    paste(input$nonpara_bayes_filename, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(nonpara_bayes_download(), file, row.names = FALSE)
  }
)

# This is for the sample data

sample_data_modified = reactive({ 
  nondiseased = c(-0.11315894, 0.03273954, -0.69180664, -0.05459313, -1.22760962)
  diseased = c(0.8934581, -0.09544302, 1.52694609, 2.30531596, 0.45009081)
  df = data.frame(nondiseased, diseased)
  df
})

output$nonpara_bayes_sample = downloadHandler(
  filename = function() {
    # Use the selected dataset as the suggested file name
    "sample data for upload.csv"
  },
  content = function(file) {
    # Write the dataset to the `file` that will be downloaded
    write.csv(sample_data_modified(), file, row.names = FALSE)
  }
)

output$nonpara_bayes_sample_2 = downloadHandler(
  filename = function() {
    # Use the selected dataset as the suggested file name
    "sample data for upload.csv"
  },
  content = function(file) {
    # Write the dataset to the `file` that will be downloaded
    write.csv(sample_data_modified(), file, row.names = FALSE)
  }
)
