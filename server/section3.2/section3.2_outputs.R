################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

output$finite_val_hypoAUC_value = renderPrint({
  if(input$finite_val_condition == "conditional"){
    list1 = list("Actual Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(closed_bracket_grid(input$finite_val_delta), sect3.2_AUC_RBR()$AUC_RBR))
    list3 = list("Plausible Region for the AUC" = sect3.2_pr()$plausible_region,
                 "Posterior Content of the Plausible Region for the AUC" = sect3.2_AUC_post_content(),
                 "Credible region for the AUC" = sect3.2_cr()$credible_region,
                 "Area Under the Line Plot (next section)" = sect3.2_lineplot_area())
    list2 = append(list1, list3)
    if(sect3.2_resample() == FALSE){ # this is if resampling is occuring 
      temp_df = data.frame(input$finite_val_nMonteCarlo - as.numeric(sect3.2_AUC_prior()$n_rejected), 
                           input$finite_val_nMonteCarlo - as.numeric(sect3.2_AUC_post()$n_rejected))
      colnames(temp_df) = c("Accepted Samples for the Prior", " | Accepted Samples for the Posterior")
      temp_list = list("Number of Samples that were accepted" = temp_df)
      list2 = append(list2, temp_list)
    }
  } else {
    list1 = list("Actual Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(closed_bracket_grid(input$finite_val_delta), sect3.2_AUC_RBR()$AUC_RBR))
    list3 = list("Plausible Region for the AUC" = sect3.2_pr()$plausible_region,
                 "Posterior Content of the Plausible Region for the AUC" = sect3.2_AUC_post_content(),
                 "Credible region for the AUC" = sect3.2_cr()$credible_region,
                 "Area Under the Line Plot (next section)" = sect3.2_lineplot_area())
    list2 = append(sect3.2_hypo_test(), list1)
    list2 = append(list2, list3)
  }
  list2
})

output$finite_val_optimal_cutoff = renderPrint({
  if(input$finite_val_cutoff_denote_copt == 'yes'){
    copt_estimate = input$finite_val_optimal_cutoff_copt
    list1 = list("Specified Cutoff Estimate" = copt_estimate)
    prior_diag_vals = sect3.2_AUC_prior_copt()$priorc_opt
    post_diag_vals = sect3.2_AUC_post_copt()$postc_opt
    pr_region = sect3.2_AUC_RBR_copt()$plausible_region
    posterior_content = sect3.2_AUC_post_copt()$postc_opt[input$finite_val_optimal_cutoff_copt]
    err_char = sect3.2_copt_est()
  } else if (input$finite_val_cutoff_denote_copt == 'no') {
    copt_estimate = sect3.2_AUC_RBR_copt()$c_optfDfND
    list1 = list("Cutoff Minimizing Error(c)" = copt_estimate)
    prior_diag_vals = sect3.2_AUC_prior_copt()$priorc_opt
    post_diag_vals = sect3.2_AUC_post_copt()$postc_opt
    pr_region = sect3.2_AUC_RBR_copt()$plausible_region
    posterior_content = sect3.2_AUC_RBR_copt()$posterior_content
    err_char = sect3.2_copt_est()
  } else if (input$finite_val_cutoff_denote_copt == 'youden'){
    copt_estimate = sect3.2_AUC_RBR_copt_youden()$c_optfDfND
    list1 = list("Cutoff Maximizing Youden's Index" = copt_estimate)
    prior_diag_vals = sect3.2_AUC_prior_copt_youden()$priorc_opt
    post_diag_vals = sect3.2_AUC_post_copt_youden()$postc_opt
    pr_region = sect3.2_AUC_RBR_copt_youden()$plausible_region
    posterior_content = sect3.2_AUC_RBR_copt_youden()$posterior_content
    err_char = sect3.2_copt_est_youden()
  } #else if (input$finite_val_cutoff_denote_copt == 'distance'){
    #copt_estimate = sect3.2_AUC_RBR_copt_closest()$c_optfDfND
    #list1 = list("Cutoff from the Closest to (0, 1) Estimate" = copt_estimate)
    #pr_region = sect3.2_AUC_RBR_copt_closest()$plausible_region
    #posterior_content = sect3.2_AUC_RBR_copt_closest()$posterior_content
  #}
  RBR_copt = c()
  for(i in 1:length(sect3.2_AUC_prior_copt()$priorc_opt)){
    if(sect3.2_AUC_prior_copt()$priorc_opt[i] > 0){
      RBR_copt = sect3.2_AUC_post_copt()$postc_opt/sect3.2_AUC_prior_copt()$priorc_opt 
    } else {
      NA
    }
  }
  list2 = list("Prior Probabilities for the Possible Diagnostic Values" = prior_diag_vals,
       "Posterior Probabilities for the Possible Diagnostic Values" = post_diag_vals,
       "Relative Belief Ratio Probabilities for the Possible Diagnostic Values" = RBR_copt)
  list3 = list("Plausible Region of the Cutoff" = pr_region,
       "Posterior Content of the Plausible Region of the Cutoff" = posterior_content,
       "Error Characteristics" = as.data.frame(err_char))
  c(list2, list1, list3)
})

################################################################
# HISTOGRAMS                                                   #
################################################################

finite_val_colours = reactive({
  # Total order of ALL colours: prior, posterior, relative belief ratio, 
  # plausible region, y = 1 line, credible region, 
  if(input$finite_val_colour == 'default1'){
    default1
  } else if (input$finite_val_colour == 'default2'){
    default2
  } else if (input$finite_val_colour == 'dull'){
    dull
  } else if (input$finite_val_colour == 'lovelymei'){
    lovelymei
  } else if (input$finite_val_colour == 'jackin'){
    jackin_execute
  } else if (input$finite_val_colour == 'manual'){
    c(convert_to_hex(input$finite_val_colour_prior),
      convert_to_hex(input$finite_val_colour_post),
      convert_to_hex(input$finite_val_colour_rbr),
      convert_to_hex(input$finite_val_colour_line_1),
      convert_to_hex(input$finite_val_colour_cr)
    )
  }
})

finite_val_copt_colours = reactive({
  if(input$finite_val_c_opt_carry_colour == 'default1'){
    default1
  } else if (input$finite_val_c_opt_carry_colour == 'default2'){
    default2
  } else if (input$finite_val_c_opt_carry_colour == 'dull'){
    dull
  } else if (input$finite_val_c_opt_carry_colour == 'lovelymei'){
    lovelymei
  } else if (input$finite_val_c_opt_carry_colour == 'jackin'){
    jackin_execute
  } else if (input$finite_val_c_opt_carry_colour == 'custom'){
    finite_val_colours()[c(1, 2, 3)]
  } else if (input$finite_val_c_opt_carry_colour == 'manual'){
    c(convert_to_hex(input$finite_val_priorc_opt_colour),
      convert_to_hex(input$finite_val_postc_opt_colour),
      convert_to_hex(input$finite_val_rbrc_opt_colour))
  }
})

sect3.2_prior_post_lty = reactive({
  c(as.numeric(input$finite_val_lty_prior),
    as.numeric(input$finite_val_lty_post))
})

sect3.2_rbr_lty = reactive({
  c(as.numeric(input$finite_val_lty_rbr),
    as.numeric(input$finite_val_lty_line_1),
    as.numeric(input$finite_val_lty_cr))
})

output$finite_val_postprior_graph = renderPlot({
  if(check.numeric(input$finite_val_gamma) == FALSE){
    density_hist_AUC_prior_post(delta = input$finite_val_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = finite_val_colours()[c(1, 2)],
                                lty_type = sect3.2_prior_post_lty(), 
                                transparency = input$finite_val_col_transparency)
    
  } else if (as.numeric(input$finite_val_gamma) >= sect3.2_AUC_post_content()){
    density_hist_AUC_prior_post(delta = input$finite_val_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = finite_val_colours()[c(1, 2)],
                                lty_type = sect3.2_prior_post_lty(), 
                                transparency = input$finite_val_col_transparency)
  } else {
    density_hist_AUC_prior_post(delta = input$finite_val_delta, 
                                AUC_prior = sect3.2_AUC_prior()$AUC, 
                                AUC_post = sect3.2_AUC_post()$AUC, 
                                densityplot = TRUE, 
                                showbars = showbarplots(),
                                colour_choice = finite_val_colours()[c(1, 2)],
                                lty_type = sect3.2_prior_post_lty(), 
                                transparency = input$finite_val_col_transparency)
  }
})

output$finite_val_RB_graph = renderPlot({
  if(check.numeric(input$finite_val_gamma) == FALSE){
    density_hist_AUC_RBR(delta = input$finite_val_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         densityplot = TRUE,
                         showbars = showbarplots(),
                         colour_choice = finite_val_colours()[c(3:5)],
                         lty_type = sect3.2_rbr_lty(), 
                         transparency = input$finite_val_col_transparency)
  } else if (as.numeric(input$finite_val_gamma) >= sect3.2_AUC_post_content()){
    density_hist_AUC_RBR(delta = input$finite_val_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         densityplot = TRUE,
                         showbars = showbarplots(),
                         colour_choice = finite_val_colours()[c(3:5)],
                         lty_type = sect3.2_rbr_lty(), 
                         transparency = input$finite_val_col_transparency)
  } else {
    density_hist_AUC_RBR(delta = input$finite_val_delta, 
                         AUC_RBR = sect3.2_AUC_RBR()$AUC_RBR, 
                         rb_line = sect3.2_cr()$rb_line,
                         densityplot = TRUE,
                         showbars = showbarplots(),
                         colour_choice = finite_val_colours()[c(3:5)],
                         lty_type = sect3.2_rbr_lty(), 
                         transparency = input$finite_val_col_transparency)
  }
})

output$finite_val_postprior_copt_graph = renderPlot({
  plots_AUC_copt(priorc_opt = sect3.2_AUC_prior_copt()$priorc_opt, 
                 postc_opt = sect3.2_AUC_post_copt()$postc_opt,
                 prior_label = as.numeric(input$finite_val_priorc_opt_label), 
                 post_label = as.numeric(input$finite_val_postc_opt_label),
                 colour_choice = finite_val_copt_colours())
})
output$finite_val_RB_copt_graph = renderPlot({
  plots_AUC_copt(RBc_opt = sect3.2_AUC_RBR_copt()$RBc_opt,
                 rb_label = as.numeric(input$finite_val_rbc_opt_label),
                 colour_choice = finite_val_copt_colours())
})


################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

finite_val_download = reactive({
  finite_val_generate_dataframe(input$finite_val_delta, 
                            sect3.2_AUC_prior()$AUC, 
                            sect3.2_AUC_post()$AUC, 
                            sect3.2_AUC_RBR()$AUC_RBR)
})

output$finite_val_dataframe = renderDataTable({
  finite_val_download()
})

output$finite_val_downloadData = downloadHandler(
  filename = function() {
    paste(input$finite_val_filename, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(finite_val_download(), file, row.names = FALSE)
  }
)
