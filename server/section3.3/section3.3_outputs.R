################################################################
# NUMERIC/TEXT OUTPUTS                                         #
################################################################

output$binormal_diag_hypoAUC_value = renderPrint({
  list("P(AUC > 1/2)" = sect3.3_AUC_prior()$probAUCprior,
       "P(AUC > 1/2 | data) / Strength of the evidence" = sect3.3_AUC_post()$probAUCpost,
       "Relative Belief Ratio of AUC > 1/2" = sect3.3_AUC_RBR()$RBprobAUC,
       "Actual Estimate of the AUC from the Relative Belief Ratio" = RBR_estimate_of_AUC(open_bracket_grid(input$binormal_diag_delta), sect3.3_AUC_RBR()$RB_AUC),
       "Plausible Region for the AUC" = sect3.3_AUC_RBR()$plausible_region,
       "Posterior Content of the Plausible Region for the AUC" = sect3.3_AUC_RBR()$postPl_AUC,
       "Credible region for the AUC" = sect3.3_cr()$credible_region)
})

output$binormal_diag_inf_opt_cutoff = renderPrint({
  temp_df = data.frame(sect3.3_AUC_RBR_error_char_copt()$FNRest,
                       sect3.3_AUC_RBR_error_char_copt()$FPRest,
                       sect3.3_AUC_RBR_error_char_copt()$Errorest,
                       sect3.3_AUC_RBR_error_char_copt()$FDRest,
                       sect3.3_AUC_RBR_error_char_copt()$FNDRest)
  colnames(temp_df) = c("FNRest", "FPRest", "Errorest", "FDRest", "FNDRest")
  list("Copt Estimate" = sect3.3_AUC_RBR_copt()$coptest,
       "Cmod Estimate" = sect3.3_AUC_RBR_copt()$cmodest,
       "Error Characteristics" = temp_df)
})

################################################################
# HISTOGRAMS                                                   #
################################################################

binormal_diag_colours = reactive({
  # Total order of ALL colours: prior, posterior, relative belief ratio, 
  # plausible region, y = 1 line, credible region, 
  if(input$binormal_diag_colour == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2", "#947aff", "#3333FF", "#5b10a7")
  } else if(input$binormal_diag_colour == 'default2'){
    c("blue", "green", "red", "#b3bfff", "royalblue1", "#81ddff")
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
    c("#FF6666", "#6699FF", "#05DEB2")
  } else if (input$binormal_diag_c_opt_carry_colour == 'default2'){
    c("blue", "green", "red", "#b3bfff")
  } else if (input$binormal_diag_c_opt_carry_colour == 'custom'){
    binormal_diag_colours()[c(1, 2, 3)]
  } else if (input$binormal_diag_c_opt_carry_colour == 'manual'){
    c(convert_to_hex(input$binormal_diag_priorc_opt_colour),
      convert_to_hex(input$binormal_diag_postc_opt_colour),
      convert_to_hex(input$binormal_diag_rbrc_opt_colour))
  }
})

binormal_diag_inferences_colours = reactive({
  if(input$binormal_diag_inferences_colour == 'default1'){
    c("#FF6666", "#6699FF", "#05DEB2")
  } else if (input$binormal_diag_inferences_colour == 'default2'){
    c("blue", "green", "red", "#b3bfff")
  } else if (input$binormal_diag_inferences_colour == 'custom'){
    binormal_diag_colours()[c(1, 2, 3)]
  } else if (input$binormal_diag_inferences_colour == 'manual'){
    c(convert_to_hex(input$binormal_diag_colour_inferences_prior),
      convert_to_hex(input$binormal_diag_colour_inferences_post),
      convert_to_hex(input$binormal_diag_colour_inferences_rbr))
  }
})

output$binormal_diag_postprior_graph = renderPlot({
  binormal_diag_prior_post_graph(delta = input$binormal_diag_delta, 
                                  prior = sect3.3_AUC_prior()$priorAUCdensity, 
                                  post = sect3.3_AUC_post()$postAUCdensity, 
                                  plausible_region = sect3.3_AUC_RBR()$plausible_region,
                                  colour_choice = binormal_diag_colours()[c(1, 2, 4, 6)],
                                  transparency = input$binormal_diag_col_transparency)
})

output$binormal_diag_RB_graph = renderPlot({
  binormal_diag_rbr_graph(delta = input$binormal_diag_delta,
                            relative_belief_ratio = sect3.3_AUC_RBR()$RB_AUC, 
                            plausible_region = sect3.3_AUC_RBR()$plausible_region,
                            colour_choice = binormal_diag_colours()[c(3:6)],
                            transparency = input$binormal_diag_col_transparency)
})

output$binormal_diag_postprior_copt_graph = renderPlot({
  binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                               priorcmoddensity = sect3.3_AUC_prior_copt()$priorcmoddensity, 
                               postcmoddensity = sect3.3_AUC_post_copt()$postcmoddensity,
                               prior_lty = as.numeric(input$binormal_diag_priorc_opt_label),
                               post_lty = as.numeric(input$binormal_diag_postc_opt_label),
                               colour_choice = binormal_diag_copt_colours(),
                               transparency = 0) #input$binormal_diag_c_opt_col_transparency)
})

output$binormal_diag_RB_copt_graph = renderPlot({
  binormal_diag_plots_AUC_copt(delta = sect3.3_copt_delta(),
                               RBcmod = sect3.3_AUC_RBR_copt()$RBcmod, 
                               rbr_lty = as.numeric(input$binormal_diag_rbc_opt_label),
                               colour_choice = binormal_diag_copt_colours(),
                               transparency = 0) #input$binormal_diag_c_opt_col_transparency)
})


# Note: colour situation here is temporary.
output$binormal_diag_inf_opt_cutoff_plot1 = renderPlot({
  binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                               prior_vals = binormal_diag_err_char_plot_type()$prior, 
                               post_vals = binormal_diag_err_char_plot_type()$post, 
                               err_type = input$binormal_diag_inferences_plot_type, 
                               prior_lty = 2, # temporary - should be changed 
                               post_lty = 1,  # temporary - should be changed
                               colour_choice = binormal_diag_inferences_colours(), # temp
                               transparency = 0) #input$binormal_diag_inferences_col_transparency)
})

output$binormal_diag_inf_opt_cutoff_plot2 = renderPlot({
  binormal_diag_err_char_plots(delta = sect3.3_copt_delta(),
                               rbr_vals = binormal_diag_err_char_plot_type()$RBR, 
                               err_type = input$binormal_diag_inferences_plot_type, 
                               rbr_lty = 6,   # temporary - should be changed
                               colour_choice = binormal_diag_inferences_colours(), # temp
                               transparency = 0) #input$binormal_diag_inferences_col_transparency) 
})



################################################################
# DOWNLOAD DATAFRAME                                           #
################################################################

binormal_diag_generate_dataframe = reactive({
  df = data.frame(open_bracket_grid(input$binormal_diag_delta),
                  sect3.3_AUC_prior()$priorAUCdensity, 
                  sect3.3_AUC_post()$postAUCdensity, 
                  sect3.3_AUC_RBR()$RB_AUC)
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
